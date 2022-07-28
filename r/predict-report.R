#!/usr/bin/env Rscript
# Generate model prediction report
# example: Rscript predict-report.R

right_now <- lubridate::now(tzone = "US/Eastern")
cat("Booting:", paste0(right_now), "\n")

cat("Loading packages...")
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(pROC))
suppressPackageStartupMessages(library(uuid))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(RPostgreSQL))
cat("done\n")

theme_set(theme_bw())
theme_update(
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = 12)
)

updated_at <- paste0("Updated: ", with_tz(now(), "America/New_York"))

# setup -------------------------------------------------------------------

source("functions.R")

config <- fromJSON("../config/index.json")

con <- dbConnect(PostgreSQL(), dbname = config$db$database, host = config$db$host, user = config$db$user, password = config$db$password)

cat("Fetching data...")
db_predict <- suppressWarnings(tbl(con, "predictions") %>%
  select(id, created_at, name, uuid, timestamp, prob, exceedance, predictors) %>%
  arrange(name, timestamp) %>%
  collect())
cat("done\n")

DBI::dbDisconnect(con)

# extract predictors
df_predict <- db_predict %>%
  mutate(
    predictors = map(predictors, function (x) {
      as_tibble(fromJSON(x)) %>%
        gather() %>%
        mutate(value = as.numeric(value))
    })
  )

df_prob <- df_predict %>%
  select(name, timestamp, exceedence_probability = prob) %>%
  gather(key, value, -name, -timestamp)
df_predictors <- df_predict %>%
  select(name, timestamp, predictors) %>%
  unnest(predictors)

df_predict <- bind_rows(
  df_prob,
  df_predictors
) %>%
  mutate(key = fct_inorder(key))

# model data --------------------------------------------------------------

df_models <- load_models("models.rds")

df_models_train <- df_models %>%
  select(df_train) %>%
  unnest(df_train)

df_models_names <- df_models %>%
  select(model_id, model) %>%
  mutate(
    predictor_names = map(model, ~ .x$finalModel$xNames)
  ) %>%
  select(-model)

df_models_predictor_ranges <- df_models_train %>%
  gather("key", "value", temp:alewife_logflow_p5d) %>%
  nest_by(model_id) %>%
  left_join(df_models_names, by = "model_id") %>%
  mutate(
    data = list(filter(data, key %in% predictor_names)),
    predictor_ranges = list({
      data %>%
        group_by(key) %>%
        summarise(min = min(value), median = median(value), mean = mean(value), max = max(value))
    })
  ) %>%
  select(model_id, predictor_ranges) %>%
  unnest(predictor_ranges) %>%
  ungroup()

# pdf ---------------------------------------------------------------------

fname <- "predictions.pdf"
cat("Generating PDF (", fname, ")...")

pdf(file.path("pdf", fname), width = 11, height = 8.5)

p <- df_predict %>%
  filter(key == "exceedence_probability") %>%
  ggplot(aes(timestamp, value)) +
  geom_line() +
  geom_hline(aes(yintercept = 0.2, linetype = "Cutoff")) +
  scale_x_datetime(expand = expansion()) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_linetype_manual("", values = "dashed") +
  facet_wrap(~ name, ncol = 1) +
  labs(
    x = "Date",
    y = "Predicted Probability of Exceedance",
    title = "Predicted Exceedance Probabilities | Period of Record",
    caption = updated_at
  )
print(p)

for (site in unique(df_predict$name)) {
  cat(site, "\n")
  x <- df_predict %>%
    filter(name == site)
  p <- x %>%
    ggplot(aes(timestamp, value)) +
    geom_rect(
      data = df_models_predictor_ranges %>%
        filter(model_id == site),
      aes(xmin = min(x$timestamp), xmax = max(x$timestamp), ymin = min, ymax = max, fill = "calibration\nrange"),
      inherit.aes = FALSE, alpha = 0.25
    ) +
    scale_fill_manual(NULL, values = "goldenrod") +
    geom_line() +
    facet_wrap(vars(key), scales = "free_y", ncol = 1) +
    scale_x_datetime(expand = expansion()) +
    labs(
      title = paste0(site, " | Period of Record"),
      y = NULL, x = "Date",
      caption = updated_at
    )
  print(p)
}


p <- df_predict %>%
  filter(as_date(timestamp) >= today() - days(365), key == "exceedence_probability") %>%
  ggplot(aes(timestamp, value)) +
  geom_line() +
  geom_hline(aes(yintercept = 0.2, linetype = "Cutoff")) +
  scale_x_datetime(expand = expansion()) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_linetype_manual("", values = "dashed") +
  facet_wrap(~ name, ncol = 1) +
  labs(
    x = "Date",
    y = "Predicted Probability of Exceedance",
    title = "Predicted Exceedance Probabilities | Last 365 Days",
    caption = updated_at
  )
print(p)

for (site in unique(df_predict$name)) {
  cat(site, "\n")
  x <- df_predict %>%
    filter(name == site) %>%
    filter(as_date(timestamp) >= today() - days(365))
  p <- x %>%
    ggplot(aes(timestamp, value)) +
    geom_rect(
      data = df_models_predictor_ranges %>%
        filter(model_id == site),
      aes(xmin = min(x$timestamp), xmax = max(x$timestamp), ymin = min, ymax = max, fill = "calibration\nrange"),
      inherit.aes = FALSE, alpha = 0.25
    ) +
    scale_fill_manual(NULL, values = "goldenrod") +
    geom_line() +
    facet_wrap(vars(key), scales = "free_y", ncol = 1) +
    scale_x_datetime(expand = expansion()) +
    labs(
      title = paste0(site, " | Last 365 Days"),
      y = NULL, x = "Date",
      caption = updated_at
    )
  print(p)
}

for (site in unique(df_predict$name)) {
  cat(site, "\n")
  x <- df_predict %>%
    filter(name == site)
  p <- x %>%
    pivot_wider(names_from = "key") %>%
    pivot_longer(-c("name", "timestamp", "exceedence_probability"), names_to = "key") %>%
    ggplot(aes(value, exceedence_probability)) +
    geom_rect(
      data = df_models_predictor_ranges %>%
        filter(model_id == site),
      aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf, fill = "calibration\nrange"),
      inherit.aes = FALSE, alpha = 0.25
    ) +
    geom_point() +
    geom_hline(aes(yintercept = 0.2, linetype = "Cutoff")) +
    scale_fill_manual(NULL, values = "goldenrod") +
    scale_linetype_manual(NULL, values = "dashed") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent, breaks = scales::pretty_breaks()) +
    facet_wrap(vars(key), scales = "free_x", strip.position = "bottom") +
    labs(
      title = paste0(site, " | Predicted Prob vs. Input Variable Values"),
      y = "Predicted Exceedence Probability", x = NULL,
      caption = updated_at
    ) +
    theme(strip.placement = "outside", aspect.ratio = 1)
  print(p)
}



z <- dev.off()

cat("done\n")

# clean up ----------------------------------------------------------------

duration <- as.numeric(difftime(now(tzone = "US/Eastern"), right_now, units = "sec"))
cat("Finished:", paste0(now(tzone = "US/Eastern")), "(", duration, "sec )\n")
