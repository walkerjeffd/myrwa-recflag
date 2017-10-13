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
df_predict <- suppressWarnings(tbl(con, "predictions") %>%
  select(id, created_at, name, uuid, timestamp, prob, exceedance) %>%
  arrange(name, timestamp) %>%
  collect())
cat("done\n")

# pdf ---------------------------------------------------------------------

fname <- "predictions.pdf"
cat("Generating PDF (", fname, ")...")

pdf(file.path("pdf", fname), width = 11, height = 8.5)

# p <- df_predict %>%
#   filter(timestamp >= (now() - days(10))) %>%
#   ggplot(aes(timestamp, prob, color = name)) +
#   geom_line() +
#   geom_hline(aes(yintercept = 0.2, linetype = "Cutoff")) +
#   scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
#   scale_color_discrete("Model") +
#   scale_linetype_manual("", values = "dashed") +
#   facet_wrap(~ name, ncol = 1) +
#   labs(
#     x = "Date",
#     y = "Predicted Probability of Exceedance",
#     title = "Exceedance Predictions - Last 10 Days",
#     caption = updated_at
#   )
# print(p)

p <- df_predict %>%
  ggplot(aes(timestamp, prob, color = name)) +
  geom_line() +
  geom_hline(aes(yintercept = 0.2, linetype = "Cutoff")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_color_discrete("Model") +
  scale_linetype_manual("", values = "dashed") +
  facet_wrap(~ name, ncol = 1) +
  labs(
    x = "Date",
    y = "Predicted Probability of Exceedance",
    title = "Exceedance Predictions - Year to Date",
    caption = updated_at
  )
print(p)


z <- dev.off()

cat("done\n")

# clean up ----------------------------------------------------------------

disconnected = dbDisconnect(con)
duration <- as.numeric(difftime(now(tzone = "US/Eastern"), right_now, units = "sec"))
cat("Finished:", paste0(now(tzone = "US/Eastern")), "(", duration, "sec )\n")
