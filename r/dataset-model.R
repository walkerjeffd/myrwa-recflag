# combine datasets

library(RPostgreSQL)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(stringr)

rm(list = ls())

df_weather <- readRDS("data/weather.rds")
df_flow <- readRDS("data/streamflow.rds")
df_wq <- readRDS("data/wq.rds") %>%
  mutate(
    timestamp = floor_date(timestamp, unit = "hours"),
    timestamp_utc = with_tz(timestamp, tzone = "UTC"),
    exceedance = as.numeric(exceedance)
  )

df_predictors <- left_join(df_weather, df_flow, by = "date") %>%
  mutate(timestamp_utc = with_tz(timestamp, tzone = "UTC")) %>%
  select(date, timestamp, timestamp_utc, everything())

df <- df_wq %>%
  left_join(
    select(df_predictors, -date, -timestamp),
    by = "timestamp_utc"
  ) %>%
  mutate(
    date = as.Date(timestamp)
  ) %>%
  select(model_id, model_location, location_id, project_id, parameter, standard_value, date, timestamp, timestamp_utc, everything()) %>%
  filter(
    timestamp >= min(df_predictors$timestamp)
  )

df %>%
  ggplot(aes(precip_sum_p48hr_lag0hr, log10(concentration), color = factor(exceedance))) +
  geom_point() +
  facet_wrap(~ model_id)

# plots -------------------------------------------------------------------

df %>%
  ggplot(aes(pressure_change_p48hr, log10(concentration), color = factor(exceedance))) +
  geom_point() +
  facet_wrap(~ model_id)

df %>%
  ggplot(aes(temp_mean_p48hr, log10(concentration), color = factor(exceedance))) +
  geom_point() +
  facet_wrap(~ model_id)


# export ------------------------------------------------------------------

df %>%
  mutate(
    timestamp_utc = format(timestamp_utc, "%Y-%m-%d %H:%M:%S"),
    timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
  ) %>%
  write_csv("data/recflag-model.csv")

df_predictors %>%
  mutate(
    timestamp_utc = format(timestamp_utc, "%Y-%m-%d %H:%M:%S"),
    timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
  ) %>%
  write_csv("data/recflag-predictors.csv")

df_wq %>%
  arrange(model_id, timestamp) %>%
  mutate(
    timestamp_utc = format(timestamp_utc, "%Y-%m-%d %H:%M:%S"),
    timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
  ) %>%
  write_csv("data/recflag-wq.csv")

df_models <- df %>%
  filter(
    year(date) >= 2005,
    year(date) <= 2016,
    month(date) >= 5,
    month(date) <= 10
  ) %>%
  mutate_at(vars(starts_with("precip_")), round, digits = 5) %>%
  mutate(
    exceedance_class = factor(if_else(exceedance == 1, "yes", "no"), levels = c("yes", "no"))
  ) %>%
  select(
    model_id, model_location, location_id, project_id, parameter, standard_value, date, timestamp, timestamp_utc, concentration,
    event_type, exceedance, exceedance_class, everything()
  ) %>%
  group_by(model_id) %>%
  nest(.key = "data") %>%
  mutate(
    n_samples = map_int(data, nrow),
    frac_exceed = map_dbl(data, ~ mean(.$exceedance))
  )

model_ids <- unique(df_models$model_id)
list_models <- lapply(model_ids, function(x) {
  df_models %>%
    filter(model_id == x) %>%
    unnest(data)
}) %>%
  setNames(nm = model_ids)

list(
  df = df_models,
  ls = list_models
) %>%
  saveRDS("data/model-data.rds")



# pdf ---------------------------------------------------------------------

pdf("pdf/model-data.pdf", width = 11, height = 8.5)

df_models %>%
  unnest(data) %>%
  ggplot(aes(date, log10(concentration), color = exceedance_class)) +
  geom_point() +
  scale_color_manual(
    "Exceedance",
    values = c(
      "yes" = "orangered",
      "no" = "deepskyblue"
    )
  ) +
  scale_x_date(breaks = scales::date_breaks("1 year"), labels = scales::date_format("%Y")) +
  labs(
    x = "Date",
    y = "log10[ Concentration ]"
  ) +
  facet_wrap(~ model_id) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

df_models %>%
  ggplot(aes(model_id, frac_exceed)) +
  geom_bar(stat = "identity", fill = "orangered") +
  geom_text(aes(label = scales::percent(frac_exceed)), vjust = 0, nudge_y = 0.01) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Model ID",
    y = "% Exceedances of All Samples"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

df_models %>%
  unnest(data) %>%
  mutate(
    year = year(date)
  ) %>%
  group_by(model_id, year) %>%
  summarize(
    frac_exceedance = mean(exceedance)
  ) %>%
  ungroup() %>%
  ggplot(aes(year, frac_exceedance)) +
  geom_bar(fill = "orangered", stat = "identity") +
  geom_text(aes(label = scales::percent(frac_exceedance)), vjust = 0, nudge_y = 0.01) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.4)) +
  labs(
    x = "Year",
    y = "% Exceedances"
  ) +
  facet_wrap(~ model_id)

df_models %>%
  unnest(data) %>%
  ggplot(aes(precip_sum_p48hr_lag0hr, log10(concentration), color = exceedance_class)) +
  geom_point() +
  scale_color_manual(
    "Exceedance",
    values = c(
      "yes" = "orangered",
      "no" = "deepskyblue"
    )
  ) +
  labs(
    x = "48-hour Antecedent Precip (in)",
    y = "log10[ Concentration ]"
  ) +
  facet_wrap(~ model_id)

for (id in model_ids) {
  p <- list_models[[id]] %>%
    ggplot(aes(date, log10(concentration), color = project_id)) +
    geom_point() +
    geom_hline(yintercept = log10(unique(list_models[[id]]$standard_value))) +
    labs(
      x = "Date",
      y = "log10[Concentration]",
      title = id
    )
  print(p)
}

dev.off()

# ECOLI vs ENT @ MYSTICBOB (RECFLAG)
# !OK BOTH = 5, ENT = 9, ECOLI = 7
df_models %>%
  filter(model_id %in% c("MYSTICBOB_ECOLI", "MYSTICBOB_ENT")) %>%
  unnest(data) %>%
  select(date, parameter, concentration) %>%
  spread(parameter, concentration) %>%
  ggplot(aes(log10(ECOLI), log10(ENT))) +
  geom_point() +
  geom_hline(yintercept = log10(350)) +
  geom_vline(xintercept = log10(1260))

# ECOLI vs ENT @ MYSTIC (CSORWM only)
# !OK - use ECOLI
df_models %>%
  filter(model_id %in% c("MYSTIC_ECOLI", "MYSTIC_ENT")) %>%
  unnest(data) %>%
  filter(project_id == "CSORWM") %>%
  select(date, parameter, concentration) %>%
  spread(parameter, concentration) %>%
  ggplot(aes(log10(ECOLI), log10(ENT))) +
  geom_point() +
  geom_hline(yintercept = log10(350)) +
  geom_vline(xintercept = log10(1260))

# CSORWM vs RECFLAG @ MYSTIC_ECOLI
# OK (1 exceedance)
df_models %>%
  filter(model_id %in% c("MYSTIC_ECOLI")) %>%
  unnest(data) %>%
  select(project_id, date, parameter, concentration) %>%
  spread(project_id, concentration) %>%
  filter(!is.na(CSORWM), !is.na(RECFLAG)) %>%
  ggplot(aes(log10(CSORWM), log10(RECFLAG))) +
  geom_point() +
  geom_hline(yintercept = log10(1260)) +
  geom_vline(xintercept = log10(1260))

# CSORWM vs RECFLAG @ MALDENLOWER_ECOLI
# OK (2 exceedances)
df_models %>%
  filter(model_id %in% c("MALDENLOWER_ECOLI")) %>%
  unnest(data) %>%
  select(project_id, date, parameter, concentration) %>%
  spread(project_id, concentration) %>%
  filter(!is.na(CSORWM), !is.na(RECFLAG)) %>%
  ggplot(aes(log10(CSORWM), log10(RECFLAG))) +
  geom_point() +
  geom_hline(yintercept = log10(1260)) +
  geom_vline(xintercept = log10(1260))


# MUNIBCH vs RECFLAG @ WEDGE_ECOLI
# !OK 2 samples from RECFLAG are exceedances, but not for MUNIBCH
df_models %>%
  filter(model_id %in% c("WEDGE_ECOLI")) %>%
  unnest(data) %>%
  select(project_id, date, parameter, concentration) %>%
  spread(project_id, concentration) %>%
  filter(!is.na(MUNIBCH), !is.na(RECFLAG)) %>%
  ggplot(aes(log10(MUNIBCH), log10(RECFLAG))) +
  geom_point() +
  geom_hline(yintercept = log10(1260)) +
  geom_vline(xintercept = log10(1260))


# DCRBCH vs RECFLAG @ SHANNON_ENT
# OK (no exceedances)
df_models %>%
  filter(model_id %in% c("SHANNON_ENT")) %>%
  unnest(data) %>%
  mutate(duplicated = duplicated(interaction(project_id, date))) %>%
  filter(!duplicated) %>%
  select(project_id, date, parameter, concentration) %>%
  spread(project_id, concentration) %>%
  filter(!is.na(DCRBCH), !is.na(RECFLAG)) %>%
  ggplot(aes(log10(DCRBCH), log10(RECFLAG))) +
  geom_point() +
  geom_hline(yintercept = log10(350)) +
  geom_vline(xintercept = log10(350))

