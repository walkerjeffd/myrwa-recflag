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
  select(model_id, model_location, parameter, standard_type, standard_value, date, timestamp, timestamp_utc, everything()) %>%
  filter(
    timestamp >= min(df_predictors$timestamp)
  )

df %>%
  ggplot(aes(precip_sum_p48hr_lag0hr, log10(concentration), color = factor(exceedance))) +
  geom_point() +
  facet_wrap(~ model_id)

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
