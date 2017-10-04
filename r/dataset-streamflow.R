library(tidyverse)
library(lubridate)
library(dataRetrieval)

rm(list = ls())


# hourly aberjona ---------------------------------------------------------

df_abj_uv_raw <- readNWISuv(siteNumbers = "01102500", parameterCd = "00060", startDate = "2010-01-01", endDate = "2016-12-31")

df_abj_uv <- df_abj_uv_raw %>%
  select(
    site_no,
    timestamp = dateTime,
    flow = X_00060_00000
  ) %>%
  mutate(
    site = "aberjona",
    timestamp = with_tz(timestamp, tzone = "US/Eastern")
  )

df_abj_hr <- df_abj_uv %>%
  mutate(
    timestamp = ceiling_date(timestamp, unit = "hour")
  ) %>%
  group_by(timestamp) %>%
  summarize(
    flow = mean(flow)
  )
# summary(df_abj_hr)

df_abj_hr <- data_frame(
  timestamp = seq(min(df_abj_hr$timestamp), max(df_abj_hr$timestamp), by = "hour")
) %>%
  left_join(df_abj_hr, by = "timestamp")
# summary(df_abj_hr)

rle_flow_na <- rle(is.na(df_abj_hr$flow))
rle_flow_na$lengths[rle_flow_na$values] %>% summary

df_abj_hr %>%
  ggplot(aes(timestamp, flow)) +
  geom_line()
# missing too much data (most of 2015)


# daily aberjona ----------------------------------------------------------

df_abj_day_raw <- readNWISdv(siteNumber = "01102500", parameterCd = "00060", startDate = "2006-01-01", endDate = "2016-12-31")

df_abj_day <- df_abj_day_raw %>%
  as_tibble() %>%
  select(
    site_no,
    date = Date,
    flow = X_00060_00003
  ) %>%
  mutate(
    site = "aberjona"
  ) %>%
  complete(site_no, site, date = seq(min(df_ale_day$date), max(df_ale_day$date), by = "day"))

df_abj_day %>%
  ggplot(aes(date, flow)) +
  geom_line()

# daily alewife -----------------------------------------------------------

df_ale_day_raw <- readNWISdv(siteNumber = "01103025", parameterCd = "00060", startDate = "2006-01-01", endDate = "2016-12-31")

df_ale_day <- df_ale_day_raw %>%
  as_tibble() %>%
  select(
    site_no,
    date = Date,
    flow = X_00060_00003
  ) %>%
  mutate(
    site = "alewife"
  ) %>%
  complete(site_no, site, date = seq(min(df_ale_day$date), max(df_ale_day$date), by = "day"))

df_ale_day %>%
  ggplot(aes(date, flow)) +
  geom_line()


# merge -------------------------------------------------------------------

df_flow_day <- bind_rows(df_abj_day, df_ale_day) %>%
  arrange(site, date) %>%
  group_by(site) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  group_by(site_no, site) %>%
  mutate(
    logflow = log10(flow)
    # flow_slope_mid = (lead(flow) - lag(flow)) / 2,
    # flow_slope = (lag(flow, n = 1) - lag(flow, n = 2)),
    # logflow_slope_mid = (lead(logflow) - lag(logflow)) / 2,
    # logflow_slope = (lag(logflow, n = 1) - lag(logflow, n = 2))
  ) %>%
  ungroup()


df_flow_day %>%
  ggplot(aes(date, flow, color = site)) +
  geom_line()

df_flow_day %>%
  ggplot(aes(date, logflow, color = site)) +
  geom_line()

df_flow_day %>%
  select(date, site, flow) %>%
  spread(site, flow) %>%
  ggplot(aes(alewife, aberjona)) +
  geom_point() +
  geom_smooth(method = "lm")

# previous flow
for (d in c(1:5)) {
  df_flow_day[[paste0("flow_p", d, "d")]] <- if_else(df_flow_day$row > d, lag(df_flow_day$flow, n = d), NA_real_)
  df_flow_day[[paste0("logflow_p", d, "d")]] <- if_else(df_flow_day$row > d, lag(df_flow_day$logflow, n = d), NA_real_)
}

for (d in c(2:5)) {
  df_flow_day[[paste0("flow_change_p", d, "d_p1d")]] <- if_else(df_flow_day$row > d, lag(df_flow_day$flow, n = 1) - lag(df_flow_day$flow, n = d), NA_real_)
  df_flow_day[[paste0("logflow_change_p", d, "d_p1d")]] <- if_else(df_flow_day$row > d, lag(df_flow_day$logflow, n = 1) - lag(df_flow_day$logflow, n = d), NA_real_)
}

df_flow_day %>%
  filter(
    year(date) == 2015,
    month(date) %in% c(9:11)
  ) %>%
  select(site, date, flow, starts_with("flow_change_p")) %>%
  gather(var, value, -site, -date) %>%
  ggplot(aes(date, value, color = var)) +
  geom_line() +
  facet_wrap(~ site, ncol = 1, scales = "free_y")

df_flow_day %>%
  filter(
    year(date) == 2015,
    month(date) %in% c(9:11)
  ) %>%
  select(site, date, logflow, starts_with("logflow_change_p")) %>%
  gather(var, value, -site, -date) %>%
  ggplot(aes(date, value, color = var)) +
  geom_line() +
  facet_wrap(~ site, ncol = 1, scales = "free_y")

df_out <- df_flow_day %>%
  select(-site_no, -row, -flow, -logflow) %>%
  gather(var, value, -site, -date) %>%
  unite(var, site, var, sep = "_") %>%
  spread(var, value)

df_out %>%
  write_csv("data/streamflow.csv")

df_out %>%
  saveRDS("data/streamflow.rds")
