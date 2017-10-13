# create weather dataset

library(RPostgreSQL)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(stringr)
library(zoo)

con <- dbConnect(PostgreSQL(), dbname = "recflag", host = "alewife.local")

query <- "SELECT id as db_id, date as db_date, data->'history'->'observations' as data FROM wunderground"

df_raw <- dbSendQuery(con, query) %>%
  fetch(n = -1)

# unnest json
df_unnest <- df_raw %>%
  mutate(
    data = map(data, function(x) {
      json <- fromJSON(x, simplifyDataFrame = TRUE)

      utc_timestamp <- json$utcdate %>%
        mutate(
          date = paste(year, mon, mday, sep = "-"),
          time = paste(hour, min, sep = ":"),
          utc_timestamp = ymd_hm(paste(date, time), tz = "UTC")
        ) %>%
        select(utc_timestamp)

      values <- select(json, -date, -utcdate)

      bind_cols(utc_timestamp, values)
    })
  ) %>%
  unnest(data) %>%
  select(-tempm, -dewptm, -wspdm, -wgustm, -wgusti, -wdire, -vism, -pressurem, -windchillm, -windchilli, -heatindexm, -heatindexi, -precipm, -icon, -tornado) %>%
  as_tibble() %>%
  arrange(utc_timestamp) %>%
  select(-db_id, -db_date) %>%
  distinct()

# clean variables
df <- df_unnest %>%
  mutate_at(vars(fog, rain, snow, hail, thunder), as.integer) %>%
  mutate_at(vars(fog, rain, snow, hail, thunder), as.logical) %>%
  mutate(hum = na_if(hum, "N/A")) %>%
  rename(temp = tempi, dewpt = dewpti, wspd = wspdi, vis = visi, pressure = pressurei, precip = precipi) %>%
  mutate_at(vars(temp, dewpt, hum, wspd, wdird, vis, pressure, precip), as.numeric) %>%
  mutate(
    temp = na_if(temp, -9999),
    dewpt = na_if(dewpt, -9999),
    hum = na_if(hum, -9999),
    wspd = na_if(wspd, -9999),
    wdird = na_if(wdird, -9999),
    vis = na_if(vis, -9999),
    pressure = na_if(pressure, -9999),
    precip = na_if(precip, -9999),
    precip = coalesce(precip, 0)
  ) %>%
  filter(str_detect(metar, "^METAR")) %>%
  mutate(
    timestamp = with_tz(utc_timestamp, tzone = "US/Eastern"),
    date = as.Date(timestamp),
    year = year(date),
    month = month(date),
    day = day(date),
    jday = yday(date),
    hour = hour(timestamp),
    minute = minute(timestamp)
  ) %>%
  select(utc_timestamp, timestamp, date, year, month, day, jday, hour, minute, everything())

# df %>%
#   ggplot(aes(timestamp, temp)) +
#   geom_line()
#
# df %>%
#   ggplot(aes(timestamp, precip)) +
#   geom_point()

# hourly dataset
df_hour <- df %>%
  mutate(
    timestamp = ceiling_date(timestamp, unit = "hour")
  ) %>%
  group_by(timestamp) %>%
  summarise(
    n_hour = n(),
    temp = mean(temp, na.rm = TRUE),
    dewpt = mean(dewpt, na.rm = TRUE),
    hum = mean(hum, na.rm = TRUE),
    wspd = mean(wspd, na.rm = TRUE),
    vis = mean(vis, na.rm = TRUE),
    pressure = mean(pressure, na.rm = TRUE),
    precip = max(precip, na.rm = TRUE)
  ) %>%
  arrange(timestamp)

df_hour <- data_frame(
    timestamp = seq(min(df_hour$timestamp), max(df_hour$timestamp), by = "hour", tz = "US/Eastern")
  ) %>%
  left_join(df_hour, by = "timestamp") %>%
  mutate(
    missing_hour = is.na(n_hour),
    n_hour = coalesce(n_hour, 0L)
  ) %>%
  mutate(
    temp = approx(timestamp, temp, xout = timestamp, method = "linear")$y,
    dewpt = approx(timestamp, dewpt, xout = timestamp, method = "linear")$y,
    hum = approx(timestamp, hum, xout = timestamp, method = "linear")$y,
    wspd = approx(timestamp, wspd, xout = timestamp, method = "linear")$y,
    vis = approx(timestamp, vis, xout = timestamp, method = "linear")$y,
    pressure = approx(timestamp, pressure, xout = timestamp, method = "linear")$y,
    precip = coalesce(precip, 0)
  )

# hour gaps
rle_hour <- rle(df_hour$missing_hour)
rle_hour$lengths[rle_hour$values] %>% max # max = 11 hours

stopifnot(!any(is.na(df_hour)))

# daily dataset
df_day <- df_hour %>%
  mutate(
    date = as.Date(timestamp)
  ) %>%
  group_by(date) %>%
  summarise(
    n_day = n(),
    n_hour = sum(n_hour),
    temp_min = min(temp),
    temp_max = max(temp),
    temp_mean = mean(temp),
    dewpt_min = min(dewpt),
    dewpt_max = max(dewpt),
    dewpt_mean = mean(dewpt),
    hum_min = min(hum),
    hum_max = max(hum),
    hum_mean = mean(hum),
    wspd_min = min(wspd),
    wspd_max = max(wspd),
    wspd_mean = mean(wspd),
    vis_min = min(vis),
    vis_max = max(vis),
    vis_mean = mean(vis),
    pressure_min = min(pressure),
    pressure_max = max(pressure),
    pressure_mean = mean(pressure),
    precip_sum = sum(precip),
    precip_max = max(precip)
  )

# df_day %>%
#   ggplot(aes(date, precip_sum)) +
#   geom_point() +
#   geom_line()

# # annual rainfall
# df_day %>%
#   mutate(year = year(date)) %>%
#   group_by(year) %>%
#   summarise(
#     precip_sum = sum(precip_sum)
#   )


# rainfall events
EVENT_MIN_DEPTH <- 0.1
INTEREVENT_PERIOD <- 8 # hours
df_precip <- df_hour %>%
  select(timestamp, precip)
rle_precip <- rle(df_precip$precip > 0)
df_precip$wet <- 1 * (rep(rle_precip$lengths * (rle_precip$values | rle_precip$lengths < INTEREVENT_PERIOD), rle_precip$lengths) > 0)
df_precip$event_id <- cumsum(abs(c(df_precip$wet[1], diff(df_precip$wet, lag = 1))))

df_precip_event <- df_precip %>%
  group_by(event_id) %>%
  summarize(
    precip_sum = sum(precip)
  ) %>%
  ungroup() %>%
  filter(precip_sum > EVENT_MIN_DEPTH)

df_precip$wet <- if_else(df_precip$event_id %in% df_precip_event$event_id, 1, 0)
df_precip$event_id <- cumsum(abs(c(df_precip$wet[1], diff(df_precip$wet, lag = 1))))

df_precip <- df_precip %>%
  group_by(event_id) %>%
  mutate(
    event_duration = n(),
    event_hour = as.numeric(seq(1, n())),
    event_type = if_else(wet == 1, "Wet", "Dry"),
    event_precip_cumsum = cumsum(precip * wet),
    event_precip_cummax = cummax(precip * wet),
    event_precip_sum = sum(precip * wet),
    event_precip_max = max(precip * wet)
    # hours_since_last_dry_start = if_else(wet == 1, 0, event_hour)
  ) %>%
  ungroup()

# df_precip_event <- df_precip %>%
#   group_by(event_id, event_type) %>%
#   summarize(
#     event_duration = n(),
#     event_precip_sum = sum(precip * wet),
#     event_precip_max = max(precip * wet)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     event_prev_duration = coalesce(lag(event_duration), 0L)
#   )

# df_precip <- df_precip %>%
#   left_join(select(df_precip_event, event_id, event_prev_duration), by = "event_id") %>%
#   mutate(
#     event_hour_since_last_wet_end = if_else(wet == 1, event_prev_duration + event_hour, event_hour)
#   )


# hours since event
for (depth in c(0.10, 0.25, 0.5, 1)) {
  depth_id <- sprintf("%03.0f", depth * 100)
  is_event <- 1 * (df_precip$event_precip_sum >= depth)
  rle_event <- rle(is_event)
  df_precip[[paste0("hours_since_", depth_id, "in_precip_event")]] <- unlist(sapply(rle_event$lengths, seq)) * as.numeric(!is_event)
}

# antecedent rainfall
for (hr in c(12, 24, 36, 48, 72, 168)) {
  x_sum <- rollsum(df_precip$precip, k = hr, align = "right", fill = 0)
  x_max <- rollmax(df_precip$precip, k = hr, align = "right", fill = 0)

  for (lag_hr in c(0, 6, 12, 18, 24, 36, 48)) {
    col_sum <- paste0("precip_sum_p", hr, "hr_lag", lag_hr, "hr")
    col_max <- paste0("precip_max_p", hr, "hr_lag", lag_hr, "hr")

    df_precip[[col_sum]] <- coalesce(lag(x_sum, n = lag_hr), 0)
    df_precip[[col_max]] <- coalesce(lag(x_max, n = lag_hr), 0)
  }
}

# previous 1 day met data
df_p_day <- df_day %>%
  select(
    date,
    temp_mean_p1d = temp_mean,
    dewpt_mean_p1d = dewpt_mean,
    hum_mean_p1d = hum_mean,
    wspd_mean_p1d = wspd_mean,
    pressure_mean_p1d = pressure_mean
  ) %>%
  mutate(date = date + days(1))

# rolling mean weather
df_p_hr <- select(df_hour, timestamp)
for (hr in c(24, 48, 72)) {
  df_p_hr[[paste0("temp_mean_p", hr, "hr")]] <- rollmean(df_hour$temp, k = hr, align = "right", fill = NA)
  df_p_hr[[paste0("dewpt_mean_p", hr, "hr")]] <- rollmean(df_hour$dewpt, k = hr, align = "right", fill = NA)
  df_p_hr[[paste0("hum_mean_p", hr, "hr")]] <- rollmean(df_hour$hum, k = hr, align = "right", fill = NA)
  df_p_hr[[paste0("wspd_mean_p", hr, "hr")]] <- rollmean(df_hour$wspd, k = hr, align = "right", fill = NA)
  df_p_hr[[paste0("pressure_mean_p", hr, "hr")]] <- rollmean(df_hour$pressure, k = hr, align = "right", fill = NA)
  df_p_hr[[paste0("temp_change_p", hr, "hr")]] <- df_hour$temp - lag(df_hour$temp, n = hr)
  df_p_hr[[paste0("dewpt_change_p", hr, "hr")]] <- df_hour$dewpt - lag(df_hour$dewpt, n = hr)
  df_p_hr[[paste0("hum_change_p", hr, "hr")]] <- df_hour$hum - lag(df_hour$hum, n = hr)
  df_p_hr[[paste0("wspd_change_p", hr, "hr")]] <- df_hour$wspd - lag(df_hour$wspd, n = hr)
  df_p_hr[[paste0("pressure_change_p", hr, "hr")]] <- df_hour$pressure - lag(df_hour$pressure, n = hr)
}

df_p_hr %>%
  filter(year(timestamp) == 2015, month(timestamp) == 6) %>%
  ggplot(aes(timestamp)) +
  geom_line(
    data = filter(df_hour, year(timestamp) == 2015, month(timestamp) == 6),
    aes(y = temp)
  ) +
  geom_line(aes(y = temp_mean_p48hr), color = "red") +
  geom_line(aes(y = temp_change_p48hr), color = "blue")

# merge
df_out <- df_hour %>%
  select(timestamp, temp, dewpt, hum, wspd, pressure) %>%
  mutate(date = as.Date(timestamp)) %>%
  left_join(df_p_hr, by = "timestamp") %>%
  left_join(df_p_day, by = "date") %>%
  left_join(
    select(df_precip, timestamp, event_type, starts_with("hours_since"), starts_with("precip_")),
    by = "timestamp"
  ) %>%
  select(date, timestamp, everything())

df_out %>%
  write_csv("data/weather.csv")

df_out %>%
  saveRDS("data/weather.rds")
