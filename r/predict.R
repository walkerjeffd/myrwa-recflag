#!/usr/bin/env Rscript
# Generate model predictions and save to database
# examples:
#    Rscript predict.R (default = current hour)
#    Rscript predict.R "2017-10-01 07:00" (sepecific timestamp)
#    Rscript predict.R "2017-05-01 07:00" "2017-10-13 07:00" "day" (range of timestamps)

args = commandArgs(trailingOnly=TRUE)
# args <- c("201710010700", "201710100700", "day")
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

latest <- FALSE
if (length(args) == 0) {
  latest <- TRUE
  target_timestamps = floor_date(right_now, unit = "hour")
} else if (length(args) == 1) {
  target_timestamps = ymd_hm(args[1], tz = "US/Eastern")
} else if (length(args) == 3) {
  target_timestamps = seq(ymd_hm(args[1], tz = "US/Eastern"), ymd_hm(args[2], tz = "US/Eastern"), by = args[3])
} else {
  stop("Invalid number of arguments (expected: 0 = current timestamp, 1 = specific timestamp, 3 = range of timestamps)")
}

if (length(target_timestamps) == 0) {
  stop("Could not detect target timestamps")
} else if (latest) {
  cat("Target timestamp: latest\n")
} else if (length(target_timestamps) == 1) {
  cat("Target timestamp:", as.character(target_timestamps), "\n")
} else {
  cat("Target timestamps:", as.character(min(target_timestamps)), "-", as.character(max(target_timestamps)), "by", args[3], "\n")
}

source("functions.R")

config <- fromJSON("../config/index.json")

con <- dbConnect(PostgreSQL(), dbname = config$db$database, host = config$db$host, user = config$db$user, password = config$db$password)

# 1. Load Models
df_models <- load_models("models.rds")

# 2. Fetch Data
start <- as.character(as.Date(min(target_timestamps), tz = "US/Eastern") - days(200))
end <- as.character(as.Date(max(target_timestamps), tz = "US/Eastern"))
cat("Fetching predictors (", start, ",", end, ")...")
df_predictors_hour <- generate_predictors(con, start, end)

if (latest) {
  df_predictors <- df_predictors_hour %>%
    filter(
      timestamp <= target_timestamps
    ) %>%
    filter(
      timestamp == max(timestamp)
    )
} else {
  df_predictors <- df_predictors_hour %>%
    filter(
      timestamp %in% target_timestamps
    )
}

if (nrow(df_predictors) == 0) {
  cat("failed\n")
  stop("No predictors available")
}

cat("done\n")

if (latest) {
  cat("Using latest timestamp:", as.character(df_predictors$timestamp[1]), "\n")
  target_timestamps <- df_predictors$timestamp[1]
}

# 3. Run and Save Predictions
for (i in seq_along(target_timestamps)) {
  target_timestamp <- target_timestamps[i]
  cat("Target timestamp:", as.character(target_timestamp), "\n")

  if (latest) {
    df_predictors_target <- df_predictors
  } else {
    df_predictors_target <- df_predictors %>%
      filter(
        timestamp == target_timestamp
      )
  }
  stopifnot(nrow(df_predictors_target) == 1)

  results <- lapply(df_models$model_id, function (id) {
    cat("Running predictions:", id, "\n")
    x <- filter(df_models, model_id == id)
    stopifnot(nrow(x) == 1)
    x <- as.list(x)

    x_predictors <- df_predictors_target[, c("timestamp", x$model[[1]]$finalModel$xNames)]
    pred_prob <- make_prediction(x$model[[1]], newdata = x_predictors)
    pred_exceedance <- pred_prob > x$cutoff

    list(
      name = x$model_id,
      uuid = x$uuid,
      timestamp = x_predictors$timestamp,
      predictors = as.list(select(x_predictors, -timestamp)),
      prob = pred_prob,
      exceedance = pred_exceedance
    )
  })

  cat("Saving predictions...\n")
  save_predictions(con, results)
}

# 4. Quit
disconnected <- dbDisconnect(con)
duration <- as.numeric(difftime(now(tzone = "US/Eastern"), right_now, units = "sec"))
cat("Finished:", paste0(now(tzone = "US/Eastern")), "(", duration, "sec )\n")
