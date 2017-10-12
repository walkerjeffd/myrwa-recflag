library(caret)
library(pROC)
library(uuid)
library(tidyverse)
library(lubridate)
library(gridExtra)
theme_set(theme_bw())
theme_update(
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = 12)
)

rm(list = ls())

cat("Booting:", paste0(now()), "\n")

make_prediction <- function(m, newdata) predict(m, newdata = as.data.frame(newdata), type = "prob")[1, "yes"]

# TODO: Add GUID for each model version
load_models <- function (filename = "models.rds") {
  cat("Loading models:", filename, "\n")
  df <- readRDS(filename) %>%
    mutate(
      uuid = map_chr(model_id, ~ UUIDgenerate())
    )
}

# TODO: Set current time
right_now <- "2011-10-04 07:00:00"

# TODO: Pull from DB
df_weather <- readRDS("data/weather.rds")
df_flow <- readRDS("data/streamflow.rds")
fetch_data <- function(ts = right_now) {
  cat("Fetching predictors:", ts, "\n")
  left_join(df_weather, df_flow, by = "date") %>%
    mutate(timestamp_utc = with_tz(timestamp, tzone = "UTC")) %>%
    dplyr::select(date, timestamp, timestamp_utc, everything()) %>%
    filter(
      timestamp == ymd_hms(ts, tz = "US/Eastern")
    )
}

# TODO: Save to DB
savePrediction <- function (df, ts = right_now) {
  stopifnot(nrow(df) > 0)
  for (i in seq(1, nrow(df))) {
    id <- df$model_id[[i]]
    pred <- df$pred[[i]]
    pred_class <- df$pred_class[[i]]
    cat(paste0("Saving prediction: (", paste(ts, id, round(pred, 2), pred_class, sep = ", "), ")"), "\n")
  }
}

# 1. Load Models
df_models <- load_models()

# 2. Fetch Data
df_new <- fetch_data(right_now)

# 3. Run Prediction
cat("Running predictions...")
df_pred <- df_models %>%
  mutate(
    pred = map_dbl(model, make_prediction, newdata = df_new),
    pred_class = if_else(pred > cutoff, "yes", "no")
  )
cat("done\n")

# 4. Save Result
savePrediction(dplyr::select(df_pred, model_id, model, pred, pred_class))

# 5. Quit
cat("Finished:", paste0(now()), "\n")
