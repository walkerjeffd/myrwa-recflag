library(caret)
library(pROC)
library(uuid)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(jsonlite)
theme_set(theme_bw())
theme_update(
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = 12)
)

rm(list = ls())

# load data ---------------------------------------------------------------

source("functions.R")

config <- fromJSON("../config/index.json")



models <- readRDS("data/model-data.rds")
df_models <- models$df
list_models <- models$ls

df_weather <- readRDS("data/weather.rds")
df_wq <- readRDS("data/wq.rds")

models2 <- readRDS("models.rds")

df <- models2$df[[1]]
m <- models2$model[[1]]

pred <- predict(m, newdata = df, type = "prob")
df$exceedance

data_frame(
  prob = pred$yes,
  exceedance = df$exceedance,
  timestamp = df$timestamp
) %>%
  ggplot(aes(timestamp)) +
  geom_point(aes(y = prob)) +
  geom_point(aes(y = exceedance), color = "red")

data_frame(
  prob = pred$yes,
  exceedance = df$exceedance,
  timestamp = df$timestamp
) %>%
  ggplot(aes(prob, exceedance)) +
  geom_point()

# predictions -------------------------------------------------------------

con <- dbConnect(PostgreSQL(), dbname = config$db$database, host = config$db$host, user = config$db$user, password = config$db$password)
df_predictors <- generate_predictors(con, as.character(min(df$date)), as.character(max(df$date)))


pred <- predict(m, newdata = df_predictors, type = "prob")[["yes"]]
df$exceedance

data_frame(
  timestamp = df_predictors$timestamp,
  prob = pred
) %>%
  ggplot(aes(timestamp, prob)) +
  geom_line() +
  geom_point(
    data = df,
    aes(timestamp, exceedance),
    color = "red"
  ) +
  scale_x_datetime(limits = ymd_hm(c(201401010000, 201601010000)))

data_frame(
  prob = pred$yes,
  exceedance = df$exceedance,
  timestamp = df$timestamp
) %>%
  ggplot(aes(timestamp)) +
  geom_point(aes(y = prob)) +
  geom_point(aes(y = exceedance), color = "red")
