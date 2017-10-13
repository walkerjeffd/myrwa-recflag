#!/usr/bin/env Rscript
# Generate model prediction report
# example: Rscript predict-report.R

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

# setup -------------------------------------------------------------------

source("functions.R")

config <- fromJSON("../config/index.json")

con <- dbConnect(PostgreSQL(), dbname = config$db$database, host = config$db$host, user = config$db$user, password = config$db$password)

df_predict <- tbl(con, "predictions") %>%
  select(id, created_at, name, uuid, timestamp, prob, exceedance) %>%
  arrange(name, timestamp) %>%
  collect()

# pdf ---------------------------------------------------------------------

pdf("pdf/predict-report.pdf", width = 11, height = 8.5)

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
    title = "Exceedance Predictions - Year to Date"
  )
print(p)

p <- df_predict %>%
  filter(timestamp >= (now() - days(10))) %>%
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
    title = "Exceedance Predictions - Last 10 Days"
  )
print(p)

dev.off()

# clean up ----------------------------------------------------------------

dbDisconnect(con)
