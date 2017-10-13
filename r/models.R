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

# load data ---------------------------------------------------------------

source("functions.R")

seed <- 20171112

models <- readRDS("data/model-data.rds")
df_models <- models$df
list_models <- models$ls

df_weather <- readRDS("data/weather.rds")
df_wq <- readRDS("data/wq.rds")

final <- list()

# utils -------------------------------------------------------------------

glm2 <- getModelInfo("glm")$glm
glm2$predict <- function (modelFit, newdata, submodels = NULL) {
  if (!is.data.frame(newdata))
    newdata <- as.data.frame(newdata)
  if (modelFit$problemType == "Classification") {
    probs <- predict(modelFit, newdata, type = "response")
    out <- ifelse(probs < 0.8, modelFit$obsLevel[1], modelFit$obsLevel[2])
  }
  else {
    out <- predict(modelFit, newdata, type = "response")
  }
  out
}

trControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

predictor_sets <- list(
  all = c(
    "temp_change_p24hr", "temp_change_p48hr", "temp_change_p72hr",
    "pressure_change_p24hr", "pressure_change_p48hr", "pressure_change_p72hr",
    "wspd_mean_p1d",
    "temp_mean_p1d",
    "hours_since_010in_precip_event", "hours_since_025in_precip_event", "hours_since_050in_precip_event", "hours_since_100in_precip_event",
    "precip_sum_p12hr_lag0hr", "precip_sum_p12hr_lag12hr", "precip_sum_p12hr_lag24hr",
    "precip_sum_p24hr_lag0hr", "precip_sum_p24hr_lag12hr", "precip_sum_p24hr_lag24hr",
    "precip_sum_p36hr_lag0hr", "precip_sum_p36hr_lag12hr", "precip_sum_p36hr_lag24hr",
    "precip_sum_p48hr_lag0hr", "precip_sum_p48hr_lag12hr", "precip_sum_p48hr_lag24hr",
    "precip_sum_p72hr_lag0hr", "precip_sum_p72hr_lag12hr", "precip_sum_p72hr_lag24hr",
    "aberjona_logflow_p1d",
    "aberjona_logflow_change_p2d_p1d", "aberjona_logflow_change_p5d_p1d"
  ),
  some = c(
    "temp_change_p72hr",
    "pressure_change_p72hr",
    "wspd_mean_p1d",
    "temp_mean_p1d",
    "hours_since_050in_precip_event", "hours_since_100in_precip_event",
    "precip_sum_p24hr_lag0hr", "precip_sum_p24hr_lag24hr",
    "precip_sum_p48hr_lag0hr",
    "aberjona_logflow_p1d",
    "aberjona_logflow_change_p2d_p1d"
  )
)

# MALDENLOWER_ECOLI -------------------------------------------------------

df <- list_models[["MALDENLOWER_ECOLI"]]

set.seed(seed)
training_rows <- createDataPartition(
  y = df$exceedance_class,
  p = 0.75,
  list = FALSE
)

df_train <- df[training_rows, ]
df_test <- df[-training_rows,]
table(df_train$exceedance_class)
table(df_test$exceedance_class)

mods <- list(
  all = train_models(df_train[, predictor_sets$all], df_train$exceedance_class),
  some = train_models(df_train[, predictor_sets$some], df_train$exceedance_class)
)

cutoffs <- c(
  "step" = 0.2,
  "net" = 0.2,
  "rf" = 0.2,
  "gbm" = 0.2,
  "glm" = 0.2
)

df_mods <- crossing(
  predictor_set = c("all", "some"),
  model_name = c("step", "net", "rf", "gbm", "glm")
) %>%
  mutate(
    model = map2(predictor_set, model_name, function (p, n) {
      mods[[p]][[n]]
    }),
    cutoff = map(model_name, ~ cutoffs[.]),
    roc_train = map(model, ~ roc(df_train$exceedance, predict(., newdata = df_train, type = "prob")[["yes"]])),
    roc_test = map(model, ~ roc(df_test$exceedance, predict(., newdata = df_test, type = "prob")[["yes"]])),
    auc_train = map_dbl(roc_train, ~ as.numeric(pROC::auc(.))),
    auc_test = map_dbl(roc_test, ~ as.numeric(pROC::auc(.))),
    auc_diff = (auc_test - auc_train) / 2,
    auc_mean = (auc_train + auc_test) / 2,
    cf_train = map2(model, cutoff, ~ cfm(m = .x, df = df_train, prob = .y)),
    cf_test = map2(model, cutoff, ~ cfm(m = .x, df = df_test, prob = .y)),
    cfpr_train = map2(model, cutoff, ~ cfm(m = .x, df = df_train, prob = .y, mode = "prec_recall")),
    cfpr_test = map2(model, cutoff, ~ cfm(m = .x, df = df_test, prob = .y, mode = "prec_recall")),
    cutoffs_train = map(model, cutoff_df, df = df_train),
    cutoffs_test = map(model, cutoff_df, df = df_test)
  )

summary(resamples(mods$all))
summary(resamples(mods$some))
bwplot(resamples(mods$all))
bwplot(resamples(mods$some))

filter(df_mods, predictor_set == "some", model_name == "step")$model[[1]] %>% summary

filter(df_mods, predictor_set == "some", model_name == "step")$cutoffs_test[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "train") +
  theme(aspect.ratio = 1)

filter(df_mods, predictor_set == "some", model_name == "step")$cf_train[[1]]
filter(df_mods, predictor_set == "some", model_name == "step")$cf_test[[1]]

plot(filter(df_mods, predictor_set == "some", model_name == "step")$roc_train[[1]])
plot(filter(df_mods, predictor_set == "some", model_name == "step")$roc_test[[1]])

df_train[, c("concentration", "exceedance_class", names(coef(mods$some$step$finalModel))[-1])] %>%
  gather(var, value, -concentration, -exceedance_class) %>%
  ggplot(aes(value, log10(concentration), color = exceedance_class)) +
  geom_point() +
  facet_wrap(~ var, scales = "free_x")

df %>%
  ggplot(aes(precip_sum_p24hr_lag0hr, precip_sum_p24hr_lag24hr, color = exceedance_class)) +
  geom_point()

m1 <- fit_glm(df_train, c("precip_sum_p24hr_lag0hr", "precip_sum_p48hr_lag0hr", "hours_since_100in_precip_event", "pressure_change_p72hr", "temp_change_p72hr"))
m2 <- fit_glm(df_train, c("precip_sum_p24hr_lag0hr", "precip_sum_p48hr_lag0hr", "hours_since_100in_precip_event", "pressure_change_p72hr"))
m3 <- fit_glm(df_train, c("precip_sum_p24hr_lag0hr", "precip_sum_p48hr_lag0hr", "hours_since_100in_precip_event"))
m4 <- fit_glm(df_train, c("precip_sum_p24hr_lag0hr", "precip_sum_p48hr_lag0hr"))
m5 <- fit_glm(df_train, c("precip_sum_p24hr_lag0hr", "precip_sum_p24hr_lag24hr"))
m6 <- fit_glm(df_train, c("precip_sum_p24hr_lag0hr", "precip_sum_p24hr_lag24hr", "hours_since_100in_precip_event", "pressure_change_p72hr", "temp_change_p72hr"))

df_glms <- data_frame(
  id = paste0("m", 1:6),
  model_name = "glm",
  model = list(m1, m2, m3, m4, m5, m6),
  cutoff = 0.25
) %>%
  mutate(
    predictors = map_chr(model, ~ paste0(sort(names(.$trainingData))[-1], collapse = ", ")),
    aic = map_dbl(model, ~ .$finalModel$aic),
    roc_train = map(model, ~ roc(df_train$exceedance, predict(., newdata = df_train, type = "prob")[["yes"]])),
    roc_test = map(model, ~ roc(df_test$exceedance, predict(., newdata = df_test, type = "prob")[["yes"]])),
    auc_train = map_dbl(roc_train, ~ as.numeric(pROC::auc(.))),
    auc_test = map_dbl(roc_test, ~ as.numeric(pROC::auc(.))),
    auc_diff = (auc_test - auc_train) / 2,
    auc_mean = (auc_train + auc_test) / 2,
    cf_train = map2(model, cutoff, ~ cfm(m = .x, df = df_train, prob = .y)),
    cf_test = map2(model, cutoff, ~ cfm(m = .x, df = df_test, prob = .y)),
    cfpr_train = map2(model, cutoff, ~ cfm(m = .x, df = df_train, prob = .y, mode = "prec_recall")),
    cfpr_test = map2(model, cutoff, ~ cfm(m = .x, df = df_test, prob = .y, mode = "prec_recall")),
    cutoffs_train = map(model, cutoff_df, df = df_train),
    cutoffs_test = map(model, cutoff_df, df = df_test)
  )
# df_glms

list_glms <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5, m6 = m6)
summary(resamples(list_glms))
bwplot(resamples(list_glms))
densityplot(resamples(list_glms))

df_glms %>%
  dplyr::select(-model, -predictors, -roc_train, -roc_test, -cf_train, -cf_test, -cfpr_train, -cfpr_test, -cutoffs_train, -cutoffs_test)

filter(df_glms, id == "m6") %>%
  dplyr::select(-model, -predictors, -roc_train, -roc_test, -cutoffs_train, -cutoffs_test)

filter(df_glms, id == "m6")$predictors

filter(df_glms, id == "m6")$cf_train[[1]]
filter(df_glms, id == "m6")$cf_test[[1]]
filter(df_glms, id == "m6")$cfpr_train[[1]]
filter(df_glms, id == "m6")$cfpr_test[[1]]


df_train[, c("concentration", "exceedance_class", names(coef(filter(df_glms, id == "m6")$model[[1]]$finalModel))[-1])] %>%
  gather(var, value, -concentration, -exceedance_class) %>%
  ggplot(aes(value, log10(concentration), color = exceedance_class)) +
  geom_point() +
  facet_wrap(~ var, scales = "free_x")

corr <- cor(filter(df_glms, id == "m6")$model[[1]]$trainingData[1:(length(filter(df_glms, id == "m6")$model[[1]]$trainingData) - 1)])
corrplot::corrplot(corr)

p1 <- filter(df_glms, id == "m6")$cutoffs_train[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "train") +
  theme(aspect.ratio = 1)
p2 <- filter(df_glms, id == "m6")$cutoffs_test[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "test") +
  theme(aspect.ratio = 1)
grid.arrange(p1, p2, ncol = 2)

final[["MALDENLOWER_ECOLI"]] <- list(
  df = df,
  training_rows = training_rows,
  df_train = df_train,
  df_test = df_test,
  model = m6,
  cutoff = 0.2
)

# MYSTIC_ECOLI -------------------------------------------------------

df <- list_models[["MYSTIC_ECOLI"]]

set.seed(seed)
training_rows <- createDataPartition(
  y = df$exceedance_class,
  p = 0.75,
  list = FALSE
)

df_train <- df[training_rows, ]
df_test <- df[-training_rows,]
table(df_train$exceedance_class)
table(df_test$exceedance_class)

mods <- list(
  all = train_models(df_train[, predictor_sets$all], df_train$exceedance_class),
  some = train_models(df_train[, predictor_sets$some], df_train$exceedance_class)
)

cutoffs <- c(
  "step" = 0.25,
  "net" = 0.2,
  "rf" = 0.2,
  "gbm" = 0.2,
  "glm" = 0.2
)

df_mods <- crossing(
  predictor_set = c("all", "some"),
  model_name = c("step", "net", "rf", "gbm", "glm")
) %>%
  mutate(
    model = map2(predictor_set, model_name, function (p, n) {
      mods[[p]][[n]]
    }),
    cutoff = map(model_name, ~ cutoffs[.]),
    roc_train = map(model, ~ roc(df_train$exceedance, predict(., newdata = df_train, type = "prob")[["yes"]])),
    roc_test = map(model, ~ roc(df_test$exceedance, predict(., newdata = df_test, type = "prob")[["yes"]])),
    auc_train = map_dbl(roc_train, ~ as.numeric(pROC::auc(.))),
    auc_test = map_dbl(roc_test, ~ as.numeric(pROC::auc(.))),
    auc_diff = (auc_test - auc_train) / 2,
    auc_mean = (auc_train + auc_test) / 2,
    cf_train = map(model, cfm, df = df_train, prob = cutoff),
    cf_test = map(model, cfm, df = df_test, prob = cutoff),
    cutoffs_train = map(model, cutoff_df, df = df_train),
    cutoffs_test = map(model, cutoff_df, df = df_test)
  )

filter(df_mods, predictor_set == "some", model_name == "step")$model[[1]] %>% summary

filter(df_mods, predictor_set == "some", model_name == "step")$cutoffs_train[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "train") +
  theme(aspect.ratio = 1)

filter(df_mods, predictor_set == "some", model_name == "step")$cf_train[[1]]
filter(df_mods, predictor_set == "some", model_name == "step")$cf_test[[1]]

plot(filter(df_mods, predictor_set == "some", model_name == "step")$roc_train[[1]])
plot(filter(df_mods, predictor_set == "some", model_name == "step")$roc_test[[1]])

df_train[, c("concentration", "exceedance_class", names(coef(mods$some$step$finalModel))[-1])] %>%
  gather(var, value, -concentration, -exceedance_class) %>%
  ggplot(aes(value, log10(concentration), color = exceedance_class)) +
  geom_point() +
  facet_wrap(~ var, scales = "free_x")

df %>%
  ggplot(aes(precip_sum_p24hr_lag0hr, precip_sum_p24hr_lag24hr, color = exceedance_class)) +
  geom_point()

m1 <- fit_glm(df_train, c("precip_sum_p24hr_lag0hr", "precip_sum_p24hr_lag24hr", "hours_since_050in_precip_event"))
m2 <- fit_glm(df_train, c("precip_sum_p48hr_lag0hr", "hours_since_050in_precip_event"))
m3 <- fit_glm(df_train, c("precip_sum_p24hr_lag0hr", "hours_since_050in_precip_event"))
m4 <- fit_glm(df_train, c("precip_sum_p24hr_lag24hr", "hours_since_050in_precip_event"))
m5 <- fit_glm(df_train, c("precip_sum_p24hr_lag0hr", "precip_sum_p24hr_lag24hr"))

df_glms <- data_frame(
  id = paste0("m", 1:5),
  model_name = "glm",
  model = list(m1, m2, m3, m4, m5),
  cutoff = 0.2
) %>%
  mutate(
    predictors = map_chr(model, ~ paste0(sort(names(.$trainingData))[-1], collapse = ", ")),
    aic = map_dbl(model, ~ .$finalModel$aic),
    roc_train = map(model, ~ roc(df_train$exceedance, predict(., newdata = df_train, type = "prob")[["yes"]])),
    roc_test = map(model, ~ roc(df_test$exceedance, predict(., newdata = df_test, type = "prob")[["yes"]])),
    auc_train = map_dbl(roc_train, ~ as.numeric(pROC::auc(.))),
    auc_test = map_dbl(roc_test, ~ as.numeric(pROC::auc(.))),
    auc_diff = (auc_test - auc_train) / 2,
    auc_mean = (auc_train + auc_test) / 2,
    cf_train = map2(model, cutoff, ~ cfm(m = .x, df = df_train, prob = .y)),
    cf_test = map2(model, cutoff, ~ cfm(m = .x, df = df_test, prob = .y)),
    cfpr_train = map2(model, cutoff, ~ cfm(m = .x, df = df_train, prob = .y, mode = "prec_recall")),
    cfpr_test = map2(model, cutoff, ~ cfm(m = .x, df = df_test, prob = .y, mode = "prec_recall")),
    cutoffs_train = map(model, cutoff_df, df = df_train),
    cutoffs_test = map(model, cutoff_df, df = df_test)
  )

list_glms <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5)
summary(resamples(list_glms))
bwplot(resamples(list_glms))
densityplot(resamples(list_glms))

df_glms %>%
  dplyr::select(-model, -predictors, -roc_train, -roc_test, -cf_train, -cf_test, -cfpr_train, -cfpr_test, -cutoffs_train, -cutoffs_test)

filter(df_glms, id == "m1")$predictors

filter(df_glms, id == "m1")$cf_train[[1]]
filter(df_glms, id == "m1")$cf_test[[1]]

filter(df_glms, id == "m1")$cfpr_train[[1]]
filter(df_glms, id == "m1")$cfpr_test[[1]]

df_train[, c("concentration", "exceedance_class", names(coef(filter(df_glms, id == "m1")$model[[1]]$finalModel))[-1])] %>%
  gather(var, value, -concentration, -exceedance_class) %>%
  ggplot(aes(value, log10(concentration), color = exceedance_class)) +
  geom_point() +
  facet_wrap(~ var, scales = "free_x")

p1 <- filter(df_glms, id == "m1")$cutoffs_train[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "train") +
  theme(aspect.ratio = 1)
p2 <- filter(df_glms, id == "m1")$cutoffs_test[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "test") +
  theme(aspect.ratio = 1)
grid.arrange(p1, p2, ncol = 2)

final[["MYSTIC_ECOLI"]] <- list(
  df = df,
  training_rows = training_rows,
  df_train = df_train,
  df_test = df_test,
  model = m1,
  cutoff = 0.2
)

# SHANNON_ENT -------------------------------------------------------

df <- list_models[["SHANNON_ENT"]]

set.seed(seed)
training_rows <- createDataPartition(
  y = df$exceedance_class,
  p = 0.75,
  list = FALSE
)

df_train <- df[training_rows, ]
df_test <- df[-training_rows,]
table(df_train$exceedance_class)
table(df_test$exceedance_class)

mods <- list(
  all = train_models(df_train[, predictor_sets$all], df_train$exceedance_class),
  some = train_models(df_train[, predictor_sets$some], df_train$exceedance_class)
)

cutoffs <- c(
  "step" = 0.2,
  "net" = 0.2,
  "rf" = 0.2,
  "gbm" = 0.2,
  "glm" = 0.2
)

df_mods <- crossing(
  predictor_set = c("all", "some"),
  model_name = c("step", "net", "rf", "gbm", "glm")
) %>%
  mutate(
    model = map2(predictor_set, model_name, function (p, n) {
      mods[[p]][[n]]
    }),
    cutoff = map(model_name, ~ cutoffs[.]),
    roc_train = map(model, ~ roc(df_train$exceedance, predict(., newdata = df_train, type = "prob")[["yes"]])),
    roc_test = map(model, ~ roc(df_test$exceedance, predict(., newdata = df_test, type = "prob")[["yes"]])),
    auc_train = map_dbl(roc_train, ~ as.numeric(pROC::auc(.))),
    auc_test = map_dbl(roc_test, ~ as.numeric(pROC::auc(.))),
    auc_diff = (auc_test - auc_train) / 2,
    auc_mean = (auc_train + auc_test) / 2,
    cf_train = map(model, cfm, df = df_train, prob = cutoff),
    cf_test = map(model, cfm, df = df_test, prob = cutoff),
    cutoffs_train = map(model, cutoff_df, df = df_train),
    cutoffs_test = map(model, cutoff_df, df = df_test)
  )

filter(df_mods, predictor_set == "some", model_name == "step")$model[[1]] %>% summary

filter(df_mods, predictor_set == "some", model_name == "step")$cutoffs_train[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "train") +
  theme(aspect.ratio = 1)

filter(df_mods, predictor_set == "some", model_name == "step")$cf_train[[1]]
filter(df_mods, predictor_set == "some", model_name == "step")$cf_test[[1]]

plot(filter(df_mods, predictor_set == "some", model_name == "step")$roc_train[[1]])
plot(filter(df_mods, predictor_set == "some", model_name == "step")$roc_test[[1]])

df_train[, c("concentration", "exceedance_class", names(coef(mods$some$step$finalModel))[-1])] %>%
  gather(var, value, -concentration, -exceedance_class) %>%
  ggplot(aes(value, log10(concentration), color = exceedance_class)) +
  geom_point() +
  facet_wrap(~ var, scales = "free_x")

df_train %>%
  ggplot(aes(aberjona_logflow_p1d, wspd_mean_p1d, color = exceedance_class)) +
  geom_point(shape = 16) +
  geom_point(data = df_test, shape = 17)

df_train %>%
  ggplot(aes(aberjona_logflow_p1d)) +
  geom_histogram()

m1 <- fit_glm(df_train, c("aberjona_logflow_p1d", "wspd_mean_p1d"))
m2 <- fit_glm(df_train, c("aberjona_logflow_p1d"))
m3 <- fit_glm(df, c("aberjona_logflow_p1d"))

df_glms <- data_frame(
  id = paste0("m", 1:3),
  model_name = "glm",
  model = list(m1, m2, m3),
  cutoff = 0.2
) %>%
  mutate(
    predictors = map_chr(model, ~ paste0(sort(names(.$trainingData))[-1], collapse = ", ")),
    aic = map_dbl(model, ~ .$finalModel$aic),
    roc_train = map(model, ~ roc(df_train$exceedance, predict(., newdata = df_train, type = "prob")[["yes"]])),
    roc_test = map(model, ~ roc(df_test$exceedance, predict(., newdata = df_test, type = "prob")[["yes"]])),
    auc_train = map_dbl(roc_train, ~ as.numeric(pROC::auc(.))),
    auc_test = map_dbl(roc_test, ~ as.numeric(pROC::auc(.))),
    auc_diff = (auc_test - auc_train) / 2,
    auc_mean = (auc_train + auc_test) / 2,
    cf_train = map2(model, cutoff, ~ cfm(m = .x, df = df_train, prob = .y)),
    cf_test = map2(model, cutoff, ~ cfm(m = .x, df = df_test, prob = .y)),
    cfpr_train = map2(model, cutoff, ~ cfm(m = .x, df = df_train, prob = .y, mode = "prec_recall")),
    cfpr_test = map2(model, cutoff, ~ cfm(m = .x, df = df_test, prob = .y, mode = "prec_recall")),
    cutoffs_train = map(model, cutoff_df, df = df_train),
    cutoffs_test = map(model, cutoff_df, df = df_test)
  )


list_glms <- list(m1 = m1, m2 = m2, m3 = m3)
summary(resamples(list_glms))
bwplot(resamples(list_glms))
densityplot(resamples(list_glms))

df_glms %>%
  dplyr::select(-model, -predictors, -roc_train, -roc_test, -cf_train, -cf_test, -cfpr_train, -cfpr_test, -cutoffs_train, -cutoffs_test)

filter(df_glms, id == "m2")$predictors

filter(df_glms, id == "m2")$cf_train[[1]]
filter(df_glms, id == "m2")$cf_test[[1]]
filter(df_glms, id == "m3")$cf_train[[1]]
filter(df_glms, id == "m3")$cf_test[[1]]

filter(df_glms, id == "m2")$cfpr_train[[1]]
filter(df_glms, id == "m2")$cfpr_test[[1]]

df_train[, c("concentration", "exceedance_class", names(coef(filter(df_glms, id == "m2")$model[[1]]$finalModel))[-1])] %>%
  gather(var, value, -concentration, -exceedance_class) %>%
  ggplot(aes(value, log10(concentration), color = exceedance_class)) +
  geom_point() +
  facet_wrap(~ var, scales = "free_x")

p1 <- filter(df_glms, id == "m2")$cutoffs_train[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "train") +
  theme(aspect.ratio = 1)
p2 <- filter(df_glms, id == "m2")$cutoffs_test[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "test") +
  theme(aspect.ratio = 1)
grid.arrange(p1, p2, ncol = 2)

final[["SHANNON_ENT"]] <- list(
  df = df,
  training_rows = training_rows,
  df_train = df_train,
  df_test = df_test,
  model = m2,
  cutoff = 0.2
)
#
# mboost <- train(
#   exceedance_class ~ aberjona_logflow_p1d,
#   data = df_train,
#   method = "glmboost",
#   metric="ROC",
#   trControl = trControl,
#   tuneLength = 5,
#   preProcess = c("center", "scale"),
#   family = Binomial(link = c("logit"))
# )
#
# cfm(m = mboost, df = df_train, prob = 0.25)
# cfm(m = mboost, df = df_test, prob = 0.25)
#
# summary(mboost)
# summary(m2)
#
# cutoff_df(m = mboost, df = df_train) %>%
#   gather(term, value, -cutoff, -roc) %>%
#   ggplot(aes(cutoff, value, color = term)) +
#   geom_line() +
#   labs(title = "train") +
#   theme(aspect.ratio = 1)


# WEDGE_ECOLI -------------------------------------------------------

df <- list_models[["WEDGE_ECOLI"]]

set.seed(seed)
training_rows <- createDataPartition(
  y = df$exceedance_class,
  p = 0.75,
  list = FALSE
)

df_train <- df[training_rows, ]
df_test <- df[-training_rows,]
table(df_train$exceedance_class)
table(df_test$exceedance_class)

mods <- list(
  all = train_models(df_train[, predictor_sets$all], df_train$exceedance_class),
  some = train_models(df_train[, predictor_sets$some], df_train$exceedance_class)
)

cutoffs <- c(
  "step" = 0.2,
  "net" = 0.2,
  "rf" = 0.2,
  "gbm" = 0.2,
  "glm" = 0.2
)

df_mods <- crossing(
  predictor_set = c("all", "some"),
  model_name = c("step", "net", "rf", "gbm", "glm")
) %>%
  mutate(
    model = map2(predictor_set, model_name, function (p, n) {
      mods[[p]][[n]]
    }),
    cutoff = map(model_name, ~ cutoffs[.]),
    roc_train = map(model, ~ roc(df_train$exceedance, predict(., newdata = df_train, type = "prob")[["yes"]])),
    roc_test = map(model, ~ roc(df_test$exceedance, predict(., newdata = df_test, type = "prob")[["yes"]])),
    auc_train = map_dbl(roc_train, ~ as.numeric(pROC::auc(.))),
    auc_test = map_dbl(roc_test, ~ as.numeric(pROC::auc(.))),
    auc_diff = (auc_test - auc_train) / 2,
    auc_mean = (auc_train + auc_test) / 2,
    cf_train = map(model, cfm, df = df_train, prob = cutoff),
    cf_test = map(model, cfm, df = df_test, prob = cutoff),
    cutoffs_train = map(model, cutoff_df, df = df_train),
    cutoffs_test = map(model, cutoff_df, df = df_test)
  )

summary(resamples(mods$all))
summary(resamples(mods$some))
bwplot(resamples(mods$some))
bwplot(resamples(mods$all))

filter(df_mods, predictor_set == "all", model_name == "net")$model[[1]] %>% varImp

filter(df_mods, predictor_set == "some", model_name == "step")$model[[1]] %>% summary

filter(df_mods, predictor_set == "some", model_name == "step")$cutoffs_train[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "train") +
  theme(aspect.ratio = 1)

filter(df_mods, predictor_set == "some", model_name == "step")$cf_train[[1]]
filter(df_mods, predictor_set == "some", model_name == "step")$cf_test[[1]]

plot(filter(df_mods, predictor_set == "some", model_name == "step")$roc_train[[1]])
plot(filter(df_mods, predictor_set == "some", model_name == "step")$roc_test[[1]])

df_train[, c("concentration", "exceedance_class", names(coef(mods$some$step$finalModel))[-1])] %>%
  gather(var, value, -concentration, -exceedance_class) %>%
  ggplot(aes(value, log10(concentration), color = exceedance_class)) +
  geom_point() +
  facet_wrap(~ var, scales = "free_x")

df_train %>%
  ggplot(aes(temp_mean_p1d, log10(concentration), color = exceedance_class)) +
  geom_point(shape = 16) +
  geom_point(data = df_test, shape = 17)

m1 <- fit_glm(df_train, c("temp_mean_p1d"))
m2 <- fit_glm(df_train, c("pressure_change_p72hr"))
m3 <- fit_glm(df, c("pressure_change_p72hr", "temp_mean_p1d"))

df_glms <- data_frame(
  id = paste0("m", 1:3),
  model_name = "glm",
  model = list(m1, m2, m3),
  cutoff = 0.2
) %>%
  mutate(
    predictors = map_chr(model, ~ paste0(sort(names(.$trainingData))[-1], collapse = ", ")),
    aic = map_dbl(model, ~ .$finalModel$aic),
    roc_train = map(model, ~ roc(df_train$exceedance, predict(., newdata = df_train, type = "prob")[["yes"]])),
    roc_test = map(model, ~ roc(df_test$exceedance, predict(., newdata = df_test, type = "prob")[["yes"]])),
    auc_train = map_dbl(roc_train, ~ as.numeric(pROC::auc(.))),
    auc_test = map_dbl(roc_test, ~ as.numeric(pROC::auc(.))),
    auc_diff = (auc_test - auc_train) / 2,
    auc_mean = (auc_train + auc_test) / 2,
    cf_train = map2(model, cutoff, ~ cfm(m = .x, df = df_train, prob = .y)),
    cf_test = map2(model, cutoff, ~ cfm(m = .x, df = df_test, prob = .y)),
    cfpr_train = map2(model, cutoff, ~ cfm(m = .x, df = df_train, prob = .y, mode = "prec_recall")),
    cfpr_test = map2(model, cutoff, ~ cfm(m = .x, df = df_test, prob = .y, mode = "prec_recall")),
    cutoffs_train = map(model, cutoff_df, df = df_train),
    cutoffs_test = map(model, cutoff_df, df = df_test)
  )

list_glms <- list(m1 = m1, m2 = m2, m3 = m3)
summary(resamples(list_glms))
bwplot(resamples(list_glms))

df_glms %>%
  dplyr::select(-model, -predictors, -roc_train, -roc_test, -cf_train, -cf_test, -cfpr_train, -cfpr_test, -cutoffs_train, -cutoffs_test)

filter(df_glms, id == "m1")$predictors

filter(df_glms, id == "m1")$cf_train[[1]]
filter(df_glms, id == "m1")$cf_test[[1]]

filter(df_glms, id == "m1")$cfpr_train[[1]]
filter(df_glms, id == "m1")$cfpr_test[[1]]

df_train[, c("concentration", "exceedance_class", names(coef(filter(df_glms, id == "m1")$model[[1]]$finalModel))[-1])] %>%
  gather(var, value, -concentration, -exceedance_class) %>%
  ggplot(aes(value, log10(concentration), color = exceedance_class)) +
  geom_point() +
  facet_wrap(~ var, scales = "free_x")

p1 <- filter(df_glms, id == "m1")$cutoffs_train[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "train") +
  theme(aspect.ratio = 1)
p2 <- filter(df_glms, id == "m1")$cutoffs_test[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "test") +
  theme(aspect.ratio = 1)
grid.arrange(p1, p2, ncol = 2)

# final[["WEDGE_ECOLI"]] <- list(
#   df = df,
#   training_rows = training_rows,
#   df_train = df_train,
#   df_test = df_test,
#   model = m2,
#   cutoff = 0.2
# )


# MYSTICBOB_ENT -------------------------------------------------------

df <- list_models[["MYSTICBOB_ENT"]]

set.seed(seed)
training_rows <- createDataPartition(
  y = df$exceedance_class,
  p = 0.75,
  list = FALSE
)

df_train <- df[training_rows, ]
df_test <- df[-training_rows,]
table(df_train$exceedance_class)
table(df_test$exceedance_class)

mods <- list(
  all = train_models(df_train[, predictor_sets$all], df_train$exceedance_class),
  some = train_models(df_train[, predictor_sets$some], df_train$exceedance_class)
)

cutoffs <- c(
  "step" = 0.2,
  "net" = 0.2,
  "rf" = 0.2,
  "gbm" = 0.2,
  "glm" = 0.2
)

df_mods <- crossing(
  predictor_set = c("all", "some"),
  model_name = c("step", "net", "rf", "gbm", "glm")
) %>%
  mutate(
    model = map2(predictor_set, model_name, function (p, n) {
      mods[[p]][[n]]
    }),
    cutoff = map(model_name, ~ cutoffs[.]),
    roc_train = map(model, ~ roc(df_train$exceedance, predict(., newdata = df_train, type = "prob")[["yes"]])),
    roc_test = map(model, ~ roc(df_test$exceedance, predict(., newdata = df_test, type = "prob")[["yes"]])),
    auc_train = map_dbl(roc_train, ~ as.numeric(pROC::auc(.))),
    auc_test = map_dbl(roc_test, ~ as.numeric(pROC::auc(.))),
    auc_diff = (auc_test - auc_train) / 2,
    auc_mean = (auc_train + auc_test) / 2,
    cf_train = map(model, cfm, df = df_train, prob = cutoff),
    cf_test = map(model, cfm, df = df_test, prob = cutoff),
    cutoffs_train = map(model, cutoff_df, df = df_train),
    cutoffs_test = map(model, cutoff_df, df = df_test)
  )

summary(resamples(mods$all))
summary(resamples(mods$some))
bwplot(resamples(mods$some))
bwplot(resamples(mods$all))

df_mods %>%
  dplyr::select(-starts_with("roc_"), -starts_with("cf_"), -starts_with("cutoffs_"))

filter(df_mods, predictor_set == "all", model_name == "net")$model[[1]] %>% varImp

filter(df_mods, predictor_set == "some", model_name == "step")$model[[1]] %>% summary

filter(df_mods, predictor_set == "some", model_name == "step")$cutoffs_train[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "train") +
  theme(aspect.ratio = 1)

filter(df_mods, predictor_set == "some", model_name == "step")$cf_train[[1]]
filter(df_mods, predictor_set == "some", model_name == "step")$cf_test[[1]]

plot(filter(df_mods, predictor_set == "some", model_name == "step")$roc_train[[1]])
plot(filter(df_mods, predictor_set == "some", model_name == "step")$roc_test[[1]])

df_train[, c("concentration", "exceedance_class", names(coef(mods$some$step$finalModel))[-1])] %>%
  gather(var, value, -concentration, -exceedance_class) %>%
  ggplot(aes(value, log10(concentration), color = exceedance_class)) +
  geom_point() +
  facet_wrap(~ var, scales = "free_x")

df_train %>%
  ggplot(aes(precip_sum_p24hr_lag0hr, log10(concentration), color = exceedance_class)) +
  geom_point(shape = 16) +
  geom_point(data = df_test, shape = 17)

# MYSTICBOB_ECOLI -------------------------------------------------------

df <- list_models[["MYSTICBOB_ECOLI"]]

set.seed(seed)
training_rows <- createDataPartition(
  y = df$exceedance_class,
  p = 0.75,
  list = FALSE
)

df_train <- df[training_rows, ]
df_test <- df[-training_rows,]
table(df_train$exceedance_class)
table(df_test$exceedance_class)

mods <- list(
  all = train_models(df_train[, predictor_sets$all], df_train$exceedance_class),
  some = train_models(df_train[, predictor_sets$some], df_train$exceedance_class)
)

cutoffs <- c(
  "step" = 0.2,
  "net" = 0.2,
  "rf" = 0.2,
  "gbm" = 0.2,
  "glm" = 0.2
)

df_mods <- crossing(
  predictor_set = c("all", "some"),
  model_name = c("step", "net", "rf", "gbm", "glm")
) %>%
  mutate(
    model = map2(predictor_set, model_name, function (p, n) {
      mods[[p]][[n]]
    }),
    cutoff = map(model_name, ~ cutoffs[.]),
    roc_train = map(model, ~ roc(df_train$exceedance, predict(., newdata = df_train, type = "prob")[["yes"]])),
    roc_test = map(model, ~ roc(df_test$exceedance, predict(., newdata = df_test, type = "prob")[["yes"]])),
    auc_train = map_dbl(roc_train, ~ as.numeric(pROC::auc(.))),
    auc_test = map_dbl(roc_test, ~ as.numeric(pROC::auc(.))),
    auc_diff = (auc_test - auc_train) / 2,
    auc_mean = (auc_train + auc_test) / 2,
    cf_train = map(model, cfm, df = df_train, prob = cutoff),
    cf_test = map(model, cfm, df = df_test, prob = cutoff),
    cutoffs_train = map(model, cutoff_df, df = df_train),
    cutoffs_test = map(model, cutoff_df, df = df_test)
  )

summary(resamples(mods$all))
summary(resamples(mods$some))
bwplot(resamples(mods$some))
bwplot(resamples(mods$all))

df_mods %>%
  dplyr::select(-starts_with("roc_"), -starts_with("cf_"), -starts_with("cutoffs_"))

filter(df_mods, predictor_set == "all", model_name == "net")$model[[1]] %>% varImp

filter(df_mods, predictor_set == "some", model_name == "step")$model[[1]] %>% summary

filter(df_mods, predictor_set == "some", model_name == "step")$cutoffs_train[[1]] %>%
  gather(term, value, -cutoff, -roc) %>%
  ggplot(aes(cutoff, value, color = term)) +
  geom_line() +
  labs(title = "train") +
  theme(aspect.ratio = 1)

filter(df_mods, predictor_set == "some", model_name == "step")$cf_train[[1]]
filter(df_mods, predictor_set == "some", model_name == "step")$cf_test[[1]]

plot(filter(df_mods, predictor_set == "some", model_name == "step")$roc_train[[1]])
plot(filter(df_mods, predictor_set == "some", model_name == "step")$roc_test[[1]])

df_train[, c("concentration", "exceedance_class", names(coef(mods$some$step$finalModel))[-1])] %>%
  gather(var, value, -concentration, -exceedance_class) %>%
  ggplot(aes(value, log10(concentration), color = exceedance_class)) +
  geom_point() +
  facet_wrap(~ var, scales = "free_x")

df_train %>%
  ggplot(aes(precip_sum_p24hr_lag0hr, log10(concentration), color = exceedance_class)) +
  geom_point(shape = 16) +
  geom_point(data = df_test, shape = 17)

# export -----------------------------------------------------------------

df_final <- data_frame(
  model_id = c("MYSTIC_ECOLI", "SHANNON_ENT", "MALDENLOWER_ECOLI")
) %>%
  mutate(
    model = map(model_id, ~ final[[.]]$model),
    df = map(model_id, ~ final[[.]]$df),
    df_train = map(model_id, ~ final[[.]]$df_train),
    df_test = map(model_id, ~ final[[.]]$df_test),
    freq_exceedance = map_dbl(df, ~ mean(.$exceedance)),
    n_sample = map_int(df, nrow),
    training_rows = map(model_id, ~ final[[.]]$training_rows),
    cutoff = map_dbl(model_id, ~ final[[.]]$cutoff),
    n_predictors = map_int(model, ~ length(.$finalModel$xNames)),
    aic = map_dbl(model, ~ .$finalModel$aic),
    nulldev = map_dbl(model, ~ .$finalModel$null.deviance),
    dev = map_dbl(model, ~ .$finalModel$deviance),
    r2 = 1 - dev / nulldev,
    roc_train = map2_dbl(model, df_train, ~ pROC::auc(roc(.y$exceedance, predict(.x, newdata = .y, type = "prob")[["yes"]]))),
    roc_test = map2_dbl(model, df_test, ~ pROC::auc(roc(.y$exceedance, predict(.x, newdata = .y, type = "prob")[["yes"]]))),
    cfm_train = pmap(list(m = model, df = df_train, prob = cutoff), cfm),
    cfm_test = pmap(list(m = model, df = df_test, prob = cutoff), cfm),
    cfm_train_df = map(cfm_train, function (x) {
      data_frame(
        accuracy_train = x$overall['Accuracy'],
        kappa_train = x$overall['Kappa'],
        accuracy_null_train = x$overall['AccuracyNull'],
        accuracy_pval_train = x$overall['AccuracyPValue'],
        sensitivity_train = x$byClass['Sensitivity'],
        specificity_train = x$byClass['Specificity'],
        precision_train =  x$byClass['Precision'],
        recall_train =  x$byClass['Recall']
      )
    }),
    cfm_test_df = map(cfm_test, function (x) {
      data_frame(
        accuracy_test = x$overall['Accuracy'],
        kappa_test = x$overall['Kappa'],
        accuracy_null_test = x$overall['AccuracyNull'],
        accuracy_pval_test = x$overall['AccuracyPValue'],
        sensitivity_test = x$byClass['Sensitivity'],
        specificity_test = x$byClass['Specificity'],
        precision_test =  x$byClass['Precision'],
        recall_test =  x$byClass['Recall']
      )
    })
  ) %>%
  unnest(cfm_train_df) %>%
  unnest(cfm_test_df)

df_final %>%
  dplyr::select(-model, -training_rows, -starts_with("df"), -starts_with("cfm")) %>%
  View

df_final %>%
  dplyr::select(-model, -training_rows, -starts_with("df"), -starts_with("cfm"))

summary(resamples(set_names(df_final$model, nm = df_final$model_id)))
bwplot(resamples(set_names(df_final$model, nm = df_final$model_id)))

saveRDS(df_final, file = "models.rds")



