library(caret)
library(pROC)
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

df <- list_models$SHANNON_ENT %>%
  mutate(
    standard_value = 104,
    exceedance = 1 * (concentration > standard_value),
    exceedance_class = factor(if_else(exceedance == 1, "yes", "no"), levels = c("yes", "no"))
  )


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
    "aberjona_logflow_p1d", "aberjona_logflow_p2d", "aberjona_logflow_p3d", "aberjona_logflow_p4d", "aberjona_logflow_p5d",
    "aberjona_logflow_change_p2d_p1d", "aberjona_logflow_change_p3d_p1d", "aberjona_logflow_change_p4d_p1d", "aberjona_logflow_change_p5d_p1d",
    "alewife_logflow_p1d", "alewife_logflow_p2d", "alewife_logflow_p3d", "alewife_logflow_p4d", "alewife_logflow_p5d",
    "alewife_logflow_change_p2d_p1d", "alewife_logflow_change_p3d_p1d", "alewife_logflow_change_p4d_p1d", "alewife_logflow_change_p5d_p1d"
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


# model -------------------------------------------------------------------


set.seed(seed)
training_rows <- createDataPartition(
  y = df$exceedance_class,
  p = 0.75,
  list = FALSE
)


df_train <- df[training_rows, ] %>% filter(!is.na(alewife_logflow_p1d))
df_test <- df[-training_rows,] %>% filter(!is.na(alewife_logflow_p1d))
table(df_train$exceedance_class)
table(df_test$exceedance_class)

mods <- list(
  all = train_models(df_train[, predictor_sets$all], df_train$exceedance_class),
  some = train_models(df_train[, predictor_sets$some], df_train$exceedance_class)
)

cutoffs <- c(
  "step" = 0.35,
  "net" = 0.35,
  "rf" = 0.35,
  "gbm" = 0.35,
  "glm" = 0.35
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
bwplot(resamples(mods$all))
bwplot(resamples(mods$some))

filter(df_mods, model_name == "step")$model[[1]] %>% summary



p <- lapply(df_mods$model_name, function (n) {
  filter(df_mods, model_name == n)$cutoffs_train[[1]] %>%
    gather(term, value, -cutoff, -roc) %>%
    ggplot(aes(cutoff, value, color = term)) +
    geom_line() +
    labs(subtitle = "train", title = n) +
    theme(aspect.ratio = 1)
})
grid.arrange(grobs = p)

filter(df_mods, model_name == "step")$cf_train[[1]]
filter(df_mods, model_name == "step")$cf_test[[1]]

plot(filter(df_mods, model_name == "step")$roc_train[[1]])
plot(filter(df_mods, model_name == "step")$roc_test[[1]])

df_train[, c("concentration", "exceedance_class", names(coef(mods$all$step$finalModel))[-1])] %>%
  gather(var, value, -concentration, -exceedance_class) %>%
  ggplot(aes(value, log10(concentration), color = exceedance_class)) +
  geom_point() +
  facet_wrap(~ var, scales = "free_x")

df_train %>%
  ggplot(aes(aberjona_logflow_p1d, pressure_change_p72hr, color = exceedance_class)) +
  geom_point(shape = 16) +
  geom_point(data = df_test, shape = 17)

df_train %>%
  mutate(precip_sum_p24hr_lag0hr = log10(if_else(precip_sum_p24hr_lag0hr < 0.1, 0.1, precip_sum_p24hr_lag0hr))) %>%
  GGally::ggpairs(aes(color = exceedance_class, alpha = 0.5), columns = c(names(coef(mods$all$step$finalModel))[-1]))

corrplot::corrplot(corr = cor(df_train[, names(coef(mods$all$step$finalModel))[-1]]))
corrgram::corrgram(df_train[, names(coef(mods$all$step$finalModel))[-1]])

df_train %>%
  ggplot(aes(aberjona_logflow_p1d)) +
  geom_histogram()


df_train %>%
  ggplot(aes(aberjona_logflow_change_p4d_p1d, log10(precip_sum_p48hr_lag12hr + 0.1))) +
  geom_point(aes(color = exceedance_class), shape = 16, alpha = 0.5, size = 3)
  # geom_hex(aes(fill = exceedance_class))
df_train %>%
  ggplot(aes(aberjona_logflow_change_p4d_p1d, if_else(precip_sum_p48hr_lag12hr > 0.1, log10(precip_sum_p48hr_lag12hr), NA_real_), color = exceedance_class)) +
  geom_point()

# TODO: dummy variable for precip_sum_p48hr_lag12hr < 0.001
df_train %>%
  filter(precip_sum_p48hr_lag12hr < 0.001, exceedance_class == "yes") %>%
  dplyr::select(timestamp, concentration, aberjona_logflow_change_p4d_p1d, precip_sum_p48hr_lag12hr)

df_train <- df_train %>% mutate(x1 = if_else(precip_sum_p48hr_lag12hr < 0.001, 0, 1))
df_test <- df_test %>% mutate(x1 = if_else(precip_sum_p48hr_lag12hr < 0.001, 0, 1))

m1 <- fit_glm(df_train, c("aberjona_logflow_p1d", "wspd_mean_p1d"))
m2 <- fit_glm(df_train, c("aberjona_logflow_p1d"))
m3 <- fit_glm(df, c("aberjona_logflow_p1d"))
m4 <- fit_glm(df_train, c("aberjona_logflow_p1d", "x1"))

df_glms <- data_frame(
  id = paste0("m", 1:4),
  model_name = "glm",
  model = list(m1, m2, m3, m4),
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


list_glms <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4)
summary(resamples(list_glms))
bwplot(resamples(list_glms))
densityplot(resamples(list_glms))

df_glms %>%
  dplyr::select(-model, -predictors, -roc_train, -roc_test, -cf_train, -cf_test, -cfpr_train, -cfpr_test, -cutoffs_train, -cutoffs_test)

filter(df_glms, id == "m1")$predictors
filter(df_glms, id == "m4")$predictors

filter(df_glms, id == "m4")$cf_train[[1]]
filter(df_glms, id == "m4")$cf_test[[1]]

filter(df_glms, id == "m2")$cfpr_train[[1]]
filter(df_glms, id == "m2")$cfpr_test[[1]]

df_train[, c("concentration", "exceedance_class", names(coef(filter(df_glms, id == "m4")$model[[1]]$finalModel))[-1])] %>%
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
