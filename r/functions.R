
train_models <- function(x, y) {
  x <- as.data.frame(x)

  set.seed(seed)
  m_step <- train(
    x = x, y = y,
    method = "glmStepAIC",
    family = "binomial",
    preProcess = c("center", "scale"),
    metric = "ROC",
    direction = "forward",
    trControl = trControl,
    trace = 0
  )
  set.seed(seed)
  m_net <- train(
    x = x, y = y,
    method = "glmnet",
    metric = "ROC",
    family = "binomial",
    preProcess = c("center", "scale"),
    tuneGrid = expand.grid(alpha = seq(0, 1, 0.2), lambda = c(0.0001, 0.001, 0.01, 0.1)),
    # tuneGrid = expand.grid(alpha = 0, lambda = c(0.0001, 0.001, 0.01, 0.1)),
    trControl = trControl
  )
  set.seed(seed)
  m_rf  <- train(
    x = x, y = y,
    method = "rf",
    preProcess = c("center", "scale"),
    trControl = trControl
  )
  set.seed(seed)
  m_gbm <- train(
    x = x, y = y,
    method = "gbm",
    metric = "ROC",
    preProcess = c("center", "scale"),
    trControl = trControl,
    verbose = FALSE
  )
  # set.seed(seed)
  # m_svm  <- train(
  #   x = x, y = y,
  #   method = "svmRadial",
  #   metric = "ROC",
  #   preProcess = c("center", "scale"),
  #   trControl = trControl
  # )
  set.seed(seed)
  m_glm <- train(
    x = x, y = y,
    method = "glm",
    metric = "ROC",
    family = "binomial",
    preProcess = c("center", "scale"),
    trControl = trControl
  )

  list(
    step = m_step,
    net = m_net,
    rf = m_rf,
    gbm = m_gbm,
    # svm = m_svm,
    glm = m_glm
  )
}

cutoff_df <- function (m, df) {
  data_frame(
    cutoff = seq(0.05, 0.95, 0.05)
  ) %>%
    mutate(
      tcs = map(cutoff, function (cutoff) {
        x <- data_frame(
          yes = predict(m, newdata = df, type = "prob")[["yes"]]
        ) %>%
          mutate(
            no = 1 - yes,
            pred = factor(if_else(yes > cutoff, "yes", "no"), levels = levels(df$exceedance_class)),
            obs = df$exceedance_class
          ) %>%
          as.data.frame()

        tcs <- twoClassSummary(data = x, lev = levels(x$obs), model = NULL)
        coords <- matrix(
          c(1, 1, tcs["Spec"], tcs["Sens"]),
          ncol = 2,
          byrow = TRUE
        )
        colnames(coords) <- c("Spec", "Sens")
        rownames(coords) <- c("Best", "Current")

        tcs_dist <- dist(coords)[1]

        data_frame(
          roc = tcs["ROC"],
          sens = tcs["Sens"],
          spec = tcs["Spec"],
          dist = tcs_dist,
          acc = sum(x$pred == x$obs) / nrow(x)
        )
      })
    ) %>%
    unnest(tcs)
}

cfm <- function (m, df, prob, mode = "sens_spec") {
  # cat(nrow(df), prob, "\n")
  pred_class = factor(
    if_else(
      predict(m, newdata = df, type = "prob")[["yes"]] > prob,
      "yes", "no"
    ),
    levels = c("yes", "no")
  )
  confusionMatrix(
    data = pred_class,
    df$exceedance_class,
    mode = mode
  )
}

fit_glm <- function(df, predictors) {
  set.seed(seed)
  train(
    x = df[, predictors],
    y = df$exceedance_class,
    method = glm2,
    metric = "ROC",
    family = "binomial",
    preProcess = c("center", "scale"),
    trControl = trControl
  )
}