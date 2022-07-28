df_models

mystic_model <- df_models$model[[1]]

malden_model <- df_models$model[[3]]

mystic_model
summary(mystic_model)

malden_model
summary(malden_model)

varImp(malden_model)

malden_model$modelType

x <- df_predict %>%
  filter(name == "MALDENLOWER_ECOLI")
x %>%
  pivot_wider(names_from = "key") %>%
  pivot_longer(-c("name", "timestamp", "exceedence_probability"), names_to = "key") %>%
  # filter(timestamp >= ymd(20220501)) %>%
  ggplot(aes(value, exceedence_probability)) +
  geom_point() +
  facet_wrap(vars(key), scales = "free_x")

x %>%
  pivot_wider(names_from = "key") %>%
  select(-name, -timestamp) %>%
  pairs()


x2 <- pivot_wider(x, names_from = "key")
predict(mystic_model$finalModel, x2, type = "prob")
predict(mystic_model$finalModel, head())

make_prediction(, newdata = x2)

x2$prob2 <- predict(mystic_model, newdata = as.data.frame(x2), type = "prob")[, "yes"]
x2$raw <- predict(mystic_model, newdata = as.data.frame(x2))

x3 <- x2 %>%
  mutate(
    across(
      c(precip_sum_p24hr_lag0hr, precip_sum_p24hr_lag24hr, hours_since_050in_precip_event),
      ~ (.x - mean(.x)) / sd(.x)
    )
  )

x2$prob3 <- predict(mystic_model$finalModel, newdata = as.data.frame(x3))

x2 %>%
  ggplot(aes(prob3, prob2)) +
  geom_point()

varImp(mystic_model)

mystic_model$finalModel$data %>%
  pivot_longer(-.outcome) %>%
  ggplot(aes(value, .outcome)) +
  geom_point() +
  facet_wrap(vars(name), ncol = 1)

library(sjPlot)

plot_model(mystic_model$finalModel, type = "eff", terms = "precip_sum_p24hr_lag0hr")
plot_model(mystic_model$finalModel, type = "eff", terms = "precip_sum_p24hr_lag24hr")
plot_model(mystic_model$finalModel, type = "eff", terms = "hours_since_050in_precip_event")

mystic_model$finalModel$obsLevels