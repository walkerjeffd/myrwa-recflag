library(dplyr)
library(dbplyr)
library(tidyverse)
library(gridExtra)
theme_set(theme_bw())

con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), dbname = "recflag", host = "localhost")

flow <- DBI::dbGetQuery(con, "SELECT id, created_at, datetime, flow FROM streamflow")
ds <- DBI::dbGetQuery(con, "SELECT id, created_at, datetime, values->'temperature' as air_temperature, values->'precipIntensity' as precip_intensity, values->'precipProbability' as precip_probability FROM darksky") %>%
  mutate_at(vars(air_temperature, precip_intensity, precip_probability), as.numeric)

var_labels <- c(
  flow = "Streamflow (cfs)",
  air_temperature = "Air Temperature (degF)",
  precip_intensity = "Precip Intensity (in/hr)",
  precip_probability = "Precip Probability (%)"
)

df <- select(flow, datetime, created_at, value = flow) %>%
  mutate(var = "flow") %>%
  bind_rows(
    select(ds, datetime, created_at, air_temperature, precip_intensity, precip_probability) %>%
      gather(var, value, air_temperature, precip_intensity, precip_probability)
  ) %>%
  mutate(
    var_label = plyr::revalue(var, var_labels)
  )


png("test-db.png", width = 12, height = 8, units = "in", res = 200)

df %>%
  ggplot(aes(datetime, value)) +
  geom_step() +
  facet_grid(var_label ~ ., scales = "free_y", switch = "y") +
  theme(
    strip.placement = "outside",
    strip.background = element_blank()
  ) +
  labs(
    x = "Date/Time",
    y = "",
    subtitle = paste0("Last Updated: ", format(max(df$created_at), "%Y-%m-%d %H:%M %Z"))
  )

dev.off()
