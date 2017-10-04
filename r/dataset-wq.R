# create water quality dataset

library(RPostgreSQL)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(stringr)

rm(list = ls())


model_ids <- c(
  "MAR036_ECOLI",
  "MWRA176_ECOLI",
  "MWRA176_ENT",
  "MAR0065_ECOLI",
  "MYR0435_ECOLI",
  "MYRBOBDOCK_ECOLI",
  "MYRBOBDOCK_ENT",
  "WEPBCH_ECOLI",
  "UPLSHBCH_ENT"
)

model_locations <- c(
  "MAR036" = "MAR036",
  "MWRA176" = "MWRA176",
  "MAR0065" = "MAR0065",
  "MWRA177" = "MYR0435",
  "MYR0435" = "MYR0435",
  "MYRBOBDOCK" = "MYRBOBDOCK",
  "WEPBCHC" = "WEPBCH",
  "WEPBOBC" = "WEPBCH",
  "UPLSHBM" = "UPLSHBCH",
  "UPLSHBC" = "UPLSHBCH"
)

model_standards <- c(
  "MAR036_ECOLI" = "BOAT",
  "MWRA176_ECOLI" = "BOAT",
  "MWRA176_ENT" = "BOAT",
  "MAR0065_ECOLI" = "BOAT",
  "MYR0435_ECOLI" = "BOAT",
  "MYRBOBDOCK_ECOLI" = "BOAT",
  "MYRBOBDOCK_ENT" = "BOAT",
  "WEPBCH_ECOLI" = "SWIM",
  "UPLSHBCH_ENT" = "SWIM"
)

bacteria_standards <- data_frame(
  CharacteristicID = c("ECOLI", "ECOLI", "ENT", "ENT"),
  StandardType = c("SWIM", "BOAT", "SWIM", "BOAT"),
  StandardValue = c(235, 1260, 104, 350)
)

df_muni <- read_csv(
  "https://raw.githubusercontent.com/nesanders/myrwa-rf-bact-prediction/testing/source_data/muni_dcr_hist.csv",
  col_types = cols(
    .default = col_character(),
    X1 = col_integer(),
    DateHour = col_datetime(format = ""),
    VisitID = col_integer(),
    ID = col_integer(),
    ResultValue = col_double(),
    MethodID = col_integer(),
    SampleDepth = col_double(),
    Datetime = col_datetime(format = ""),
    HasFlow = col_integer(),
    Latitude = col_double(),
    Longitude = col_double(),
    LocationTypeID = col_integer(),
    InWatershed = col_integer(),
    StaffGage = col_integer(),
    Precip.48 = col_double()
  )
)
df_mwra <- read_csv(
  "https://raw.githubusercontent.com/nesanders/myrwa-rf-bact-prediction/testing/source_data/mwra_base_recflag.csv",
  col_types = cols(
    .default = col_character(),
    X1 = col_integer(),
    DateHour = col_datetime(format = ""),
    VisitID = col_integer(),
    ID = col_integer(),
    ResultValue = col_double(),
    MethodID = col_integer(),
    SampleDepth = col_double(),
    Datetime = col_datetime(format = ""),
    HasFlow = col_integer(),
    Latitude = col_double(),
    Longitude = col_double(),
    LocationTypeID = col_integer(),
    InWatershed = col_integer(),
    StaffGage = col_integer(),
    Precip.48 = col_double()
  )
)
df_myrwa <- read_csv(
  "https://raw.githubusercontent.com/nesanders/myrwa-rf-bact-prediction/testing/source_data/rec_flag_2015_16.csv",
  col_types = cols(
    .default = col_character(),
    X1 = col_integer(),
    DateHour = col_datetime(format = ""),
    VisitID = col_integer(),
    ID = col_integer(),
    ResultValue = col_double(),
    MethodID = col_integer(),
    SampleDepth = col_double(),
    Datetime = col_datetime(format = ""),
    HasFlow = col_integer(),
    Latitude = col_double(),
    Longitude = col_double(),
    LocationTypeID = col_integer(),
    InWatershed = col_integer(),
    StaffGage = col_integer(),
    Precip.48 = col_double()
  )
)



df <- bind_rows(
  df_muni,
  df_mwra,
  df_myrwa
) %>%
  filter(CharacteristicID %in% c("ENT", "ECOLI")) %>%
  filter(LocationID %in% names(model_locations)) %>%
  mutate(
    Datetime = force_tz(Datetime, tzone = "US/Eastern"),
    ModelLocation = plyr::revalue(LocationID, model_locations),
    ModelID = paste(ModelLocation, CharacteristicID, sep = "_"),
    StandardType = plyr::revalue(ModelID, model_standards)
  ) %>%
  filter(ModelID %in% model_ids) %>%
  left_join(bacteria_standards, by = c("CharacteristicID", "StandardType"))

table(df$LocationID, df$ProjectID)
table(df$ModelLocation, df$CharacteristicID)
table(df$ModelID)
table(df$ModelID, df$ProjectID)
table(df$ModelID, df$StandardType)

df %>%
  ggplot(aes(Datetime, log10(ResultValue), color = ResultValue > StandardValue)) +
  geom_point() +
  facet_wrap(~ModelID)

df_out <- df %>%
  select(
    model_id = ModelID,
    model_location = ModelLocation,
    parameter = CharacteristicID,
    standard_type = StandardType,
    standard_value = StandardValue,
    timestamp = Datetime,
    concentration = ResultValue
  ) %>%
  mutate(
    exceedance = concentration > standard_value
  )

table(df_out$model_id, df_out$exceedance)
table(hour(df_out$timestamp))

df_out %>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
  write_csv("data/wq.csv")

df_out %>%
  saveRDS("data/wq.rds")
