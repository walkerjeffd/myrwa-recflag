# create water quality dataset

library(RPostgreSQL)
library(tidyverse)
library(jsonlite)
library(lubridate)
library(stringr)

rm(list = ls())


# load wq -----------------------------------------------------------------


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
) %>%
  filter(SampleTypeID == "S")
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
) %>%
  filter(SampleTypeID == "S")


# load model --------------------------------------------------------------

location_model <- read_csv(
  "data/location_model.csv",
  col_types = cols(
    LocationID = col_character(),
    ModelLocation = col_character()
  )
)

model_characteristic <- read_csv(
  "data/model_characteristic.csv",
  col_types = cols(
    ModelLocation = col_character(),
    CharacteristicID = col_character()
  )
)

bacteria_standards <- c(
  "ECOLI" = 1260,
  "ENT" = 350
)

df <- bind_rows(
  df_muni,
  df_mwra,
  df_myrwa
) %>%
  filter(CharacteristicID %in% c("ENT", "ECOLI")) %>%
  inner_join(location_model, by = "LocationID") %>%
  inner_join(model_characteristic, by = c("ModelLocation", "CharacteristicID")) %>%
  mutate(
    ModelID = paste(ModelLocation, CharacteristicID, sep = "_"),
    Datetime = force_tz(Datetime, tzone = "US/Eastern"),
    StandardValue = as.numeric(plyr::revalue(CharacteristicID, bacteria_standards)),
    Exceedance = ResultValue > StandardValue
  )

table(df$LocationID, df$ProjectID)
table(df$LocationID, df$ModelLocation)
table(df$ModelLocation, df$CharacteristicID)
table(df$ModelLocation, df$ProjectID)
table(df$ModelID, df$Exceedance)
table(df$ModelID, df$Exceedance) %>% prop.table(margin = 1)
table(df$CharacteristicID, df$StandardValue)

df %>%
  ggplot(aes(Datetime, log10(ResultValue), color = ResultValue > StandardValue)) +
  geom_point() +
  facet_wrap(~ModelID)

df_out <- df %>%
  select(
    model_id = ModelID,
    model_location = ModelLocation,
    location_id = LocationID,
    project_id = ProjectID,
    parameter = CharacteristicID,
    standard_value = StandardValue,
    timestamp = Datetime,
    concentration = ResultValue,
    exceedance = Exceedance
  )

table(df_out$model_id, df_out$exceedance)
table(hour(df_out$timestamp))

df_out %>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
  write_csv("data/wq.csv")

df_out %>%
  saveRDS("data/wq.rds")
