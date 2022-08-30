library(tidyverse)
library(lubridate)
library(agclimtools)
library(ggplot2)
library(modelr)
library(dplyr)

Climate <- read.csv("/home/supriya.savalkar/Disaggregation/Climate_Change/Disaggregation_climate_model_data_RCP45.csv")

df_dat <- split(FST, FST$STATION_NAME)
lapply(df_dat, function(x) write.csv(x, paste0(x$STATION_NAME[1], '.csv'), row.names = FALSE))
