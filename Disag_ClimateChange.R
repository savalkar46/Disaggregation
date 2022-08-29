library(tidyverse)
library(lubridate)
library(agclimtools)
library(ggplot2)
library(Metrics)
library(modelr)
library(plotrix)
library(dplyr)
library(raster)
library(tidybayes)
library(ggpubr)
library(hrbrthemes)
library(plotly)
library(viridis)
library(heatmaply)
library(RColorBrewer)
packages = c('dplyr','seriation', 'dendextend', 'heatmaply', 'tidyverse')

Full_Model <- read.csv("E:/Ag_WeatherNet/AWN/AWN_DAILY.csv/Analysis_Kamiak/SR_Temp_Combine.csv")

Temp_Full <- Full_Model |> dplyr::select(STATION_ID, STATION_NAME,LATITUDE,LONGITUDE,Elevation,Year,Month,Day, DOY,
                                         hour, WindSpeed, RELATIVE_HUMIDITY,DewPoint,
                                         tmax, tmin,DisagC,AirTemp,FS_TempS_SRS,FS_TempD_SRS,
                                         FS_TempD_SRD,FS_TempS_SRD, SR_hourly_Obs, Disag_SR , Deviation)
## Sunburn
## Case1: Sunburn:  ST SR: ST temp
Temp_Full$hour24 <-Temp_Full$hour+1
Temp_Full$time <- Temp_Full$hour24-Temp_Full$hour

## Station Observed
ST_T_SR_FST <- Temp_Full |>  
  filter_at(vars(18), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure)|> group_by(Year, DOY, STATION_NAME) |> mutate(STT_STSR=sum(Exposure))|> 
  distinct(STT_STSR)

## Disaggreagted
SRD_TD_FST <- Temp_Full|>  
  filter_at(vars(20), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure)|> group_by(Year,DOY, STATION_NAME) |> mutate(SRD_TD=sum(Exposure))|> 
  distinct(SRD_TD)

df_list <- list(ST_T_SR_FST,SRD_TD_FST)      

FST <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list) 

Tmax <- Temp_Full |> select( STATION_NAME, Year, DOY,tmin, tmax)

Data <- merge(FST ,Tmax, by=c('STATION_NAME', 'Year', 'DOY')) |> distinct()
Data[is.na(Data)] <- 0
Data <- Data |> mutate(Error=(((SRD_TD-STT_STSR)/SRD_TD)*100))
Data <- Data [!is.infinite(Data$Error),]

ggplot(Data, aes(x=tmax, y=Error, group=STATION_NAME)) +
    geom_point()+ theme_bw()+facet_wrap(~STATION_NAME)
