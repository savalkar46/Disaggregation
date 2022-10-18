
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

Full_Model <- read.csv("E:/Ag_WeatherNet/AWN/AWN_DAILY.csv/Analysis_Kamiak/SR_Temp_Combine.csv")

Temp_Full <- Full_Model |> dplyr::select(STATION_ID, STATION_NAME,LATITUDE,LONGITUDE,Elevation,Year,Month,Day, DOY,
                                         hour, WindSpeed, RELATIVE_HUMIDITY,DewPoint,
                                         tmax, tmin,DisagC,AirTemp,FS_TempS_SRS,FS_TempD_SRS,
                                         FS_TempD_SRD,FS_TempS_SRD, SR_hourly_Obs, Disag_SR , Deviation)

is.na(Temp_Full$SR_hourly_Obs) <- Temp_Full$SR_hourly_Obs==0

Wind <- Temp_Full |> group_by(STATION_NAME, Year, DOY) |> filter(Month %in% c("7", "8")) |> mutate(WindT=mean(WindSpeed),
                                                                                                   SolarT=mean(SR_hourly_Obs, na.rm =TRUE))

Wind <- Wind |> select(STATION_ID, STATION_NAME,LATITUDE,LONGITUDE,Elevation,Year,Month,Day, DOY,WindT, SolarT) |> unique()
Wind <- Wind |> na.omit()

##write.csv(Wind, "E:/Ag_WeatherNet/AWN/ClimateChange/Sensitivity/Wind_summer.csv")
Wind <- read.csv("E:/Ag_WeatherNet/AWN/ClimateChange/Sensitivity/Wind_summer.csv")

## Obtaining percentile combination between wind and solar radiation filtering stations
## Low wind with all solar
W1 <- dplyr::filter(Wind, between(WindT, round(0.99*(quantile(Wind$WindT, prob = .10)), 2),round(1.01*(quantile(Wind$WindT, prob = .10)), 2)) &
                      between(SolarT,round(0.99*(quantile(Wind$SolarT, prob = .10)), 2),round(1.01*(quantile(Wind$SolarT, prob = .10)), 2)))
W2 <- dplyr::filter(Wind, between(WindT, round(0.99*(quantile(Wind$WindT, prob = .10)), 2),round(1.01*(quantile(Wind$WindT, prob = .10)), 2)) &
                      between(SolarT,round(0.99*(quantile(Wind$SolarT, prob = .50)), 2),round(1.01*(quantile(Wind$SolarT, prob = .50)), 2)))
W3 <- dplyr::filter(Wind, between(WindT, round(0.99*(quantile(Wind$WindT, prob = .10)), 2),round(1.01*(quantile(Wind$WindT, prob = .10)), 2)) &
                      between(SolarT,round(0.99*(quantile(Wind$SolarT, prob = .90)), 2),round(1.01*(quantile(Wind$SolarT, prob = .90)), 2)))
## Mid wind with all Solar
W4 <- dplyr::filter(Wind, between(WindT, round(0.99*(quantile(Wind$WindT, prob = .50)), 2),round(1.01*(quantile(Wind$WindT, prob = .50)), 2)) &
                      between(SolarT,round(0.99*(quantile(Wind$SolarT, prob = .10)), 2),round(1.01*(quantile(Wind$SolarT, prob = .10)), 2)))
W5 <- dplyr::filter(Wind, between(WindT, round(0.99*(quantile(Wind$WindT, prob = .50)), 2),round(1.01*(quantile(Wind$WindT, prob = .50)), 2)) &
                      between(SolarT,round(0.99*(quantile(Wind$SolarT, prob = .50)), 2),round(1.01*(quantile(Wind$SolarT, prob = .50)), 2)))
W6 <- dplyr::filter(Wind, between(WindT, round(0.99*(quantile(Wind$WindT, prob = .50)), 2),round(1.01*(quantile(Wind$WindT, prob = .50)), 2)) &
                      between(SolarT,round(0.99*(quantile(Wind$SolarT, prob = .90)), 2),round(1.01*(quantile(Wind$SolarT, prob = .90)), 2)))
## high wind with all Solar
W7 <- dplyr::filter(Wind, between(WindT, round(0.99*(quantile(Wind$WindT, prob = .90)), 2),round(1.01*(quantile(Wind$WindT, prob = .90)), 2)) &
                      between(SolarT,round(0.99*(quantile(Wind$SolarT, prob = .10)), 2),round(1.01*(quantile(Wind$SolarT, prob = .10)), 2)))
W8 <- dplyr::filter(Wind, between(WindT, round(0.99*(quantile(Wind$WindT, prob = .90)), 2),round(1.01*(quantile(Wind$WindT, prob = .90)), 2)) &
                      between(SolarT,round(0.99*(quantile(Wind$SolarT, prob = .50)), 2),round(1.01*(quantile(Wind$SolarT, prob = .50)), 2)))
W9 <- dplyr::filter(Wind, between(WindT, round(0.99*(quantile(Wind$WindT, prob = .90)), 2),round(1.01*(quantile(Wind$WindT, prob = .90)), 2)) &
                      between(SolarT,round(0.99*(quantile(Wind$SolarT, prob = .90)), 2),round(1.01*(quantile(Wind$SolarT, prob = .90)), 2)))

dfNames <- list(W1, W2, W3, W4, W5, W6, W7, W8, W9)
AppendMe <- function(dfNames) {
  do.call(rbind, lapply(dfNames, function(x) {
    cbind(get(x), source = x)
  }))
}
K <- AppendMe(c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9"))

## Joining with the entire data 

Data <- left_join(K,Temp_Full, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year"))
Data <- Data |> dplyr::select(STATION_ID.x,STATION_NAME,LATITUDE.x,LONGITUDE.x, Elevation.x,Year, 
                              Month.x, Day.x,DOY,WindT, SolarT, source, hour, WindSpeed, RELATIVE_HUMIDITY, DewPoint,
                              tmax, tmin, AirTemp, SR_hourly_Obs )

colnames(Data) <- c("STATION_ID","STATION_NAME","LATITUDE","LONGITUDE", "Elevation","Year",
                    "Month", "Day","DOY","WindT", "SolarT", "source", "hour", "WindSpeed", 
                    "RELATIVE_HUMIDITY", "DewPoint", "tmax_ST", "tmin_ST", "AirTemp", "Solar")
Data$Solar[is.na(Data$Solar)] <- 0
##write.csv(Data, "E:/Ag_WeatherNet/AWN/ClimateChange/Sensitivity/Filtered_9Classes_summer.csv")

## Start fresh calculation from here
## Increasing temperature
Data <- read.csv("E:/Ag_WeatherNet/AWN/ClimateChange/Sensitivity/Filtered_9Classes_summer.csv")

### Appending columns
i=0.28
for(i in seq(from = 0.28, to =4.48, by =0.28) ){      # Head of for-loop
  T <- Data$AirTemp+i                       # Create new column
  Data[ , ncol(Data) + 1] <- T                  # Append new column
  colnames(Data)[ncol(Data)] <- paste0("T", i)  # Rename column name
}
Data <- Data[-1]

colnames(Data) <- c("STATION_ID","STATION_NAME","LATITUDE","LONGITUDE", "Elevation","Year",
                    "Month", "Day","DOY","WindT", "SolarT", "source", "hour", "WindSpeed", 
                    "RELATIVE_HUMIDITY", "DewPoint", "tmax_ST", "tmin_ST", "AirTemp", "Solar",
                    "T0.5", "T1", "T1.5", "T2", "T2.5", "T3", "T3.5", "T4", "T4.5", "T5", "T5.5", "T6", "T6.5", "T7", "T7.5", 'T8' )

Data <- Data |> mutate(date = make_date(Year, Month, Day))



Data <- Data |> group_by(STATION_NAME, DOY, Year) |> mutate(tmax_T0.5=max(T0.5),
                                                            tmin_T0.5=min(T0.5),
                                                            tmax_T1=max(T1),
                                                            tmin_T1=min(T1),
                                                            tmax_T1.5=max(T1.5),
                                                            tmin_T1.5=min(T1.5),
                                                            tmax_T2=max(T2),
                                                            tmin_T2=min(T2),
                                                            tmax_T2.5=max(T2.5),
                                                            tmin_T2.5=min(T2.5),
                                                            tmax_T3=max(T3),
                                                            tmin_T3=min(T3),
                                                            tmax_T3.5=max(T3.5),
                                                            tmin_T3.5=min(T3.5),
                                                            tmax_T4=max(T4),
                                                            tmin_T4=min(T4),
                                                            tmax_T4.5=max(T4.5),
                                                            tmin_T4.5=min(T4.5),
                                                            tmax_T5=max(T5),
                                                            tmin_T5=min(T5),
                                                            tmax_T5.5=max(T5.5),
                                                            tmin_T5.5=min(T5.5),
                                                            tmax_T6=max(T6),
                                                            tmin_T6=min(T6),
                                                            tmax_T6.5=max(T6.5),
                                                            tmin_T6.5=min(T6.5),
                                                            tmax_T7=max(T7),
                                                            tmin_T7=min(T7),
                                                            tmax_T7.5=max(T7.5),
                                                            tmin_T7.5=min(T7.5),
                                                            tmax_T8=max(T8),
                                                            tmin_T8=min(T8)) |> ungroup()

## Disaggregation of temperature

P0.5 <- Data[,c(1:12,37,38:39)]
P1<- Data[,c(1:12,37,40:41)]
P1.5 <- Data[,c(1:12,37,42:43)]
P2 <- Data[,c(1:12,37,44:45)]
P2.5 <- Data[,c(1:12,37,46:47)]
P3 <- Data[,c(1:12,37,48:49)]
P3.5 <- Data[,c(1:12,37,50:51)]
P4 <- Data[,c(1:12,37,52:53)]
P4.5 <- Data[,c(1:12,37,54:55)]
P5 <- Data[,c(1:12,37,56:57)]
P5.5 <- Data[,c(1:12,37,58:59)]
P6 <- Data[,c(1:12,37,60:61)]
P6.5 <- Data[,c(1:12,37,62:63)]
P7 <- Data[,c(1:12,37,64:65)]
P7.5 <- Data[,c(1:12,37,66:67)]
P8 <- Data[,c(1:12,37,68:69)]

P0.5_Disag <- P0.5  |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T0.5, tmin=tmin_T0.5) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P1_Disag <- P1  |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T1, tmin=tmin_T1) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P1.5_Disag <- P1.5  |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T1.5, tmin=tmin_T1.5) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P2_Disag <- P2  |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T2, tmin=tmin_T2) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P2.5_Disag <- P2.5  |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T2.5, tmin=tmin_T2.5) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P3_Disag <- P3  |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T3, tmin=tmin_T3) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P3.5_Disag <- P3.5 |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T3.5, tmin=tmin_T3.5) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P4_Disag <- P4  |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T4, tmin=tmin_T4) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P4.5_Disag <- P4.5 |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T4.5, tmin=tmin_T4.5) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P5_Disag <- P5  |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T5, tmin=tmin_T5) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P5.5_Disag <- P5.5  |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T5.5, tmin=tmin_T5.5) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P6_Disag <- P6  |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T6, tmin=tmin_T6) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P6.5_Disag <- P6.5  |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T6.5, tmin=tmin_T6.5) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P7_Disag <- P7 |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T7, tmin=tmin_T7) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P7.5_Disag <- P7.5  |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T7.5, tmin=tmin_T7.5) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

P8_Disag <- P8 |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_T8, tmin=tmin_T8) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

## Station disaggregation 

ST <- Data[,c(1:12,37,17:18)]

ST$tmax_S <- round(((ST$tmax_ST-32)*(5/9)),2)
ST$tmin_S <- round(((ST$tmin_ST-32)*(5/9)),2)

ST <- ST[,c(1:13,16:17)]

ST_Disag <- ST  |> 
  group_by(STATION_NAME, LATITUDE) |>  
  rename(tmax=tmax_S, tmin=tmin_S) |> 
  nest() |> mutate(data=map2(data,LATITUDE,add_hourly_temps, 0:23)) |> unnest(data)

Data2 <- Data |> select("STATION_ID","STATION_NAME","LATITUDE","LONGITUDE", "Elevation","Year",
                        "Month", "Day","DOY","source", "hour", "WindSpeed",
                        "RELATIVE_HUMIDITY", "DewPoint", "AirTemp", "Solar", "T0.5", "T1","T1.5", "T2",
                        "T2.5","T3", "T3.5", "T4", "T4.5", "T5", "T5.5", "T6", "T6.5", "T7", "T7.5", "T8")

Data2 <- Data2 |> mutate(Dew0.5= dew_point(RELATIVE_HUMIDITY,T0.5),
                         Dew1= dew_point(RELATIVE_HUMIDITY,T1),
                         Dew1.5= dew_point(RELATIVE_HUMIDITY,T1.5),
                         Dew2= dew_point(RELATIVE_HUMIDITY,T2),
                         Dew2.5= dew_point(RELATIVE_HUMIDITY,T2.5),
                         Dew3= dew_point(RELATIVE_HUMIDITY,T3),
                         Dew3.5= dew_point(RELATIVE_HUMIDITY,T3.5),
                         Dew4= dew_point(RELATIVE_HUMIDITY,T4),
                         Dew4.5= dew_point(RELATIVE_HUMIDITY,T4.5),
                         Dew5= dew_point(RELATIVE_HUMIDITY,T5),
                         Dew5.5= dew_point(RELATIVE_HUMIDITY,T5.5),
                         Dew6= dew_point(RELATIVE_HUMIDITY,T6),
                         Dew6.5= dew_point(RELATIVE_HUMIDITY,T6.5),
                         Dew7= dew_point(RELATIVE_HUMIDITY,T7),
                         Dew7.5= dew_point(RELATIVE_HUMIDITY,T7.5),
                         Dew8= dew_point(RELATIVE_HUMIDITY,T8))

## FST and Sunburn station
ST <- left_join(ST_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
ST <- ST[,c(1:17, 25:29)]

ST$FS_Air<- fruit_surface_temp(t_air = ST$AirTemp, wind_speed = ST$WindSpeed, 
                                s_rad = ST$Solar, t_dew = ST$DewPoint)
ST$FS_Dis<- fruit_surface_temp(t_air = ST$temp, wind_speed = ST$WindSpeed, 
                               s_rad = ST$Solar, t_dew = ST$DewPoint)

ST <-ST|> mutate(hour24=hour+1)
ST <- ST |> mutate(time=hour24-hour)
ST_FSTAir <- ST |>  
  filter_at(vars(23), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(ST_FSTAir=sum(Exposure)) |> 
  distinct(ST_FSTAir)

ST_FstDisag <- ST |>  
  filter_at(vars(24), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(ST_FstDisag=sum(Exposure)) |> 
  distinct(ST_FstDisag)

ST_SB <- full_join(ST_FSTAir,ST_FstDisag, by=c('STATION_NAME', 'Year', 'source.x'))

### I stands for increase
## need to find a better way to loop it out

## 0.5 increase
I0.5 <- left_join(P0.5_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I0.5 <- I0.5[,c(1:17, 25:29, 30, 46)]

I0.5$FS_I0.5<- fruit_surface_temp(t_air = I0.5$T0.5, wind_speed = I0.5$WindSpeed, 
                                    s_rad = I0.5$Solar, t_dew = I0.5$Dew0.5)
I0.5$FS_I0.5Dis<- fruit_surface_temp(t_air = I0.5$temp, wind_speed = I0.5$WindSpeed, 
                                       s_rad = I0.5$Solar, t_dew = I0.5$Dew0.5)

I0.5 <-I0.5|> mutate(hour24=hour+1)
I0.5 <- I0.5 |> mutate(time=hour24-hour)
I0.5_Air<- I0.5  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I0.5_Air =sum(Exposure)) |> 
  distinct(I0.5_Air)

I0.5_Disag<- I0.5  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I0.5_Disag =sum(Exposure)) |> 
  distinct(I0.5_Disag)

I0.5_SB <- full_join(I0.5_Air,I0.5_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 1 increase

I1 <- left_join(P1_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I1 <- I1[,c(1:17, 25:29, 31, 47)]

I1$FS_I1<- fruit_surface_temp(t_air = I1$T1, wind_speed = I1$WindSpeed, 
                                  s_rad = I1$Solar, t_dew = I1$Dew1)
I1$FS_I1Dis<- fruit_surface_temp(t_air = I1$temp, wind_speed = I1$WindSpeed, 
                                     s_rad = I1$Solar, t_dew = I1$Dew1)

I1 <-I1|> mutate(hour24=hour+1)
I1 <- I1 |> mutate(time=hour24-hour)
I1_Air<- I1 |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I1_Air =sum(Exposure)) |> 
  distinct(I1_Air)

I1_Disag<- I1  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I1_Disag =sum(Exposure)) |> 
  distinct(I1_Disag)

I1_SB <- full_join(I1_Air,I1_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 1.5 increase

I1.5 <- left_join(P1.5_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I1.5 <- I1.5[,c(1:17, 25:29, 32, 48)]

I1.5$FS_I1.5<- fruit_surface_temp(t_air = I1.5$T1.5, wind_speed = I1.5$WindSpeed, 
                                    s_rad = I1.5$Solar, t_dew = I1.5$Dew1.5)
I1.5$FS_I1.5Dis<- fruit_surface_temp(t_air = I1.5$temp, wind_speed = I1.5$WindSpeed, 
                                       s_rad = I1.5$Solar, t_dew = I1.5$Dew1.5)

I1.5 <-I1.5|> mutate(hour24=hour+1)
I1.5 <- I1.5 |> mutate(time=hour24-hour)
I1.5_Air<- I1.5  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I1.5_Air =sum(Exposure)) |> 
  distinct(I1.5_Air)

I1.5_Disag<- I1.5  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I1.5_Disag =sum(Exposure)) |> 
  distinct(I1.5_Disag)

I1.5_SB <- full_join(I1.5_Air,I1.5_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 2 increase

I2 <- left_join(P2_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I2 <- I2[,c(1:17, 25:29, 33, 49)]

I2$FS_I2<- fruit_surface_temp(t_air = I2$T2, wind_speed = I2$WindSpeed, 
                              s_rad = I2$Solar, t_dew = I2$Dew2)
I2$FS_I2Dis<- fruit_surface_temp(t_air = I2$temp, wind_speed = I2$WindSpeed, 
                                 s_rad = I2$Solar, t_dew = I2$Dew2)

I2 <-I2|> mutate(hour24=hour+1)
I2 <- I2 |> mutate(time=hour24-hour)
I2_Air<- I2  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I2_Air =sum(Exposure)) |> 
  distinct(I2_Air)

I2_Disag<- I2  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I2_Disag =sum(Exposure)) |> 
  distinct(I2_Disag)

I2_SB <- full_join(I2_Air,I2_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 2.5 increase

I2.5 <- left_join(P2.5_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I2.5 <- I2.5[,c(1:17, 25:29, 34, 50)]

I2.5$FS_I2.5<- fruit_surface_temp(t_air = I2.5$T2.5, wind_speed = I2.5$WindSpeed, 
                                  s_rad = I2.5$Solar, t_dew = I2.5$Dew2.5)
I2.5$FS_I2.5Dis<- fruit_surface_temp(t_air = I2.5$temp, wind_speed = I2.5$WindSpeed, 
                                     s_rad = I2.5$Solar, t_dew = I2.5$Dew2.5)

I2.5 <-I2.5|> mutate(hour24=hour+1)
I2.5 <- I2.5 |> mutate(time=hour24-hour)
I2.5_Air<- I2.5  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I2.5_Air =sum(Exposure)) |> 
  distinct(I2.5_Air)

I2.5_Disag<- I2.5  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I2.5_Disag =sum(Exposure)) |> 
  distinct(I2.5_Disag)

I2.5_SB <- full_join(I2.5_Air,I2.5_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 1.5 increase

I3 <- left_join(P3_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I3 <- I3[,c(1:17, 25:29, 35, 51)]

I3$FS_I3<- fruit_surface_temp(t_air = I3$T3, wind_speed = I3$WindSpeed, 
                              s_rad = I3$Solar, t_dew = I3$Dew3)
I3$FS_I3Dis<- fruit_surface_temp(t_air = I3$temp, wind_speed = I3$WindSpeed, 
                                 s_rad = I3$Solar, t_dew = I3$Dew3)

I3 <-I3|> mutate(hour24=hour+1)
I3 <- I3 |> mutate(time=hour24-hour)
I3_Air<- I3  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I3_Air =sum(Exposure)) |> 
  distinct(I3_Air)

I3_Disag<- I3  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I3_Disag =sum(Exposure)) |> 
  distinct(I3_Disag)

I3_SB <- full_join(I3_Air,I3_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 3.5 increase

I3.5 <- left_join(P3.5_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I3.5 <- I3.5[,c(1:17, 25:29, 36, 52)]

I3.5$FS_I3.5<- fruit_surface_temp(t_air = I3.5$T3.5, wind_speed = I3.5$WindSpeed, 
                                  s_rad = I3.5$Solar, t_dew = I3.5$Dew3.5)
I3.5$FS_I3.5Dis<- fruit_surface_temp(t_air = I3.5$temp, wind_speed = I3.5$WindSpeed, 
                                     s_rad = I3.5$Solar, t_dew = I3.5$Dew3.5)

I3.5 <-I3.5|> mutate(hour24=hour+1)
I3.5 <- I3.5 |> mutate(time=hour24-hour)
I3.5_Air<- I3.5  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I3.5_Air =sum(Exposure)) |> 
  distinct(I3.5_Air)

I3.5_Disag<- I3.5  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I3.5_Disag =sum(Exposure)) |> 
  distinct(I3.5_Disag)

I3.5_SB <- full_join(I3.5_Air,I3.5_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 4 increase

I4 <- left_join(P4_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I4 <- I4[,c(1:17, 25:29, 37, 53)]

I4$FS_I4<- fruit_surface_temp(t_air = I4$T4, wind_speed = I4$WindSpeed, 
                              s_rad = I4$Solar, t_dew = I4$Dew4)
I4$FS_I4Dis<- fruit_surface_temp(t_air = I4$temp, wind_speed = I4$WindSpeed, 
                                 s_rad = I4$Solar, t_dew = I4$Dew4)

I4 <-I4|> mutate(hour24=hour+1)
I4 <- I4 |> mutate(time=hour24-hour)
I4_Air<- I4  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I4_Air =sum(Exposure)) |> 
  distinct(I4_Air)

I4_Disag<- I4  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I4_Disag =sum(Exposure)) |> 
  distinct(I4_Disag)

I4_SB <- full_join(I4_Air,I4_Disag, by=c('STATION_NAME', 'Year', 'source.x'))


## 4.5 increase
I4.5 <- left_join(P4.5_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I4.5 <- I4.5[,c(1:17, 25:29, 38, 54)]

I4.5$FS_I4.5<- fruit_surface_temp(t_air = I4.5$T4.5, wind_speed = I4.5$WindSpeed, 
                                  s_rad = I4.5$Solar, t_dew = I4.5$Dew4.5)
I4.5$FS_I4.5Dis<- fruit_surface_temp(t_air = I4.5$temp, wind_speed = I4.5$WindSpeed, 
                                     s_rad = I4.5$Solar, t_dew = I4.5$Dew4.5)

I4.5 <-I4.5|> mutate(hour24=hour+1)
I4.5 <- I4.5 |> mutate(time=hour24-hour)
I4.5_Air<- I4.5  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I4.5_Air =sum(Exposure)) |> 
  distinct(I4.5_Air)

I4.5_Disag<- I4.5  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I4.5_Disag =sum(Exposure)) |> 
  distinct(I4.5_Disag)

I4.5_SB <- full_join(I4.5_Air,I4.5_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 5 increase
I5 <- left_join(P5_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I5 <- I5[,c(1:17, 25:29, 39, 55)]

I5$FS_I5<- fruit_surface_temp(t_air = I5$T5, wind_speed = I5$WindSpeed, 
                              s_rad = I5$Solar, t_dew = I5$Dew5)
I5$FS_I5Dis<- fruit_surface_temp(t_air = I5$temp, wind_speed = I5$WindSpeed, 
                                 s_rad = I5$Solar, t_dew = I5$Dew5)

I5 <-I5|> mutate(hour24=hour+1)
I5 <- I5 |> mutate(time=hour24-hour)
I5_Air<- I5  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I5_Air =sum(Exposure)) |> 
  distinct(I5_Air)

I5_Disag<- I5  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I5_Disag =sum(Exposure)) |> 
  distinct(I5_Disag)

I5_SB <- full_join(I5_Air,I5_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 5.5 increase
I5.5 <- left_join(P5.5_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I5.5 <- I5.5[,c(1:17, 25:29, 40, 56)]

I5.5$FS_I5.5<- fruit_surface_temp(t_air = I5.5$T5.5, wind_speed = I5.5$WindSpeed, 
                                  s_rad = I5.5$Solar, t_dew = I5.5$Dew5.5)
I5.5$FS_I5.5Dis<- fruit_surface_temp(t_air = I5.5$temp, wind_speed = I5.5$WindSpeed, 
                                     s_rad = I5.5$Solar, t_dew = I5.5$Dew5.5)

I5.5 <-I5.5|> mutate(hour24=hour+1)
I5.5 <- I5.5 |> mutate(time=hour24-hour)
I5.5_Air<- I5.5  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I5.5_Air =sum(Exposure)) |> 
  distinct(I5.5_Air)

I5.5_Disag<- I5.5  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I5.5_Disag =sum(Exposure)) |> 
  distinct(I5.5_Disag)

I5.5_SB <- full_join(I5.5_Air,I5.5_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 6 increase

I6 <- left_join(P6_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I6 <- I6[,c(1:17, 25:29, 41, 57)]

I6$FS_I6<- fruit_surface_temp(t_air = I6$T6, wind_speed = I6$WindSpeed, 
                              s_rad = I6$Solar, t_dew = I6$Dew6)
I6$FS_I6Dis<- fruit_surface_temp(t_air = I6$temp, wind_speed = I6$WindSpeed, 
                                 s_rad = I6$Solar, t_dew = I6$Dew6)

I6 <-I6|> mutate(hour24=hour+1)
I6 <- I6 |> mutate(time=hour24-hour)
I6_Air<- I6  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I6_Air =sum(Exposure)) |> 
  distinct(I6_Air)

I6_Disag<- I6  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I6_Disag =sum(Exposure)) |> 
  distinct(I6_Disag)

I6_SB <- full_join(I6_Air,I6_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 6.5 increase

I6.5 <- left_join(P6.5_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I6.5 <- I6.5[,c(1:17, 25:29, 42, 58)]

I6.5$FS_I6.5<- fruit_surface_temp(t_air = I6.5$T6.5, wind_speed = I6.5$WindSpeed, 
                                  s_rad = I6.5$Solar, t_dew = I6.5$Dew6.5)
I6.5$FS_I6.5Dis<- fruit_surface_temp(t_air = I6.5$temp, wind_speed = I6.5$WindSpeed, 
                                     s_rad = I6.5$Solar, t_dew = I6.5$Dew6.5)

I6.5 <-I6.5|> mutate(hour24=hour+1)
I6.5 <- I6.5 |> mutate(time=hour24-hour)
I6.5_Air<- I6.5  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I6.5_Air =sum(Exposure)) |> 
  distinct(I6.5_Air)

I6.5_Disag<- I6.5  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I6.5_Disag =sum(Exposure)) |> 
  distinct(I6.5_Disag)

I6.5_SB <- full_join(I6.5_Air,I6.5_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 7 increase

I7 <- left_join(P7_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I7 <- I7[,c(1:17, 25:29, 43, 59)]

I7$FS_I7<- fruit_surface_temp(t_air = I7$T7, wind_speed = I7$WindSpeed, 
                              s_rad = I7$Solar, t_dew = I7$Dew7)
I7$FS_I7Dis<- fruit_surface_temp(t_air = I7$temp, wind_speed = I7$WindSpeed, 
                                 s_rad = I7$Solar, t_dew = I7$Dew7)

I7 <-I7|> mutate(hour24=hour+1)
I7 <- I7 |> mutate(time=hour24-hour)
I7_Air<- I7  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I7_Air =sum(Exposure)) |> 
  distinct(I7_Air)

I7_Disag<- I7  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I7_Disag =sum(Exposure)) |> 
  distinct(I7_Disag)

I7_SB <- full_join(I7_Air,I7_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 7.5 increase

I7.5 <- left_join(P7.5_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I7.5 <- I7.5[,c(1:17, 25:29, 44, 60)]

I7.5$FS_I7.5<- fruit_surface_temp(t_air = I7.5$T7.5, wind_speed = I7.5$WindSpeed, 
                                  s_rad = I7.5$Solar, t_dew = I7.5$Dew7.5)
I7.5$FS_I7.5Dis<- fruit_surface_temp(t_air = I7.5$temp, wind_speed = I7.5$WindSpeed, 
                                     s_rad = I7.5$Solar, t_dew = I7.5$Dew7.5)

I7.5 <-I7.5|> mutate(hour24=hour+1)
I7.5 <- I7.5 |> mutate(time=hour24-hour)
I7.5_Air<- I7.5  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I7.5_Air =sum(Exposure)) |> 
  distinct(I7.5_Air)

I7.5_Disag<- I7.5  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I7.5_Disag =sum(Exposure)) |> 
  distinct(I7.5_Disag)

I7.5_SB <- full_join(I7.5_Air,I7.5_Disag, by=c('STATION_NAME', 'Year', 'source.x'))

## 8 increase

I8 <- left_join(P8_Disag,Data2, by=c("STATION_NAME"="STATION_NAME", "DOY"="DOY", "Year"="Year", "hour"="hour"))
I8 <- I8[,c(1:17, 25:29, 45, 61)]

I8$FS_I8<- fruit_surface_temp(t_air = I8$T8, wind_speed = I8$WindSpeed, 
                              s_rad = I8$Solar, t_dew = I8$Dew8)
I8$FS_I8Dis<- fruit_surface_temp(t_air = I8$temp, wind_speed = I8$WindSpeed, 
                                 s_rad = I8$Solar, t_dew = I8$Dew8)

I8 <-I8|> mutate(hour24=hour+1)
I8 <- I8 |> mutate(time=hour24-hour)
I8_Air<- I8  |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I8_Air =sum(Exposure)) |> 
  distinct(I8_Air)

I8_Disag<- I8  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(Year, STATION_NAME,source.x) |> mutate(I8_Disag =sum(Exposure)) |> 
  distinct(I8_Disag)

I8_SB <- full_join(I8_Air,I8_Disag, by=c('STATION_NAME', 'Year', 'source.x'))


## Merging all Sunburn categories

Sunburn_df <- list(ST_SB, I0.5_SB, I1_SB, I1.5_SB, I2_SB, I2.5_SB, I3_SB, I3.5_SB, I4_SB,
                I4.5_SB, I5_SB, I5.5_SB, I6_SB, I6.5_SB, I7_SB, I7.5_SB, I8_SB)

#merge all data frames in list
Sunburn <- Reduce(function(x, y) merge(x, y, all=TRUE), Sunburn_df)
Sunburn[is.na(Sunburn)] <- 0

## Sunburn Plots
Sunburn_Air <- Sunburn |> mutate(I0.5=I0.5_Air-ST_FSTAir,
                                 I1=I1_Air-ST_FSTAir,
                                 I1.5=I1.5_Air-ST_FSTAir,
                                 I2=I2.5_Air-ST_FSTAir,
                                 I2.5=I2.5_Air-ST_FSTAir,
                                 I3=I3_Air-ST_FSTAir,
                                 I3.5=I3.5_Air-ST_FSTAir,
                                 I4=I4_Air-ST_FSTAir,
                                 I4.5=I4.5_Air-ST_FSTAir,
                                 I5=I5_Air-ST_FSTAir,
                                 I5.5=I5.5_Air-ST_FSTAir,
                                 I6=I6_Air-ST_FSTAir,
                                 I6.5=I6.5_Air-ST_FSTAir,
                                 I7=I7_Air-ST_FSTAir,
                                 I7.5=I7.5_Air-ST_FSTAir,
                                 I8=I8_Air-ST_FSTAir) |> select(1:3,38:53)

Sunburn_Air <- Sunburn_Air |> 
  pivot_longer(
    cols = starts_with("I"),
    names_to = "Case",
    names_prefix = "x",
    values_to = "Error",
    values_drop_na = FALSE
  )
Sunburn_Air <- Sunburn_Air |> group_by(source.x, Case) |> mutate(Error_Mean=mean(Error)) |> 
  select(source.x, Case,Error_Mean ) |> distinct()

## Disaggregated Sunburn

Sunburn_Disag <- Sunburn |> mutate(I0.5=I0.5_Disag-ST_FstDisag,
                                   I1=I1_Disag-ST_FstDisag,
                                   I1.5=I1.5_Disag-ST_FstDisag,
                                   I2=I2.5_Disag-ST_FstDisag,
                                   I2.5=I2.5_Disag-ST_FstDisag,
                                   I3=I3_Disag-ST_FstDisag,
                                   I3.5=I3.5_Disag-ST_FstDisag,
                                   I4=I4_Disag-ST_FstDisag,
                                   I4.5=I4.5_Disag-ST_FstDisag,
                                   I5=I5_Disag-ST_FstDisag,
                                   I5.5=I5.5_Disag-ST_FstDisag,
                                   I6=I6_Disag-ST_FstDisag,
                                   I6.5=I6.5_Disag-ST_FstDisag,
                                   I7=I7_Disag-ST_FstDisag,
                                   I7.5=I7.5_Disag -ST_FstDisag,
                                   I8=I8_Disag -ST_FstDisag) |> select(1:3,38:53)

Sunburn_Disag <- Sunburn_Disag |> 
  pivot_longer(
    cols = starts_with("I"),
    names_to = "Case",
    names_prefix = "x",
    values_to = "Error",
    values_drop_na = FALSE
  )
Sunburn_Disag <- Sunburn_Disag|> group_by(source.x, Case) |> mutate(Error_Mean=mean(Error)) |> 
  select(source.x, Case,Error_Mean ) |> distinct()

ggplot() + 
  geom_point(data= Sunburn_Disag, aes(x=Case, y=Error_Mean),color='magenta', size=3)+ 
  geom_point(data= Sunburn_Air, aes(x=Case, y=Error_Mean),color='blue', size=3)+theme_bw()+
  labs(y="Sunburn hours", x="Case")+facet_wrap(~source.x, scales ="free")


## Merging all Sunburn categories

Sunburn_mean <- list(Sunburn_Air, Sunburn_Disag)

AppendSunburn <- function(Sunburn_mean) {
  do.call(rbind, lapply(Sunburn_mean, function(x) {
    cbind(get(x), source = x)
  }))
}
Sunburn_Merged <- AppendSunburn(c("Sunburn_Air", "Sunburn_Disag"))

ggplot() + 
  geom_point(data= Sunburn_Merged, aes(x=Case, y=Error_Mean, col=source), size=3)+theme_bw()+
  labs(y="Sunburn hours", x="Case")+facet_wrap(~source.x, scales ="free")


ggplot() + 
  geom_point(data= Sunburn_Merged, aes(y=Case, x=Error_Mean, col=source), size=3)+theme_bw()+
  labs(y="Sunburn hours", x="Case")+facet_wrap(~source.x, scales ="free")

## Sunburn disag as per location
ggplot() + 
  geom_point(data= Sunburn_Disag, aes(y=Error, x=STATION_NAME, col=STATION_NAME), size=3)+theme_bw()+
  labs(y="Sunburn hours", x="Case")+facet_grid(source.x~Case)+theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90))

## Sunburn days
## below is the sample code
I8_Disag<- I8  |>  
  filter_at(vars(26), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |>  group_by(source.x) |> count()

Sunburn_days_Merged <- read.csv("E:/Ag_WeatherNet/AWN/ClimateChange/Sensitivity/Sunburn_Days_Disag.csv")
ggplot() + 
  geom_point(data= Sunburn_days_Merged, aes(y=n, x=source, col=source.x), size=3)+theme_bw()+
  labs(y="Sunburn days", x="Case")+theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90))


## Sample air
I0.5_Air<- I0.5 |>  
  filter_at(vars(25), any_vars(. > 43)) |> 
  group_by(Year, DOY ,STATION_NAME,source.x) |>
  mutate(Exposure=sum(time)) |> 
  distinct(Exposure) |> group_by(source.x) |> count()

Sunburn_days_Air_Merged <- read.csv("E:/Ag_WeatherNet/AWN/ClimateChange/Sensitivity/Sunburn_Days_Air.csv")
ggplot() + 
  geom_point(data= Sunburn_days_Air_Merged, aes(y=n, x=source, col=source.x), size=3)+theme_bw()+
  labs(y="Sunburn days", x="Case")+theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90))

## Sunburn days join

SB_Days <- list(Sunburn_days_Merged, Sunburn_days_Air_Merged)

AppendSunburn <- function(SB_days) {
  do.call(rbind, lapply(SB_days, function(x) {
    cbind(get(x), source.d = x)
  }))
}
SB_Days <- AppendSunburn(c("Sunburn_days_Merged", "Sunburn_days_Air_Merged"))

ggplot() + 
  geom_point(data= SB_Days, aes(y=n, x=source, col=source.d), size=3)+theme_bw()+
  labs(y="Sunburn days", x="case")+facet_wrap(~source.x,scales ="free")+theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90))

### Updated plot for merged sunburn hours-- to refine the plot

Sunburn_Disag <- read.csv("E:/Ag_WeatherNet/AWN/ClimateChange/Sensitivity/Sunburn_Days_Disag.csv")
Sunburn_Air <- read.csv("E:/Ag_WeatherNet/AWN/ClimateChange/Sensitivity/Sunburn_Days_Air.csv")

SB_Days <- list(Sunburn_Disag, Sunburn_Air)

AppendSunburn <- function(SB_days) {
  do.call(rbind, lapply(SB_days, function(x) {
    cbind(get(x), source.d = x)
  }))
}
SB_Days <- AppendSunburn(c("Sunburn_Disag", "Sunburn_Air"))

SB_Days<- SB_Days |> separate(source , c("Scenario", "Type"), sep = "_")

ggplot() + 
  geom_point(data= SB_Days, aes(y=n, x=Scenario, col=source.d), size=3)+theme_bw()+
  labs(y="Sunburn days", x="case")+facet_wrap(~source.x,scales ="fixed", ncol=4)+theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90))

## new plot for difference
SB_Disag <- Sunburn_Disag |> separate(source , c("Scenario", "Type"), sep = "_") |> select(1:4)
SB_Air <- Sunburn_Air |> separate(source , c("Scenario", "Type"), sep = "_") |> select(1:4)
SB_diff <- full_join(SB_Air, SB_Disag, by=c("Scenario"="Scenario", "source.x"="source.x")) |> select(2:4,6)
colnames(SB_diff) <- c("Source", "Station", "Scenario", "Disaggregation")
SB_diff <- SB_diff |> mutate(Difference=Disaggregation-Station)

SB_diff <- SB_diff |> 
  mutate(Scenario = as.factor(Scenario))

SB_diff$Scenario <- gsub(pattern = "I",replacement = "",SB_diff$Scenario)
SB_diff[SB_diff$Scenario=="ST",3] <- "0"

ggplot() + 
  geom_point(data= SB_diff, aes(y=Difference, x=Scenario), size=3, color="blue")+theme_bw()+
  labs(y="Sunburn days", x="Increase in temperature degree F")+facet_wrap(~Source,scales ="fixed", ncol=4)+theme_bw(base_size = 12)+
  theme(axis.text.x = element_text(angle = 90))

