rm(list = ls())
gc()
setwd("D:/ATtest/Europe_version3")
library(lubridate)
library(data.table)
library(tidyr)
Humidex <- function(temp, dew) {
  k1 <- 6.11 * exp(5417.7530 * ((1 / 273.16) - (1 / (dew + 273.15))))
  Hum <- temp + 0.5555 * (k1 - 10)
  return(Hum)
}

dataagg = function(wd){
  wd$week = format(wd$ymd_local, format = "%Y-W%V")
  wd$weeknum = as.numeric(substring(wd$week,7,8))
  wd = wd[which(weeknum>=22&weeknum<=35),]
  new <- wd[, .(
    tem_max = max(tem), Hum_max = max(hum),
    tem_min = min(tem), Hum_min = min(hum),
    tem = mean(tem), Hum = mean(hum)
  ), by = .(NUTS_ID, weeknum,week)]
  
  new2 = wd
  new2$NUTS_ID = substring(new2$NUTS_ID,1,4)
  new2 = new2[, .(
    tem = mean(tem), hum = mean(hum)
  ), by = .(NUTS_ID,ymd_local,weeknum,week)]
  new2 = new2[, .(
    tem_max = max(tem), Hum_max = max(hum),
    tem_min = min(tem), Hum_min = min(hum),
    tem = mean(tem), Hum = mean(hum)
  ), by = .(NUTS_ID,weeknum,week)]
 
  new3 = wd
  new3$NUTS_ID = substring(new3$NUTS_ID,1,3)
  new3 = new3[, .(
    tem = mean(tem), hum = mean(hum)
  ), by = .(NUTS_ID,ymd_local,weeknum,week)]
  new3 = new3[, .(
    tem_max = max(tem), Hum_max = max(hum),
    tem_min = min(tem), Hum_min = min(hum),
    tem = mean(tem), Hum = mean(hum)
  ), by = .(NUTS_ID,weeknum,week)]
  new = rbind(new,new2,new3)
  return(new)
}
dataprocess = function(Year,weather_dir,mortalitydir){
  weatherdir  = file.path(weather_dir,paste0("Daily_aggregate_",Year,".csv"))
 # threname = file.path(thredir,paste0("heat_threshold_",Year,".csv"))
  morname = file.path(mortalitydir,paste0("weekly_mortality_totalage_",Year,".csv"))
  wd =  fread(weatherdir) 
  wd = wd[which(wd$tem!=0),]
  f = dataagg(wd)
  mor =  fread(morname)
  mwd = merge(mor,f,by.x=c("geo","Week"),by.y=c("NUTS_ID","weeknum"), all.x = TRUE, allow.cartesian = TRUE)
 # mwd = mwd[Week>=22&Week<=35,]
  mwd = mwd[complete.cases(mwd),]
  mwd$Year= Year
  return(mwd)
  rm(mwd)
  rm(f)
  gc()
}


weather_dir = "D:/ATtest/Europe_version2/Daily_data"
mortalitydir = "D:/ATtest/Europe_version2/Mortality_data"

alldata = rbindlist(lapply(seq(2000,2022),function(y){
  print(paste(y,"start:"))
  d = dataprocess(y,weather_dir,mortalitydir)
  return(d)
  print(paste(y,"End!"))
}))
outdir = "heat_threshold"
if (!file.exists(outdir)){
  dir.create(outdir)
}

outname = file.path(outdir,paste0("Weekly_data_for_threshold_V2.csv"))
fwrite(alldata, outname)

library(dplyr)
stat =alldata%>%group_by(geo,age)%>%summarise(start = min(Year),end=max(Year),num=length(death))



