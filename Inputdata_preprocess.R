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

datamerge = function(wd){
  wd1 = wd[which(time=="Day"),]
  wd1[, `:=` (Heat_hum = as.integer(hum>=thdH),
              Heat_tem = as.integer(tem>=thdT))]
  wd2 = wd[which(time=="Night"),]
  wd2[, `:=` (Heat_hum = as.integer(hum>=thdH),
              Heat_tem = as.integer(tem>=thdT))]
  wd = do.call(rbind,list(wd1,wd2))
  new <- wd[, .(
    tem = mean(tem), Hum = mean(hum),
    tem_max = max(tem_max), Hum_max = max(hum_max),
    tem_min = min(tem_min), Hum_min = min(hum_min),
    Heatday = as.integer(Heat_hum[daytime == 1] == 1 &Heat_hum[daytime == 0] == 0),
    Heatnight = as.integer(Heat_hum[daytime == 1] == 0 & Heat_hum[daytime == 0] == 1),
    Heatall = as.integer(Heat_hum[daytime == 1] == 1 & Heat_hum[daytime == 0] == 1),
    Heatday_tem = as.integer(Heat_tem[daytime == 1] == 1 & Heat_tem[daytime == 0] == 0),
    Heatnight_tem = as.integer(Heat_tem[daytime == 1] == 0 & Heat_tem[daytime == 0] == 1),
    Heatall_tem = as.integer(Heat_tem[daytime == 1] == 1 & Heat_tem[daytime == 0] == 1)
  ), by = .(NUTS_ID, ymd_local)]
  new$week = format(new$ymd_local, format = "%Y-W%V")
  new$weeknum = as.numeric(substring(new$week,7,8))
  new = new[which(weeknum>=22&weeknum<=35),]
  
  new2 = new
  new2$NUTS_ID = substring(new2$NUTS_ID,1,4)
  new2 = new2[, .(
    tem = mean(tem), Hum = mean(Hum),
    tem_max = max(tem_max), Hum_max = max(Hum_max),
    tem_min = min(tem_min), Hum_min = min(Hum_min),
    Heatday = round(mean(Heatday)),
    Heatnight = round(mean(Heatnight)),
    Heatall= round(mean(Heatall)),
    Heatday_tem =round(mean(Heatday_tem)),
    Heatnight_tem =round(mean(Heatnight_tem)),
    Heatall_tem =round(mean(Heatall_tem))
  ), by = .(NUTS_ID, ymd_local,week,weeknum)]
  
  
  new3 = new
  new3$NUTS_ID = substring(new3$NUTS_ID,1,3)
  new3 = new3[, .(
    tem = mean(tem), Hum = mean(Hum),
    tem_max = max(tem_max), Hum_max = max(Hum_max),
    tem_min = min(tem_min), Hum_min = min(Hum_min),
    Heatday = round(mean(Heatday)),
    Heatnight = round(mean(Heatnight)),
    Heatall= round(mean(Heatall)),
    Heatday_tem =round(mean(Heatday_tem)),
    Heatnight_tem =round(mean(Heatnight_tem)),
    Heatall_tem =round(mean(Heatall_tem))
  ), by = .(NUTS_ID, ymd_local,week,weeknum)]
  
  
  
  new = rbind(new,new2,new3)
  weeklynew = 
    new[,.(tem = mean(tem),Hum = mean(Hum),
           tem_max = max(tem_max),Hum_max = max(Hum_max),
           tem_min = min(tem_min),Hum_min = min(Hum_min),
           Heatday= sum(Heatday),
           CHD = {
             rle_values <- rle(Heatday == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatday_tem = sum(Heatday_tem),
           CHD_tem = {
             rle_values <- rle(Heatday_tem == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           
           Heatnight = sum(Heatnight),
           CHN = {
             rle_values <- rle(Heatnight == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
      
           Heatnight_tem = sum(Heatnight_tem),
           CHN_tem = {
             rle_values <- rle(Heatnight_tem == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           
           Heatall = sum(Heatall),
           CH = {
             rle_values <- rle(Heatall == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatall_tem = sum(Heatall_tem),
           CH_tem = {
             rle_values <- rle(Heatall_tem == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}}
    ) , by = .(NUTS_ID, week,weeknum)]
  return(weeklynew)
}
dataprocess = function(Year,weather_dir,threfile,mortalitydir){
  weatherdir  = file.path(weather_dir,paste0("Daily_aggregate_",Year,".csv"))
  morname = file.path(mortalitydir,paste0("weekly_mortality_totalage_",Year,".csv"))
  wd =  fread(weatherdir) 
  wd = wd[which(wd$tem!=0),]
  thd = fread(threfile) 
  thdH= thd[which(variable=="Humidex"),]
  colnames(thdH)[2] = "thdH"
  thdT= thd[which(variable=="Temp"),]
  colnames(thdT)[2] = "thdT"
  wd$region = substring(wd$NUTS_ID,1,3)
  wd = merge(wd,thdH[,c("region","thdH","Time")],by.x=c("region","time"),by.y=c("region","Time"))
  wd = merge(wd,thdT[,c("region","thdT","Time")],by.x=c("region","time"),by.y=c("region","Time"))
  f = datamerge(wd)
  mor =  fread(morname)
  mwd = merge(mor,f,by.x=c("geo","Week"),by.y=c("NUTS_ID","weeknum"), all.x = TRUE, allow.cartesian = TRUE)
  mwd = mwd[Week>=22&Week<=35,]
  mwd = mwd[complete.cases(mwd),]
  mwd$Year= Year
  return(mwd)
  rm(mwd)
  rm(f)
  gc()
}

weather_dir = "D:/ATtest/Europe_version2/Daily_data"
mortalitydir = "D:/ATtest/Europe_version2/Mortality_data"
threfile = "heat_threshold/Heat_threshold_countrylevel_v2.csv"


alldata = rbindlist(lapply(seq(2000,2022),function(y){
  print(paste(y,"start:"))
  d = dataprocess(y,weather_dir,threfile,mortalitydir)
  return(d)
  print(paste(y,"End!"))
}))
outdir2 = "Input_model_data"
if (!file.exists(outdir2)){
  dir.create(outdir2)
}

outname = file.path(outdir2,paste0("Input_data_all_v2.csv"))
fwrite(alldata, outname)
library(dplyr)
stat =alldata%>%group_by(geo,age)%>%summarise(start = min(Year),end=max(Year),num=length(death))




