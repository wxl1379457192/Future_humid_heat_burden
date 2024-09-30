rm(list = ls())
gc()
setwd("D:/ATtest/Europe_version2")
library(lubridate)
library(data.table)
Humidex <- function(temp, dew) {
  k1 <- 6.11 * exp(5417.7530 * ((1 / 273.16) - (1 / (dew + 273.15))))
  Hum <- temp + 0.5555 * (k1 - 10)
  return(Hum)
}


mortality_process = function(mortality_path){
  mortality <- fread(mortality_path, 
                     select = c("TIME_PERIOD", "age", "sex","geo","OBS_VALUE"), 
                     stringsAsFactors = FALSE)
  
  mortality[, `:=` (Year = as.numeric(substring(TIME_PERIOD, 1, 4)),
                    Week = as.numeric(substring(TIME_PERIOD, 7, 8)))]
  mortality[, age := fcase(
    grepl("LT5|Y5-9|Y10-14", age), "0-15",
    grepl("Y15-19|Y20-24|Y25-29|Y30-34|Y35-39|Y40-44|Y45-49|Y50-54|Y55-59|Y60-64", age), "15-65",
    grepl("Y65-69|Y70-74|Y75-79|Y80-84|Y85-89|Y_GE90", age), "65+",
    grepl("TOTAL", age), "TOTAL",
    default = NA_character_
  )]
  mortality_new <- mortality[, .(death = sum(OBS_VALUE)), by = .(geo, Year, Week, sex, age)]
  mortality_new <- mortality_new[!is.na(age) & sex == "T"]
  mor1 <- mortality_new[geo %like% "DE" & geo != "DE"]
  mor2 <- mortality_new[!geo %like% "DE"]
  mor1_all <- mortality_new[geo == "DE"]
  mor1new = do.call(rbind,lapply(split(mor1_all,mor1_all$Year),function(y){
    y2 = mor1[which(mor1$Year==unique(y$Year)),]
    y  = do.call(rbind,lapply(split(y,y$Week),function(w){
      w2 = y2[which(y2$Week==unique(w$Week)),]
      yd = data.frame(geo=unique(w$geo),Year = unique(w$Year),Week =  unique(w$Week),
                      sex = unique(w$sex),age="0-15",
                      death = w$death[which(w$age=="TOTAL")]-w$death[which(w$age=="15-65")]-
                        w$death[which(w$age=="65+")])
      w = do.call(rbind,list(w,yd))
      w$prop = w$death/w$death[which(w$age=="TOTAL")]
      w2n = do.call(rbind,lapply(unique(w$age),function(a){
        data.frame(geo=w2$geo,Year = w2$Year,Week =  w2$Week,sex = w2$sex,
                   age = rep(a,nrow(w2)), death = w2$death*w$prop[which(w$age==a)])
      }))
      return(w2n)
    }))
    return(y)
  }))
  mortality_new <- rbindlist(list(mor1new, mor2), use.names = TRUE, fill = TRUE)
  return(mortality_new)
}
mortality_batch_storage = function(mortalitydata,Year,outdir){

  if (file.exists(morname)){
    print(paste(morname," has existed"))
  }else{
    m <- mortalitydata[Year == Year,]
    fwrite(m, morname)
  }
}
mortality_path = "D:/ATtest/Europe/Mortality_Dataset/demo_r_mweek3_linear.csv/demo_r_mweek3_linear.csv"
mortality_new = mortality_process(mortality_path)
outdir = "Mortality_data"
if (!file.exists(outdir)){
  dir.create(outdir)
}
for (m in split(mortality_new,mortality_new$Year)){
  morname = file.path(outdir,paste0("Weekly_mortality_totalage_",unique(m$Year),".csv"))
  fwrite(m, morname)
}

datamerge = function(wd,win){
  all= merge(wd,win,by.x=c("NUTS_ID","time","ymd_local"),by.y=c("NUTS_ID","time","Date"))
  all[, `:=` (Heat_90th = as.integer(hum>=Humidex_90th),
              Heat_95th = as.integer(hum>=Humidex_95th),
              Heat_99th = as.integer(hum>=Humidex_99th),
              Heat_90th_tem = as.integer(tem>=Temp_90th),
              Heat_95th_tem = as.integer(tem>=Temp_95th),
              Heat_99th_tem = as.integer(tem>=Temp_99th))]
  new <- all[, .(
    tem = mean(tem), Hum = mean(hum),
    tem_max = max(tem_max), Hum_max = max(hum_max),
    tem_min = min(tem_min), Hum_min = min(hum_min),
    Heatday_90th = as.integer(Heat_90th[daytime == 1] == 1 & Heat_90th[daytime == 0] == 0),
    Heatnight_90th = as.integer(Heat_90th[daytime == 1] == 0 & Heat_90th[daytime == 0] == 1),
    Heatall_90th = as.integer(Heat_90th[daytime == 1] == 1 & Heat_90th[daytime == 0] == 1),
    Heatday_90th_tem = as.integer(Heat_90th_tem[daytime == 1] == 1 & Heat_90th_tem[daytime == 0] == 0),
    Heatnight_90th_tem = as.integer(Heat_90th_tem[daytime == 1] == 0 & Heat_90th_tem[daytime == 0] == 1),
    Heatall_90th_tem = as.integer(Heat_90th_tem[daytime == 1] == 1 & Heat_90th_tem[daytime == 0] == 1),
    Heatday_95th =as.integer(Heat_95th[which(daytime==1)]== 1& Heat_95th[which(daytime==0)]==0),
    Heatnight_95th = as.integer(Heat_95th[daytime == 1] == 0 & Heat_95th[daytime == 0] == 1),
    Heatall_95th = as.integer(Heat_95th[daytime == 1] == 1 & Heat_95th[daytime == 0] == 1),
    Heatday_95th_tem =as.integer(Heat_95th_tem[which(daytime==1)]== 1& Heat_95th_tem[which(daytime==0)]==0),
    Heatnight_95th_tem = as.integer(Heat_95th_tem[daytime == 1] == 0 & Heat_95th_tem[daytime == 0] == 1),
    Heatall_95th_tem = as.integer(Heat_95th_tem[daytime == 1] == 1 & Heat_95th_tem[daytime == 0] == 1),
    Heatday_99th =as.integer(Heat_99th[which(daytime==1)]== 1& Heat_99th[which(daytime==0)]==0),
    Heatnight_99th = as.integer(Heat_99th[daytime == 1] == 0 & Heat_99th[daytime == 0] == 1),
    Heatall_99th = as.integer(Heat_99th[daytime == 1] == 1 & Heat_99th[daytime == 0] == 1),
    Heatday_99th_tem =as.integer(Heat_99th_tem[which(daytime==1)]== 1& Heat_99th_tem[which(daytime==0)]==0),
    Heatnight_99th_tem = as.integer(Heat_99th_tem[daytime == 1] == 0 & Heat_99th_tem[daytime == 0] == 1),
    Heatall_99th_tem = as.integer(Heat_99th_tem[daytime == 1] == 1 & Heat_99th_tem[daytime == 0] == 1)
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
    Heatday_90th = round(mean(Heatday_90th)),
    Heatnight_90th = round(mean(Heatnight_90th)),
    Heatall_90th = round(mean(Heatall_90th)),
    Heatday_90th_tem =round(mean(Heatday_90th_tem)),
    Heatnight_90th_tem =round(mean(Heatnight_90th_tem)),
    Heatall_90th_tem =round(mean(Heatall_90th_tem)),
    Heatday_95th =round(mean(Heatday_95th)),
    Heatnight_95th = round(mean(Heatnight_95th)),
    Heatall_95th = round(mean(Heatall_95th)),
    Heatday_95th_tem =round(mean(Heatday_95th_tem)),
    Heatnight_95th_tem = round(mean(Heatnight_95th_tem)),
    Heatall_95th_tem = round(mean(Heatall_95th_tem)),
    Heatday_99th =round(mean(Heatday_99th)),
    Heatnight_99th = round(mean(Heatnight_99th)),
    Heatall_99th =round(mean(Heatall_99th)),
    Heatday_99th_tem =round(mean(Heatday_99th_tem)),
    Heatnight_99th_tem = round(mean(Heatnight_99th_tem)),
    Heatall_99th_tem = round(mean(Heatall_99th_tem))
  ), by = .(NUTS_ID, ymd_local,week,weeknum)]
  
  
  new3 = new
  new3$NUTS_ID = substring(new3$NUTS_ID,1,3)
  new3 = new3[, .(
    tem = mean(tem), Hum = mean(Hum),
    tem_max = max(tem_max), Hum_max = max(Hum_max),
    tem_min = min(tem_min), Hum_min = min(Hum_min),
    Heatday_90th = round(mean(Heatday_90th)),
    Heatnight_90th = round(mean(Heatnight_90th)),
    Heatall_90th = round(mean(Heatall_90th)),
    Heatday_90th_tem =round(mean(Heatday_90th_tem)),
    Heatnight_90th_tem =round(mean(Heatnight_90th_tem)),
    Heatall_90th_tem =round(mean(Heatall_90th_tem)),
    Heatday_95th =round(mean(Heatday_95th)),
    Heatnight_95th = round(mean(Heatnight_95th)),
    Heatall_95th = round(mean(Heatall_95th)),
    Heatday_95th_tem =round(mean(Heatday_95th_tem)),
    Heatnight_95th_tem = round(mean(Heatnight_95th_tem)),
    Heatall_95th_tem = round(mean(Heatall_95th_tem)),
    Heatday_99th =round(mean(Heatday_99th)),
    Heatnight_99th = round(mean(Heatnight_99th)),
    Heatall_99th =round(mean(Heatall_99th)),
    Heatday_99th_tem =round(mean(Heatday_99th_tem)),
    Heatnight_99th_tem = round(mean(Heatnight_99th_tem)),
    Heatall_99th_tem = round(mean(Heatall_99th_tem))
  ), by = .(NUTS_ID, ymd_local,week,weeknum)]
  
  
  new = rbind(new,new2,new3)
  weeklynew = 
    new[,.(tem = mean(tem),Hum = mean(Hum),
           tem_max = max(tem_max),Hum_max = max(Hum_max),
           tem_min = min(tem_min),Hum_min = min(Hum_min),
           Heatday_90th = sum(Heatday_90th),
           CHD_90th = {
             rle_values <- rle(Heatday_90th == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatday_95th = sum(Heatday_95th),
           CHD_95th = {
             rle_values <- rle(Heatday_95th == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatday_99th = sum(Heatday_99th),
           CHD_99th = {
             rle_values <- rle(Heatday_99th == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatday_90th_tem = sum(Heatday_90th_tem),
           CHD_90th_tem = {
             rle_values <- rle(Heatday_90th_tem == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatday_95th_tem = sum(Heatday_95th_tem),
           CHD_95th_tem = {
             rle_values <- rle(Heatday_95th_tem == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatday_99th_tem = sum(Heatday_99th_tem),
           CHD_99th_tem = {
             rle_values <- rle(Heatday_99th_tem == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           
           Heatnight_90th = sum(Heatnight_90th),
           CHN_90th = {
             rle_values <- rle(Heatnight_90th == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatnight_95th = sum(Heatnight_95th),
           CHN_95th = {
             rle_values <- rle(Heatnight_95th == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatnight_99th = sum(Heatnight_99th),
           CHN_99th = {
             rle_values <- rle(Heatnight_99th == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatnight_90th_tem = sum(Heatnight_90th_tem),
           CHN_90th_tem = {
             rle_values <- rle(Heatnight_90th_tem == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatnight_95th_tem = sum(Heatnight_95th_tem),
           CHN_95th_tem = {
             rle_values <- rle(Heatnight_95th_tem == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatnight_99th_tem = sum(Heatnight_99th_tem),
           CHN_99th_tem = {
             rle_values <- rle(Heatnight_99th_tem == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           
           Heatall_90th = sum(Heatall_90th),
           CH_90th = {
             rle_values <- rle(Heatall_90th == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatall_95th = sum(Heatall_95th),
           CH_95th = {
             rle_values <- rle(Heatall_95th == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatall_99th = sum(Heatall_99th),
           CH_99th = {
             rle_values <- rle(Heatall_99th == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatall_90th_tem = sum(Heatall_90th_tem),
           CH_90th_tem = {
             rle_values <- rle(Heatall_90th_tem == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatall_95th_tem = sum(Heatall_95th_tem),
           CH_95th_tem = {
             rle_values <- rle(Heatall_95th_tem == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}},
           Heatall_99th_tem = sum(Heatall_99th_tem),
           CH_99th_tem = {
             rle_values <- rle(Heatall_99th_tem == 1)
             if (any(rle_values$values)) {
               as.double(max(rle_values$lengths[rle_values$values]))
             } else {0}}
    ) , by = .(NUTS_ID, week,weeknum)]
  return(weeklynew)
}
dataprocess = function(Year,weather_dir,thredir,mortalitydir){
  weatherdir  = file.path(weather_dir,paste0("Daily_aggregate_",Year,".csv"))
  threname = file.path(thredir,paste0("heat_threshold_",Year,".csv"))
  morname = file.path(mortalitydir,paste0("weekly_mortality_totalage_",Year,".csv"))
  wd =  fread(weatherdir) 
  thd = fread(threname) 
 
  #win10 =addDate(win10)
  f = rbindlist(lapply(c(10,14,30),function(w){
    win = thd[which(thd$window==w),]
    win$Date = as.Date(paste0(win$year,"-",win$Day))
    k = datamerge(wd,win)
    k$window = w
    print(w)
    return(k)
  }))
  mor =  fread(morname)
  mwd = merge(mor,f,by.x=c("geo","Week"),by.y=c("NUTS_ID","weeknum"))
  return(mwd)
}


addDate = function(indata){
  do.call(rbind,lapply(split(indata,indata$NUTS_ID),function(t1){
    t = do.call(rbind,lapply(split(t1,t1$time),function(dn){
      dn$Date = seq.Date(as.Date(paste0(unique(dn$year),"-05-01")),
                         as.Date(paste0(unique(dn$year),"-09-30")),
                         by="day")
      return(dn)
    }))
    return(t)
  }))
}

thredir = "heat_threshold"
weather_dir = "Daily_data"
mortalitydir = "Mortality_data"

alldata = rbindlist(lapply(seq(2000,2023),function(y){
  print(paste(y,"start:"))
  d = dataprocess(y,weather_dir,thredir,mortalitydir)
  return(d)
  print(paste(y,"End!"))
}))
outdir2 = "Input_model_data"
if (!file.exists(outdir2)){
  dir.create(outdir2)
}
for (i in split(alldata,alldata$window)){
  outname = file.path(outdir2,paste0("Input_data_window",unique(i$window),".csv"))
  fwrite(i, outname)
}






