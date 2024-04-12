rm(list = ls())
gc()
setwd("D:/ATtest/Europe/")

###############处理hourly数据，按周聚合数据#########
thredir = "DLMN_model_V0530"
library(lubridate)
library(progress)
library(dplyr)
hourly_data = read.csv(file.path("Hourly_data_2022","Hourly_Humidex_2022.csv"),stringsAsFactors = F)
thredata = read.csv(file.path(thredir,"Temperature_threshold_2022_v2.csv"),stringsAsFactors = F)
da = read.csv(file.path("DLMN_model_V0530/pooled_model_age_group_dataV2","Inputdata_withinage.csv"),stringsAsFactors  = F)
#geolist = unique(da$geom
#创建进度条
pb <- progress_bar$new(
  format = "[:bar] :percent ETA: :eta",
  total = length(unique(da$geom)),
  clear = FALSE
)
weekly =lapply(unique(da$geom),function(geo){
 # print(paste0(geo," begin:"))
  hg = hourly_data[grep(geo,hourly_data$NUTS_ID),]
  if (length(unique(hg)) > 1) {
    hg <- summarise(group_by(hg, Date, hour), Humidex_mean = mean(Humidex_mean))
    hg <- hg %>% group_by(Date, hour) %>% summarise(Humidex_mean = mean(Humidex_mean))
    hg$NUTS_ID <- geo
  }
  thr <- thredata[grep(geo, thredata$geo),]
  if (length(unique(thr)) > 1) {
    thr <- thr %>% group_by(date) %>% summarise(Humidex_90th = mean(Humidex_90th),
                                                Humidex_95th = mean(Humidex_95th),
                                                Humidex_99th = mean(Humidex_99th),
                                                Temp_90th = mean(Temp_90th),
                                                Temp_95th = mean(Temp_95th),
                                                Temp_99th = mean(Temp_99th))
    thr$geo <- geo
  }
  if(nrow(hg)>0&nrow(thr)>0){
    newdata = hg[,c("Humidex_mean","NUTS_ID","Date","hour")]
    newdata$month = as.numeric(substring(newdata$Date,6,7))
    newdata$Date = as.Date(newdata$Date)
    
    newdata$time_segment = 0
    newdata$time_segment[newdata$hour >= 22|newdata$hour <6] <- "night"
    newdata$time_segment[newdata$hour >= 6 &newdata$hour < 14] <- "morning"
    newdata$time_segment[newdata$hour>= 14  & newdata$hour < 22 ] <- "afternoon"
    dailycal = do.call(rbind,lapply(unique(newdata$Date),function(j){
      thrd = subset(thr,thr$date==gsub(paste0(2022,"-"),"",j))
      hourd = subset(newdata,newdata$Date==j)
      hourd$hour = as.numeric(hourd$hour)
      hourd$extreme_heat = 0
      hourd$extreme_heat[which(hourd$Humidex_mean>=thrd$Humidex_95th)] = 1
      
      dd = data.frame(Date = j,geo = unique(hourd$NUTS_ID),
                      heat_thre_hum = thrd$Humidex_95th,
                      mean_humidex = mean(hourd$Humidex_mean),
                      
                      hum_mean_ni = mean(hourd$Humidex_mean[which(hourd$time_segment=="night")]),
                      hum_mean_mo = mean(hourd$Humidex_mean[which(hourd$time_segment=="morning")]),
                      hum_mean_af = mean(hourd$Humidex_mean[which(hourd$time_segment=="afternoon")]),
                      cumhum_ni = sum(hourd$extreme_heat[which(hourd$time_segment=="night")]),
                      cumhum_mo = sum(hourd$extreme_heat[which(hourd$time_segment=="morning")]),
                      cumhum_af = sum(hourd$extreme_heat[which(hourd$time_segment=="afternoon")])
      )
      return(dd)
    }))
    
    #######daily to weekly#############
    dailycal$heat_morning = 0
    dailycal$heat_morning[which(dailycal$hum_mean_mo>=dailycal$heat_thre_hum)] = 1
    dailycal$heat_afternoon= 0
    dailycal$heat_afternoon[which(dailycal$hum_mean_af>=dailycal$heat_thre_hum)] = 1
    dailycal$heat_night= 0
    dailycal$heat_night[which(dailycal$hum_mean_ni>=dailycal$heat_thre_hum)] = 1
    
    dailycal$week =format(dailycal$Date, format = "%Y-W%V")
    geonew = do.call(rbind,lapply(
      split(dailycal,dailycal$week),function(geow){
        if(nrow(geow)>7){
          geow = geow[which(geow$Date<=min(geow$Date)+6),]
        }
        wd = data.frame(geo = unique(geow$geo),
                        year = year(geow$Date)[1],
                        week = unique(geow$week),
                        Humidex_mean = mean(geow$mean_humidex),
                        Humidex_mean_morning = mean(geow$hum_mean_mo),
                        Humidex_mean_afternoon = mean(geow$hum_mean_af),
                        Humidex_mean_night = mean(geow$hum_mean_ni),
                        DD_num =  nrow(geow[which(geow$heat_morning==1&geow$heat_afternoon==0&geow$heat_night==0),])+
                          nrow(geow[which(geow$heat_morning==1&geow$heat_afternoon==1&geow$heat_night==0),])+
                          nrow(geow[which(geow$heat_morning==0&geow$heat_afternoon==1&geow$heat_night==0),]),
                        DN_num =  nrow(geow[which(geow$heat_morning==1&geow$heat_afternoon==0&geow$heat_night==1),])+
                          nrow(geow[which(geow$heat_morning==1&geow$heat_afternoon==1&geow$heat_night==1),])+
                          nrow(geow[which(geow$heat_morning==0&geow$heat_afternoon==1&geow$heat_night==1),])
                        
        )
        return(wd)
      }))
    pb$tick()
    return(geonew)
  }else{
    return(NULL)
  }
})

weekly = do.call(rbind,weekly)
outdir = "2022_heat_weekly_V1223"
if(dir.exists(outdir)){
  print(paste(outdir,"has existed!"))
}else{
  dir.create(outdir)
}

write.csv(weekly,file.path(outdir,"Weekly_data_2022.csv"),row.names = F)