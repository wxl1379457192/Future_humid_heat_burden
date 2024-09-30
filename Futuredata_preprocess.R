rm(list = ls())
gc()
library(data.table)
library(progress)     
setwd("D:/ATtest/Europe_version2/")

dir = "E:/D_data/CEMS_Humidex_pop"
file_names <- list.files(dir)
outdir = "Future_heat_weekly"
if(dir.exists(outdir)){
  print(paste(outdir,"has existed!"))
}else{
  dir.create(outdir)
}
warming = read.csv("Result/Global_warming.csv",stringsAsFactors = F)
###############处理CESM数据，提取每个geo每个月的delta#########
CESM_list = grep("Hourly_future", file_names, value = TRUE)
CESM_data = do.call(rbind,lapply(CESM_list,function(c){
  cesm = fread(file.path(dir,c),stringsAsFactors = F)
  cesm$Humidex_mean[which(cesm$Humidex_mean>60|cesm$Humidex_mean<=-60)] = NA
  cesm = cesm[complete.cases(cesm),]
  cesm$year = as.numeric(cesm$year)
  rcplist = warming$Year[which(warming$Scenario==strsplit(c,"_")[[1]][3])]
  proc = do.call(rbind,lapply(rcplist,function(y){
    cy = cesm[which(year==y),]
    base = cesm[which(cesm$year==2022),]
    #base = read.csv(file.path("Future_data_CESM_2022",c),stringsAsFactors = F)
    colnames(base)[1] = "base"
    colnames(base)[2] = "base_ta"
    cy = merge(cy,base[,c(1,2,3,14)],by=c("NUTS_ID","month"))
    cy$delta = cy$Humidex_mean-cy$base
    cy$delta_ta = cy$Tas_mean-cy$base_ta
    cy = cy[,c("NUTS_ID","month","year","delta","delta_ta")]
    return(cy)
  }))
  proc$rcp = strsplit(c,"_")[[1]][3]
  proc$member = strsplit(strsplit(c,"_")[[1]][4],".csv")[[1]][1]
  print(paste(c,"has been processed."))
  return(proc)
}))
fwrite(CESM_data,file.path(outdir,"CESM_delta.csv"),row.names = F)
#CESM_data = read.csv(file.path(outdir,"CESM_delta.csv"),stringsAsFactors = F)
###############处理hourly数据，按周聚合数据#########
thredir = "DLMN_model_V0530"
library(lubridate)
library(progress)
library(dplyr)
hourly_data = fread(file.path("D:/ATtest/Europe/Hourly_data_2022","Hourly_Humidex_2022.csv"),stringsAsFactors = F)

modeldir = "Model"
da = fread(file.path(modeldir,"Inputdata_win14_95th.csv"),stringsAsFactors  = F)
#geolist = unique(da$geom)
CESM_data_V2 = do.call(rbind,lapply(split(da,da$geo),function(geo){
  cegeo = subset(CESM_data,CESM_data$NUTS_ID==unique(geo$geo))
  if(nrow(cegeo)==0){
    cegeo = CESM_data[grep(unique(geo$geo), CESM_data$NUTS_ID),]
    if(nrow(cegeo)>0){
      cegeo$geom = unique(geo$geo)
      cegeo = cegeo%>%group_by(geom,month,year,rcp,member)%>%summarise(delta=mean(delta),delta_ta=mean(delta_ta))
      colnames(cegeo)[1]="NUTS_ID"
    }else{
      print(paste0(unique(geo$geo)," did not exist!"))
    }
  }
  cegeo = cegeo[,c("NUTS_ID","month","year","rcp","member","delta","delta_ta")]
  return(cegeo)
}))
fwrite(CESM_data_V2,file.path(outdir,"CESM_delta.csv"),row.names = F)

#CESM_data = CESM_data[which(CESM_data$NUTS_ID%in%geolist),]




CESM_data = fread(file.path(outdir,"CESM_delta.csv"))
suntime = fread("suncal_times.csv")
k1 = suntime
k1$NUTS_ID = substring(k1$NUTS_ID,1,3)
k1 = k1%>%group_by(NUTS_ID,Date)%>%summarise(sunrise = round(mean(sunrise)),
                                             sunset = round(mean(sunset)),
                                             solarNoon= round(mean(solarNoon)))
k2 = suntime
k2$NUTS_ID = substring(k2$NUTS_ID,1,4)
k2 = k2%>%group_by(NUTS_ID,Date)%>%summarise(sunrise = round(mean(sunrise)),
                                             sunset = round(mean(sunset)),
                                             solarNoon= round(mean(solarNoon)))
suntime = suntime[,c("NUTS_ID","Date","sunrise","sunset","solarNoon")]
suntime = rbindlist(list(suntime,k1,k2))
fwrite(suntime,"suncal_times.csv")
# 创建进度条
pb <- progress_bar$new(
  format = "[:bar] :percent ETA: :eta",
  total = length(unique(CESM_data$NUTS_ID)),
  clear = FALSE
)
library(doParallel)
library(foreach)
library(dplyr)

# 设置并行处理的核心数，根据你的计算机配置进行调整
cores <- 8  # 你可以根据需要更改核心数

# 设置并行处理
cl <- makeCluster(cores)
registerDoParallel(cl)

# 设置总任务数，用于进度条
total_tasks <- length(unique(CESM_data$NUTS_ID))
pb <- txtProgressBar(min = 0, max = total_tasks, style = 3)

foreach(subset_geo = split(unique(CESM_data$NUTS_ID),
                           1:length(unique(CESM_data$NUTS_ID)) %% cores),
        .packages = c("dplyr", "lubridate", "data.table"),
        .errorhandling = 'pass') %dopar% {
          tryCatch({
            for (geo in subset_geo) {
              suntime = fread("suncal_times.csv")
              thredata = fread(file.path("heat_threshold","heat_threshold_2022.csv"),stringsAsFactors = F)
              outname <- file.path(outdir, paste0("Future_weekly_data_", geo, ".csv"))
              if (file.exists(outname)) {
                print(paste(outname, "has exists"))
              } else {
                print(paste(outname, "begin:\n"))
                hg <- hourly_data[grep(geo, hourly_data$NUTS_ID),]
                if (length(unique(hg)) > 0) {
                  hg <- summarise(group_by(hg, Date, hour), Humidex_mean = mean(Humidex_mean))
                  hg <- hg %>% group_by(Date, hour) %>% summarise(Humidex_mean = mean(Humidex_mean))
                  hg$NUTS_ID <- geo
                }
                cg <- CESM_data[which(CESM_data$NUTS_ID == geo),]
                cg$scenario <- paste(cg$rcp, cg$member)
                thr <- thredata[grep(geo, thredata$NUTS_ID),]
                thr <- thr[window==14,]
                if (nrow(hg) > 0 & nrow(cg) > 0 & nrow(thr) > 0) {
                  mergedata <- do.call(rbind, lapply(split(cg, cg$scenario), function(cgs) {
                    # print(unique(cgs$scenario))
                    myear <- do.call(rbind, lapply(split(cgs, cgs$year), function(y) {
                      #print(unique(y$year))
                      newdata <- hg[, c("Humidex_mean", "NUTS_ID", "Date", "hour")]
                      newdata$month <- as.numeric(substring(newdata$Date, 6, 7))
                      newdata$Humidex_mean[which(newdata$month == 6)] <-
                        newdata$Humidex_mean[which(newdata$month == 6)] + y$delta[which(y$month == 6)]
                      newdata$Humidex_mean[which(newdata$month == 7)] <-
                        newdata$Humidex_mean[which(newdata$month == 7)] + y$delta[which(y$month == 7)]
                      if (nrow(y[which(y$month == 8), ]) > 0) {
                        newdata$Humidex_mean[which(newdata$month == 8)] <-
                          newdata$Humidex_mean[which(newdata$month == 8)] + y$delta[which(y$month == 8)]
                      } else {
                        newdata$Humidex_mean[which(newdata$month == 8)] <-
                          newdata$Humidex_mean[which(newdata$month == 8)] + y$delta[which(y$month == 7)]
                      }
                      newdata$rcp <- unique(y$rcp)
                      de <- as.Date(paste0(unique(y$year), "-", "06-01")) - as.Date("2022-06-01")
                      newdata$Date <- as.Date(newdata$Date) + de
                      newdata$member <- unique(y$member)
                      newdata$day = substring(newdata$Date,6,10)
                      localsun = suntime[NUTS_ID==geo,]
                      newdata = merge(newdata,localsun[,c("sunrise","sunset","Date")],by.x="day",by.y="Date")
                      newdata$time_segment <- 0
                      newdata$time_segment[newdata$hour >= newdata$sunrise & newdata$hour < newdata$sunset] <- "Day"
                      newdata$time_segment[newdata$hour < newdata$sunrise | newdata$hour >= newdata$sunset] <- "Night"
                      dailydata = newdata%>%group_by(Date,NUTS_ID,month,rcp,member,sunrise,sunset,time_segment)%>%summarise(Humidex_mean=mean(Humidex_mean))
                      dailydata$Day = substring(dailydata$Date,6,10)
                      dailydata = merge(dailydata,thr,by.x =c("Day","time_segment"),by.y = c("Day","time") )
                      setDT(dailydata)
                      dailydata[, `:=` (Heat_95th = as.integer(Humidex_mean>=Humidex_95th))]
                      colnames(dailydata)[4]  = "NUTS_ID"
                      new <- dailydata[, .(
                        Hum = mean(Humidex_mean),
                        Heatday_95th =sum(Heat_95th[which(time_segment=="Day")]),
                        Heatnight_95th = sum(Heat_95th[which(time_segment=="Night")]),
                        Heatall_95th =  sum(Heat_95th)
                      ), by = .(NUTS_ID,rcp,member, Date)]
                      new$week = format(new$Date, format = "%Y-W%V")
                      new$weeknum = as.numeric(substring(new$week,7,8))
                      new = new[which(weeknum>=22&weeknum<=35),]
                      
                      new2 = new
                      new2$NUTS_ID = substring(new2$NUTS_ID,1,4)
                      new2 = new2[, .(
                        Hum = mean(Hum),
                        Heatday_95th =round(mean(Heatday_95th)),
                        Heatnight_95th = round(mean(Heatnight_95th)),
                        Heatall_95th = round(mean(Heatall_95th))
                      ), by = .(NUTS_ID, Date,rcp,member, week,weeknum)]
                      
                      
                      new3 = new
                      new3$NUTS_ID = substring(new3$NUTS_ID,1,3)
                      new3 = new3[, .(
                        Hum = mean(Hum),
                        Heatday_95th =round(mean(Heatday_95th)),
                        Heatnight_95th = round(mean(Heatnight_95th)),
                        Heatall_95th = round(mean(Heatall_95th))
                      ), by = .(NUTS_ID, Date,rcp,member, week,weeknum)]
                      
                      new = rbind(new,new2,new3)
                      weeklynew = 
                        new[,.(Hum = mean(Hum),
                               Heatday_95th = sum(Heatday_95th),
                               CHD_95th = {
                                 rle_values <- rle(Heatday_95th == 1)
                                 if (any(rle_values$values)) {
                                   as.double(max(rle_values$lengths[rle_values$values]))
                                 } else {0}},
                               Heatnight_95th = sum(Heatnight_95th),
                               CHN_95th = {
                                 rle_values <- rle(Heatnight_95th == 1)
                                 if (any(rle_values$values)) {
                                   as.double(max(rle_values$lengths[rle_values$values]))
                                 } else {0}},
                               
                               Heatall_95th = sum(Heatall_95th),
                               CH_95th = {
                                 rle_values <- rle(Heatall_95th == 1)
                                 if (any(rle_values$values)) {
                                   as.double(max(rle_values$lengths[rle_values$values]))
                                 } else {0}}
                        ) , by = .(NUTS_ID,rcp,member, week,weeknum)]
                      
                      return(weeklynew)
                    }))
                    return(myear)
                  }))
                  fwrite(mergedata, outname, row.names = F)
                  # 更新进度条
                  setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
                }
              }
            }
            }, error = function(e) {
              # 打印错误信息和相关的geo值
              message("Error in geo: ", geo, "\n", e$message)
              NULL  # 返回NULL，以便foreach可以继续处理其他任务
            })
          }
      
# 停止并行处理
stopCluster(cl)
registerDoSEQ()  # 恢复顺序处理
#sink()
# 清除进度条


# weekly =lapply(unique(CESM_data$NUTS_ID),function(geo){
#   outname = file.path(outdir,paste0("Future_weekly_data_",geo,".csv"))
#   if(file.exists(outname)){
#     print(paste(outname,"has exists"))
#   }else{
#     print(paste(outname,"begin:"))
#     hg =  hourly_data[grep(geo, hourly_data$NUTS_ID),]
#     if(length(unique(hg))>0){
#       hg = hg%>%group_by(Date,hour)%>%summarise(Humidex_mean = mean(Humidex_mean))
#       hg$NUTS_ID = geo
#     }
#     cg = CESM_data[which(CESM_data$NUTS_ID==geo),]
#     cg$scenario = paste(cg$rcp,cg$member)
#     thr = thredata[grep(geo,thredata$geo),]
#     if(length(unique(thr))>0){
#       thr = thr%>%group_by(date)%>%summarise(Humidex_90th = mean(Humidex_90th),
#                                              Humidex_95th = mean(Humidex_95th),
#                                              Humidex_99th = mean(Humidex_99th),
#                                              Temp_90th = mean(Temp_90th),
#                                              Temp_95th = mean(Temp_95th),
#                                              Temp_99th = mean(Temp_99th))
#       thr$geo = geo
#     }
#     if(nrow(hg)>0&nrow(cg)>0&nrow(thr)>0){
#       mergedata = do.call(rbind,lapply(split(cg,cg$scenario),function(cgs){
#         #print(unique(cgs$scenario))
#         myear = do.call(rbind,lapply(split(cgs,cgs$year),function(y){
#           print(unique(y$year))
#           newdata = hg[,c("Humidex_mean","NUTS_ID","Date","hour")]
#           newdata$month = as.numeric(substring(newdata$Date,6,7))
#           newdata$Humidex_mean[which(newdata$month==6)] = 
#             newdata$Humidex_mean[which(newdata$month==6)]+y$delta[which(y$month==6)]
#           newdata$Humidex_mean[which(newdata$month==7)] = 
#             newdata$Humidex_mean[which(newdata$month==7)]+y$delta[which(y$month==7)]
#           if(nrow(y[which(y$month==8),])>0){
#             newdata$Humidex_mean[which(newdata$month==8)] = 
#               newdata$Humidex_mean[which(newdata$month==8)]+y$delta[which(y$month==8)]
#           }else{
#             newdata$Humidex_mean[which(newdata$month==8)] = 
#               newdata$Humidex_mean[which(newdata$month==8)]+y$delta[which(y$month==7)]
#           }
#           newdata$rcp = unique(y$rcp)
#           de = as.Date(paste0(unique(y$year),"-","06-01"))-as.Date("2022-06-01")
#           newdata$Date = as.Date(newdata$Date)+de
#           newdata$member = unique(y$member)
#           
#           newdata$time_segment = 0
#           newdata$time_segment[newdata$hour >= 22|newdata$hour <6] <- "night"
#           newdata$time_segment[newdata$hour >= 6 &newdata$hour < 14] <- "morning"
#           newdata$time_segment[newdata$hour>= 14  & newdata$hour < 22 ] <- "afternoon"
#           dailycal = do.call(rbind,lapply(unique(newdata$Date),function(j){
#             thrd = subset(thr,thr$date==gsub(paste0(unique(y$year),"-"),"",j))
#             hourd = subset(newdata,newdata$Date==j)
#             hourd$hour = as.numeric(hourd$hour)
#             hourd$extreme_heat = 0
#             hourd$extreme_heat[which(hourd$Humidex_mean>=thrd$Humidex_95th)] = 1
#             
#             dd = data.frame(Date = j,geo = unique(hourd$NUTS_ID),
#                             heat_thre_hum = thrd$Humidex_95th,
#                             mean_humidex = mean(hourd$Humidex_mean),
#                             
#                             hum_mean_ni = mean(hourd$Humidex_mean[which(hourd$time_segment=="night")]),
#                             hum_mean_mo = mean(hourd$Humidex_mean[which(hourd$time_segment=="morning")]),
#                             hum_mean_af = mean(hourd$Humidex_mean[which(hourd$time_segment=="afternoon")]),
#                             cumhum_ni = sum(hourd$extreme_heat[which(hourd$time_segment=="night")]),
#                             cumhum_mo = sum(hourd$extreme_heat[which(hourd$time_segment=="morning")]),
#                             cumhum_af = sum(hourd$extreme_heat[which(hourd$time_segment=="afternoon")])
#             )
#             return(dd)
#           }))
#           
#           #######daily to weekly#############
#           dailycal$heat_morning = 0
#           dailycal$heat_morning[which(dailycal$hum_mean_mo>=dailycal$heat_thre_hum)] = 1
#           dailycal$heat_afternoon= 0
#           dailycal$heat_afternoon[which(dailycal$hum_mean_af>=dailycal$heat_thre_hum)] = 1
#           dailycal$heat_night= 0
#           dailycal$heat_night[which(dailycal$hum_mean_ni>=dailycal$heat_thre_hum)] = 1
#           
#           dailycal$week =format(dailycal$Date, format = "%Y-W%V")
#           geonew = do.call(rbind,lapply(
#             split(dailycal,dailycal$week),function(geow){
#               if(nrow(geow)>7){
#                 geow = geow[which(geow$Date<=min(geow$Date)+6),]
#               }
#               wd = data.frame(geo = unique(geow$geo),rcp = unique(y$rcp),
#                               member =  unique(y$member),
#                               year = year(geow$Date)[1],
#                               week = unique(geow$week),
#                               Humidex_mean = mean(geow$mean_humidex),
#                               Humidex_mean_morning = mean(geow$hum_mean_mo),
#                               Humidex_mean_afternoon = mean(geow$hum_mean_af),
#                               Humidex_mean_night = mean(geow$hum_mean_ni),
#                               DD_num =  nrow(geow[which(geow$heat_morning==1&geow$heat_afternoon==0&geow$heat_night==0),])+
#                                 nrow(geow[which(geow$heat_morning==1&geow$heat_afternoon==1&geow$heat_night==0),])+
#                                 nrow(geow[which(geow$heat_morning==0&geow$heat_afternoon==1&geow$heat_night==0),]),
#                               DN_num =  nrow(geow[which(geow$heat_morning==1&geow$heat_afternoon==0&geow$heat_night==1),])+
#                                 nrow(geow[which(geow$heat_morning==1&geow$heat_afternoon==1&geow$heat_night==1),])+
#                                 nrow(geow[which(geow$heat_morning==0&geow$heat_afternoon==1&geow$heat_night==1),])
#                               
#               )
#               
#               return(wd)
#             }))
#           return(geonew)
#         }))
#       }))
#       write.csv(mergedata,outname,row.names = F)
#       pb$tick()
#     }
#   }
# })
