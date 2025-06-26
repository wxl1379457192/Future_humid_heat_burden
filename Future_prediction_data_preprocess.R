rm(list = ls())
gc()
library(data.table)
library(progress)     
library(dplyr)
setwd("D:/ATtest/Europe_version3/")

outdir = "Future_heat_weekly"
if(dir.exists(outdir)){
  print(paste(outdir,"has existed!"))
}else{
  dir.create(outdir)
}
warming = read.csv("D:/ATtest/Europe_version2/Result/Global_warming.csv",stringsAsFactors = F)

CESM_data = fread("D:/ATtest/Europe_version2/Future_heat_weekly/CESM_delta.csv")

hourly_data = fread(file.path("D:/ATtest/Europe/Hourly_data_2022","Hourly_Humidex_2022.csv"),stringsAsFactors = F)
suntime = fread("D:/ATtest/Europe_version2/suncal_times.csv")
thredata = fread(file.path("heat_threshold","Heat_threshold_countrylevel_v2.csv"),stringsAsFactors = F)
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
cores <- 10  # 保留一个核心给系统  # 你可以根据需要更改核心数

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
                thr <- thredata[grep(substring(geo,1,3), thredata$region),]
                thr <- thr[variable=="Humidex",]
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
                      dailydata = newdata%>%group_by(Date,NUTS_ID,month,rcp,member,sunrise,sunset,time_segment)%>%
                        summarise(Humidex_max= max(Humidex_mean),Humidex_min = min(Humidex_mean),Humidex_mean=mean(Humidex_mean))
                      dailydata$Day = substring(dailydata$Date,6,10)
                     # dailydata = merge(dailydata,thr,by.x =c("Day","time_segment"),by.y = c("Day","time") )
                      dailydata$threshold = 0
                      dailydata$threshold[which(dailydata$time_segment=="Day")] = thr$thre[which(thr$Time=="Day")]
                      dailydata$threshold[which(dailydata$time_segment=="Night")] = thr$thre[which(thr$Time=="Night")]
                      setDT(dailydata)
                      dailydata$Heat = 0
                      dailydata$Heat = as.integer(dailydata$Humidex_mean>=dailydata$threshold)
                      new <- dailydata[, .(
                        Hum = mean(Humidex_mean),
                        Heatday =sum(Heat[which(time_segment=="Day")]),
                        Heatnight = sum(Heat[which(time_segment=="Night")])
                      ), by = .(NUTS_ID,rcp,member,Date)]
                      new$Heatall =  0
                      new$Heatall[which(new$Heatday==1&new$Heatnight==1)]=1
                      new$Heatday[which(new$Heatall==1)]=0
                      new$Heatnight[which(new$Heatall==1)]=0
                      new$week = format(new$Date, format = "%Y-W%V")
                      new$weeknum = as.numeric(substring(new$week,7,8))
                      new = new[which(weeknum>=22&weeknum<=35),]
                      
                      new2 = new
                      new2$NUTS_ID = substring(new2$NUTS_ID,1,4)
                      new2 = new2[, .(
                        Hum = mean(Hum),
                        Heatday =round(mean(Heatday)),
                        Heatnight = round(mean(Heatnight)),
                        Heatall = round(mean(Heatall))
                      ), by = .(NUTS_ID, Date,rcp,member, week,weeknum)]
                      
                      
                      new3 = new
                      new3$NUTS_ID = substring(new3$NUTS_ID,1,3)
                      new3 = new3[, .(
                        Hum = mean(Hum),
                        Heatday =round(mean(Heatday)),
                        Heatnight = round(mean(Heatnight)),
                        Heatall = round(mean(Heatall))
                      ), by = .(NUTS_ID, Date,rcp,member, week,weeknum)]
                      
                      new = rbind(new,new2,new3)
                      weeklynew = 
                        new[,.(Hum = mean(Hum),
                               Heatday = sum(Heatday),
                               CHD = {
                                 rle_values <- rle(Heatday == 1)
                                 if (any(rle_values$values)) {
                                   as.double(max(rle_values$lengths[rle_values$values]))
                                 } else {0}},
                               Heatnight = sum(Heatnight),
                               CHN = {
                                 rle_values <- rle(Heatnight == 1)
                                 if (any(rle_values$values)) {
                                   as.double(max(rle_values$lengths[rle_values$values]))
                                 } else {0}},
                               
                               Heatall = sum(Heatall),
                               CH = {
                                 rle_values <- rle(Heatall == 1)
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
