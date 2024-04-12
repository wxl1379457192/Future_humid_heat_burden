setwd("D:/ATtest/Europe/")
library(lubridate)

Humidex = function(temp,dew){
  k1 = 6.11*exp(5417.7530*((1/273.16)-(1/(dew+273.15))))
  Hum = temp + 0.5555*(k1-10)
  return(Hum)
}

daily_extremeheat_hours= function(Year,dir,thredir,outdir){
  filelist  = list.files(dir, pattern=paste0("*",Year,"_",Year+1,".csv"))
  thredata = read.csv(paste0(thredir,"Temperature_threshold_",Year,"_v2.csv"))
  filename = paste0(outdir,"Daily_weather_data_",Year,"_V2.csv")
  if (file.exists(filename)){
    print(paste(filename," has existed"))
  }else{
    for (i in filelist){
      f = read.csv(paste0(dir,"/",i),stringsAsFactors = F)
      f$Date = substring(f$imageId,1,8)
      f$Hour = substring(f$imageId,10,11)
      f$Date = as.Date(f$Date,format='%Y%m%d')
      f = f[,c(-3)]
      if (grepl("dewpoint_temperature_2m", i, fixed=TRUE)){
        dew = f
        colnames(dew) = c("Dew","NUTs_ID","Date","Hour")
      }else{
        tem = f
        colnames(tem) = c("Temp","NUTs_ID","Date","Hour")
      }
      print(paste(i," has been added"))
      remove(f)
      gc()
    }
    all = do.call(rbind,lapply(split(dew,dew$NUTs_ID),function(d){
      t = subset(tem,tem$NUTs_ID==unique(d$NUTs_ID))
      t = t[,-c(2)]
      a = merge(d, t, by = c("Date","Hour"))
      a = a[format(a$Date, '%m') %in% c('05', '06', '07', '08'), ]
      a$Humidex = Humidex(a$Temp,a$Dew)
      thr = subset(thredata,thredata$geo==unique(d$NUTs_ID))
      thr$Date = as.Date(paste0(as.character(Year),"-",thr$date))
      remove(d,t)
      a$datetime <- paste0(as.character(a$Date),"-",a$Hour)
      a$datetime = ymd_h(a$datetime)
      a$hour = hour(a$datetime)
      
      # ????ʱ???ֶν??з???
      a$time_segment = 0
      a$time_segment[a$hour >= 22| a$hour <6] <- "night"
      a$time_segment[a$hour >= 6 & a$hour < 14] <- "morning"
      a$time_segment[a$hour  >= 14  & a$hour < 22 ] <- "afternoon"
      
      dailycal = do.call(rbind,lapply(unique(a$Date),function(j){
        thrd = subset(thr,thr$Date==j)
        hourd = subset(a,a$Date==j)
        hourd$Hour = as.numeric(hourd$Hour)
        hourd$extreme_heat = 0
        hourd$extreme_heat[which(hourd$Humidex>=thrd$Humidex_95th)] = 1
        hourd$extreme_temp = 0
        hourd$extreme_temp[which(hourd$Temp>=thrd$Temp_95th)] = 1
        
        dd = data.frame(Date = j,geo = unique(hourd$NUTs_ID),
                        heat_thre_hum = thrd$Humidex_95th,
                        heat_thre_tem = thrd$Temp_95th,
                        mean_humidex = mean(hourd$Humidex),
                        mean_Temp = mean(hourd$Temp),
                        mean_Dew = mean(hourd$Dew),
                        hum_mean_ni = mean(hourd$Humidex[which(hourd$time_segment=="night")]),
                        hum_mean_mo = mean(hourd$Humidex[which(hourd$time_segment=="morning")]),
                        hum_mean_af = mean(hourd$Humidex[which(hourd$time_segment=="afternoon")]),
                        cumhum_ni = sum(hourd$extreme_heat[which(hourd$time_segment=="night")]),
                        cumhum_mo = sum(hourd$extreme_heat[which(hourd$time_segment=="morning")]),
                        cumhum_af = sum(hourd$extreme_heat[which(hourd$time_segment=="afternoon")]),
                        
                        tem_mean_ni = mean(hourd$Temp[which(hourd$time_segment=="night")]),
                        tem_mean_mo = mean(hourd$Temp[which(hourd$time_segment=="morning")]),
                        tem_mean_af = mean(hourd$Temp[which(hourd$time_segment=="afternoon")]),
                        cumtem_ni = sum(hourd$extreme_temp[which(hourd$time_segment=="night")]),
                        cumtem_mo = sum(hourd$extreme_temp[which(hourd$time_segment=="morning")]),
                        cumtem_af = sum(hourd$extreme_temp[which(hourd$time_segment=="afternoon")]),
                        
                        
                        hum_min_ni = min(hourd$Humidex[which(hourd$time_segment=="night")]),
                        hum_min_mo = min(hourd$Humidex[which(hourd$time_segment=="morning")]),
                        hum_min_af = min(hourd$Humidex[which(hourd$time_segment=="afternoon")]),
                        tem_min_ni = min(hourd$Temp[which(hourd$time_segment=="night")]),
                        tem_min_mo = min(hourd$Temp[which(hourd$time_segment=="morning")]),
                        tem_min_af = min(hourd$Temp[which(hourd$time_segment=="afternoon")]),
                        
                        hum_max_ni = max(hourd$Humidex[which(hourd$time_segment=="night")]),
                        hum_max_mo = max(hourd$Humidex[which(hourd$time_segment=="morning")]),
                        hum_max_af = max(hourd$Humidex[which(hourd$time_segment=="afternoon")]),
                        tem_max_ni = max(hourd$Temp[which(hourd$time_segment=="night")]),
                        tem_max_mo = max(hourd$Temp[which(hourd$time_segment=="morning")]),
                        tem_max_af = max(hourd$Temp[which(hourd$time_segment=="afternoon")])
                        )
        
        return(dd)
      }))
      print(paste(unique(a$NUTs_ID)," has been processed"))
      return(dailycal)
    }))
    write.csv(all,filename,row.names = F)
  }
}

#################????ÿ?յ?????ʱ?????ж?????????#######
data_clean= function(Year,outdir){
  filename = paste0(outdir,"Daily_weather_data_",Year,"_V2.csv")
  if (file.exists(filename)){
    print(paste(filename," has existed"))
    f = read.csv(filename,stringsAsFactors = F)
    colnames(f)=c("Date","geo","heat_thre_hum","heat_thre_tem",
                  "mean_humidex","mean_Temp","mean_Dew","hum_mean_ni","hum_mean_mo", 
                  "hum_mean_af","cumhum_ni","cumhum_mo","cumhum_af","tem_mean_ni","tem_mean_mo",
                  "tem_mean_af","cumtem_ni","cumtem_mo","cumtem_af","hum_min_ni","hum_min_mo",
                  "hum_min_af","tem_min_ni","tem_min_mo","tem_min_af","hum_max_ni","hum_max_mo",
                  "hum_max_af","tem_max_ni","tem_max_mo","tem_max_af")
    write.csv(f,filename,row.names = F)
  }
}
dir = "Longtime_ERA5/earthengine/"
thredir = "DLMN_model_V0530/"
outdir = thredir
dir.create(outdir)

#threshold = read.csv(paste0(outdir,"MMT_DLNM.csv"),stringsAsFactors = F)
#threshold$MMT[which(threshold$MMT<threshold$Hum_75th)] = threshold$Hum_75th[which(threshold$MMT<threshold$Hum_75th)]
for(i in seq(2010,2020,1)){
  daily_extremeheat_hours(i,dir,thredir,outdir)
  #data_clean(i,outdir)
  gc()
}

################weekly ???ݺϳ?##########################
setwd("D:/ATtest/Europe")
library(lubridate)
library(dplyr)
weekly_process = function(outdir,year,mortalitydir){
  Daily_temperature_data = read.csv(paste0(outdir,"Daily_weather_data_",year,"_V2.csv"),stringsAsFactors = F)
  mortality = read.csv(paste0(mortalitydir,"weekly_mortality_totalage_",year,".csv"),stringsAsFactors = F)
  #mortality = subset(mortality,mortality$sex == "T")
  #mortality = subset(mortality,mortality$age == "TOTAL")
  mortality$date <- as.Date(paste(year, "-01-01", sep = ""), format = "%Y-%m-%d") + (mortality$Week - 1) * 7
  mortality$week <- format(mortality$date, "%G-W%V")
  
  Weeklydata = do.call(rbind,
                       lapply(split(Daily_temperature_data,Daily_temperature_data$geo),
                              function(geodata){
                        
                                geodata$heat_morning = 0
                                geodata$heat_morning[which(geodata$hum_mean_mo>=geodata$heat_thre_hum)] = 1
                                geodata$heat_afternoon= 0
                                geodata$heat_afternoon[which(geodata$hum_mean_af>=geodata$heat_thre_hum)] = 1
                                geodata$heat_night= 0
                                geodata$heat_night[which(geodata$hum_mean_ni>=geodata$heat_thre_hum)] = 1
                                
                                geodata$heat_morning_tem = 0
                                geodata$heat_morning_tem[which(geodata$tem_mean_mo>=geodata$heat_thre_tem)] = 1
                                geodata$heat_afternoon_tem= 0
                                geodata$heat_afternoon_tem[which(geodata$tem_mean_af>=geodata$heat_thre_tem)] = 1
                                geodata$heat_night_tem= 0
                                geodata$heat_night_tem[which(geodata$tem_mean_ni>=geodata$heat_thre_tem)] = 1
                                
                                geodata$Date = as.Date(geodata$Date)
                                geodata$week =format(geodata$Date, format = "%Y-W%V")
                                geonew = do.call(rbind,lapply(
                                  split(geodata,geodata$week),function(geow){
                                    if(nrow(geow)>7){
                                      geow = geow[which(geow$Date<=min(geow$Date)+6),]
                                    }
                                    wd = data.frame(geo = unique(geow$geo),
                                                    year = year(geow$Date)[1],
                                                    week = unique(geow$week),
                                                    Humidex_mean = mean(geow$mean_humidex),
                                                    Temp_mean = mean(geow$mean_Temp),
                                                    Humidex_mean_morning = mean(geow$hum_mean_mo),
                                                    Humidex_mean_afternoon = mean(geow$hum_mean_af),
                                                    Humidex_mean_night = mean(geow$hum_mean_ni),
                                                    Temp_mean_morning = mean(geow$tem_mean_mo),
                                                    Temp_mean_afternoon = mean(geow$tem_mean_af),
                                                    Temp_mean_night = mean(geow$tem_mean_ni),
                                                    Humidex_min_morning = mean(geow$hum_min_mo),
                                                    Humidex_min_afternoon = mean(geow$hum_min_af),
                                                    Humidex_min_night = mean(geow$hum_min_ni),
                                                    Temp_min_morning = mean(geow$tem_min_mo),
                                                    Temp_min_afternoon = mean(geow$tem_min_af),
                                                    Temp_min_night = mean(geow$tem_min_ni),
                                                    Humidex_max_morning = mean(geow$hum_max_mo),
                                                    Humidex_max_afternoon = mean(geow$hum_max_af),
                                                    Humidex_max_night = mean(geow$hum_max_ni),
                                                    Temp_max_morning = mean(geow$tem_max_mo),
                                                    Temp_max_afternoon = mean(geow$tem_max_af),
                                                    Temp_max_night = mean(geow$tem_max_ni),
                                                    
                                                    cumhour_morning = sum(geow$cumhum_mo),
                                                    cumhour_afternoon= sum(geow$cumhum_af),
                                                    cumhour_night = sum(geow$cumhum_ni),
                                                    cumhour =sum(geow$cumhum_mo)+sum(geow$cumhum_af)+sum(geow$cumhum_ni),
                                                    
                                                    DD_num =  nrow(geow[which(geow$heat_morning==1&geow$heat_afternoon==0&geow$heat_night==0),])+
                                                      nrow(geow[which(geow$heat_morning==1&geow$heat_afternoon==1&geow$heat_night==0),])+
                                                      nrow(geow[which(geow$heat_morning==0&geow$heat_afternoon==1&geow$heat_night==0),]),
                                                    DN_num =  nrow(geow[which(geow$heat_morning==1&geow$heat_afternoon==0&geow$heat_night==1),])+
                                                      nrow(geow[which(geow$heat_morning==1&geow$heat_afternoon==1&geow$heat_night==1),])+
                                                      nrow(geow[which(geow$heat_morning==0&geow$heat_afternoon==1&geow$heat_night==1),]),
                                                    
                                                    DD_num_tem =  nrow(geow[which(geow$heat_morning_tem==1&geow$heat_afternoon_tem==0&geow$heat_night_tem==0),])+
                                                      nrow(geow[which(geow$heat_morning_tem==1&geow$heat_afternoon_tem==1&geow$heat_night_tem==0),])+
                                                      nrow(geow[which(geow$heat_morning_tem==0&geow$heat_afternoon_tem==1&geow$heat_night_tem==0),]),
                                                    
                                                    DN_num_tem =   nrow(geow[which(geow$heat_morning_tem==1&geow$heat_afternoon_tem==0&geow$heat_night_tem==1),])+
                                                      nrow(geow[which(geow$heat_morning_tem==1&geow$heat_afternoon_tem==1&geow$heat_night_tem==1),])+
                                                      nrow(geow[which(geow$heat_morning_tem==0&geow$heat_afternoon_tem==1&geow$heat_night_tem==1),])
                                    )
                                                    
                                    return(wd)
                                  }))
                                
                                mor = mortality[which(mortality$geo==unique(geodata$geo)),]
                                if(nrow(mor)>0){
                                  mor = mor[,c("sex","age","geo","death","week")]
                                  geonew = merge(geonew,mor,by=c("geo","week"))
                                  geonew$geom = geonew$geo
                                  geonew = geonew[,c("geom","geo","week","year","Humidex_mean","DD_num","DN_num","death",
                                                     "Temp_mean","DD_num_tem","DN_num_tem","age","sex","cumhour")]
                                }else{
                                  mor = mortality[grep(substring(unique(geodata$geo),1,2), mortality$geo),]
                                  max_length <-max(nchar(unique(mor$geo)))
                                  mor = mor[which(nchar(mor$geo)==max_length),]
                                  mor = mor[,c("sex","age","geo","death","week")]
                                  geonew$geom = substring(geonew$geo,1,max_length)
                                  geonew = merge(geonew,mor,by.x=c("geom","week"),by.y = c("geo","week"))
                                  geonew = geonew[,c("geom","geo","week","year","Humidex_mean","DD_num","DN_num","death",
                                                     "Temp_mean","DD_num_tem","DN_num_tem","age","sex","cumhour")]
                                    
                                }
                                print(unique(geonew$geom))
                                return(geonew)
                              }))
  Weeklydata =  Weeklydata%>%group_by(geom,week,year,age,sex)%>%summarise(Humidex_mean  = mean(Humidex_mean),DD_num = mean(DD_num),DN_num = mean(DN_num),
                                                                          death  = mean(death),Temp_mean = mean(Temp_mean),
                                                                          DD_num_tem = mean(DD_num_tem),DN_num_tem = mean(DN_num_tem),cumhour=mean(cumhour))
  print("Weekly data has generated")
  #colnames(mortality)[4] = c("death")
   return(Weeklydata)
}

weeklydata = function(outdir,mortalitydir,outputdir){
  data = do.call(rbind,lapply(seq(2010,2022,1),function(i){
    w  = weekly_process(outdir,i,mortalitydir)
    gc()
    print(paste0(i," has been added"))
    return(w)
  }))
  write.csv(data,paste0(outputdir,"all_data_weekly","_V2.csv"),row.names = F)
}

outdir = "DLMN_model_V0530/"
#######???෽ʽΪ0-15,15-65,65+
weeklydata(outdir,outdir,outdir)
#k = data2

#k$week_num = as.integer(substring(k$week,7,8))
#k = k[which(k$week_num >=16&k$week_num<=40),]
#f = do.call(rbind,lapply(split(k,k$age),function(x){
#  do.call(rbind,lapply(split(x,x$geo),function(k1){
#    k1$death =k1$death+1
#    k1$DD = k1$Dayheat_num#Lag(k1$Dayheat_num,2,group = k1$year)
#    k1$DN = k1$DNheat_num#Lag(k1$DNheat_num,2,group = k1$year)
#    data.frame(geo = unique(k1$geo),age = unique(k1$age),
#               cor_Day = cor(log(k1$death),k1$DD), 
#               corDN =  cor(log(k1$death),k1$DN))
#    }))
#}))


#library(ggplot2)
#w = split(Weeklydata,Weeklydata$geo)[[7]]
#ggplot(Weeklydata)+geom_boxplot(aes(x=as.factor(cumhour_dayheat),y=death))

