rm(list = ls())
gc()
setwd("D:/ATtest/Europe/")
library(lubridate)


Humidex = function(temp,dew){
  k1 = 6.11*exp(5417.7530*((1/273.16)-(1/(dew+273.15))))
  Hum = temp + 0.5555*(k1-10)
  return(Hum)
}
dataprocess = function(Year,weather_dir,outdir,mortalitydata){
  filelist  = list.files(weather_dir, pattern=paste0("*",Year,"_",Year+1,".csv"))
  threname = paste0(outdir,"Temperature_threshold_",Year,".csv")
  if (file.exists(threname)){
    print(paste(threname," has existed"))
    }else{
      for (i in filelist){
        f = read.csv(paste0(weather_dir,"/",i),stringsAsFactors = F)
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
      }
      
      if(file.exists(threname)==F){
        th = do.call(rbind,lapply(split(dew,dew$NUTs_ID),function(d){
          t = subset(tem,tem$NUTs_ID==unique(d$NUTs_ID))
          t = t[,-c(2)]
          a = merge(d, t, by = c("Date","Hour"))
          a$Humidex = Humidex(a$Temp,a$Dew)
          a = do.call(rbind,lapply(split(a,a$Date),function(ad){
            ad =  data.frame(NUTs_ID = unique(ad$NUTs_ID),
                             Date = unique(ad$Date),
                             Humidex = mean(ad$Humidex),
                             Temp = mean(ad$Temp))
            return(ad)
          }))
          thre = data.frame(geo = unique(a$NUTs_ID),Year = year(a$Date)[1],
                            Humidex_90th = quantile(a$Humidex,0.90),
                            Humidex_95th = quantile(a$Humidex,0.95),
                            Humidex_99th = quantile(a$Humidex,0.99),
                            Temp_90th = quantile(a$Temp,0.90),
                            Temp_95th = quantile(a$Temp,0.95),
                            Temp_99th = quantile(a$Temp,0.99))
          print(paste(unique(d$NUTs_ID)," has been processed"))
          return(thre)
        }))
        write.csv(th,threname,row.names = F)
      }
  }
}

threshold_calculate = function(yearlist,filedir,outdir){
  filelist = list.files(filedir, pattern=paste0("*.csv"))
  for (i in filelist){
    f = read.csv(paste0(filedir,"/",i),stringsAsFactors = F)
    f$Date = substring(f$imageId,1,8)
    f$Date = as.Date(f$Date,format='%Y%m%d')
    # ɸѡ???·?Ϊ6?µ?7?µ?????
    #f <- f[format(f$Date, '%m') %in% c('06', '07'), ]
    f = f[,c(-3)]
    if (grepl("dewpoint_temperature_2m", i, fixed=TRUE)){
      dew = f
      colnames(dew) = c("Dew","NUTs_ID","Date")
    }else{
      tem = f
      colnames(tem) = c("Temp","NUTs_ID","Date")
    }
    print(paste(i," has been added"))
    remove(f)
  }
  for (year in yearlist){
    threname = paste0(outdir,"Temperature_threshold_",year,"_v2.csv")
    if(file.exists(threname)==F){
      dewall = dew[which(format(dew$Date, '%Y')>= year-50 &format(dew$Date, '%Y')< year),]
      temall = tem[which(format(tem$Date, '%Y')>= year-50 &format(tem$Date, '%Y')< year),]
      th = do.call(rbind,lapply(split(dewall,dewall$NUTs_ID),function(d){
        t = subset(temall,temall$NUTs_ID==unique(d$NUTs_ID))
        t = t[,-c(2)]
        a = merge(d, t, by = c("Date"))
        a$Humidex = Humidex(a$Temp,a$Dew)
        
        thre =do.call(rbind,lapply(unique(format(a$Date, "%m-%d")),function(date){
          c = subset(a,format(a$Date,"%m-%d")>=format(as.Date(date,"%m-%d")-7,"%m-%d")&format(a$Date,"%m-%d")<=format(as.Date(date,"%m-%d")+7,"%m-%d"))
          k=data.frame(geo = unique(c$NUTs_ID), date = date,
                     Humidex_90th = quantile(c$Humidex,0.90),
                     Humidex_95th = quantile(c$Humidex,0.95),
                     Humidex_99th = quantile(c$Humidex,0.99),
                     Temp_90th = quantile(c$Temp,0.90),
                     Temp_95th = quantile(c$Temp,0.95),
                     Temp_99th = quantile(c$Temp,0.99))
          return(k)
        }))
        print(paste(year,unique(d$NUTs_ID)," has been processed"))
        return(thre)
      }))
      write.csv(th,threname,row.names = F)
    }
  } 
}

mortality_process = function(Year,outdir,mortalitydata){
  morname = paste0(outdir,"weekly_mortality_totalage_",Year,".csv")
  if (file.exists(morname)){
    print(paste(morname," has existed"))
  }else{
    mortalitydata = mortalitydata[which(mortalitydata$Year == as.numeric(Year)),]
    #mortalitydata = subset(mortalitydata,mortalitydata$sex == "T")
    #mortalitydata = subset(mortalitydata,mortalitydata$age == "TOTAL")
    mortalitydata $length = nchar(mortalitydata$geo)
    #mortalitydata= subset(mortalitydata,mortalitydata$length==5)
    mortalitydata=mortalitydata[,c(-7)]
    write.csv(mortalitydata, morname,row.names = F)
  }
}
####??ȡ?????˿ڵ?????????
mortality = read.csv("Mortality_Dataset/demo_r_mweek3_linear.csv/demo_r_mweek3_linear.csv",stringsAsFactors = F)
mortality = mortality[,c(-1,-2,-3,-4)]
mortality$Year = as.numeric(substring(mortality$TIME_PERIOD,1,4))
mortality$Week = as.numeric(substring(mortality$TIME_PERIOD,7,8))
mortality = mortality[,c(-4,-6)]
#mortality = subset(mortality,mortality$Year==2022)
# ???ȣ??????????ַ???ת??Ϊ???֣???????һ???µ??? age_group
mortality$age <- ifelse(grepl("LT5", mortality$age), "0-15",
                        ifelse(grepl("Y5-9", mortality$age), "0-15",
                        ifelse(grepl("Y10-14", mortality$age), "0-15",
                        ifelse(grepl("Y15-19", mortality$age), "15-65",
                        ifelse(grepl("Y20-24", mortality$age), "15-65",
                        ifelse(grepl("Y25-29", mortality$age), "15-65",
                        ifelse(grepl("Y30-34", mortality$age), "15-65",
                        ifelse(grepl("Y35-39", mortality$age), "15-65",
                        ifelse(grepl("Y40-44", mortality$age), "15-65",
                        ifelse(grepl("Y45-49", mortality$age), "15-65",
                        ifelse(grepl("Y50-54", mortality$age), "15-65",
                        ifelse(grepl("Y55-59", mortality$age), "15-65",
                        ifelse(grepl("Y60-64", mortality$age), "15-65",
                        ifelse(grepl("Y65-69", mortality$age), "65+",
                        ifelse(grepl("Y70-74", mortality$age), "65+",
                        ifelse(grepl("Y75-79", mortality$age), "65+",
                        ifelse(grepl("Y80-84", mortality$age), "65+",
                        ifelse(grepl("Y85-89", mortality$age), "65+",
                        ifelse(grepl("Y_GE90", mortality$age), "65+",
                        ifelse(grepl("TOTAL", mortality$age), "TOTAL",NA)
                        )))))))))))))))))))


library(dplyr)
mortality_new <- mortality %>%
  group_by(geo,Year,Week,sex,age) %>%
  summarize(death = sum(OBS_VALUE))
mortality_new = mortality_new[!is.na(mortality_new$age),]
mortality_new = mortality_new[which(mortality_new$sex=="T"),]
mor1 = mortality_new[grep("DE", mortality_new$geo),]
mor2 = mortality_new[!grepl("DE", mortality_new$geo),]
mor1_all = subset(mor1,mor1$geo=="DE")
mor1 = subset(mor1,mor1$geo!="DE")
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
mortality_new = do.call(rbind,list(mor1new,mor2))

dir = "Longtime_ERA5/earthengine/"
outdir = "DLMN_model_V0530/"
dir.create(outdir)
#####???㼫????????ֵ#########
filedir = "Longtime_ERA5/daily"
yearlist = seq(2010,2022,1)
outdir = "DLMN_model_V0530/"
threshold_calculate(yearlist,filedir,outdir)
#????????????
for(i in seq(2010,2023,1)){
  mortality_process(i,outdir,mortality_new)
  print(paste("Data of year",i,"has been processed"))
  gc()
}



















