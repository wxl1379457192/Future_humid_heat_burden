setwd("D:/ATtest/Europe_version2")
library(lubridate)
start_date <- as.Date("2024-05-01")
end_date <- as.Date("2024-11-01")
dates <- seq.Date(start_date, end_date, by="day")
formatted_dates <- format(dates, "%m-%d")

alldate = do.call(rbind,lapply(formatted_dates,function(d){
  filelist = list.files("Suncal_time/", pattern=paste0("*",d,".csv"))
  sa = do.call(rbind,lapply(filelist,function(f){
    suntime = read.csv(file.path("Suncal_time",f),stringsAsFactors = F)
    return(suntime)
  }))
  sa = do.call(rbind,lapply(split(sa,sa$NUTS_ID),function(ID){
    ID$sunrise=paste(ID$target_date,ID$sunrise)
    ID$sunset=paste(ID$target_date,ID$sunset)
    ID$solarNoon=paste(ID$target_date,ID$solarNoon)
    k = data.frame(NUTS_ID= unique(ID$NUTS_ID),lon = unique(ID$lon),lat = unique(ID$lat),
                   sunrise = mean(as.numeric(substring(ceiling_date(ymd_hms(ID$sunrise), "hour"),12,13))),
                   sunset =mean(as.numeric(substring(ceiling_date(ymd_hms(ID$sunset), "hour"),12,13))),
                   solarNoon = mean(as.numeric(substring(ceiling_date(ymd_hms(ID$solarNoon), "hour"),12,13))),
                   Date = unique(substring(ID$target_date,6,10))
    )
    
    return(k)
  }))
  print(d)
  sa$num = length(filelist)
  return(sa)
}))
alldate$sunrise = round(alldate$sunrise)
alldate$sunset = round(alldate$sunset)
alldate$solarNoon = round(alldate$solarNoon)
write.csv(alldate,"suncal_times.csv",row.names = F)


k1 = split(alldate,alldate$Date)[[1]]
