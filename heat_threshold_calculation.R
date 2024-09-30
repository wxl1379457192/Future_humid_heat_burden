setwd("D:/ATtest/Europe_version2")
library(lubridate)
library(lutz)
library(dplyr)
library(data.table)

indir = "Hourly_data/"
file_list <- list.files(path = indir, pattern = "Daily_aggregate", full.names = TRUE)

longtimedf = do.call(rbind,lapply(file_list,function(path){
  outname = file.path(indir,paste0("Daily_aggregate_",strsplit(path,"_")[[1]][4]))
  if (file.exists(outname)){
    mdf_agg = fread(outname,stringsAsFactors = F)
    print(paste(outname,"has added"))
  }else{
    print(paste(outname,"did not exist!"))
    mdf_agg = NULL
  }
  return(mdf_agg)
}))

longtimedf[, `:=` (Year = format(ymd_local, '%Y'),
                   Day = format(ymd_local, '%m-%d'))]

threshold_calculation = function(window,data){
  reda = do.call(rbind,lapply(unique(data$Day),function(date){
    win = window/2
    c = data[Day>=format(as.Date(date,"%m-%d")-win,"%m-%d")&Day<=format(as.Date(date,"%m-%d")+win,"%m-%d")]
    c = c%>%group_by(time, NUTS_ID)%>%
      summarise(Humidex_90th = quantile(hum, 0.90),
                Humidex_95th = quantile(hum, 0.95),
                Humidex_99th = quantile(hum, 0.99),
                Temp_90th = quantile(tem, 0.90),
                Temp_95th = quantile(tem, 0.95),
                Temp_99th = quantile(tem, 0.99),
                Humidex_max_90th = quantile(hum_max, 0.90),
                Humidex_max_95th = quantile(hum_max, 0.95),
                Humidex_max_99th = quantile(hum_max, 0.99),
                Humidex_min_90th = quantile(hum_min, 0.90),
                Humidex_min_95th = quantile(hum_min, 0.95),
                Humidex_min_99th = quantile(hum_min, 0.99),
                Temp_max_90th = quantile(tem_max, 0.90),
                Temp_max_95th = quantile(tem_max, 0.95),
                Temp_max_99th = quantile(tem_max, 0.99),
                Temp_min_90th = quantile(tem_min, 0.90),
                Temp_min_95th = quantile(tem_min, 0.95),
                Temp_min_99th = quantile(tem_min, 0.99),
                .groups = 'drop')
    c$window = window
    c$Day = date
    print(paste("Window:",window,"Date:",date,"has been calculated!"))
    return(c)
  }))
  return(reda)
}
# 使用lapply而不是循环
results_list <- lapply(seq(2000, 2023, 1), function(year) {
  outname = file.path("heat_threshold",paste0("heat_threshold_",year,".csv"))
  if (file.exists(outname)) {
    k <- fread(outname)
  }else{
    print(paste(year,"start:"))
    obf <- longtimedf[longtimedf$Year >= as.character(year-50) & longtimedf$Year < as.character(year), ]
    print("Data has been read:")
    k = do.call(rbind,lapply(c(10,14,30),function(x){
      return(threshold_calculation(x,obf))
    }))
    k$year = year
    write.csv(k, outname,row.names = F)
  }
  # 使用data.table来提高速度
  return(k)
  gc()
})


