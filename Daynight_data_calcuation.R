library(data.table)
library(lubridate)
library(lutz)
library(parallel)

# 设置工作目录
setwd("D:/ATtest/Europe_version2")

# Humidex 函数定义
Humidex <- function(temp, dew) {
  k1 <- 6.11 * exp(5417.7530 * ((1 / 273.16) - (1 / (dew + 273.15))))
  Hum <- temp + 0.5555 * (k1 - 10)
  return(Hum)
}

# 文件路径和读取
indir <- "Hourly_data_pop/"
file_list <- list.files(path = indir, pattern = "ERA5_time_series_temperature_2m", full.names = TRUE)
suntime <- fread("suncal_times.csv")

# 并行设置
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)

# 在每个工作节点上加载库
clusterEvalQ(cl, {
  library(data.table)
  library(lubridate)
  library(lutz)
})

# 将必要的变量传递给工作节点
clusterExport(cl, varlist = c("Humidex", "suntime", "indir", "gsub", "fread", "fwrite"))

# 数据处理函数
process_data <- function(tair_path) {
  dew_path <- gsub("temperature_2m", "dewpoint_temperature_2m", tair_path)
  outname <- file.path(indir, paste0("Daily_aggregate_", strsplit(tair_path, "_")[[1]][8]))
  Sys.sleep(runif(1, 0.5, 2))  # 随机延迟0.5到2秒
  if (file.exists(outname)) {
    mdf_agg <- fread(outname)
  } else {
    if (file.exists(tair_path) & file.exists(dew_path)) {
      log_file <- file.path(indir, "process_log.txt")
      cat(paste("Start:", Sys.time()), file = log_file, append = TRUE)
      tf <- fread(tair_path)
      setnames(tf, old = "sum", new = "Temp")
      df <- fread(dew_path)
      setnames(df, old = "sum", new = "Dew")
      mdf <- merge(df, tf, by = c("NUTS_ID", "imageId"))
      mdf[, `:=`(Year = as.numeric(substr(mdf$imageId, 1, 4)),
                 Month = as.numeric(substr(mdf$imageId, 5, 6)),
                 Day = as.numeric(substr(mdf$imageId, 7, 8)),
                 Hour = as.numeric(substr(mdf$imageId, 10, 11)))]
      mdf[,`:=`(Date = as.IDate(paste(Year, Month, Day, sep = "-")))]
      mdf <- mdf[Date >= as.IDate(paste(unique(mdf$Year), "05", "01", sep = "-"))]
      mdf_agg <- rbindlist(lapply(split(mdf, mdf$NUTS_ID), function(id) {
        id[, month_day := format(Date, "%m-%d")]
        idsun <- suntime[suntime$NUTS_ID == unique(id$NUTS_ID)]
        id <- merge(id, idsun[, .(Date, sunrise, sunset)], by.x = "month_day", by.y = "Date")
        id$lat = as.numeric(id$lat)
        id$lon = as.numeric(id$lon)
        if(!is.numeric(id$lat) || !is.numeric(id$lon)) {
          stop("lat and lon must be numeric vectors")
        }
        if(length(id$lat) != length(id$lon)) {
          stop("lat and lon must have the same length")
        }
        id[, daytime := as.integer((Hour >= sunrise) & (Hour < sunset))]
        id[, datetime := ymd_hms(paste(imageId, ":00:00"), tz = "UTC")]
        id[, tz := tz_lookup_coords(id$lat, id$lon, method = "accurate", warn = TRUE)]
        id[, Date_local := with_tz(id$datetime, unique(id$tz))]
        id[, ymd_local := substr(id$Date_local, 1, 10)]
        id[, Hum := Humidex(id$Temp, id$Dew)]
        aggdf <- id[, .(tem = mean(Temp), dew = mean(Dew), hum = mean(Hum),
                        tem_max = max(Temp), dew_max = max(Dew), hum_max = max(Hum),
                        tem_min = min(Temp), dew_min = min(Dew), hum_min = min(Hum)),
                    by = .(NUTS_ID, daytime, ymd_local)]
        aggdf[, time := ifelse(daytime == 1, "Day", "Night")]
        return(aggdf)
      }))
      fwrite(mdf_agg, outname)
      cat(paste("End:", Sys.time()), file = log_file, append = TRUE)
    } else {
      mdf_agg <- NULL
      print(paste(tair_path, "or", dew_path, "did not exist!"))
    }
  }
  return(mdf_agg)
}
results <- parLapply(cl, file_list, process_data)

stopCluster(cl)
final_result <- rbindlist(results)