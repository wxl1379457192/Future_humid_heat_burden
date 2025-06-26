rm(list = ls())
gc()
setwd("D:/ATtest/Europe_version3")
library(lubridate)
library(data.table)
library(tidyr)
library(dlnm)
dataagg = function(wd){
  wd$week = format(wd$ymd_local, format = "%Y-W%V")
  wd$weeknum = as.numeric(substring(wd$week,7,8))
  wd = wd[which(weeknum>=22&weeknum<=35),]
  wd$year = substring(wd$week,1,4)
  new <- wd[, .(
    tem = mean(tem), Hum = mean(hum)
  ), by = .(NUTS_ID, year)]
  
  new2 = wd
  new2$NUTS_ID = substring(new2$NUTS_ID,1,4)
  new2 = new2[, .(
    tem = mean(tem), Hum = mean(hum)
  ), by = .(NUTS_ID,year)]
  
  new3 = wd
  new3$NUTS_ID = substring(new3$NUTS_ID,1,3)
  new3 = new3[, .(
    tem = mean(tem), Hum = mean(hum)
  ), by = .(NUTS_ID,year)]
  new = rbind(new,new2,new3)
  return(new)
}
dataprocess = function(Year,weather_dir,mortalitydir){
  weatherdir  = file.path(weather_dir,paste0("Daily_aggregate_",Year,".csv"))
  # threname = file.path(thredir,paste0("heat_threshold_",Year,".csv"))
  morname = file.path(mortalitydir,paste0("weekly_mortality_totalage_",Year,".csv"))
  wd =  fread(weatherdir) 
  wd = wd[which(wd$tem!=0),]
  f = dataagg(wd)
  f$year = as.integer(f$year)
  mor =  fread(morname)
  mor = mor[which(mor$age=="TOTAL"),]
  mor = mor[which(mor$Week>=22&mor$Week<=35),]
  mor = mor[, .(
    death = sum(death)
  ), by = .(geo,Year)]
  mwd = merge(mor,f,by.x=c("geo","Year"),by.y=c("NUTS_ID","year"), all.x = TRUE, allow.cartesian = TRUE)
  mwd = mwd[complete.cases(mwd),]
  
  return(mwd)
  rm(mwd)
  rm(f)
  gc()
}


weather_dir = "D:/ATtest/Europe_version2/Daily_data"
mortalitydir = "D:/ATtest/Europe_version2/Mortality_data"
gdp_dir = "Input_model_data/estat_nama_10r_3gdp_en.csv"

alldata = rbindlist(lapply(seq(2000,2022),function(y){
  print(paste(y,"start:"))
  d = dataprocess(y,weather_dir,mortalitydir)
  return(d)
  print(paste(y,"End!"))
}))
outdir = "GPD_relationship"
if (!file.exists(outdir)){
  dir.create(outdir)
}
gdpfile = fread(gdp_dir) 
gdpfile = gdpfile[which(gdpfile$unit=="EUR_HAB"),]
gdpfile = gdpfile[,c("geo","TIME_PERIOD","OBS_VALUE")]
all = merge(alldata,gdpfile,by.x = c("geo","Year"),by.y=c("geo","TIME_PERIOD"))
outname = file.path(outdir,paste0("Yearly_data.csv"))
fwrite(all, outname)



all = fread(file.path(outdir,paste0("Yearly_data.csv")))
all  = all[which(all$Year>=2010&all$Year<=2019),]
filter_geo <- function(geo) {
  geo <- sort(unique(geo), decreasing = T)
  final_geo <- character(0)
  
  for (g in geo) {
    if (!any(grepl(g, final_geo))) {
      final_geo <- c(final_geo, g)
    }
  }
  return(final_geo)
}

daall = fread("Input_model_data/Input_data_all_v2.csv")
filter_list<- filter_geo(daall$geo)
all= all[all$geo %in% filter_list, ]

fit_temperature_mortality <- function(data) {
  model <- glm(death ~ Hum+ geo, family = quasipoisson(link = "log"), data = data, na.action="na.exclude")
  linear_pred <- predict(model, newdata = data, type = "link")  # This gives the log of expected mortality
  predicted_mortality <- exp(linear_pred)
  baseline_pred <- exp(predict(model, newdata = data.frame(Hum = mean(data$Hum, na.rm = TRUE), geo = data$geo), type = "link"))
  data$RR <- predicted_mortality / baseline_pred
  return(data)
}

all<- fit_temperature_mortality(all)
outname = file.path(outdir,paste0("b_inter_calculation.csv"))
fwrite(all, outname)

all = fread(file.path(outdir,paste0("b_inter_calculation.csv")))
library(ggplot2)
all$OBS_VALUE = all$OBS_VALUE*1.2
all$logGDP <- log(all$OBS_VALUE)
fit_gdp_rr_relationship <- function(data) {
  model_gdp_rr <- lm(RR ~ logGDP, data = data)  # 注意：这里用logGDP而非log(OBS_VALUE)
  coefficients <- coef(model_gdp_rr)
  return(list(intercept = coefficients["(Intercept)"], 
              slope = coefficients["logGDP"]))
}
coefs <- fit_gdp_rr_relationship(all)
b <- coefs$intercept  
k <- coefs$slope      
library(cowplot)

p_left <- ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = b, yend = b + k * 1), 
               color = "#0076a8", linewidth = 0.5)+
  scale_x_continuous(breaks=c(0,1))+
  scale_y_continuous(limits=c(0.5,1.5))+
  labs(y="RR",x="")+
  theme_classic()

p_right <- ggplot(all[all$logGDP > 8, ], aes(x = logGDP, y = RR)) +
  geom_point(color = "#b9e0f9", alpha = 0.3, size = 0.5) +
  geom_segment(aes(x = 7, xend = 12, y = b + k * 7, yend = b + k * 12), 
               color = "#0076a8", linewidth = 0.5)+
  xlim(7, max(all$logGDP)) + 
  scale_y_continuous(limits=c(0.5,1.5))+
  theme_classic() +
  theme(axis.text.y = element_blank(), axis.title.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())  

g = plot_grid(p_left, p_right, nrow = 1, align = "h", rel_widths = c(0.3, 0.7))

ggsave(paste0(figout,"/RR_GDP.jpg"),g, width=10, height=8, units="cm", scale=1)

