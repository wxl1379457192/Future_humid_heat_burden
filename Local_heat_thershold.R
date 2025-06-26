rm(list = ls())
gc()
setwd("D:/ATtest/Europe_version3")
library(dplyr)
library(forecast)
library(tseries)
library(data.table)
library(ggplot2)
library(pbapply)
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
residuals_extract<- function(morc){
  morc <- morc %>%
    group_by(geo) %>%
    arrange(Year, Week) %>%  # Rank by Year and Week 
    ungroup()

  death_ts <- ts(morc$death, frequency = 24)
  arima_model <- auto.arima(death_ts, seasonal =T,max.p = 10, max.q = 10, max.P = 2, max.Q = 2)  
  Box.test(arima_model$residuals, type = "Ljung-Box")
  residuals <- as.numeric(residuals(arima_model))
  detrended_data <- data.frame(
    Region = unique(morc$geo),
    Week = morc$Week,
    Year = morc$Year,
    Humidex = morc$Hum,
    Temp = morc$tem,
    Humidex_max = morc$Hum_max,
    Temp_max = morc$tem_max,
    Humidex_min = morc$Hum_min,
    Temp_min = morc$tem_min,
    death_detrended = residuals
  )
  return(detrended_data)
}
outdir = "heat_threshold"
morname = file.path(outdir,paste0("Weekly_data_for_threshold_V2.csv"))

mordata =  fread(morname)
filter_list<- filter_geo(mordata$geo)
mordata= mordata[mordata$geo %in% filter_list, ]
mordata = mordata[which(mordata$age=="TOTAL"),]
mordata = mordata[which(mordata$Year>=2010&mordata$Year<2020),]
detrended_data <- do.call(rbind, pblapply(split(mordata, mordata$geo), function(x) {
  f = residuals_extract(x)
  return(f)
}))
library(stringr)
fwrite(detrended_data, file.path(outdir,paste0("NUTS_level3_residuals.csv")))

threshold_extract_hum <- function(dayd,v){
  numbers <- str_extract_all(dayd$Humidex_Group, "-?\\d+")  
  dayd$lower = sapply(numbers, function(x) as.numeric(x[1]))
  dayd$upper = sapply(numbers, function(x) as.numeric(x[2]))
  dayd$value = (dayd$lower+dayd$upper)/ 2
  
  group_means_day <- dayd %>%
    group_by(Humidex_Group,value) %>%
    summarise(
      Mean_Death_Detrended = mean(death_detrended, na.rm = TRUE),  
      Sample_Size = n() 
    )
  overall_mean_day <- mean(dayd$death_detrended, na.rm = TRUE)
  group_means_day <- group_means_day %>%
    mutate(Overall_Mean =overall_mean_day)
  
  # 进行 t 检验
  t_test_day <- dayd %>%
    group_by(Humidex_Group) %>%
    summarise(
      t_test_p_value = tryCatch({
        if (n() >= 2) {  # 确保样本量大于等于 2
          t.test(death_detrended, mu = overall_mean_day)$p.value
        } else {
          NA 
        }
      }, error = function(e) NA) 
    )
  final_results_day <- group_means_day %>%
    left_join(t_test_day, by = "Humidex_Group")
  the = final_results_day$value[which(final_results_day$Mean_Death_Detrended>overall_mean_day&
                                        final_results_day$t_test_p_value<0.05&
                                        final_results_day$value>0&
                                        final_results_day$value>mean(v))][1]
  if (is.na(the)){
    the = final_results_day$value[which(final_results_day$Mean_Death_Detrended>overall_mean_day&
                                          final_results_day$t_test_p_value<0.1&
                                          final_results_day$value>0&
                                          final_results_day$value>mean(v))][1]
  }
  if (is.na(the)){
    the=round(quantile(v, probs = 0.98),0)
    red = data.frame(region = unique(dayd$geo),thre = the, percentile = 98,
                     p_value = NA)
  }else{
    perc = ecdf(v)(the)*100
    red = data.frame(region = unique(dayd$geo),thre = the, percentile = perc,
                     p_value = final_results_day$t_test_p_value[which(final_results_day$value==the)])
  }
  return(red)
}
threshold_extract_tem <- function(dayd,v){
  dayd$value = (as.numeric(substring(dayd$Tem_Group,2,3)) + 
                  as.numeric(substring(dayd$Tem_Group,5,6))) / 2
  
  group_means_day <- dayd %>%
    group_by(Tem_Group,value) %>%
    summarise(
      Mean_Death_Detrended = mean(death_detrended, na.rm = TRUE),  
      Sample_Size = n() 
    )
  overall_mean_day <- mean(dayd$death_detrended, na.rm = TRUE)
  group_means_day <- group_means_day %>%
    mutate(Overall_Mean =overall_mean_day)
  
  # 进行 t 检验
  t_test_day <- dayd %>%
    group_by(Tem_Group) %>%
    summarise(
      t_test_p_value = tryCatch({
        if (n() >= 2) {  # 确保样本量大于等于 2
          t.test(death_detrended, mu = overall_mean_day)$p.value
        } else {
          NA 
        }
      }, error = function(e) NA) 
    )
  final_results_day <- group_means_day %>%
    left_join(t_test_day, by = "Tem_Group")
  the = final_results_day$value[which(final_results_day$Mean_Death_Detrended>overall_mean_day&
                                        final_results_day$t_test_p_value<0.05&
                                        final_results_day$value>0&
                                        final_results_day$value>mean(v))][1]
  if (is.na(the)){
    the = final_results_day$value[which(final_results_day$Mean_Death_Detrended>overall_mean_day&
                                          final_results_day$t_test_p_value<0.1&
                                          final_results_day$value>0&
                                          final_results_day$value>mean(v))][1]
  }
  if (is.na(the)){
    the=round(quantile(v, probs = 0.98),0)
    red = data.frame(region = unique(dayd$geo),thre = the, percentile = 98,
                     p_value = NA)
  }else{
    perc = ecdf(v)(the)*100
    red = data.frame(region = unique(dayd$geo),thre = the, percentile = perc,
                     p_value = final_results_day$t_test_p_value[which(final_results_day$value==the)])
  }
  return(red)
}
detrended_data$geo = substring(detrended_data$Region,1,3)
threshold_geo_hum = do.call(rbind, pblapply(split(detrended_data, detrended_data$geo), function(data) {
  print(unique(data$geo))
  dayd <-data %>%
    mutate(Humidex_Group = cut(Humidex_max, 
                               breaks = seq(floor(min(Humidex_max, na.rm = TRUE)), 
                                            floor(max(Humidex_max, na.rm = TRUE)) + 2, 
                                            by = 2),
                               include.lowest = TRUE))
  tday = threshold_extract_hum(dayd,dayd$Humidex_max)
  tday$Time = "Day"
  dayn <-data %>%
    mutate(Humidex_Group = cut(Humidex_min, 
                               breaks = seq(floor(min(Humidex_min, na.rm = TRUE)), 
                                            floor(max(Humidex_min, na.rm = TRUE)) + 2, 
                                            by = 2),
                               include.lowest = TRUE))
  tnight = threshold_extract_hum(dayn,dayn$Humidex_min)
  tnight$Time = "Night"
  tall = do.call(rbind,list(tday,tnight))
  return(tall)
}))

threshold_geo_tem = do.call(rbind, pblapply(split(detrended_data, detrended_data$geo), function(data) {
  #print(unique(data$geo))
  dayd <-data %>%
    mutate(Tem_Group = cut(Temp_max, 
                               breaks = seq(floor(min(Temp_max, na.rm = TRUE)), 
                                            floor(max(Temp_max, na.rm = TRUE)) + 2, 
                                            by = 2),
                               include.lowest = TRUE))
  tday = threshold_extract_tem(dayd,dayd$Temp_max)
  tday$Time = "Day"
  dayn <-data %>%
    mutate(Tem_Group = cut(Temp_min, 
                               breaks = seq(floor(min(Temp_min, na.rm = TRUE)), 
                                            floor(max(Temp_min, na.rm = TRUE)) + 2, 
                                            by = 2),
                               include.lowest = TRUE))
  tnight = threshold_extract_tem(dayn,dayn$Temp_min)
  tnight$Time = "Night"
  tall = do.call(rbind,list(tday,tnight))
  return(tall)
}))
threshold_geo_hum$variable = "Humidex"
threshold_geo_tem$variable = "Temp"
all_the = do.call(rbind,list(threshold_geo_hum,threshold_geo_tem))
outname = file.path(outdir,paste0("Heat_threshold_countrylevel_v2.csv"))
fwrite(all_the, outname)


k = split(detrended_data, detrended_data$geo)[[1]]
k <-k %>%
  mutate(Humidex_Group = cut(Humidex_max, 
                         breaks = seq(floor(min(Humidex_max, na.rm = TRUE)), 
                                      floor(max(Humidex_max, na.rm = TRUE)) + 2, 
                                      by = 2),
                         include.lowest = TRUE))
overall_mean <- mean(k$death_detrended, na.rm = TRUE)
group_means_day <- k %>%
  group_by(Humidex_Group) %>%
  summarise(
    Mean_Death_Detrended = mean(death_detrended, na.rm = TRUE),  
    Sample_Size = n() 
  )
t_test_day <- k %>%
  group_by(Humidex_Group) %>%
  summarise(
    t_test_p_value = tryCatch({
      if (n() >= 2) {  # 确保样本量大于等于 2
        t.test(death_detrended, mu = overall_mean)$p.value
      } else {
        NA 
      }
    }, error = function(e) NA) 
  )
group_means_day  <- group_means_day %>%
  left_join(t_test_day, by = "Humidex_Group")

g = ggplot() +
  geom_boxplot(data = k,aes(x = as.factor(Humidex_Group), y = death_detrended),
               fill = "#F0F8FF", color = "#5D8AA8",
               outlier.shape = NA,width=0.65, size = 0.5) +
  geom_point(data =group_means_day,aes(x = as.factor(Humidex_Group), y = Mean_Death_Detrended),
             shape =8,color="#E34234",size=1)+
  geom_hline(yintercept = overall_mean, color = "black", linetype = "dashed", size = 0.5) +
  scale_y_continuous(limits = c(-20, 40))+
  labs(
    x = "Humidex Group (Every 2°C)",
    y = "Detrended Deaths"
  ) +
  theme_test() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(data = group_means_day, 
            aes(x = as.factor(Humidex_Group), 
                y = 35, 
                label = paste("n =", Sample_Size)),
            color = "black", 
            size = 3, 
            vjust = 0, 
            inherit.aes = FALSE) +
  geom_label(data = group_means_day, 
             aes(x = as.factor(Humidex_Group), 
                 y = 35, 
                 label = paste("n =", Sample_Size)),
             fill = "grey90", 
             color = "black", 
             size = 3, 
             vjust = 0, 
             inherit.aes = FALSE) +
  geom_text(data = group_means_day, 
            aes(x = as.factor(Humidex_Group), 
                y = 30, 
                label = ifelse(t_test_p_value < 0.05, 
                               paste("p =", round(t_test_p_value, 3)), 
                               "")),
            color = "black", 
            size = 3, 
            vjust = 0, 
            inherit.aes = FALSE)
figout = "Figures"
if (!file.exists(figout)){
  dir.create(figout)
}

ggsave(paste0(figout,"/heat_threshold_example.jpg"),g, width=14, height=6, units="cm", scale=1.5)


