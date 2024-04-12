setwd("D:/ATtest/Europe/")
###############Prediction of heat related mortality from 2030-2100##############
library(dlnm)
library(splines)
library(patchwork)
library(zoo)
library(Epi)
library(lubridate)
library(dplyr)
library(reshape2)
folder_path <- "Future_heat_weekly_V1223"
out<- "Mortality_prediction_2023"
dir.create(out)
pop = read.csv(file.path("pop_structure_projected","proj_pop_age_proportion.csv"),stringsAsFactors = F)
pop = pop[which(pop$TIME_PERIOD==2023),]
uklist = geolist[grep("UK",geolist)]
ukrate = do.call(rbind,lapply(uklist,function(g){
  pc = pop[grep(substring(g,1,3),pop$geo),]
  pc$geo=g
  return(pc)
}))
pop= do.call(rbind,list(pop,ukrate))

modeldir = "DLMN_model_V0530/pooled_model_age_group_dataV2_ns"
old_model = readRDS(file.path(modeldir,"stratamodel_65+.rds"))
adult_model = readRDS(file.path(modeldir,"stratamodel_16-65.rds"))
young_model = readRDS(file.path(modeldir,"stratamodel_0-15.rds"))

da = read.csv(file.path("DLMN_model_V0530/pooled_model_age_group_dataV2","Inputdata_withinage.csv"),stringsAsFactors  = F)
data = subset(da,da$age=="65+")
knots = quantile(data$Humidex_mean,c(80)/100,na.rm=T)
geolist = unique(da$geo)
lag = 4
lagnk = 2

preda = read.csv(file.path(folder_path,"Weekly_data_2023.csv"),stringsAsFactors = F)
preda = merge(preda,pop[,c("geo","age_group","pop")],by="geo")
preda$scenario = paste(preda$rcp,preda$member)
preda = preda[which(preda$geo!="EE00"),]
# predac = preda
# predac$geo = substring(predac$geo,1,3)
# predac = predac%>%group_by(geo,rcp,member,year,week,age_group,scenario)%>%summarise(Humidex_mean =mean(Humidex_mean),
#                                                         DD_num = mean(DD_num),DN_num = mean(DN_num),
#                                                         pop = sum(pop))
# predad = preda
# predad$geo = substring(predad$geo,1,4)
# predad = predad%>%group_by(geo,rcp,member,year,week,age_group,scenario)%>%summarise(Humidex_mean =mean(Humidex_mean),
#                                                                                     DD_num = mean(DD_num),DN_num = mean(DN_num),
#                                                                                     pop = sum(pop))
# preda = preda[,-c(7,8,9)]
# preda = do.call(rbind,list(preda,predac,predad))
nd = data.frame()
for(d in split(preda,preda$scenario)){
  pre= d[which(d$geo%in%geolist),]
  pre$weeknum = as.integer(substring(pre$week,7,8))
  pre$gender_group = interaction(pre$geo,pre$year)
  
  data = subset(pre,pre$age_group=="65+")
  data= data[which(data$geo%in%geolist),]
  newdata = data[,c("geo","year","Humidex_mean",
                   "DD_num","DN_num",
                   "gender_group","weeknum","pop")]
  colnames(newdata)[1] = "geom" 
  colnames(newdata)[7] = "week_num" 
  cb = crossbasis(newdata$Humidex_mean,
                  lag=lag,
                  argvar = list(fun="ns",knots =  quantile(data$Humidex_mean,c(80)/100,na.rm=T), 
                                Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                  arglag= list(knots = logknots(lag, lagnk)),
                  group =  newdata$gender_group)
  DD = onebasis(newdata$DD_num,fun="strata",breaks=c(1,4))
  DN = onebasis(newdata$DN_num,fun="strata",breaks=c(1,4))
  data$predicted <- round(predict(old_model,type="response",newdata = newdata))
  data = data[complete.cases(data),]
  
  data2 = subset(pre,pre$age_group=="16-65")
  newdata = data2[,c("geo","year","Humidex_mean",
                    "DD_num","DN_num",
                    "gender_group","weeknum","pop")]
  colnames(newdata)[1] = "geom" 
  colnames(newdata)[7] = "week_num" 
  cb2 = crossbasis(newdata$Humidex_mean,
                  lag=lag,
                  argvar = list(fun="ns",knots =  quantile(data2$Humidex_mean,c(80)/100,na.rm=T), 
                                Boundary.knots = range(data2$Humidex_mean,na.rm=T)),
                  arglag= list(knots = logknots(lag, lagnk)),
                  group =  newdata$gender_group)
  DD2 = onebasis(newdata$DD_num,fun="strata",breaks=c(1,4))
  DN2 = onebasis(newdata$DN_num,fun="strata",breaks=c(1,4))
  data2$predicted <- round(predict(adult_model,type="response",newdata = newdata))
  data2 = data2[complete.cases(data2),]
  
  data3 = subset(pre,pre$age_group=="<15")
  newdata = data3[,c("geo","year","Humidex_mean",
                     "DD_num","DN_num",
                     "gender_group","weeknum","pop")]
  colnames(newdata)[1] = "geom" 
  colnames(newdata)[7] = "week_num" 
  cb3 = crossbasis(newdata$Humidex_mean,
                  lag=lag,
                  argvar = list(fun="ns",knots =  quantile(data3$Humidex_mean,c(80)/100,na.rm=T), 
                                Boundary.knots = range(data3$Humidex_mean,na.rm=T)),
                  arglag= list(knots = logknots(lag, lagnk)),
                  group =  newdata$gender_group)
  DD3 = onebasis(newdata$DD_num,fun="strata",breaks=c(1,4))
  DN3 = onebasis(newdata$DN_num,fun="strata",breaks=c(1,4))
  data3$predicted <- round(predict(young_model,type="response",newdata = newdata))
  data3 = data3[complete.cases(data3),]
  
  pre = do.call(rbind,list(data,data2,data3))
  print(unique(pre$scenario))
  remove(cb,cb2,cb3)
  nd = do.call(rbind,list(nd,pre))
}

write.csv(nd,file.path(out,"Predicted_mortality_2023_scenario.csv"),row.names = F)

pred= nd%>%
  group_by(year,rcp,geo,week,age_group)%>%
  summarize(premdeath = mean(predicted),predicted_lower = quantile(predicted,0.05),predicted_upper = quantile(predicted,0.95))
write.csv(pred,file.path(out,"Predicted_mortality_2023_weekly.csv"),row.names = F)

