rm(list = ls())
gc()
###########################################
setwd("D:/ATtest/Europe/DLMN_model_V0530")
library(dlnm)
library(splines)
library(ggplot2)
library(reshape2)
library(patchwork)
library(zoo)
library(Epi)
library(data.table)
outdir = "pooled_model_age_group_dataV2"
da = read.csv(paste0(outdir,"/Inputdata_withinage.csv"),stringsAsFactors  = F)
da = subset(da,da$year>=2015&da$year<=2019)
# PARAMETERS FOR THE LAG-RESPONSE FUNCTION

da$week_num = as.integer(substring(da$week,7,8))
da = subset(da,da$week_num>=22&da$week_num<36)
#da$pop = da$pop/max(da$pop)
# Lag-Response: Minimum and Maximum Lags (Weeks)
lag <- 4
lagnk <-2
#######################################
data = subset(da,da$age=="65+")
data$death[which(data$death==0)]=1
data$age = as.factor(data$age)
cb= crossbasis(data$Temp_mean,
               lag=lag,
               argvar = list(fun="ns",knots =  quantile(data$Temp_mean,c(80)/100,na.rm=T), 
                             Boundary.knots = range(data$Temp_mean,na.rm=T)),
               arglag= list(knots = logknots(lag, lagnk)),
               group = data$gender_group)

DD =onebasis(data$DD_num_tem,fun="strata",breaks=c(1,4))
DN = onebasis(data$DN_num_tem,fun="strata",breaks=c(1,4))

model <- glm(death ~ cb + DD + DN + ns(year, df = 4)+ns(week_num, df = 3)+ log(pop) + geom ,
             family = quasipoisson(link="log"), data = data, na.action="na.exclude")

#############################
data2 = subset(da,da$age=="15-65")
data2$death[which(data2$death==0)]=1
data2$age = as.factor(data2$age)
cb2= crossbasis(data2$Temp_mean,
                lag=lag,
                argvar = list(fun="ns",knots =  quantile(data$Temp_mean,c(80)/100,na.rm=T), 
                              Boundary.knots = range(data$Temp_mean,na.rm=T)), #quantile(data2$Humidex_mean,c(50,90)/100,na.rm=T)
                arglag= list(knots = logknots(lag, lagnk)),
                group = data2$gender_group)
DD2 = onebasis(data2$DD_num_tem,fun="strata",breaks=c(1,4))
DN2 = onebasis(data2$DN_num_tem,fun="strata",breaks=c(1,4))

model2 <- glm(death ~ cb2 + DD2 + DN2+ ns(year, df = 4)+ns(week_num, df = 3)+ log(pop) + geom,
              family = quasipoisson(link="log"), data = data2, na.action="na.exclude")

##############model for age 0-15#####################
data3 = subset(da,da$age=="0-15")
data3$death[which(data3$death==0)]=1
data3$age = as.factor(data3$age)

cb3= crossbasis(data3$Temp_mean,
                lag=lag,
                argvar = list(fun="ns",knots =  quantile(data$Temp_mean,c(80)/100,na.rm=T), 
                              Boundary.knots = range(data$Temp_mean,na.rm=T)), #quantile(data2$Humidex_mean,c(50,90)/100,na.rm=T)
                arglag = list(knots = logknots(lag, lagnk)),
                group = data3$gender_group)

DD3 = onebasis(data3$DD_num_tem,fun="strata",breaks=c(1,4))
DN3 = onebasis(data3$DN_num_tem,fun="strata",breaks=c(1,4))

model3 <- glm(death ~ cb3 + DD3 + DN3+ ns(year, df = 4)+ns(week_num, df = 3)+ log(pop)+ geom,
              family = quasipoisson(link="log"), data = data3, na.action="na.exclude")


outdir = "pooled_model_age_group_dataV2_ns"
dir.create(outdir)
saveRDS(model,paste0(outdir,"/stratamodel_65+_temp.rds"))
saveRDS(model2,paste0(outdir,"/stratamodel_16-65_temp.rds"))
saveRDS(model3,paste0(outdir,"/stratamodel_0-15_temp.rds"))


#######################
chindex=  grep("pop",names(coef(model)))
coef_old = coef(model)[chindex]
coef_adult = coef(model2)[chindex]
coef_young = coef(model3)[chindex]
conheat = as.data.frame(do.call(rbind,list(coef_old,coef_adult,coef_young)))
colnames(conheat) = "value"
conheat$age = c("65+","16-65","0-15")
conheat$effect = exp(-conheat$value)
write.csv(conheat,paste0(outdir,"/pop_effect_withinage.csv"),row.names = F)

