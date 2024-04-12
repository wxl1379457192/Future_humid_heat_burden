
###############Prediction of heat related mortality from 2030-2100##############
library(dlnm)
library(splines)
library(patchwork)
library(zoo)
library(Epi)
library(lubridate)
library(dplyr)
library(reshape2)
setwd("D:/ATtest/Europe/")
preout = "Mortality_prediction_2022_V1223"
if(dir.exists(preout)){
  print(paste(preout,"has existed!"))
}else{
  dir.create(preout)
}
modeldir = "DLMN_model_V0530/pooled_model_age_group_dataV2_ns"

da = read.csv(file.path("DLMN_model_V0530/pooled_model_age_group_dataV2","Inputdata_withinage.csv"),stringsAsFactors  = F)
da = subset(da,da$year>=2015&da$year<=2019)
da$week_num = as.integer(substring(da$week,7,8))
da = subset(da,da$week_num>=22&da$week_num<36)
geolist = unique(da$geom)
lag = 4
lagnk = 2
basepop = read.csv("Auxdata/2015-2019_age_group_pop.csv",stringsAsFactors  = F)
basepop = basepop[which(basepop$TIME_PERIOD=="2022"|basepop$TIME_PERIOD=="2021"|basepop$TIME_PERIOD=="2019")
                  ,c("age","geo","OBS_VALUE","TIME_PERIOD")]
basepop = basepop[which(basepop$geo%in%geolist),]
basepop$age[which(basepop$age=="Y_LT15")] = "0-15"
basepop$age[which(basepop$age=="Y15-64")] = "15-65"
basepop$age[which(basepop$age=="Y_GE65")] = "65+"
basepop = do.call(rbind,lapply(split(basepop,basepop$geo),function(bg){
  bg = do.call(rbind,lapply(split(bg,bg$age),function(k){
    if(nrow(k)>2){
      k= k[which(k$TIME_PERIOD=="2022"),]
    }
    if(nrow(k)>1&nrow(k)<=2){
      k= k[which(k$TIME_PERIOD=="2021"),]
    }
    return(k)
  }))
  return(bg)
}))
modeldir = "DLMN_model_V0530/pooled_model_age_group_dataV2_ns"
old_model = readRDS(file.path(modeldir,"stratamodel_65+_temp.rds"))
adult_model = readRDS(file.path(modeldir,"stratamodel_16-65_temp.rds"))
young_model = readRDS(file.path(modeldir,"stratamodel_0-15_temp.rds"))

preda = read.csv(file.path("2022_heat_weekly_V1223",
                           "Weekly_data_2022.csv"),stringsAsFactors = F)
preda = preda[,-c(2,5,6,7)]
colnames(preda) = c("geom","weeknum","Temp_mean","DD_num_tem","DN_num_tem")
preda$year = as.integer(substring(preda$weeknum,1,4))
preda$weeknum = as.integer(substring(preda$weeknum,7,8))
preda$gender_group = interaction(preda$geom,preda$year)
colnames(basepop)[3] = "pop"
preda = merge(preda,basepop[,-c(4)],by.x="geom",by.y="geo")


pre= preda[which(preda$geom%in%geolist),]
pre$gender_group = interaction(pre$geom,pre$year)

data = subset(pre,pre$age=="65+")
newdata = data[,c("geom","year","Temp_mean",
                  "DD_num_tem","DN_num_tem",
                  "gender_group","weeknum","pop")]
colnames(newdata)[7] = "week_num" 
cb = crossbasis(newdata$Temp_mean,
                lag=lag,
                argvar =list(fun="ns",knots =  quantile(newdata$Temp_mean,c(80)/100,na.rm=T), 
                             Boundary.knots = range(newdata$Temp_mean,na.rm=T)),
                arglag= list(knots = logknots(lag, lagnk)),
                group =  newdata$gender_group)
DD = onebasis(newdata$DD_num_tem,fun="strata",breaks=c(1,4))
DN = onebasis(newdata$DN_num_tem,fun="strata",breaks=c(1,4))
data$predicted <- round(predict(old_model,type="response",newdata = newdata))
data = data[complete.cases(data),]

data2 = subset(pre,pre$age=="15-65")
newdata = data2[,c("geom","year","Temp_mean",
                   "DD_num_tem","DN_num_tem",
                   "gender_group","weeknum","pop")]
colnames(newdata)[7] = "week_num" 
cb2 = crossbasis(newdata$Temp_mean,
                 lag=lag,
                 argvar =list(fun="ns",knots =  quantile(newdata$Temp_mean,c(80)/100,na.rm=T), 
                              Boundary.knots = range(newdata$Temp_mean,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group =  newdata$gender_group)
DD2 = onebasis(newdata$DD_num_tem,fun="strata",breaks=c(1,4))
DN2 = onebasis(newdata$DN_num_tem,fun="strata",breaks=c(1,4))
data2$predicted <- round(predict(adult_model,type="response",newdata = newdata))
data2 = data2[complete.cases(data2),]

data3 = subset(pre,pre$age=="0-15")
newdata = data3[,c("geom","year","Temp_mean",
                   "DD_num_tem","DN_num_tem",
                   "gender_group","weeknum","pop")]
colnames(newdata)[7] = "week_num" 
cb3 = crossbasis(newdata$Temp_mean,
                 lag=lag,
                 argvar =list(fun="ns",knots =  quantile(newdata$Temp_mean,c(80)/100,na.rm=T), 
                              Boundary.knots = range(newdata$Temp_mean,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group =  newdata$gender_group)
DD3 = onebasis(newdata$DD_num_tem,fun="strata",breaks=c(1,4))
DN3 = onebasis(newdata$DN_num_tem,fun="strata",breaks=c(1,4))
data3$predicted <- round(predict(young_model,type="response",newdata = newdata))
data3 = data3[complete.cases(data3),]

pre = do.call(rbind,list(data,data2,data3))

write.csv(pre,file.path(preout,"Predicted_mortality_2022_weekly_temp.csv"),row.names = F)


#mortality = read.csv("DLMN_model_V0530/weekly_mortality_totalage_2022.csv",stringsAsFactors = F)
#mortality = subset(mortality,mortality$Week>=22&mortality$Week<36)
#mortality = subset(mortality,mortality$age == "TOTAL")
#mortality$death = as.numeric(mortality$death)
#mortality = mortality[,c(-4,-5)]
#df_summarized <-  mortality%>% 
#  group_by(geo,Year) %>% 
#  summarise(total_value = sum(death))
#df_summarized$length = nchar(df_summarized$geo)
#df_summarized= subset(df_summarized,df_summarized$length==5)
#write.csv(df_summarized,"validation/mortality_2022.csv",row.names = F）
pre_hum = read.csv(file.path(preout,"Predicted_mortality_2022_weekly.csv"),stringsAsFactors = F)
val = read.csv("validation/mortality_2022.csv",stringsAsFactors = F)
prehum = pre_hum%>%group_by(geom,year) %>% summarise(total_pre_hum = sum(predicted))
pretem = pre%>%group_by(geom,year) %>% summarise(total_pre_temp = sum(predicted))
valm=merge(val,pretem,by.x=c("geo","Year"),by.y=c("geom","year"))
valm=merge(valm,prehum,by.x=c("geo","Year"),by.y=c("geom","year"))
R2<-function(x,y){
  xm<-mean(x)
  ssres<-sum((x-xm)^2)
  ssreg<-sum((y-x)^2)
  return(ssreg/(ssres+ssreg))
}
rmse = function(a,b){
  s = (a-b)^2
  rmse = sqrt(sum(s)/length(s))
  return(rmse)
}
lm = lm(total_value~total_pre_hum,data=valm)
r2= summary(lm)$r.squared
R = R2(valm$total_value,valm$total_pre_hum)
RM = rmse(valm$total_value,valm$total_pre_hum)
print(paste("Hum",r2,R,RM))

lm = lm(total_value~total_pre_temp,data=valm)
r2= summary(lm)$r.squared
R = R2(valm$total_value,valm$total_pre_temp)
RM = rmse(valm$total_value,valm$total_pre_temp)
print(paste("Temp",r2,R,RM))


