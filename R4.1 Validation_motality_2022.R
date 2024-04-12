
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
geolist = unique(da$geo)
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
ageprocess = function(dlnm_model,data,basepop){
  data$gender_group = paste(data$geom,data$year)
  d = data%>%group_by(geom,week_num)%>%summarise(death = mean(death),pop=mean(pop))
  colnames(d) = c("geom","weeknum","death_base","pop_base")
  cb= crossbasis(data$Humidex_mean,lag=lag,
                 argvar = list(fun="ns",knots =  quantile(data$Humidex_mean,c(80)/100,na.rm=T), 
                               Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group = data$gender_group)
  red <- crossreduce(cb,dlnm_model,at=10:40)
  MMT = red$predvar[which.min(red$RRfit)]
  print(MMT)
  preda = read.csv(file.path("2022_heat_weekly_V1223",
                                   "Weekly_data_2022.csv"),stringsAsFactors = F)
  preda = preda[,-c(2,5,6,7)]
  colnames(preda) = c("geom","weeknum","Humidex_mean","DD_num","DN_num")
  preda$year = as.integer(substring(preda$weeknum,1,4))
  preda$weeknum = as.integer(substring(preda$weeknum,7,8))
  
  
  knots = quantile(data$Humidex_mean,c(80)/100,na.rm=T)
  preda$gender_group = interaction(preda$geom,preda$year)
  bpage = split(basepop,basepop$age)[[unique(data$age)]]
  colnames(bpage)[3] = "pop_2022"
  preda = merge(preda,bpage[,-c(1,4)],by.x="geom",by.y="geo")
  
  newdata = preda[,c("geom","year","Humidex_mean","DD_num","DN_num",
                     "gender_group","weeknum","pop_2022")]
  newdata = newdata[complete.cases(newdata),]
  colnames(newdata)[7] = "week_num" 
  colnames(newdata)[8] = "pop"
  DD = onebasis(newdata$DD_num,fun="strata",breaks=c(1,4))
  DN = onebasis(newdata$DN_num,fun="strata",breaks=c(1,4))
  pre = crosspred(cb,dlnm_model,cumul = T,
                  at = newdata$Humidex_mean,cen=MMT,model.link = "log")

  ff =data.frame(cbind(pre$predvar,pre$matfit[,1],pre$matse[,1])) 
  colnames(ff) <- c("Humidex", "rr","se")
  ff$rr.low = ff$rr-1.96*ff$se
  ff$rr.up = ff$rr+1.96*ff$se
  
  DDpre = crosspred(DD,dlnm_model,
                    at = seq(0,7),cen=0,model.link = "log")
  D_f = data.frame(cbind(as.integer(DDpre$predvar),DDpre$allfit,DDpre$allse)) 
  colnames(D_f) <- c("DD_num", "rr.dd","rr.ddse")
  D_f$rr.dd.low = D_f$rr.dd-1.96*D_f$rr.ddse
  D_f$rr.dd.up = D_f$rr.dd+1.96*D_f$rr.ddse
  
  DNpre = crosspred(DN,dlnm_model,
                    at = seq(0,7),cen=0,model.link = "log")#,model.link = "log"
  DN_f = data.frame(cbind(as.integer(DNpre$predvar),DNpre$allfit,DNpre$allse)) 
  colnames(DN_f) <- c("DN_num", "rr.dn","rr.dnse")
  DN_f$rr.dn.low = DN_f$rr.dn-1.96*DN_f$rr.dnse
  DN_f$rr.dn.up = DN_f$rr.dn+1.96*DN_f$rr.dnse
  
  preo = merge(preda,ff,by="Humidex_mean",by.y = "Humidex")
  preo = merge(preo,D_f,by="DD_num")
  preo = merge(preo,DN_f,by="DN_num")
  preo$morrisk = exp(preo$rr+preo$rr.dd+preo$rr.dn)-1
  preo$morrisk[which(preo$morrisk<0)] = 0
  preo$morrisk.low = exp(preo$rr.low+preo$rr.dd.low+preo$rr.dn.low)-1
  preo$morrisk.low[which(preo$morrisk.low<0)] = 0
  preo$morrisk.up = exp(preo$rr.up+preo$rr.dd.up+preo$rr.dn.up)-1
  preo$morrisk.up[which(preo$morrisk.up<0)] = 0
  preo = merge(preo,d,by=c("geom","weeknum"))
  preo$predicted = preo$morrisk*preo$death_base*preo$pop_2022/preo$pop_base
  preo$predicted.low = preo$morrisk.low*preo$death_base*preo$pop_2022/preo$pop_base
  preo$predicted.up = preo$morrisk.up *preo$death_base*preo$pop_2022/preo$pop_base
  pred= preo%>%group_by(geom,year)%>%
    summarize(age = unique(data$age),Humidex_mean = mean(Humidex_mean),
              DD_num = sum(DD_num),DN_num = sum(DN_num),
              pop_2022  = unique(pop_2022),
              death_base = mean(death_base),pop_base = mean(pop_base),
              Predicted = sum(predicted),Predicted.low = sum(predicted.low),
              Predicted.up=sum(predicted.up)
    )
  return(pred)
}


all = do.call(rbind, lapply(split(da, da$age), function(data) {
  #data = split(da, da$age)[[1]]
  if(unique(data$age)=="15-65"){
    model = readRDS(file.path(modeldir, paste0("stratamodel_16-65.rds")))
  }else{
    model = readRDS(file.path(modeldir, paste0("stratamodel_", unique(data$age), ".rds")))
  }
  print(paste(unique(data$age), "start:"))
  k = ageprocess(model, data, basepop)
  print(paste(unique(data$age), "end."))
  return(k)
}))
  
write.csv(all,file.path(preout,"Mortality_prediction_2022.csv"),row.names = F)

