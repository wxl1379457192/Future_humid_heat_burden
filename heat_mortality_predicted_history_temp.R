rm(list = ls())
gc()
###############Prediction of heat related mortality from 2030-2100##############
library(dlnm)
library(splines)
library(patchwork)
library(zoo)
library(Epi)
library(lubridate)
library(dplyr)
library(reshape2)
library(data.table)
setwd("D:/ATtest/Europe_version2/")
preout = "Heatmor_prediction_history"
if(dir.exists(preout)){
  print(paste(preout,"has existed!"))
}else{
  dir.create(preout)
}
filter_geo <- function(geo) {
  # 对 geo 字段进行排序，确保最长的代码在前面
  geo <- sort(unique(geo), decreasing = T)
  
  # 初始化一个向量来存储最终的结果
  final_geo <- character(0)
  
  # 遍历 geo 字段中的每个元素
  for (g in geo) {
    #print(g)
    # 检查当前元素是否是 final_geo 中任何元素的子字符串
    if (!any(grepl(g, final_geo))) {
      # 如果不是，将其添加到 final_geo 中
      final_geo <- c(final_geo, g)
    }
  }
  
  # 返回筛选后的结果
  return(final_geo)
}

agelist = c("0-15","15-65","65+")
daall = fread("Input_model_data/Input_data_window14.csv")
filter_list<- filter_geo(daall$geo)
pop = fread(file.path("Input_model_data","Aux_pop_data.csv"))
pop = pop[age_group!="TOTAL",]
daall= daall[daall$geo %in% filter_list, ]
daall =daall[age!="TOTAL",]
daall = daall[complete.cases(daall),]
daall = merge(daall,pop,
              by.x = c("Year","geo","age"),
              by.y = c("Year","geo","age_group"))
daall$age = as.factor(daall$age)
daall$gender_group = paste(daall$geo,daall$Year)

heatdeath = list()
da = daall[which(Year==2022),]
for (a in agelist){
  data = da[which(age==a),]
  modeldir = paste0("Model/stratamodel_win14_",a,"_ns_tem95th.rds")
  model = readRDS(modeldir)
  lag = 4
  lagnk = 2
  cb =  crossbasis(data$tem,lag=lag,
                   argvar = list(fun="ns",knots =  quantile(data$tem,c(80)/100,na.rm=T), 
                                 Boundary.knots = range(data$tem,na.rm=T)),
                   arglag= list(knots = logknots(lag, lagnk)),
                   group = data$gender_group)
  red <- crosspred(cb,model,at=data$tem,model.link = "log")
  
  ff =data.frame(cbind(red$predvar,red$allfit)) 
  colnames(ff) <- c("tem", "rr")
  CDD = onebasis(data$CHD_90th_tem,fun="strata",breaks=c(1,2,4))
  CDN = onebasis(data$CHN_95th_tem,fun="strata",breaks=c(1,2,4))
  CDA = onebasis(data$CH_95th_tem,fun="strata",breaks=c(1,2,4))
  
  CDDpre = crosspred(CDD,model,
                     at = seq(0,7),cen=0,model.link = "log")
  CDD_f = data.frame(cbind(as.integer(CDDpre$predvar),CDDpre$allfit)) 
  colnames(CDD_f) <- c("CDD_num", "rr.cdd")
  CDNpre = crosspred(CDN,model,
                     at = seq(0,7),cen=0,model.link = "log")#,model.link = "log"
  CDN_f = data.frame(cbind(as.integer(CDNpre$predvar),CDNpre$allfit)) 
  colnames(CDN_f) <- c("CDN_num", "rr.cdn")
  
  CDApre = crosspred(CDA,model,
                     at = seq(0,7),cen=0,model.link = "log")#,model.link = "log"
  CDA_f = data.frame(cbind(as.integer(CDApre$predvar),CDApre$allfit)) 
  colnames(CDA_f) <- c("CDA_num", "rr.cda")
  preo = merge(data,ff,by = "tem")
  preo = merge(preo,CDD_f,by.x="CHD_95th_tem",by.y = "CDD_num")
  preo = merge(preo,CDN_f,by.x="CHN_95th_tem",by.y = "CDN_num")
  preo = merge(preo,CDA_f,by.x="CH_95th_tem",by.y = "CDA_num")
  preo$morrisk = exp(preo$rr+preo$rr.cdd+preo$rr.cdn+preo$rr.cda)-1
  preo$morrisk[which(preo$morrisk<0)] = 0
  preo$heat_predicted = preo$morrisk*preo$death
  preo = preo[which(Week>22&Week<=36),]
  heatdeath[[a]] = preo%>%group_by(Year,geo,age)%>%summarise(heat_death = sum(heat_predicted),pop= unique(pop))
}

heatmor = rbindlist(heatdeath)
heatmor$country = substring(heatmor$geo,1,2)
heatmor = heatmor%>%group_by(country,Year)%>%
  summarise(pre_heatdeath = sum(heat_death),pop = sum(pop))
heatmor$pre_deathrate = heatmor$pre_heatdeath/heatmor$pop
valdata = fread("Heatmor_prediction_history/validation_data_history.csv")
ab = data.frame(Country = c("Albania","Austria","Belgium","Bulgaria",
                            "Switzerland","Cyprus","Czechia","Germany","Denmark",
                            "Estonia","Greece","Spain","Finland",
                            "France","Croatia","Hungary","Ireland","Iceland",
                            "Italy","Liechtenstein","Lithuania","Luxembourg",
                            "Latvia","Montenegro","Malta","Netherlands",
                            "Norway","Poland","Portugal","Romania","Serbia",
                            "Sweden","Slovenia","Slovakia","United Kingdom",
                            "Bosnia and Herzegovina","North Macedonia","Belarus",
                            "Czech Republic","Moldova","Russian Federation",
                            "Slovak Republic","Ukraine"),
                ID = c("AL","AT","BE","BG","CH","CY","CZ","DE",
                       "DK","EE","EL","ES","FI","FR","HR","HU",
                       "IE","IS","IT","LI","LT","LU","LV","ME",
                       "MT","NL","NO","PL","PT","RO","RS","SE",
                       "SI","SK","UK","BA","MK","BY","CZ","MD",
                       "RU","SK","UA"))
valdata = merge(valdata,ab,by="Country")
#hd1 = heatmor[which(heatmor$Year==2010&heatmor$Year<=2019),]
#hd1 = hd1%>%group_by(country)%>%summarise(pre_heatdeath = mean(pre_heatdeath))
#hd1$pre_deathrate = 0
val = merge(valdata[which(Year==2022),],heatmor,by.x = "ID",by.y ="country")



heatdeath = list()
da = daall[which(Year>=2010&Year<=2019),]
for (a in agelist){
  data = da[which(age==a),]
  modeldir = paste0("Model/stratamodel_win14_",a,"_ns_hum95th.rds")
  model = readRDS(modeldir)
  lag = 4
  lagnk = 2
  cb =  crossbasis(data$tem,lag=lag,
                   argvar = list(fun="ns",knots =  quantile(data$tem,c(80)/100,na.rm=T), 
                                 Boundary.knots = range(data$tem,na.rm=T)),
                   arglag= list(knots = logknots(lag, lagnk)),
                   group = data$gender_group)
  red <- crosspred(cb,model,at=data$tem,model.link = "log")
  
  ff =data.frame(cbind(red$predvar,red$allfit)) 
  colnames(ff) <- c("tem", "rr")
  CDD = onebasis(data$CHD_90th_tem,fun="strata",breaks=c(1,2,4))
  CDN = onebasis(data$CHN_95th_tem,fun="strata",breaks=c(1,2,4))
  CDA = onebasis(data$CH_95th_tem,fun="strata",breaks=c(1,2,4))
  
  CDDpre = crosspred(CDD,model,
                     at = seq(0,7),cen=0,model.link = "log")
  CDD_f = data.frame(cbind(as.integer(CDDpre$predvar),CDDpre$allfit)) 
  colnames(CDD_f) <- c("CDD_num", "rr.cdd")
  CDNpre = crosspred(CDN,model,
                     at = seq(0,7),cen=0,model.link = "log")#,model.link = "log"
  CDN_f = data.frame(cbind(as.integer(CDNpre$predvar),CDNpre$allfit)) 
  colnames(CDN_f) <- c("CDN_num", "rr.cdn")
  
  CDApre = crosspred(CDA,model,
                     at = seq(0,7),cen=0,model.link = "log")#,model.link = "log"
  CDA_f = data.frame(cbind(as.integer(CDApre$predvar),CDApre$allfit)) 
  colnames(CDA_f) <- c("CDA_num", "rr.cda")
  preo = merge(data,ff,by = "tem")
  preo = merge(preo,CDD_f,by.x="CHD_95th_tem",by.y = "CDD_num")
  preo = merge(preo,CDN_f,by.x="CHN_95th_tem",by.y = "CDN_num")
  preo = merge(preo,CDA_f,by.x="CH_95th_tem",by.y = "CDA_num")
  preo$morrisk = exp(preo$rr+preo$rr.cdd+preo$rr.cdn+preo$rr.cda)-1
  preo$morrisk[which(preo$morrisk<0)] = 0
  preo$heat_predicted = preo$morrisk*preo$death
  preo = preo[which(Week>22&Week<=36),]
  heatdeath[[a]] = preo%>%group_by(Year,geo,age)%>%summarise(heat_death = sum(heat_predicted),pop= unique(pop))
}


heatmor = rbindlist(heatdeath)
heatmor$country = substring(heatmor$geo,1,2)
heatmor = heatmor%>%group_by(country,Year)%>%
  summarise(pre_heatdeath = sum(heat_death),pop = sum(pop))
heatmor = heatmor%>%group_by(country)%>%
  summarise(pre_heatdeath = mean(pre_heatdeath),pop = mean(pop))
heatmor$pre_deathrate = heatmor$pre_heatdeath/heatmor$pop
val2 = merge(valdata[which(Year=="2010-2019"),],heatmor,by.x = "ID",by.y ="country")
val = val[,-10]
colnames(val)[9] = "Year"
allval = rbindlist(list(val,val2))

library(ggplot2)
val$Deaths = as.numeric(val$Deaths)

R2<-function(x,y){
  xm<-mean(x)
  ssres<-sum((xm-x)^2)
  ssreg<-sum((y-x)^2)
  return(1-ssreg/ssres)
}
rmse = function(a,b){
  s = (a-b)^2
  rmse = sqrt(mean(s))
  return(rmse)
}
lm = lm(Deaths~pre_heatdeath,data=val)
r2= summary(lm)$r.squared
R = R2(val$Deaths,val$pre_heatdeath)
RM = rmse(val$Deaths,val$pre_heatdeath)
print(paste("Tem",r2,R,RM))


g = ggplot(allval)+geom_point(aes(x=Deaths,y=pre_heatdeath,color = Year))+
  scale_x_sqrt(limits = c(0,20000))+
  scale_y_sqrt(limits = c(0,20000))+
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.x= element_text(color="black",hjust = 0.5,vjust=0,size =unit(9,"pt")),
        axis.text.x = element_text(color="black",size = unit(9,"pt")),
        axis.title.y= element_text(color="black",hjust = 0.5,vjust=0,size =unit(9,"pt")),
        axis.text.y = element_text(color="black",size = unit(9,"pt")),
        legend.position="top"
  ) +
  labs(
    x="Predicted heat-related deaths from references",
    y="Predicted heat-related deaths from our model\n using dry-bulb surface air temperature"
  )+
  scale_color_manual(values = c("grey30","#a1d7ed"),
                     labels = c("Annual average heat-related deaths from 2010 to 2019", "Heat-related deaths in 2022"),
                     name = "")
outdir = "Figure_0721"
ggsave(paste0(outdir,"/fig_heat_related_mortality_validation_temp.jpg"),g, width=12, height=12, units="cm", scale=1.3)

R2(allval$Deaths,allval$pre_heatdeath)
