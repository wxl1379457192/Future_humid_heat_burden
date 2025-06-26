rm(list = ls())
gc()
setwd("D:/ATtest/Europe_version3")

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
library(tidyr)

filename = file.path( "GPD_relationship",paste0("b_inter_calculation.csv"))
all = fread(filename)
all$OBS_VALUE = all$OBS_VALUE*1.2
fit_gdp_rr_relationship <- function(data) {
  model_gdp_rr <- lm(RR ~ log(OBS_VALUE), data = data)
  b <- coef(model_gdp_rr)["(Intercept)"]
  return(b)
}
b <- fit_gdp_rr_relationship(all)

print(paste("Estimated b:", b))
rm(filename,all,fit_gdp_rr_relationship)

preout = "Heatmor_prediction_future_withadaptation"
if(dir.exists(preout)){
  print(paste(preout,"has existed!"))
}else{
  dir.create(preout)
}
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
data = fread("Input_model_data/Input_data_all_v2.csv")

d = data%>%group_by(geo,Week,age)%>%summarise(death = mean(death))
colnames(d) = c("geo","weeknum","age","death_base")
d = d[which(d$age!="TOTAL"),]

filter_list<- filter_geo(data$geo)
agelist = c("0-15","15-65","65+")
poprate = fread(file.path("D:/ATtest/Europe_version2/pop_structure_projected","proj_pop_age_proportion.csv"),stringsAsFactors = F)
poprate$age_group[which(poprate$age_group=="<15")]="0-15"
poprate$age_group[which(poprate$age_group=="16-65")]="15-65"  
uklist = filter_list[grep("UK",filter_list)]
ukrate = do.call(rbind,lapply(uklist,function(g){
  pc = poprate[grep(substring(g,1,3),poprate$geo),]
  pc$geo=g
  return(pc)
}))
poprate = do.call(rbind,list(poprate,ukrate))
filelist = list("Future_pop_SSP1.csv","Future_pop_SSP2.csv","Future_pop_SSP3.csv","Future_pop_SSP5.csv")
popnum = do.call(rbind,lapply(filelist,function(f){
  fread(file.path("D:/ATtest/Europe_version2/pop_structure_projected",f),stringsAsFactors = F)
}))


popnum = do.call(rbind,lapply(split(popnum,popnum$year),function(y){
  py = do.call(rbind,lapply(seq(unique(y$year),unique(y$year)+9,1),function(k){
    py = y[,c("total_population","NUTS_ID","scenario")]
    py = dcast(py , NUTS_ID ~ scenario, value.var = "total_population")
    py$year =k
    return(py)
  }))
  return(py)
}))
popnum = do.call(rbind,lapply(filter_list,function(g){
  pc = popnum[grep(g, popnum$NUTS_ID),]
  if(length(unique(pc$NUTS_ID))>1){
    pc$NUTS_ID = g
    pc = pc%>%group_by(NUTS_ID,year)%>%summarise(ssp1=sum(ssp1),ssp2=sum(ssp2),
                                                 ssp3=sum(ssp3),ssp5=sum(ssp5))
  }else{
    pc$NUTS_ID = g
  }
  pc <- pc %>%
    pivot_longer(cols = c(ssp1, ssp2, ssp3, ssp5),  # 要转换为长格式的列
                 names_to = "scenario",  # 新的列名
                 values_to = "pop_ssp")  
  return(pc)
}))

indir = "Future_heat_weekly"
filename = list.files(indir)
file_list =  filename[grep("Future", filename)]
ym =  readRDS(paste0("Model/stratamodel_0-15_ns_hum.rds"))
am =  readRDS(paste0("Model/stratamodel_15-65_ns_hum.rds"))
om =  readRDS(paste0("Model/stratamodel_65+_ns_hum.rds"))
lag = 4
lagnk = 2
da1 = data[which(Year>=2010&Year<=2019),]
cb =  crossbasis(da1$Hum,lag=lag,
                 argvar = list(fun="ns",knots =  quantile(da1$Hum,c(80)/100,na.rm=T), 
                               Boundary.knots = range(da1$Hum,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group =da1$gender_group)
CDD = onebasis(da1$CHD,fun="strata",breaks=c(1,2,4))
CDN = onebasis(da1$CHN,fun="strata",breaks=c(1,2,4))
CDA = onebasis(da1$CH,fun="strata",breaks=c(1,2,4))

gdpdata = fread("Input_model_data/Predicted_GDP.csv")
gdpdata_l2 = gdpdata
gdpdata_l2$NUTS_ID= substring(gdpdata_l2$NUTS_ID,1,4)
gdpdata_l2=gdpdata_l2[, .(
  GDP = sum(GDP)
), by = .(NUTS_ID, Year,scenario)]

gdpdata_l3 = gdpdata
gdpdata_l3$NUTS_ID= substring(gdpdata_l3$NUTS_ID,1,3)
gdpdata_l3=gdpdata_l3[, .(
  GDP = sum(GDP)
), by = .(NUTS_ID, Year,scenario)]
gdpdata = do.call(rbind,list(gdpdata,gdpdata_l2,gdpdata_l3))
gdpdata =  gdpdata[which(gdpdata$GDP>0),]
rm(gdpdata_l2,gdpdata_l3)

basepop = fread("D:/ATtest/Europe_version2/pop_structure_projected/2015-2019_age_group_pop.csv",stringsAsFactors  = F)
basepop = basepop[which(basepop$TIME_PERIOD=="2022"|basepop$TIME_PERIOD=="2021"|basepop$TIME_PERIOD=="2019")
                  ,c("age","geo","OBS_VALUE","TIME_PERIOD")]
basepop = basepop[which(basepop$geo%in%filter_list),]
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
basesum = basepop%>%group_by(geo)%>%summarise(sum_pop=sum(OBS_VALUE))
basepop = merge(basepop,basesum,by="geo")
basepop$base_agerate = basepop$OBS_VALUE/basepop$sum_pop

adaratelist = c(0,0.05,0.1,0.25,0.5,0.8)
all = lapply(file_list[0:length(file_list)],function(f){
  print(f)
  fs = fread(file.path(indir,f))
  filtered_fs<-fs %>%
    filter(NUTS_ID %in% filter_list)
  unique_nuts_id <- unique(filtered_fs$NUTS_ID)
  filtered_fs <- filtered_fs %>%
    mutate(
      IHD = Heatday - CHD,
      IHN = Heatnight - CHN,
      IHA = Heatall - CH,
      year = as.numeric(substring(week, 1, 4))
    )
  filtered_fs$scenario = toupper(substring(filtered_fs$rcp,1,4))
  name = paste0("Projected_future_deaths_",unique(filtered_fs$NUTS_ID),".csv")
  if(file.exists(file.path(preout,name))|nrow(filtered_fs)==0){
    print(paste(name,"has existed!"))
  }else{
    dg <- d %>%
      filter(geo == unique_nuts_id)
    
    gp <- gdpdata %>%
      filter(NUTS_ID == unique_nuts_id)
    gp_base = gp[which(gp$Year=="2020"),]
    colnames(gp_base)[1]="GDP_base"
    
    bp <- basepop %>%
      filter(geo == unique_nuts_id)
    pn <- popnum %>%
      filter(NUTS_ID == unique_nuts_id)
    pn$scenario = toupper(pn$scenario)
    pr <- poprate %>%
      filter(geo == unique_nuts_id)
    if (length(pr$geo)<1){
      pr <- poprate %>%
        filter(geo == substring(unique_nuts_id,1,4))
      if (length(pr$geo)<1){
        pr <- poprate %>%
          filter(geo == substring(unique_nuts_id,1,3))
      }
    }
    if(nrow(pr)>0&nrow(filtered_fs)>0&nrow(bp)>0&nrow(gp)>0){
      allada = lapply(adaratelist,function(adarate){
        ageall = lapply(split(dg,dg$age),function(dga){
          unique_age <- unique(dga$age)
          data_base <- data[data$age == unique_age & data$geo == unique_nuts_id, ]
          if(nrow(data_base)>0){
            pra <- pr[pr$age_group == unique_age, ]
            colnames(pra)[4:5] <- c("projected_pop","BSL")
            filtered_fs$base_pop <- bp$sum_pop[bp$age == unique_age]
            daall <- filtered_fs %>%
              merge(pn, by = c("NUTS_ID", "year","scenario")) %>%
              merge(pra[,-c("geo")], by.x = c("year"), by.y = c("TIME_PERIOD")) %>%
              merge(dga, by.x = c("NUTS_ID", "weeknum"), by.y = c("geo", "weeknum"))
            daall$Year = floor(daall$year/10)*10
            daall <- daall %>%
              merge(gp, by = c("NUTS_ID", "Year","scenario")) %>%
              merge(gp_base[,-"Year"],by = c("NUTS_ID","scenario"))
            daall = daall[,-c("Year","scenario")]
            
            model <- switch(unique_age,
                            "0-15" = ym,
                            "15-65" = am,
                            om)
            
            colnames(daall)[1:3] <- c("geo", "weeknum","Year")
            daall$gender_group <- paste(daall$geo, daall$Year)
            
            red <- crosspred(cb,model,at=daall$Hum,model.link = "log")
            ff <- data.frame(Hum = red$predvar, rr = red$allfit)
            
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
            
            preo <- daall %>%
              merge(ff, by = "Hum") %>%
              merge(CDD_f, by.x = "CHD", by.y = "CDD_num") %>%
              merge(CDN_f, by.x = "CHN", by.y = "CDN_num") %>%
              merge(CDA_f, by.x = "CH", by.y = "CDA_num")
            preo$RR = exp(preo$rr+preo$rr.cdd+preo$rr.cdn+preo$rr.cda)
            preo$RR[which(preo$RR<1)] = 1
            preo$x = log(preo$GDP/preo$projected_pop)/log(preo$GDP_base/preo$projected_pop)
            preo$RR_adaptation = (((1-adarate)*(preo$RR-1)+1)+((preo$RR-b)*preo$x+b))/2 - 1
            preo$RR_adaptation[which(preo$RR_adaptation<0)] = 0
            
            
            preo$projected_pop=preo$projected_pop/preo$BSL
            preo$proj_death <- preo$RR_adaptation*preo$death_base*(preo$projected_pop/preo$base_pop)
            preobase <- preo[preo$Year == 2022, c("weeknum", "RR_adaptation")]
            preobase <- unique(preobase)
            colnames(preobase)[2] <- "RR_adaptation_2022"
            preobase <- preobase[!duplicated(preobase$weeknum), ]
            preo = merge(preo,preobase,by="weeknum")
            preo$base_death <- preo$RR_adaptation_2022*preo$death_base*(preo$projected_pop/preo$base_pop)
            preo = preo[which(weeknum>=22&weeknum<=35),]
            heatdeath = preo%>%group_by(Year,geo,age,rcp,member)%>%
              summarise(pop = unique(projected_pop),
                        Humidex_mean = mean(Hum),
                        Pre_death_ada= sum(proj_death),
                        Base_death_ada= sum(base_death))
            heatdeath$adaptation_rate = adarate
            return(heatdeath)
          }else{
            return(NULL)
          }
        })
        ag = do.call(rbind,ageall)
        return(ag)
      })
      ag = do.call(rbind,allada)
      if(!is.null(ag)){
        fwrite(ag,file.path(preout,name),row.names = F)
      }
      print(paste(unique(filtered_fs$NUTS_ID),"has been processed. Next:"))
      return(ag)
    }else{
      print(paste(unique(filtered_fs$NUTS_ID),"did not exist!!!!"))
      return(NULL)
    }
  }
})
