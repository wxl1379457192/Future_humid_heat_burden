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
setwd("D:/ATtest/Europe_version3/")
preout = "Heatmor_prediction_future_baseline"
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
  return(pc)
}))
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

all = lapply(file_list,function(f){
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
  name = paste0("Projected_future_deaths_",unique(filtered_fs$NUTS_ID),".csv")
  if(file.exists(file.path(preout,name))|nrow(filtered_fs)==0){
    print(paste(name,"has existed!"))
  }else{
    dg <- d %>%
      filter(geo == unique_nuts_id)
    
    bp <- basepop %>%
      filter(geo == unique_nuts_id)
    
    pn <- popnum %>%
      filter(NUTS_ID == unique_nuts_id)
    
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
    if(nrow(pr)>0&nrow(filtered_fs)>0&nrow(bp)>0){
      ageall = lapply(split(bp,bp$age),function(a){
        unique_age <- unique(a$age)
        data_base <- data[data$age == unique_age & data$geo == unique_nuts_id, ]
        if(nrow(data_base)>0){
          dga <- dg[dg$age == unique_age, ]
          pra <- pr[pr$age_group == unique_age, ]
          colnames(pra)[4:5] <- c("projected_pop","BSL")
          filtered_fs$base_pop_age <- bp$OBS_VALUE[bp$age == unique_age]
          filtered_fs$base_pop <- bp$sum_pop[bp$age == unique_age]
          filtered_fs$base_agerate <- bp$base_agerate[bp$age == unique_age]
          daall <- filtered_fs %>%
            merge(pn, by = c("NUTS_ID", "year")) %>%
            merge(pra[,-c("geo")], by.x = c("year"), by.y = c("TIME_PERIOD")) %>%
            merge(dga, by.x = c("NUTS_ID", "weeknum"), by.y = c("geo", "weeknum"))
          
          
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
          preo$morrisk = exp(preo$rr+preo$rr.cdd+preo$rr.cdn+preo$rr.cda)-1
          preo$morrisk[which(preo$morrisk<0)] = 0
          preo <- preo %>%
            mutate( pop_ssp1_BSL = ssp1 * BSL,
                   pop_ssp2_BSL = ssp2 * BSL,
                   pop_ssp3_BSL = ssp3 * BSL,
                   pop_ssp5_BSL = ssp5 * BSL,
                   pop_ssp1_BR2022 = ssp1 * base_agerate,
                   pop_ssp2_BR2022 = ssp2 * base_agerate,
                   pop_ssp3_BR2022 = ssp3 * base_agerate,
                   pop_ssp5_BR2022 = ssp5 * base_agerate,
                   pop_2022_BSL = base_pop * BSL,
                   pop_2022_BR2022 = base_pop * base_agerate,
                   predicted_SSP1 = morrisk * death_base * pop_ssp1_BSL / base_pop_age,
                   predicted_SSP2 = morrisk * death_base * pop_ssp2_BSL / base_pop_age,
                   predicted_SSP3 = morrisk * death_base * pop_ssp3_BSL / base_pop_age,
                   predicted_SSP5 = morrisk * death_base * pop_ssp5_BSL / base_pop_age,
                   predicted_SSP1_BR2022 = morrisk * death_base * pop_ssp1_BR2022 / base_pop_age,
                   predicted_SSP2_BR2022 = morrisk * death_base * pop_ssp2_BR2022 / base_pop_age,
                   predicted_SSP3_BR2022 = morrisk * death_base * pop_ssp3_BR2022 / base_pop_age,
                   predicted_SSP5_BR2022 = morrisk * death_base * pop_ssp5_BR2022 / base_pop_age,
                   predicted_2022_BSL = morrisk * death_base * pop_2022_BSL / base_pop_age,
                   predicted_2022_BR2022 = morrisk * death_base * pop_2022_BR2022 / base_pop_age)
          
          
          preobase <- preo[preo$Year == 2022, c("weeknum", "morrisk")]
          preobase <- unique(preobase)
          colnames(preobase)[2] <- "morrisk_2022"
          preobase <- preobase[!duplicated(preobase$weeknum), ]
          preo = merge(preo,preobase,by="weeknum")
          
          preo <- preo %>%
            mutate(base_SSP1 = morrisk_2022 * death_base * pop_ssp1_BSL / base_pop_age,
                   base_SSP2 = morrisk_2022 * death_base * pop_ssp2_BSL / base_pop_age,
                   base_SSP3 = morrisk_2022 * death_base * pop_ssp3_BSL / base_pop_age,
                   base_SSP5 = morrisk_2022 * death_base * pop_ssp5_BSL / base_pop_age,
                   base_SSP1_BR2022 = morrisk_2022 * death_base * pop_ssp1_BR2022 / base_pop_age,
                   base_SSP2_BR2022 = morrisk_2022 * death_base * pop_ssp2_BR2022 / base_pop_age,
                   base_SSP3_BR2022 = morrisk_2022 * death_base * pop_ssp3_BR2022 / base_pop_age,
                   base_SSP5_BR2022 = morrisk_2022 * death_base * pop_ssp5_BR2022 / base_pop_age,
                   base_2022_BSL = morrisk_2022 * death_base * pop_2022_BSL / base_pop_age,
                   base_2022_BR2022 = morrisk_2022 * death_base * pop_2022_BR2022 / base_pop_age)
          
          
          
          preo = preo[which(weeknum>=22&weeknum<=35),]
          heatdeath = preo%>%group_by(Year,geo,age,rcp,member)%>%
            summarise(Humidex_mean = mean(Hum),
                      pop_2022_BR2022  = unique(pop_2022_BR2022),pop_2022_BSL  = unique(pop_2022_BSL),
                      pop_ssp1_BSL= unique(pop_ssp1_BSL),pop_ssp2_BSL= unique(pop_ssp2_BSL),
                      pop_ssp3_BSL= unique(pop_ssp3_BSL), pop_ssp5_BSL= unique(pop_ssp5_BSL),
                      pop_ssp1_BR2022= unique(pop_ssp1_BR2022),pop_ssp2_BR2022= unique(pop_ssp2_BR2022),
                      pop_ssp3_BR2022= unique(pop_ssp3_BR2022),pop_ssp5_BR2022= unique(pop_ssp5_BR2022),
                      death_base = sum(death_base),pop_base = mean(base_pop),
                      Pre_ssp1_BSL= sum(predicted_SSP1),Pre_ssp2_BSL= sum(predicted_SSP2),
                      Pre_ssp3_BSL= sum(predicted_SSP3),Pre_ssp5_BSL= sum(predicted_SSP5),
                      Pre_ssp1_BR2022= sum(predicted_SSP1_BR2022),Pre_ssp2_BR2022= sum(predicted_SSP2_BR2022),
                      Pre_ssp3_BR2022= sum(predicted_SSP3_BR2022),Pre_ssp5_BR2022= sum(predicted_SSP5_BR2022),
                      Pre_2022 = sum(predicted_2022_BR2022), Pre_2022_BSL = sum(predicted_2022_BSL),
                      Base_ssp1_BSL= sum(base_SSP1),Base_ssp2_BSL= sum(base_SSP2),
                      Base_ssp3_BSL= sum(base_SSP3),Base_ssp5_BSL= sum(base_SSP5),
                      Base_ssp1_BR2022= sum(base_SSP1_BR2022),
                      Base_ssp2_BR2022= sum(base_SSP2_BR2022),Base_ssp3_BR2022= sum(base_SSP3_BR2022),
                      Base_ssp5_BR2022= sum(base_SSP5_BR2022),
                      Base_2022_BSL = sum(base_2022_BSL),
                      Base_2022 = sum(base_2022_BR2022))
          return(heatdeath)
        }else{
          return(NULL)
        }
      })
      ag = do.call(rbind,ageall)
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
