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
setwd("D:/ATtest/Europe/")
climate_dir = "Future_heat_weekly_V1223"
preout = "Mortality_prediction_future_V1225"
if(dir.exists(preout)){
  print(paste(preout,"has existed!"))
}else{
  dir.create(preout)
}

modeldir = "DLMN_model_V0530/pooled_model_age_group_dataV2_ns"

da = read.csv(file.path( "DLMN_model_V0530/pooled_model_age_group_dataV2","Inputdata_withinage.csv"),stringsAsFactors  = F)
da = subset(da,da$year>=2015&da$year<=2019)
da$week_num = as.integer(substring(da$week,7,8))
da = subset(da,da$week_num>=22&da$week_num<36)
geolist = unique(da$geom)

poprate = read.csv(file.path("pop_structure_projected",
                             "proj_pop_age_proportion.csv"),stringsAsFactors = F)
poprate$age_group[which(poprate$age_group=="<15")]="0-15"
poprate$age_group[which(poprate$age_group=="16-65")]="15-65"  
uklist = geolist[grep("UK",geolist)]
ukrate = do.call(rbind,lapply(uklist,function(g){
  pc = poprate[grep(substring(g,1,3),poprate$geo),]
  pc$geo=g
  return(pc)
}))
poprate = do.call(rbind,list(poprate,ukrate))

popnum = read.csv(file.path("Future_pop","Future_pop.csv"),stringsAsFactors = F)
popnum = do.call(rbind,lapply(split(popnum,popnum$year),function(y){
  py = do.call(rbind,lapply(seq(unique(y$year),unique(y$year)+9,1),function(k){
    py = y[,c("pop","NUTS_ID","scenario")]
    py = dcast(py , NUTS_ID ~ scenario, value.var = "pop")
    py$year =k
    return(py)
  }))
  return(py)
}))
popnum = do.call(rbind,lapply(geolist,function(g){
  pc = popnum[grep(g, popnum$NUTS_ID),]
  if(length(unique(pc$NUTS_ID))>1){
    pc$NUTS_ID = g
    pc = pc%>%group_by(NUTS_ID,year)%>%summarise(ssp1=sum(ssp1),ssp2=sum(ssp2),
                                      ssp3=sum(ssp3),ssp4=sum(ssp4),ssp5=sum(ssp5))
  }
  return(pc)
}))

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
basesum = basepop%>%group_by(geo)%>%summarise(sum_pop=sum(OBS_VALUE))
basepop = merge(basepop,basesum,by="geo")
basepop$base_agerate = basepop$OBS_VALUE/basepop$sum_pop
lag = 4
lagnk = 2

baseclimate = read.csv(file.path("2022_heat_weekly_V1223",
                                 "Weekly_data_2022.csv"),stringsAsFactors = F)
baseclimate = baseclimate[,-c(2,5,6,7)]
colnames(baseclimate) = c("geo","weeknum","Humidex_2022","DD_2022","DN_2022")
baseclimate$weeknum = as.integer(substring(baseclimate$weeknum,7,8))

prediction = function(cb,preda,popscenario_list,konts,
                      climate_name=c("Humidex_mean","DD_num","DN_num"),
                      outlabel = "Pred_",dlnm_model,MMT){
  
  for (pops in popscenario_list){
    newdata = preda[,c("geo","year",climate_name,
                       "gender_group","weeknum",pops)]
    newdata[,climate_name[1]][which(newdata[,climate_name[1]]==0)] = NA
    colnames(newdata)[1] = "geom" 
    colnames(newdata)[7] = "week_num" 
    colnames(newdata)[8] = "pop"
    colnames(newdata)[3] = "Humidex_mean"
    colnames(newdata)[4] = "DD_num"
    colnames(newdata)[5] = "DN_num"
    #newdata = unique(newdata)
    DD = onebasis(newdata$DD_num,fun="strata",breaks=c(1,4))
    DN = onebasis(newdata$DN_num,fun="strata",breaks=c(1,4))
    pre = crosspred(cb,dlnm_model,cumul = T,
                    at = newdata$Humidex_mean,cen=MMT,model.link = "log")
    ff =data.frame(cbind(pre$predvar,pre$matfit[,1])) 
    colnames(ff) <- c("Humidex", "rr")
    DDpre = crosspred(DD,dlnm_model,
                      at = seq(0,7),cen=0,model.link = "log")
    D_f = data.frame(cbind(as.integer(DDpre$predvar),DDpre$allfit)) 
    colnames(D_f) <- c("DD_num", "rr.dd")
    DNpre = crosspred(DN,dlnm_model,
                      at = seq(0,7),cen=0,model.link = "log")#,model.link = "log"
    DN_f = data.frame(cbind(as.integer(DNpre$predvar),DNpre$allfit)) 
    colnames(DN_f) <- c("DN_num", "rr.dn")
    
    preo = merge(preda,ff,by.x=climate_name[1],by.y = "Humidex")
    preo = merge(preo,D_f,by.x=climate_name[2],by.y = "DD_num")
    preo = merge(preo,DN_f,by.x=climate_name[3],by.y = "DN_num")
    preo$morrisk = exp(preo$rr+preo$rr.dd+preo$rr.dn)-1
    preo$morrisk[which(preo$morrisk<0)] = 0
    preo$predicted = preo$morrisk*preo$death_base*preo[,pops]/preo$pop_base
    preo = preo[,c("geo","weeknum","year","predicted","scenario")]
    preda = merge(preda,preo,by=c("geo","weeknum","year","scenario"))
    colnames(preda)[which(colnames(preda)=="predicted")]=paste0(outlabel,pops)
    rm(newdata,DD,DN)
    gc()
  }
  return(preda)
}
ageprocess = function(dlnm_model,data,climate_dir,basepop,poprate,outdir,popnum,baseclimate){
  data$gender_group = paste(data$geom,data$year)
  d = data%>%group_by(geom,week_num)%>%summarise(death = mean(death),pop=mean(pop))
  colnames(d) = c("geo","weeknum","death_base","pop_base")
  cb= crossbasis(data$Humidex_mean,lag=lag,
                 argvar = list(fun="ns",knots =  quantile(data$Humidex_mean,c(80)/100,na.rm=T), 
                               Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group = data$gender_group)
  red <- crossreduce(cb,dlnm_model,at=10:40)
  MMT = red$predvar[which.min(red$RRfit)]
  print(MMT)
  file_names <- list.files(climate_dir)
  pattern = "Future_weekly_data"
  matching_files <- grep(pattern, file_names, value = TRUE)
  
  pre_list = do.call(rbind,lapply(matching_files,function(f){
    print(paste(f,"start:"))
    name = file.path(outdir,paste0(strsplit(f,".csv")[[1]][1],"_",unique(data$age),".csv"))
    if (file.exists(name)){
      # pred = read.csv(name,stringsAsFactors = F)
      print(paste(name,"has existed"))
      #return(pred)
      return(NULL)
    }else{
      knots = quantile(data$Humidex_mean,c(80)/100,na.rm=T)
      preda = read.csv(file.path(climate_dir,f),stringsAsFactors = F)
      if(length(which(is.na(preda$Humidex_mean)==F))>0){
        preda$gender_group = interaction(preda$geo,preda$year)
        preda$weeknum = as.numeric(substring(preda$week,7,8))
        bpage = split(basepop,basepop$age)[[unique(data$age)]]
        popnumc = subset(popnum,popnum$NUTS_ID==unique(preda$geo))
        pr = subset(poprate,poprate$age_group==unique(data$age))
        pr = subset(pr,pr$geo==unique(preda$geo))
        # pr= do.call(rbind,lapply(split(pr,pr$TIME_PERIOD),function(y){
        #   py = y[,c("pop","geo","ratio")]
        #   py = dcast(py, geo ~ pop, value.var = "ratio")
        #   py$year = unique(y$TIME_PERIOD)
        #   return(py)
        # }))
        colnames(pr)[2] = "year"
        colnames(pr)[4] = "projected_pop"
        colnames(pr)[5] = "BSL"
        colnames(bpage)[3] = "pop_2022"
        if(unique(preda$geo)%in%unique(pr$geo)&
           unique(preda$geo)%in%unique(popnumc$NUTS_ID)&
           unique(preda$geo)%in%unique(baseclimate$geo)&
           unique(preda$geo)%in%unique(bpage$geo)
        ){
          preda = merge(preda,baseclimate,by=c("geo","weeknum"))
          preda = merge(preda,popnumc,by.x=c("geo","year"),by.y=c("NUTS_ID","year"))
          preda = merge(preda,bpage,by="geo")
          preda = merge(preda,pr,by=c("geo","year"))
          #preda = preda[complete.cases(preda),]
          preda$pop_ssp1_BSL = preda$ssp1*preda$BSL
          preda$pop_ssp2_BSL = preda$ssp2*preda$BSL
          preda$pop_ssp3_BSL = preda$ssp3*preda$BSL
          preda$pop_ssp4_BSL = preda$ssp4*preda$BSL
          preda$pop_ssp5_BSL = preda$ssp5*preda$BSL
          preda$pop_ssp1_BR2022 =preda$ssp1*preda$base_agerate#BR2022 was a scenario without ageing (age structure using data in 2022)
          preda$pop_ssp2_BR2022 =preda$ssp2*preda$base_agerate 
          preda$pop_ssp3_BR2022 =preda$ssp3*preda$base_agerate
          preda$pop_ssp4_BR2022 =preda$ssp4*preda$base_agerate
          preda$pop_ssp5_BR2022 =preda$ssp5*preda$base_agerate
          preda$pop_2022_BSL = preda$sum_pop*preda$BSL #2022_* 是人口基数不变,只变年龄结构的情景
          popscenario_list = c("pop_ssp1_BSL", "pop_ssp2_BSL","pop_ssp3_BSL", "pop_ssp4_BSL","pop_ssp5_BSL",
                               "pop_ssp1_BR2022","pop_ssp2_BR2022","pop_ssp3_BR2022","pop_ssp4_BR2022","pop_ssp5_BR2022",
                               "pop_2022_BSL","pop_2022")#"pop_2022"是人口基数和年龄结构都不变的情景
          preda = merge(preda,d,by=c("geo","weeknum"))
          preda$scenario = paste0(preda$rcp,preda$member)
          preda = prediction(cb,preda,popscenario_list,knots,
                             climate_name=c("Humidex_mean","DD_num","DN_num"),
                             outlabel = "Pred_",dlnm_model,MMT)
          preda = prediction(cb,preda,popscenario_list,knots,
                             climate_name=c("Humidex_2022","DD_2022","DN_2022"),
                             outlabel = "Base_",dlnm_model,MMT)
          
          pred= preda%>%
            group_by(geo,year,age,rcp,member)%>%
            summarize(Humidex_mean = mean(Humidex_mean),
                      DD_num = sum(DD_num),DN_num = sum(DN_num),
                      Humidex_2022 = mean(Humidex_2022),
                      DD_2022 = sum(DD_2022),DN_2022 = sum(DN_2022),
                      pop_2022  = unique(pop_2022),
                      pop_ssp1_BSL= unique(pop_ssp1_BSL),pop_ssp2_BSL= unique(pop_ssp2_BSL),
                      pop_ssp3_BSL= unique(pop_ssp3_BSL),pop_ssp4_BSL= unique(pop_ssp4_BSL),
                      pop_ssp5_BSL= unique(pop_ssp5_BSL),
                      pop_ssp1_BR2022= unique(pop_ssp1_BR2022),pop_ssp2_BR2022= unique(pop_ssp2_BR2022),
                      pop_ssp3_BR2022= unique(pop_ssp3_BR2022),pop_ssp4_BR2022= unique(pop_ssp4_BR2022),
                      pop_ssp5_BR2022= unique(pop_ssp5_BR2022),
                      death_base = mean(death_base),pop_base = mean(pop_base),
                      Pre_ssp1_BSL= sum(Pred_pop_ssp1_BSL),
                      Pre_ssp2_BSL= sum(Pred_pop_ssp2_BSL),Pre_ssp3_BSL= sum(Pred_pop_ssp3_BSL),
                      Pre_ssp4_BSL= sum(Pred_pop_ssp4_BSL),Pre_ssp5_BSL= sum(Pred_pop_ssp5_BSL),
                      Pre_ssp1_BR2022= sum(Pred_pop_ssp1_BR2022),
                      Pre_ssp2_BR2022= sum(Pred_pop_ssp2_BR2022),Pre_ssp3_BR2022= sum(Pred_pop_ssp3_BR2022),
                      Pre_ssp4_BR2022= sum(Pred_pop_ssp4_BR2022),Pre_ssp5_BR2022= sum(Pred_pop_ssp5_BR2022),
                      Pre_2022 = sum(Pred_pop_2022), Pre_2022_BSL = sum(Pred_pop_2022_BSL),
                      Base_ssp1_BSL= sum(Base_pop_ssp1_BSL),
                      Base_ssp2_BSL= sum(Base_pop_ssp2_BSL),Base_ssp3_BSL= sum(Base_pop_ssp3_BSL),
                      Base_ssp4_BSL= sum(Base_pop_ssp4_BSL),Base_ssp5_BSL= sum(Base_pop_ssp5_BSL),
                      Base_ssp1_BR2022= sum(Base_pop_ssp1_BR2022),
                      Base_ssp2_BR2022= sum(Base_pop_ssp2_BR2022),Base_ssp3_BR2022= sum(Base_pop_ssp3_BR2022),
                      Base_ssp4_BR2022= sum(Base_pop_ssp4_BR2022),Base_ssp5_BR2022= sum(Base_pop_ssp5_BR2022),
                      Base_2022_BSL = sum(Base_pop_2022_BSL),
                      Base_2022 = sum(Base_pop_2022)
            )
          write.csv(pred,name,row.names = F)
          print(paste(f,"has been processed!"))
          return(pred)
        }else{
          print(paste(unique(preda$geo),"have some missing data"))
          return(NULL)
        }
      }else{
        print(paste(unique(preda$geo),"have some missing data"))
        return(NULL)
      }
    }
  }))
  return(pre_list)
}
# library(doParallel)
# 
# # 设置并行计算
# cl <- makeCluster(6)
# registerDoParallel(cl)
all <-lapply(split(da, da$age),function(data){
  if (unique(data$age) == "15-65") {
    model = readRDS(file.path(modeldir, paste0("stratamodel_16-65.rds")))
  } else {
    model = readRDS(file.path(modeldir, paste0("stratamodel_", unique(data$age), ".rds")))
  }
  print(paste(unique(data$age), "start:"))
  k = ageprocess(model, data, climate_dir, basepop, poprate, preout, popnum, baseclimate)
  print(paste(unique(data$age), "end."))
  return(k)
})
