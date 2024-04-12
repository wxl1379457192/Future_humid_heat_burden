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
da$week_num = as.integer(substring(da$week,7,8))
da = subset(da,da$week_num>=22&da$week_num<36)
outdir = "pooled_model_age_group_dataV2_SE"
dir.create(outdir)

SE_knots = function(da,SEnum,outdir){
  # PARAMETERS FOR THE LAG-RESPONSE FUNCTION
  k1 = split(da,da$age)[[1]]
  knots = quantile(k1$Humidex_mean,SEnum,na.rm=T)
  #knots = quantile(k1$Humidex_mean,c(95)/100,na.rm=T)
  da$week_num = as.integer(substring(da$week,7,8))
  da = subset(da,da$week_num>=22&da$week_num<36)
  #da$pop = da$pop/max(da$pop)
  lag <- 4
  lagnk <- 2
  #######################################
  data = subset(da,da$age=="65+")
  data$death[which(data$death==0)]=1
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geom,data$year)
  cb= crossbasis(data$Humidex_mean,lag=lag,
                 argvar = list(fun="ns",knots = knots,
                               Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),group = data$gender_group)
  
  DD = onebasis(data$DD_num,fun="strata",breaks=c(1,4))
  DN = onebasis(data$DN_num,fun="strata",breaks=c(1,4))
  filename = paste0(outdir,"/stratamodel_65+_knots",SEnum,".rds")
  if(file.exists(filename)){
    model <- readRDS(filename)
  }else{
    model <- glm(death ~ cb + DD + DN + ns(year, df = 4)+ns(week_num, df = 3)+ log(pop) + geom ,
                 family = quasipoisson(link="log"), data = data, na.action="na.exclude")
    saveRDS(model,filename)
  }
  red <- crossreduce(cb,model,at=10:40)
  MMT = red$predvar[which.min(red$RRfit)]
  red <- crossreduce(cb,model,at=10:40,cen = MMT)
  print(MMT)
  df <- data.frame(
    humidex = rep(10:40, 1),
    age = rep(c("Age:65+"), each = 31),
    RR = c(red$RRfit),
    low = c(red$RRlow),
    high = c(red$RRhigh)
  )
  return(df)
}

SE_cbdf = function(da,dfnum,outdir){
  # PARAMETERS FOR THE LAG-RESPONSE FUNCTION
  k1 = split(da,da$age)[[1]]
  knots = quantile(k1$Humidex_mean,0.8,na.rm=T)
  #knots = quantile(k1$Humidex_mean,c(95)/100,na.rm=T)
  da$week_num = as.integer(substring(da$week,7,8))
  da = subset(da,da$week_num>=22&da$week_num<36)
  #da$pop = da$pop/max(da$pop)
  lag <- 4
  lagnk <- 2
  #######################################
  data = subset(da,da$age=="65+")
  data$death[which(data$death==0)]=1
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geom,data$year)
  cb= crossbasis(data$Humidex_mean,lag=lag,
                 argvar = list(fun="ns",knots = knots,
                               Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),group = data$gender_group)
  
  DD = onebasis(data$DD_num,fun="strata",breaks=c(1,4))
  DN = onebasis(data$DN_num,fun="strata",breaks=c(1,4))
  
  filename = paste0(outdir,"/stratamodel_65+_cbDF",dfnum,".rds")
  if(file.exists(filename)){
    model <- readRDS(filename)
  }else{
    model <- glm(death ~ cb + DD + DN + ns(year, df = 4)+ns(week_num, df = 3)+ log(pop) + geom ,
                 family = quasipoisson(link="log"), data = data, na.action="na.exclude")
    saveRDS(model,filename)
  }
  
  red <- crossreduce(cb,model,at=10:40)
  MMT = red$predvar[which.min(red$RRfit)]
  red <- crossreduce(cb,model,at=10:40,cen = MMT)
  print(MMT)
  df <- data.frame(
    humidex = rep(10:40, 1),
    age = rep(c("Age:65+"), each = 31),
    RR = c(red$RRfit),
    low = c(red$RRlow),
    high = c(red$RRhigh)
  )
  return(df)
}

SE_yeardf = function(da,dfnum,outdir){
  # PARAMETERS FOR THE LAG-RESPONSE FUNCTION
  k1 = split(da,da$age)[[1]]
  knots = quantile(k1$Humidex_mean,0.8,na.rm=T)
  #knots = quantile(k1$Humidex_mean,c(95)/100,na.rm=T)
  da$week_num = as.integer(substring(da$week,7,8))
  da = subset(da,da$week_num>=22&da$week_num<36)
  #da$pop = da$pop/max(da$pop)
  lag <- 4
  lagnk <- 2
  #######################################
  data = subset(da,da$age=="65+")
  data$death[which(data$death==0)]=1
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geom,data$year)
  cb= crossbasis(data$Humidex_mean,lag=lag,
                 argvar = list(fun="ns",knots = knots,
                               Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),group = data$gender_group)
  
  DD = onebasis(data$DD_num,fun="strata",breaks=c(1,4))
  DN = onebasis(data$DN_num,fun="strata",breaks=c(1,4))
  
  filename = paste0(outdir,"/stratamodel_65+_yearDF",dfnum,".rds")
  if(file.exists(filename)){
    model <- readRDS(filename)
  }else{
    model <- glm(death ~ cb + DD + DN + ns(year, df = dfnum)+ns(week_num, df = 3)+ log(pop) + geom ,
                 family = quasipoisson(link="log"), data = data, na.action="na.exclude")
    saveRDS(model,filename)
  }
  
  red <- crossreduce(cb,model,at=10:40)
  MMT = red$predvar[which.min(red$RRfit)]
  red <- crossreduce(cb,model,at=10:40,cen = MMT)
  print(MMT)
  df <- data.frame(
    humidex = rep(10:40, 1),
    age = rep(c("Age:65+"), each = 31),
    RR = c(red$RRfit),
    low = c(red$RRlow),
    high = c(red$RRhigh)
  )
  return(df)
}

SE_weekdf = function(da,dfnum,outdir){
  # PARAMETERS FOR THE LAG-RESPONSE FUNCTION
  k1 = split(da,da$age)[[1]]
  knots = quantile(k1$Humidex_mean,0.8,na.rm=T)
  #knots = quantile(k1$Humidex_mean,c(95)/100,na.rm=T)
  da$week_num = as.integer(substring(da$week,7,8))
  da = subset(da,da$week_num>=22&da$week_num<36)
  #da$pop = da$pop/max(da$pop)
  lag <- 4
  lagnk <- 2
  #######################################
  data = subset(da,da$age=="65+")
  data$death[which(data$death==0)]=1
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geom,data$year)
  cb= crossbasis(data$Humidex_mean,lag=lag,
                 argvar = list(fun="ns",knots = knots,
                               Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),group = data$gender_group)
  
  DD = onebasis(data$DD_num,fun="strata",breaks=c(1,4))
  DN = onebasis(data$DN_num,fun="strata",breaks=c(1,4))
  
  filename = paste0(outdir,"/stratamodel_65+_weekDF",dfnum,".rds")
  if(file.exists(filename)){
    model <- readRDS(filename)
  }else{
    model <- glm(death ~ cb + DD + DN + ns(year, df = 4)+ns(week_num, df = dfnum)+ log(pop) + geom ,
                 family = quasipoisson(link="log"), data = data, na.action="na.exclude")
    saveRDS(model,filename)
  }
  
  red <- crossreduce(cb,model,at=10:40)
  MMT = red$predvar[which.min(red$RRfit)]
  red <- crossreduce(cb,model,at=10:40,cen = MMT)
  print(MMT)
  df <- data.frame(
    humidex = rep(10:40, 1),
    age = rep(c("Age:65+"), each = 31),
    RR = c(red$RRfit),
    low = c(red$RRlow),
    high = c(red$RRhigh)
  )
  return(df)
}

knotdf= do.call(rbind,lapply(c(0.8,0.7,0.9),function(s){
  d = SE_knots(da,s,outdir)
  d$SE = paste0("knots",s)
  return(d)
  print(paste0(s,"has been processed"))
}))

cbdf = do.call(rbind,lapply(c(3,4),function(s){
  d = SE_cbdf(da,s,outdir)
  d$SE = paste0("cbDF",s)
  return(d)
  print(paste0(s,"has been processed"))
}))

ydf = do.call(rbind,lapply(c(2,3),function(s){
  d = SE_yeardf(da,s,outdir)
  d$SE = paste0("yearDF",s)
  return(d)
  print(paste0(s,"has been processed"))
}))

wdf = do.call(rbind,lapply(c(2,4),function(s){
  d = SE_weekdf(da,s,outdir)
  d$SE = paste0("weekDF",s)
  return(d)
  print(paste0(s,"has been processed"))
}))



all = do.call(rbind,list(knotdf,cbdf,ydf,wdf))
all$var = "DS"

all$var[which(all$SE=="knots0.7")] = "S1"
all$var[which(all$SE=="knots0.9")] = "S2"
all$var[which(all$SE=="cbDF3")] = "S3"
all$var[which(all$SE=="cbDF4")] = "S4"
all$var[which(all$SE=="yearDF2")] = "S5"
all$var[which(all$SE=="yearDF3")] = "S6"
all$var[which(all$SE=="weekDF2")] = "S7"
all$var[which(all$SE=="weekDF4")] = "S8"

g = ggplot(all) +
  geom_point(aes(x = humidex, y =  RR,color = var),
             show.legend = TRUE,alpha=0.5,size=1,
             position= position_dodge(width = 0.85))+
  geom_linerange(aes(x = humidex, ymin = low, ymax = high,color = var),
                 position=position_dodge(width = 0.85),show.legend = FALSE,
                 size=0.5,
                 alpha = 0.6) +
  scale_color_manual(values = c("#001f22","#7ab8cc","#00808c","#ff7300","#ffcc00",
                                "#605276","#e63995","#769701","#228b22"))+
  theme_bw()+ 
  #geom_vline(show.legend = TRUE,aes(xintercept = -3, color=var),alpha=0.6)+
  scale_x_continuous(expand = c(0.01,0.01),limits = c(9,41))+
  scale_y_continuous(expand = c(0,0),limits = c(0.9,1.8))+
  theme(legend.key=element_rect(fill='transparent'),
        panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_line(colour = "transparent"),
        axis.title.x= element_text(color="black",hjust = 0.5,vjust=0,size =unit(8,"pt")),
        axis.text.x = element_text(color="black",size = unit(8,"pt")),
        axis.title.y= element_text(color="black",hjust = 0.5,vjust=0,size =unit(8,"pt")),
        axis.text.y = element_text(color="black",size = unit(8,"pt")),
        legend.position="bottom"
  ) +
  labs(
    x="Weekly average Humidex",
    y="Relative risk (RR)"
  )



rextra = function(filedir,cbvar,cblag){
  da = read.csv("pooled_model_age_group_dataV2/Inputdata_withinage.csv",stringsAsFactors  = F)
  da = subset(da,da$year>=2015&da$year<=2019)
  # PARAMETERS FOR THE LAG-RESPONSE FUNCTION
  da$week_num = as.integer(substring(da$week,7,8))
  da = subset(da,da$week_num>=22&da$week_num<36)
  #da$pop = da$pop/max(da$pop)
  lag <- 4
  lagnk <-2
  
  ######################################
  #######################################
  data = subset(da,da$age=="65+")
  data$death[which(data$death==0)]=1
  data$age = as.factor(data$age)
  knots  = quantile(data$Humidex_mean,c(80)/100,na.rm=T)
  if(cbvar=="bs"){
    varfun=list(fun="bs",degree=2,
                knots =  knots)
  }else if(cbvar=="ns"){
    varfun = list(fun="ns",knots =  quantile(data$Humidex_mean,c(80)/100,na.rm=T), 
                  Boundary.knots = range(data$Humidex_mean,na.rm=T))
  }
  
  if(cblag=="logknots"){
    lagfun  = list(knots = logknots(lag, lagnk))
  }else if(cblag=="integer"){
    lagfun  = list(fun = "integer")
  }
  
  cb= crossbasis(data$Humidex_mean,
                 lag=lag,
                 argvar =varfun,
                 arglag= lagfun,
                 group = data$gender_group)
  
  DD = onebasis(data$DD_num,fun="strata",breaks=c(1,4))
  DN = onebasis(data$DN_num,fun="strata",breaks=c(1,4))
  
  model <-  readRDS(file.path(filedir,"stratamodel_65+.rds"))
  
  #############################
  data2 = subset(da,da$age=="15-65")
  data2$death[which(data2$death==0)]=1
  data2$age = as.factor(data2$age)
  
  cb2= crossbasis(data2$Humidex_mean,
                  lag=lag,
                  argvar = varfun,
                  arglag= lagfun,
                  group = data2$gender_group)
  DD2 = onebasis(data2$DD_num,fun="strata",breaks=c(1,4))
  DN2 = onebasis(data2$DN_num,fun="strata",breaks=c(1,4))
  
  model2 <-  readRDS(file.path(filedir,"stratamodel_16-65.rds"))
  
  ##############model for age 0-15#####################
  data3 = subset(da,da$age=="0-15")
  data3$death[which(data3$death==0)]=1
  data3$age = as.factor(data3$age)
  
  cb3= crossbasis(data3$Humidex_mean,lag=lag,
                  argvar = varfun,
                  arglag= lagfun,
                  group = data3$gender_group)
  
  DD3 = onebasis(data3$DD_num,fun="strata",breaks=c(1,4))
  DN3 = onebasis(data3$DN_num,fun="strata",breaks=c(1,4))
  
  model3 <- readRDS(file.path(filedir,"stratamodel_0-15.rds"))
  
  red <- crossreduce(cb,model,at=10:40)
  MMT = red$predvar[which.min(red$RRfit)]
  
  red3 <- crossreduce(cb3,model3,at=10:40,cen = MMT)
  red2 <- crossreduce(cb2,model2,at=10:40,cen =MMT)
  red <- crossreduce(cb,model,at=10:40,cen = MMT)
  print(MMT)
  df <- data.frame(
    humidex = rep(10:40, 1),
    age = rep(c("Age:0-15", "Age:16-65", "Age:65+"), each = 31),
    RR = c(red3$RRfit, red2$RRfit, red$RRfit),
    low = c(red3$RRlow, red2$RRlow, red$RRlow),
    high = c(red3$RRhigh, red2$RRhigh, red$RRhigh)
  )
  df$varfun = cbvar
  df$lagfun = cblag
  return(df)
}

se  = do.call(rbind,list(rextra("pooled_model_age_group_dataV2_ns","ns","logknots"),
                         rextra("pooled_model_age_group_dataV2_bs","bs","logknots"),
                         rextra("pooled_model_age_group_dataV2_lagfun","ns","integer")))
sefile = subset(se,se$humidex==34)
sefile$var = paste(sefile$varfun,sefile$lagfun)

sefile$var = factor(sefile$var,levels=c("ns logknots","ns integer","bs logknots"))

g2 = ggplot(data = sefile,aes(x=age,fill=var))+
  geom_col(aes(y=RR),color = "grey20",
           position = position_dodge2(preserve = 'single'),
           show.legend = TRUE,alpha=0.7,size=0.2)+
  geom_errorbar(aes(ymin =low, ymax = high),color = "grey20",size=0.3,
                position = position_dodge2(preserve = 'single', padding = 0.5))+
  scale_fill_manual(values = c("grey30","#0e56a2","#a1d7ed"))+
  scale_y_continuous(expand = c(0,0),limits = c(0,1.7))+
  theme_bw() +
  theme(strip.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x= element_text(color="black",hjust = 0.5,vjust=0,size =unit(7,"pt")),
        axis.text.x = element_text(color="black",size = unit(7,"pt")),
        axis.title.y= element_text(color="black",hjust = 0.5,vjust=0,size =unit(7,"pt")),
        axis.text.y = element_text(color="black",size = unit(7,"pt")),
        axis.line.x = element_line(color = "black", size = 0.2),
        axis.line.y = element_line(color = "black", size =0.2),
        legend.position="bottom"
  )

  


setwd("D:/ATtest/Europe")
ggsave(paste0("Figure_1225","/FigS6a_SE.pdf"),g, width=15, height=7, units="cm", scale=2)
ggsave(paste0("Figure_1225","/FigS6b_SE.pdf"),g2, width=8, height=8, units="cm", scale=1)



