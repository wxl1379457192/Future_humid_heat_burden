rm(list = ls())
gc()
setwd("D:/ATtest/Europe_version3")

library(dlnm)
library(splines)
library(ggplot2)
library(reshape2)
library(patchwork)
library(zoo)
library(Epi)
library(data.table)
indir = "Input_model_data"
pop = fread(file.path("D:/ATtest/Europe_version2",indir,"Aux_pop_data_V2.csv"))

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
SE_knots = function(data,SEnum,outdir){
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  data$IHD = data$Heatday-data$CHD
  data$IHN = data$Heatnight-data$CHN
  data$IHA = data$Heatall-data$CH
  knots = quantile(data$Hum,SEnum,na.rm=T)
  
  cb= crossbasis(data$Hum,
                 lag=lag,
                 argvar =list(fun="ns",knots =  knots, 
                              Boundary.knots = range(data$Hum,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group = data$gender_group)
  
  CDD = onebasis(data$CHD,fun="strata",breaks=c(1,2,4))
  CDN = onebasis(data$CHN,fun="strata",breaks=c(1,2,4))
  CDA = onebasis(data$CH,fun="strata",breaks=c(1,2,4))
  UDD = onebasis(data$IHD,fun="strata",breaks=c(1,2,4))
  UDN = onebasis(data$IHN,fun="strata",breaks=c(1,2,4))
  UDA = onebasis(data$IHA,fun="strata",breaks=c(1,2,4))
  modelname = paste0(outdir,"/stratamodel_",data$age[1],"_ns_hum_knots",SEnum,".rds")
  if (file.exists(modelname)){
    model <- readRDS(modelname)
  }else{
    model <- glm(death ~ cb + CDD + CDN+ CDA + UDD + UDN+ UDA + ns(Year, df = 4)+ ns(Week, df = 3)+ geo +log(pop),
                 family = quasipoisson(link="log"), data = data, na.action="na.exclude")
    saveRDS(model,modelname)
  }
  
  data$predictions <-predict(model,newdata = data,type = "response")
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

SE_yeardf = function(data,dfnum,outdir){
  data$death_rate[which(data$death_rate==0)]=0.000001
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  data$IHD = data$Heatday-data$CHD
  data$IHN = data$Heatnight-data$CHN
  data$IHA = data$Heatall-data$CH
  knots = quantile(data$Hum,0.8,na.rm=T)
  
  cb= crossbasis(data$Hum,
                 lag=lag,
                 argvar =list(fun="ns",knots =  knots, 
                              Boundary.knots = range(data$Hum,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group = data$gender_group)
  
  CDD = onebasis(data$CHD,fun="strata",breaks=c(1,2,4))
  CDN = onebasis(data$CHN,fun="strata",breaks=c(1,2,4))
  CDA = onebasis(data$CH,fun="strata",breaks=c(1,2,4))
  UDD = onebasis(data$IHD,fun="strata",breaks=c(1,2,4))
  UDN = onebasis(data$IHN,fun="strata",breaks=c(1,2,4))
  UDA = onebasis(data$IHA,fun="strata",breaks=c(1,2,4))
  modelname = paste0(outdir,"/stratamodel_",data$age[1],"_ns_hum_dfyear",dfnum,".rds")
  if (file.exists(modelname)){
    model <- readRDS(modelname)
  }else{
    model <- glm(death ~ cb + CDD + CDN+ CDA + UDD + UDN+ UDA + ns(Year, df = dfnum)+ ns(Week, df = 3)+ geo +log(pop),
                 family = quasipoisson(link="log"), data = data, na.action="na.exclude")
    
    saveRDS(model,modelname)
  }
  
  data$predictions <-predict(model,newdata = data,type = "response")
  
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

SE_weekdf = function(data,weekdf,outdir){
  data$death_rate[which(data$death_rate==0)]=0.000001
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  data$IHD = data$Heatday-data$CHD
  data$IHN = data$Heatnight-data$CHN
  data$IHA = data$Heatall-data$CH
  knots = quantile(data$Hum,0.8,na.rm=T)
  
  cb= crossbasis(data$Hum,
                 lag=lag,
                 argvar =list(fun="ns",knots =  knots, 
                              Boundary.knots = range(data$Hum,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group = data$gender_group)
  
  CDD = onebasis(data$CHD,fun="strata",breaks=c(1,2,4))
  CDN = onebasis(data$CHN,fun="strata",breaks=c(1,2,4))
  CDA = onebasis(data$CH,fun="strata",breaks=c(1,2,4))
  UDD = onebasis(data$IHD,fun="strata",breaks=c(1,2,4))
  UDN = onebasis(data$IHN,fun="strata",breaks=c(1,2,4))
  UDA = onebasis(data$IHA,fun="strata",breaks=c(1,2,4))
  modelname = paste0(outdir,"/stratamodel_",data$age[1],"_ns_hum_dfweek",weekdf,".rds")
  if (file.exists(modelname)){
    model <- readRDS(modelname)
  }else{
    model <- glm(death ~ cb + CDD + CDN+ CDA + UDD + UDN+ UDA + ns(Year, df = 4)+ ns(Week, df = weekdf)+ geo +log(pop),
                 family = quasipoisson(link="log"), data = data, na.action="na.exclude")
    saveRDS(model,modelname)
  }
  
  data$predictions <-predict(model,newdata = data,type = "response")
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

indata = fread(file.path(indir,paste0("Input_data_all_v2.csv")))
filter_list<- filter_geo(indata$geo)
pop = pop[age_group!="TOTAL",]
indata= indata[indata$geo %in% filter_list, ]
indata =indata[age!="TOTAL",]

indata = merge(indata,pop,
               by.x = c("Year","geo","age"),
               by.y = c("Year","geo","age_group"))
indata = indata[complete.cases(indata),]
indata = indata[pop>0]
gc()

lag <- 4
lagnk <-2
outdir = "Model"
if (!dir.exists(outdir)){
  dir.create(outdir)
}
#######################################
data = subset(indata,indata$age=="65+")
data = data[Year>=2010&Year<=2019,]
knotdf= do.call(rbind,lapply(c(0.8,0.7,0.9),function(s){
  d = SE_knots(data,s,outdir)
  d$SE = paste0("knots",s)
  return(d)
  print(paste0(s,"has been processed"))
}))
ydf = do.call(rbind,lapply(c(2,3),function(s){
  d = SE_yeardf(data,s,outdir)
  d$SE = paste0("yearDF",s)
  return(d)
  print(paste0(s,"has been processed"))
}))

wdf = do.call(rbind,lapply(c(2,4),function(s){
  d = SE_weekdf(data,s,outdir)
  d$SE = paste0("weekDF",s)
  return(d)
  print(paste0(s,"has been processed"))
}))



all = do.call(rbind,list(knotdf,ydf,wdf))
all$var = "DS"
all$var[which(all$SE=="knots0.7")] = "S1"
all$var[which(all$SE=="knots0.9")] = "S2"
all$var[which(all$SE=="yearDF2")] = "S3"
all$var[which(all$SE=="yearDF3")] = "S4"
all$var[which(all$SE=="weekDF2")] = "S5"
all$var[which(all$SE=="weekDF4")] = "S6"

g = ggplot(all) +
  geom_point(aes(x = humidex, y =  RR,color = var),
             show.legend = TRUE,alpha=0.5,size=0.8,
             position= position_dodge(width = 0.95))+
  geom_linerange(aes(x = humidex, ymin = low, ymax = high,color = var),
                 position=position_dodge(width = 0.95),show.legend = FALSE,
                 linewidth=0.5,
                 alpha = 0.6) +
  scale_color_manual(values = c("#001f22","#7ab8cc","#00808c","#ff7300","#ffcc00",
                                "#605276","#e63995","#769701","#228b22"))+
  theme_bw()+ 
  #geom_vline(show.legend = TRUE,aes(xintercept = -3, color=var),alpha=0.6)+
  scale_x_continuous(expand = c(0.01,0.01),limits = c(9,41))+
  scale_y_continuous(expand = c(0,0),limits = c(0.9,1.6))+
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

outdir = "Figures"
ggsave(paste0(outdir,"/figS5b.pdf"),g, width=15.5, height=10, units="cm", scale=1)


