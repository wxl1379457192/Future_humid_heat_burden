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
model_train = function(data,outdir){
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  data$IHD = data$Heatday-data$CHD
  data$IHN = data$Heatnight-data$CHN
  data$IHA = data$Heatall-data$CH
  
  cb= crossbasis(data$Hum,
                 lag=lag,
                 argvar = list(fun="ns",knots =  quantile(data$Hum,c(80)/100,na.rm=T), 
                               Boundary.knots = range(data$Hum,na.rm=T)),
                 arglag= list(fun = "integer"),
                 group = data$gender_group)
  
  CDD = onebasis(data$CHD,fun="strata",breaks=c(1,2,4))
  CDN = onebasis(data$CHN,fun="strata",breaks=c(1,2,4))
  CDA = onebasis(data$CH,fun="strata",breaks=c(1,2,4))
  UDD = onebasis(data$IHD,fun="strata",breaks=c(1,2,4))
  UDN = onebasis(data$IHN,fun="strata",breaks=c(1,2,4))
  UDA = onebasis(data$IHA,fun="strata",breaks=c(1,2,4))
  model <- glm(death ~ cb + CDD + CDN+ CDA + UDD + UDN+ UDA + ns(Year, df = 4)+ ns(Week, df = 3)+ geo +log(pop),
               family = quasipoisson(link="log"), data = data, na.action="na.exclude")
  data$predictions <-predict(model,newdata = data,type = "response")
  saveRDS(model,paste0(outdir,"/stratamodel_",data$age[1],"_ns_hum_lagfun.rds"))
  return(data)
}

pop = fread(file.path("D:/ATtest/Europe_version2",indir,"Aux_pop_data_V2.csv"))
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

#####################################################
data2 = subset(indata,indata$age=="15-65")
data2 = data2[Year>=2010&Year<=2019,]
k2 = model_train(data2,outdir)
gc()
print(unique(data2$age))
##############model for age 0-15#####################
data3 = subset(indata,indata$age=="0-15")
data3 = data3[Year>=2010&Year<=2019,]
k3 = model_train(data3,outdir)
gc()
print(unique(data3$age))

#######################################
data = subset(indata,indata$age=="65+")
data = data[Year>=2010&Year<=2019,]
k1 = model_train(data,outdir)
gc()
print(unique(data$age))












