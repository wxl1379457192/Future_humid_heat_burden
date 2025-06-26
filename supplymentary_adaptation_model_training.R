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
model_train = function(data,outdir){
  lag <- 4
  lagnk <-2
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  data$IHD = data$Heatday-data$CHD
  data$IHN = data$Heatnight-data$CHN
  data$IHA = data$Heatall-data$CH
  
  cb= crossbasis(data$Hum,
                 lag=lag,
                 argvar = list(fun="ns",knots =  quantile(data$Hum,c(80)/100,na.rm=T), 
                               Boundary.knots = range(data$Hum,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
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
  saveRDS(model,paste0(outdir,"/stratamodel_",min(data$Year),"_",max(data$Year),"_",data$age[1],"_ns.rds"))
  return(data)
}

yearly_train = function(indata,outdir){
  data = subset(indata,indata$age=="65+")
  k1 = model_train(data,outdir)
  gc()
  print(unique(data$age))
  #############################
  data2 = subset(indata,indata$age=="15-65")
  k2 = model_train(data2,outdir)
  gc()
  print(unique(data2$age))
  ##############model for age 0-15#####################
  data3 = subset(indata,indata$age=="0-15")
  k3 = model_train(data3,outdir)
  gc()
  print(unique(data3$age))
  
  k = rbindlist(list(k1,k2,k3))
  fwrite(k,paste0(outdir,"/Inputdata_",min(indata$Year),
                  "_",max(indata$Year),".csv"))
  
}

#######################################

indata = fread(file.path(indir,paste0("Input_data_all_v2.csv")))
filter_list<- filter_geo(indata$geo)
indata =indata[age!="TOTAL",]
pop = pop[age_group!="TOTAL",]
indata= indata[indata$geo %in% filter_list, ]

indata = merge(indata,pop,
               by.x = c("Year","geo","age"),
               by.y = c("Year","geo","age_group"))
indata = indata[complete.cases(indata),]
indata = indata[pop>0]
gc()
library(dplyr)

indata$year = as.factor(indata$Year)

year_count <- indata %>%
  group_by(geo) %>%
  summarise(years_present = n_distinct(year)) %>%
  filter(years_present > 20)  
valid_geo <- year_count$geo

ind_complete_years <- indata[indata$geo %in% valid_geo, ]


outdir = paste0("Model_supply_10year")
dir.create(outdir)
yearly_train(ind_complete_years[Year>=2000&Year<=2009,],outdir)
yearly_train(ind_complete_years[Year>=2001&Year<=2010,],outdir)
yearly_train(ind_complete_years[Year>=2002&Year<=2011,],outdir)
yearly_train(ind_complete_years[Year>=2003&Year<=2012,],outdir)
yearly_train(ind_complete_years[Year>=2004&Year<=2013,],outdir)
yearly_train(ind_complete_years[Year>=2005&Year<=2014,],outdir)
yearly_train(ind_complete_years[Year>=2006&Year<=2015,],outdir)
yearly_train(ind_complete_years[Year>=2007&Year<=2016,],outdir)
yearly_train(ind_complete_years[Year>=2008&Year<=2017,],outdir)
yearly_train(ind_complete_years[Year>=2009&Year<=2018,],outdir)
yearly_train(ind_complete_years[Year>=2010&Year<=2019,],outdir)




outdir = paste0("Model_supply_5year")
dir.create(outdir)
yearly_train(ind_complete_years[Year>=2000&Year<=2004,],outdir)
yearly_train(ind_complete_years[Year>=2005&Year<=2009,],outdir)
yearly_train(ind_complete_years[Year>=2010&Year<=2014,],outdir)
yearly_train(ind_complete_years[Year>=2015&Year<=2019,],outdir)






























