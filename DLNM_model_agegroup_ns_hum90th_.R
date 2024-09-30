rm(list = ls())
gc()
setwd("D:/ATtest/Europe_version2")

library(dlnm)
library(splines)
library(ggplot2)
library(reshape2)
library(patchwork)
library(zoo)
library(Epi)
library(data.table)
indir = "Input_model_data"
pop = fread(file.path(indir,"Aux_pop_data.csv"))
# 定义一个函数来筛选 geo 字段
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
model_train = function(data,outdir){
  data$death_rate[which(data$death_rate==0)]=0.000001
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  data$IHD = data$Heatday_90th-data$CHD_90th
  data$IHN = data$Heatnight_90th-data$CHN_90th
  data$IHA = data$Heatall_90th-data$CH_90th
  
  cb= crossbasis(data$Hum,
                 lag=lag,
                 argvar = list(fun="ns",knots =  quantile(data$Hum,c(80)/100,na.rm=T), 
                               Boundary.knots = range(data$Hum,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group = data$gender_group)
  
  CDD = onebasis(data$CHD_90th,fun="strata",breaks=c(1,2,4))
  CDN = onebasis(data$CHN_90th,fun="strata",breaks=c(1,2,4))
  CDA = onebasis(data$CH_90th,fun="strata",breaks=c(1,2,4))
  UDD = onebasis(data$Heatday_90th-data$CHD_90th,fun="strata",breaks=c(1,2,4))
  UDN = onebasis(data$Heatnight_90th - data$CHN_90th,fun="strata",breaks=c(1,2,4))
  UDA = onebasis(data$Heatall_90th - data$CH_90th,fun="strata",breaks=c(1,2,4))

  model <- glm(death ~ cb + CDD + CDN+ CDA + UDD +ns(Year, df = 4)+ ns(Week, df = 3)+ geo +log(pop),
               family = quasipoisson(link="log"), data = data, na.action="na.exclude")
  data$predictions <-predict(model,newdata = data,type = "response")
  saveRDS(model,paste0(outdir,"/stratamodel_win",data$window[1],"_",data$age[1],"_ns_hum95th.rds"))
  return(data)
}

i = 14
indata = fread(file.path(indir,paste0("Input_data_window",i,".csv")))
filter_list<- filter_geo(indata$geo)

pop = pop[age_group!="TOTAL",]
indata= indata[indata$geo %in% filter_list, ]
indata =indata[age!="TOTAL",]
indata = indata[complete.cases(indata),]
indata = merge(indata,pop,
               by.x = c("Year","geo","age"),
               by.y = c("Year","geo","age_group"))
indata$death_rate = indata$death/indata$pop*10000
gc()

lag <- 4
lagnk <-2
outdir = "Model"
dir.create(outdir)
#######################################
data = subset(indata,indata$age=="65+")
data = data[Year>=2010&Year<=2019,]
k1 = model_train(data,outdir)
gc()
print(unique(data$age))
#############################
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
print(paste("Window",i, "has been processed!"))

k = rbindlist(list(k1,k2,k3))
fwrite(k,paste0(outdir,"/Inputdata_win",data$window[1],"_hum90th.csv") )
