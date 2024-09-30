rm(list = ls())
gc()
###########################################
setwd("D:/ATtest/Europe_version2")
library(dlnm)
library(splines)
library(ggplot2)
library(reshape2)
library(patchwork)
library(zoo)
library(Epi)
library(data.table)
outdir = "Figure_0721"
if (dir.exists(outdir)){
  print("Output dir has existed!")
}else{
  dir.create(outdir)
}
indir = "Input_model_data"
pop = fread(file.path(indir,"Aux_pop_data.csv"))
modeldir = "Model"
#######################################
#######################################
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
indata = fread(file.path(indir,paste0("Input_data_window14.csv")))
filter_list<- filter_geo(indata$geo)

thre = rbindlist(lapply(seq(2010,2022),function(y){
  threname = file.path("heat_threshold",paste0("heat_threshold_",y,".csv"))
  thd = fread(threname) 
  thd = thd[thd$NUTS_ID %in% filter_list, ]
  thd = thd[which(thd$window==14),]
  thd$Date = as.Date(paste0(thd$year,"-",thd$Day))
  thd$week <- format(thd$Date, "%G-W%V")
  Weeklythd = thd%>%group_by(NUTS_ID,week)%>%summarise(Humidex_95th  = mean(Humidex_95th))
  Weeklythd$weeknum = as.numeric(substring(Weeklythd$week,7,8))
  Weeklythd = Weeklythd[which(Weeklythd$weeknum>=22&Weeklythd$weeknum<=35),]
  return(Weeklythd)
}))

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
daall$IHD = daall$Heatday_95th-daall$CHD_95th
daall$IHN = daall$Heatnight_95th-daall$CHN_95th
daall$IHA = daall$Heatall_95th-daall$CH_95th

heatdeath = list()

for (a in agelist){
  data = daall[which(age==a&Year>=2010),]
  modeldir = paste0("Model/stratamodel_win14_",a,"_ns_hum95th.rds")
  model = readRDS(modeldir)
  lag = 4
  lagnk = 2
  
  da1 = data[which(Year>=2010&Year<=2019),]
  cb =  crossbasis(da1$Hum,lag=lag,
                   argvar = list(fun="ns",knots =  quantile(da1$Hum,c(80)/100,na.rm=T), 
                                 Boundary.knots = range(da1$Hum,na.rm=T)),
                   arglag= list(knots = logknots(lag, lagnk)),
                   group =da1$gender_group)
  red <- crosspred(cb,model,at=data$Hum,model.link = "log")
  
  ff =data.frame(cbind(red$predvar,red$allfit,red$allfit-1.96*red$allse,red$allfit+1.96*red$allse)) 
  
  
  colnames(ff) <- c("Hum", "rr","rr.low","rr.high")
  
  CDD = onebasis(data$CHD_95th,fun="strata",breaks=c(1,2,4))
  CDN = onebasis(data$CHN_95th,fun="strata",breaks=c(1,2,4))
  CDA = onebasis(data$CH_95th,fun="strata",breaks=c(1,2,4))
  
  CDDpre = crosspred(CDD,model,
                     at = seq(0,7),cen=0,model.link = "log")
  CDD_f = data.frame(cbind(as.integer(CDDpre$predvar),CDDpre$allfit, CDDpre$allfit-1.96*CDDpre$allse,CDDpre$allfit+1.96*CDDpre$allse)) 
  colnames(CDD_f) <- c("CDD_num", "rr.cdd","rr.cdd.low","rr.cdd.high")
  CDNpre = crosspred(CDN,model,
                     at = seq(0,7),cen=0,model.link = "log")#,model.link = "log"
  CDN_f = data.frame(cbind(as.integer(CDNpre$predvar),CDNpre$allfit, CDNpre$allfit-1.96*CDNpre$allse,CDNpre$allfit+1.96*CDNpre$allse)) 
  colnames(CDN_f) <- c("CDN_num", "rr.cdn","rr.cdn.low","rr.cdn.high")
  
  CDApre = crosspred(CDA,model,
                     at = seq(0,7),cen=0,model.link = "log")#,model.link = "log"
  CDA_f = data.frame(cbind(as.integer(CDApre$predvar),CDApre$allfit, CDApre$allfit-1.96*CDApre$allse,CDApre$allfit+1.96*CDApre$allse)) 
  colnames(CDA_f) <- c("CDA_num", "rr.cda","rr.cda.low","rr.cda.high")
  preo = merge(data,ff,by = "Hum")
  preo = merge(preo,CDD_f,by.x="CHD_95th",by.y = "CDD_num")
  preo = merge(preo,CDN_f,by.x="CHN_95th",by.y = "CDN_num")
  preo = merge(preo,CDA_f,by.x="CH_95th",by.y = "CDA_num")
  preo$morrisk = exp(preo$rr+preo$rr.cdd+preo$rr.cdn+preo$rr.cda)-1
  preo$morrisk.low = exp(preo$rr.low+preo$rr.cdd.low+preo$rr.cdn.low+preo$rr.cda.low)-1
  preo$morrisk.high = exp(preo$rr.high+preo$rr.cdd.high+preo$rr.cdn.high+preo$rr.cda.high)-1
  preo$morrisk[which(preo$morrisk<0)] = 0
  preo$morrisk.low[which(preo$morrisk.low<0)] = 0
  preo$morrisk.high[which(preo$morrisk.high<0)] = 0
  preo$heat_predicted = preo$morrisk*preo$death
  preo$heat_predicted.low = preo$morrisk.low*preo$death
  preo$heat_predicted.high = preo$morrisk.high*preo$death
  heatdeath[[a]] = preo[which(Week>=22&Week<=35),]
}


preda =  rbindlist(heatdeath)
preda = preda[,c("week","Hum","geo","age","heat_predicted","heat_predicted.low","heat_predicted.high")]
colnames(thre)[1] = "geo"
preda = merge(preda,thre,by=c("week","geo"))
preda$morthre = ifelse(preda$age=="65+",18,ifelse(preda$age=="15-65",14,-1))

preda$type = ifelse(preda$Hum>=preda$morthre&preda$Hum<preda$Humidex_95th,"Moderate",
                    ifelse(preda$Hum>=preda$Humidex_95th,"Extreme",
                    "None"))
preda$num = 1
preda$year = as.numeric(substring(preda$week,1,4))

result = preda%>%group_by(year,type,age)%>%summarise(mortality = sum(heat_predicted),
                                                     low = sum(heat_predicted.low),
                                                     high = sum(heat_predicted.high))
hum = preda%>%group_by(year)%>%summarise(
       Humidex =mean(Hum),Hum.low=min(Hum),Hum.high=max(Hum))
result = result[which(result$type!="None"),]
result$mortality = round(result$mortality,0)
result$low = round(result$low,0)
result$high = round(result$high,0)
result$Humidex =round(result$Humidex,2)
result$Hum.low =round(result$Hum.low,2)
result$Hum.high =round(result$Hum.high,2)
result$range.Hum = paste0(result$Hum.low,"-",result$Hum.high)
result$range.death = paste0(result$low,"-",result$high)
write.csv(result,paste0(outdir,"/heat_related_mortality_attribute.csv"),row.names = F)


r2= preda%>%group_by(year,type)%>%summarise(
  mortality = sum(heat_predicted),
  low = sum(heat_predicted.low),
  high = sum(heat_predicted.high))

r2 = r2[which(r2$type!="None"),]



preda$country = substring(preda$geo,1,2)
rc = preda%>%group_by(year,type,country)%>%summarise(
  mortality = sum(heat_predicted),
  low = sum(heat_predicted.low),
  high = sum(heat_predicted.high))
label = data.frame(
  country = c("AL", "AT", "BE", "BG", "CH", "CY", "CZ", "DK", "EE", "EL", "ES", "FI", "FR", "HU", "IS", "IT", "LI", "LT", "LU", "LV",
              "ME", "NL", "NO", "PL", "PT", "RO", "RS", "SE", "SK", "UK"),
  Country_name = c("Albania", "Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Greece",
                   "Spain", "Finland", "France", "Hungary", "Iceland", "Italy", "Liechtenstein", "Lithuania", "Luxembourg", "Latvia",
                   "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Serbia", "Sweden", "Slovakia", "United Kingdom"),
  geo = c("Southern Europe", "Western Europe", "Western Europe", "Eastern Europe", "Western Europe", "Southern Europe", "Eastern Europe", "Northern Europe", "Northern Europe", "Southern Europe",
          "Southern Europe", "Northern Europe", "Western Europe", "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe", "Northern Europe", "Western Europe", "Northern Europe",
          "Eastern Europe", "Western Europe", "Northern Europe", "Eastern Europe", "Southern Europe", "Eastern Europe", "Eastern Europe", "Northern Europe", "Eastern Europe", "Northern Europe")
)
rc = merge(rc,label,by="country")

rcspain = rc[which(rc$Country_name=="Spain"),]




