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
  modelname = paste0(outdir,"/stratamodel_win14_",data$age[1],"_ns_hum95th_season.rds")
  if(!file.exists(modelname)){
    data$death_rate[which(data$death_rate==0)]=0.000001
    data$age = as.factor(data$age)
    data$gender_group = paste(data$geo,data$Year)
    data$IHD = data$Heatday_95th-data$CHD_95th
    data$IHN = data$Heatnight_95th-data$CHN_95th
    data$IHA = data$Heatall_95th-data$CH_95th
    data$season = ifelse(data$Week<=29,1,2)
    cb= crossbasis(data$Hum,
                   lag=lag,
                   argvar = list(fun="ns",knots =  quantile(data$Hum,c(80)/100,na.rm=T), 
                                 Boundary.knots = range(data$Hum,na.rm=T)),
                   arglag= list(knots = logknots(lag, lagnk)),
                   group = data$gender_group)
    
    CDD = onebasis(data$CHD_95th,fun="strata",breaks=c(1,2,4))
    CDN = onebasis(data$CHN_95th,fun="strata",breaks=c(1,2,4))
    CDA = onebasis(data$CH_95th,fun="strata",breaks=c(1,2,4))
    UDD = onebasis(data$IHD,fun="strata",breaks=c(1,2,4))
    UDN = onebasis(data$IHN,fun="strata",breaks=c(1,2,4))
    UDA = onebasis(data$IHA,fun="strata",breaks=c(1,2,4))
    
    model <- glm(death ~ cb + CDD + CDN+ CDA + UDD + UDN+ UDA + ns(Year, df = 4)+ ns(Week, df = 3)+ geo +log(pop)+season,
                 family = quasipoisson(link="log"), data = data, na.action="na.exclude")
    saveRDS(model,modelname)
  }
  return(data)
}
indata = fread(file.path(indir,paste0("Input_data_window14.csv")))
filter_list<- filter_geo(indata$geo)

pop = pop[age_group!="TOTAL",]
indata= indata[indata$geo %in% filter_list, ]
indata =indata[age!="TOTAL",]
indata = indata[complete.cases(indata),]
indata = merge(indata,pop,
               by.x = c("Year","geo","age"),
               by.y = c("Year","geo","age_group"))
indata$death_rate = indata$death/indata$pop*10000
indata = indata[Year>=2010&Year<=2019,]
gc()
outdir = "Model"
k = lapply(split(indata,indata$age),function(data){
  lag <- 4
  lagnk <-2
  #######################################
  d  = model_train(data,outdir)
  gc()
  print(unique(data$age))
  return(d)
})


seex = function(data,modelname){
  lag <- 4
  lagnk <-2
  data$death_rate[which(data$death_rate==0)]=0.000001
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  model <-  readRDS(modelname)
  d = data.frame(age = unique(data$age),season_effect = coef(model)["season"])
  return(d)
}

df1 = do.call(rbind,lapply(split(indata,indata$age),function(data){
    modelname = paste0(outdir,"/stratamodel_win14_",data$age[1],"_ns_hum95th_season.rds")
    return(seex(data,modelname))
}))


df1$effect = exp(df1$season_effect)





df = rbindlist(figdf)
df$weeklabel = as.factor(df$weeklabel)
g = ggplot(df) +
  geom_line(aes(x = humidex, y = RR,color = weeklabel),size=0.4) +
  geom_ribbon(aes(x = humidex, ymin = low, ymax = high, fill = weeklabel), alpha = 0.2) +
  facet_wrap(weeklabel~age, nrow = 2) +
  theme_bw() +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),limits=c(0.70,2.0))+
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.x= element_text(color="black",hjust = 0.5,vjust=0,size =unit(7,"pt")),
        axis.text.x = element_text(color="black",size = unit(7,"pt")),
        axis.title.y= element_text(color="black",hjust = 0.5,vjust=0,size =unit(7,"pt")),
        axis.text.y = element_text(color="black",size = unit(7,"pt")),
        legend.position="bottom"
  ) +
  labs(
    x="Weekly average Humidex",
    y="Relative risk (RR)"
  )



















