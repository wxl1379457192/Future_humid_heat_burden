rm(list = ls())
gc()
setwd("D:/ATtest/Europe_version3/")
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
figout = "Figures"
indir = "Heatmor_prediction_future_baseline"
filelist = list.files(path = indir,pattern = "\\.csv$",full.names=TRUE)
# 打印文件读取进度的函数
print_progress <- function(file) {
  message("Reading file: ", file)
  return(fread(file))
}

newdir = "Heatmor_prediction_future_baseline_agg"
if (dir.exists(newdir)){
  print("Output dir has existed!")
}else{
  dir.create(newdir)
}
premor <- do.call(rbind,lapply(filelist[500:length(filelist)],function(k){
  f = print_progress(k)
  filename = file.path(newdir,strsplit(k,"/")[[1]][2])
  if (!file.exists(filename)){
    if(is.numeric(f$Pre_ssp1_BSL)==T){
      
      f1 = f%>%group_by(rcp,Year,geo,age)%>%
        summarise(Humidex = mean(Humidex_mean),death_base = mean(death_base),
                  pop_2022_BSL = mean(pop_2022_BSL),pop_2022_BR2022 = mean(pop_2022_BR2022),
                  pop_base = mean(pop_base),
                  pop_ssp1_BSL = mean(pop_ssp1_BSL),pop_ssp2_BSL = mean(pop_ssp2_BSL),
                  pop_ssp3_BSL = mean(pop_ssp3_BSL),pop_ssp5_BSL = mean(pop_ssp5_BSL),
                  pop_ssp1_BR2022 = mean(pop_ssp1_BR2022),pop_ssp2_BR2022 = mean(pop_ssp2_BR2022),
                  pop_ssp3_BR2022 = mean(pop_ssp3_BR2022),pop_ssp5_BR2022 = mean(pop_ssp5_BR2022),
                  Pre_ssp1_BSL_low= quantile(Pre_ssp1_BSL,0.25), Pre_ssp1_BSL_up= quantile(Pre_ssp1_BSL,0.75),
                  Pre_ssp1_BSL =quantile(Pre_ssp1_BSL,0.5), 
                  Pre_ssp2_BSL_low= quantile(Pre_ssp2_BSL,0.25), Pre_ssp2_BSL_up= quantile(Pre_ssp2_BSL,0.75),
                  Pre_ssp2_BSL = quantile(Pre_ssp2_BSL,0.5), 
                  Pre_ssp3_BSL_low= quantile(Pre_ssp3_BSL,0.25), Pre_ssp3_BSL_up= quantile(Pre_ssp3_BSL,0.75),
                  Pre_ssp3_BSL = quantile(Pre_ssp3_BSL,0.5), 
                  Pre_ssp5_BSL_low= quantile(Pre_ssp5_BSL,0.25), Pre_ssp5_BSL_up= quantile(Pre_ssp5_BSL,0.75),
                  Pre_ssp5_BSL = quantile(Pre_ssp5_BSL,0.5), 
                  Pre_ssp1_BR2022_low= quantile(Pre_ssp1_BR2022,0.25), Pre_ssp1_BR2022_up= quantile(Pre_ssp1_BR2022,0.75),#以下为年龄比例保持在2022不变
                  Pre_ssp1_BR2022 = quantile(Pre_ssp1_BR2022,0.5), 
                  Pre_ssp2_BR2022_low= quantile(Pre_ssp2_BR2022,0.25), Pre_ssp2_BR2022_up= quantile(Pre_ssp2_BR2022,0.75),
                  Pre_ssp2_BR2022 = quantile(Pre_ssp2_BR2022,0.5), 
                  Pre_ssp3_BR2022_low= quantile(Pre_ssp3_BR2022,0.25), Pre_ssp3_BR2022_up= quantile(Pre_ssp3_BR2022,0.75),
                  Pre_ssp3_BR2022 = quantile(Pre_ssp3_BR2022,0.5), 
                  Pre_ssp5_BR2022_low= quantile(Pre_ssp5_BR2022,0.25), Pre_ssp5_BR2022_up= quantile(Pre_ssp5_BR2022,0.75),
                  Pre_ssp5_BR2022 = quantile(Pre_ssp5_BR2022,0.5), 
                  Pre_2022_BSL_low= quantile(Pre_2022_BSL,0.25), Pre_2022_BSL_up= quantile(Pre_2022_BSL,0.75),#以下为人口总数不变
                  Pre_2022_BSL = quantile(Pre_2022_BSL,0.5),
                  Base_ssp1_BSL_low= quantile(Base_ssp1_BSL,0.25), Base_ssp1_BSL_up= quantile(Base_ssp1_BSL,0.75),#以下为气候态不变
                  Base_ssp1_BSL =quantile(Base_ssp1_BSL,0.5), 
                  Base_ssp2_BSL_low= quantile(Base_ssp2_BSL,0.25), Base_ssp2_BSL_up= quantile(Base_ssp2_BSL,0.75),
                  Base_ssp2_BSL =quantile(Base_ssp2_BSL,0.5), 
                  Base_ssp3_BSL_low= quantile(Base_ssp3_BSL,0.25), Base_ssp3_BSL_up= quantile(Base_ssp3_BSL,0.75),
                  Base_ssp3_BSL =quantile(Base_ssp3_BSL,0.5), 
                  Base_ssp5_BSL_low= quantile(Base_ssp5_BSL,0.25), Base_ssp5_BSL_up= quantile(Base_ssp5_BSL,0.75),
                  Base_ssp5_BSL =quantile(Base_ssp5_BSL,0.5), 
                  Base_ssp1_BR2022_low= quantile(Base_ssp1_BR2022,0.25), Base_ssp1_BR2022_up= quantile(Base_ssp1_BR2022,0.75),#以下为气候态和年龄分层都不变
                  Base_ssp1_BR2022 =  quantile(Base_ssp1_BR2022,0.5), 
                  Base_ssp2_BR2022_low= quantile(Base_ssp2_BR2022,0.25), Base_ssp2_BR2022_up= quantile(Base_ssp2_BR2022,0.75),
                  Base_ssp2_BR2022 = quantile(Base_ssp2_BR2022,0.5), 
                  Base_ssp3_BR2022_low= quantile(Base_ssp3_BR2022,0.25), Base_ssp3_BR2022_up= quantile(Base_ssp3_BR2022,0.75),
                  Base_ssp3_BR2022 =quantile(Base_ssp3_BR2022,0.5), 
                  Base_ssp5_BR2022_low= quantile(Base_ssp5_BR2022,0.25), Base_ssp5_BR2022_up= quantile(Base_ssp5_BR2022,0.75),
                  Base_ssp5_BR2022 =  quantile(Base_ssp5_BR2022,0.5), 
                  Pre_2022_low= quantile(Pre_2022,0.25), Pre_2022_up= quantile(Pre_2022,0.75),#以下为人口总数不变，年龄分层也不变
                  Pre_2022 =  quantile(Pre_2022,0.5),
                  Base_2022_BSL_low= quantile(Base_2022_BSL,0.25), Base_2022_BSL_up= quantile(Base_2022_BSL,0.75),#以下为气候态和人口总数不变
                  Base_2022_BSL = quantile(Base_2022_BSL,0.5),
                  Base_2022 = mean(Base_2022)#均不变
        )
      fwrite(f1,filename,row.names = F)
    }
    else{f1=NULL}
  }
  else{
    f1 = fread(filename)
  }
  return(f1)
}))
fwrite(premor,file.path("Result","Heatmor_prediction_future_baseline.csv"),row.names = F)


premor = fread(file.path("Result","Heatmor_prediction_future_baseline.csv"),stringsAsFactors = F)
warming = read.csv(file.path("D:/ATtest/Europe_version2/Result","Global_warming.csv"),stringsAsFactors =F)
base = read.csv(file.path("Result","Mortality_prediction_history.csv"),stringsAsFactors = F)
premor$country = substring(premor$geo,1,2)
colnames(premor)[3] = "geom"
label = data.frame(
  country = c("AL", "AT", "BE", "BG", "CH",
              "CY", "CZ","DE", "DK", "EE",
              "EL", "ES", "FI", "FR", "HR",
              "HU", "IS", "IT", "LI", "LT", 
              "LU", "LV","ME", "MT","NL", 
              "NO", "PL", "PT", "RO", "RS", 
              "SE", "SI","SK", "UK"),
  Country_name = c("Albania", "Austria", "Belgium", "Bulgaria", "Switzerland",
                   "Cyprus", "Czech Republic", "Germany","Denmark", "Estonia", 
                   "Greece","Spain", "Finland", "France", "Croatia",
                   "Hungary", "Iceland", "Italy", "Liechtenstein", "Lithuania",
                   "Luxembourg","Latvia","Montenegro","Malta", "Netherlands",
                   "Norway", "Poland", "Portugal", "Romania", "Serbia",
                   "Sweden", "Slovenia","Slovakia", "United Kingdom"),
  geo = c("Southern Europe", "Western Europe", "Western Europe", "Eastern Europe", "Western Europe", 
          "Southern Europe", "Eastern Europe", "Western Europe","Northern Europe", "Northern Europe", 
          "Southern Europe","Southern Europe", "Northern Europe", "Western Europe","Southern Europe",
          "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe", "Northern Europe",
          "Western Europe", "Northern Europe","Eastern Europe", "Southern Europe","Western Europe", 
          "Northern Europe", "Eastern Europe", "Southern Europe", "Eastern Europe", "Eastern Europe", 
          "Northern Europe",  "Southern Europe","Eastern Europe", "Northern Europe")
)
premor = merge(premor,label,by="country")
premor = merge(premor,warming,by.x=c("Year","rcp"),by.y =c("Year","Scenario"))
premor$warming_level = round(premor$warming,1)

nsamples = do.call(rbind,lapply(split(premor,premor$geo),function(g){
  data.frame(geo = unique(g$geo),geom_num = length(unique(g$geom)))
}))
########计算不同升温水平下的平均死亡率###########

spag = premor%>%group_by(rcp,Year,warming_level,geom)%>%
  summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL)/sum(pop_ssp1_BSL)*1000000, 
            Pre_ssp1_BSL_low= sum(Pre_ssp1_BSL_low)/sum(pop_ssp1_BSL)*1000000, 
            Pre_ssp1_BSL_up=sum(Pre_ssp1_BSL_up)/sum(pop_ssp1_BSL)*1000000,
            Pre_ssp2_BSL =sum(Pre_ssp2_BSL)/sum(pop_ssp2_BSL)*1000000, 
            Pre_ssp2_BSL_low= sum(Pre_ssp2_BSL_low)/sum(pop_ssp2_BSL)*1000000,
            Pre_ssp2_BSL_up= sum(Pre_ssp2_BSL_up)/sum(pop_ssp2_BSL)*1000000,
            Pre_ssp3_BSL= sum(Pre_ssp3_BSL)/sum(pop_ssp3_BSL)*1000000,
            Pre_ssp3_BSL_low= sum(Pre_ssp3_BSL_low)/sum(pop_ssp3_BSL)*1000000,
            Pre_ssp3_BSL_up= sum(Pre_ssp3_BSL_up)/sum(pop_ssp3_BSL)*1000000,
            Pre_ssp5_BSL= sum(Pre_ssp5_BSL)/sum(pop_ssp5_BSL)*1000000,
            Pre_ssp5_BSL_low= sum(Pre_ssp5_BSL_low)/sum(pop_ssp5_BSL)*1000000,
            Pre_ssp5_BSL_up=sum(Pre_ssp5_BSL_up)/sum(pop_ssp5_BSL)*1000000)
spag = spag%>%group_by(rcp,geom,warming_level)%>%
  summarise(Pre_ssp1_BSL = mean(Pre_ssp1_BSL), 
            Pre_ssp1_BSL_low= mean(Pre_ssp1_BSL_low), 
            Pre_ssp1_BSL_up= mean(Pre_ssp1_BSL_up),
            Pre_ssp2_BSL = mean(Pre_ssp2_BSL), 
            Pre_ssp2_BSL_low= mean(Pre_ssp2_BSL_low),
            Pre_ssp2_BSL_up= mean(Pre_ssp2_BSL_up),
            Pre_ssp3_BSL= mean(Pre_ssp3_BSL),
            Pre_ssp3_BSL_low= mean(Pre_ssp3_BSL_low),
            Pre_ssp3_BSL_up= mean(Pre_ssp3_BSL_up),
            Pre_ssp5_BSL= mean(Pre_ssp5_BSL),
            Pre_ssp5_BSL_low= mean(Pre_ssp5_BSL_low),
            Pre_ssp5_BSL_up= mean(Pre_ssp5_BSL_up))

name = c("Pre_ssp1_BSL","Pre_ssp2_BSL",
         "Pre_ssp3_BSL","Pre_ssp5_BSL")
warming_levels = c(1.5,2.0,2.5,3.0,3.5,4.0)
for (ydr in split(spag,spag$rcp)){
  n = grep(substring(ydr$rcp[1],1,4), name, value = TRUE)
  k = ydr[,c("geom","warming_level",n)]
  for (w in warming_levels){
    k1 = k[which(k$warming_level==w),]
    write.csv(k1,file.path(figout,paste0("Predicted_total_heat_deaths_",
                                         unique(ydr$rcp),"_Warming_",w,".csv")))
  }
}
##############Fig 3已经可以出图##########
#######################加载不同升温水平下每个国家的热死亡人数预测###################
warming_deaths_geo = premor%>%group_by(rcp,Year,warming_level,country,Country_name)%>%
  summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL)/sum(pop_ssp1_BSL)*1000000, 
            Pre_ssp1_BSL_low= sum(Pre_ssp1_BSL_low)/sum(pop_ssp1_BSL)*1000000, 
            Pre_ssp1_BSL_up=sum(Pre_ssp1_BSL_up)/sum(pop_ssp1_BSL)*1000000,
            Pre_ssp2_BSL =sum(Pre_ssp2_BSL)/sum(pop_ssp2_BSL)*1000000, 
            Pre_ssp2_BSL_low= sum(Pre_ssp2_BSL_low)/sum(pop_ssp2_BSL)*1000000,
            Pre_ssp2_BSL_up= sum(Pre_ssp2_BSL_up)/sum(pop_ssp2_BSL)*1000000,
            Pre_ssp3_BSL= sum(Pre_ssp3_BSL)/sum(pop_ssp3_BSL)*1000000,
            Pre_ssp3_BSL_low= sum(Pre_ssp3_BSL_low)/sum(pop_ssp3_BSL)*1000000,
            Pre_ssp3_BSL_up= sum(Pre_ssp3_BSL_up)/sum(pop_ssp3_BSL)*1000000,
            Pre_ssp5_BSL= sum(Pre_ssp5_BSL)/sum(pop_ssp5_BSL)*1000000,
            Pre_ssp5_BSL_low= sum(Pre_ssp5_BSL_low)/sum(pop_ssp5_BSL)*1000000,
            Pre_ssp5_BSL_up=sum(Pre_ssp5_BSL_up)/sum(pop_ssp5_BSL)*1000000)
warming_deaths_geo = warming_deaths_geo%>%group_by(rcp,warming_level,country,Country_name)%>%
  summarise(Pre_ssp1_BSL = mean(Pre_ssp1_BSL), 
            Pre_ssp1_BSL_low= mean(Pre_ssp1_BSL_low), 
            Pre_ssp1_BSL_up= mean(Pre_ssp1_BSL_up),
            Pre_ssp2_BSL = mean(Pre_ssp2_BSL), 
            Pre_ssp2_BSL_low= mean(Pre_ssp2_BSL_low),
            Pre_ssp2_BSL_up= mean(Pre_ssp2_BSL_up),
            Pre_ssp3_BSL= mean(Pre_ssp3_BSL),
            Pre_ssp3_BSL_low= mean(Pre_ssp3_BSL_low),
            Pre_ssp3_BSL_up= mean(Pre_ssp3_BSL_up),
            Pre_ssp5_BSL= mean(Pre_ssp5_BSL),
            Pre_ssp5_BSL_low= mean(Pre_ssp5_BSL_low),
            Pre_ssp5_BSL_up= mean(Pre_ssp5_BSL_up))

name = c("Pre_ssp1_BSL","Pre_ssp2_BSL",
         "Pre_ssp3_BSL","Pre_ssp5_BSL")
warming_deaths_geo = do.call(rbind,lapply(split(warming_deaths_geo,
                                                warming_deaths_geo$country),function(g){
                                                  k1 = do.call(rbind,lapply(split(g,g$rcp),
                                                                            function(g1){
                                                                              n = grep(substring(g1$rcp[1],1,4), name, value = TRUE)
                                                                              k = g1[,c("country","warming_level",n,"Country_name")]  
                                                                              colnames(k)[3] = "value"
                                                                              k$rcp = g1$rcp[1]
                                                                              return(k)
                                                                            }))
                                                }))
wspot = warming_deaths_geo[which(warming_deaths_geo$warming_level==1.5|
                                   warming_deaths_geo$warming_level==4),]

wspotc= wspot[which(wspot$Country_name=="Hungary"|wspot$Country_name=="Italy"|
                      wspot$Country_name=="Greece"|wspot$Country_name=="Bulgaria"|
                      wspot$Country_name=="Romania"),]
for(w in split(wspotc,wspotc$Country_name)){
  w$value =round(w$value,1)
  print(paste(unique(w$Country_name),":",min(w$value),"-",max(w$value)))
}
#####################不同升温水平下整个欧洲的热死亡人数预测###############
warming_deaths_all =  premor%>%group_by(rcp,Year,warming,warming_level)%>%
  summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL)/sum(pop_ssp1_BSL)*1000000, 
            Pre_ssp1_BSL_low= sum(Pre_ssp1_BSL_low)/sum(pop_ssp1_BSL)*1000000, 
            Pre_ssp1_BSL_up=sum(Pre_ssp1_BSL_up)/sum(pop_ssp1_BSL)*1000000,
            Pre_ssp2_BSL =sum(Pre_ssp2_BSL)/sum(pop_ssp2_BSL)*1000000, 
            Pre_ssp2_BSL_low= sum(Pre_ssp2_BSL_low)/sum(pop_ssp2_BSL)*1000000,
            Pre_ssp2_BSL_up= sum(Pre_ssp2_BSL_up)/sum(pop_ssp2_BSL)*1000000,
            Pre_ssp3_BSL= sum(Pre_ssp3_BSL)/sum(pop_ssp3_BSL)*1000000,
            Pre_ssp3_BSL_low= sum(Pre_ssp3_BSL_low)/sum(pop_ssp3_BSL)*1000000,
            Pre_ssp3_BSL_up= sum(Pre_ssp3_BSL_up)/sum(pop_ssp3_BSL)*1000000,
            Pre_ssp5_BSL= sum(Pre_ssp5_BSL)/sum(pop_ssp5_BSL)*1000000,
            Pre_ssp5_BSL_low= sum(Pre_ssp5_BSL_low)/sum(pop_ssp5_BSL)*1000000,
            Pre_ssp5_BSL_up=sum(Pre_ssp5_BSL_up)/sum(pop_ssp5_BSL)*1000000)
warming_deaths_warming_level = warming_deaths_all%>%group_by(rcp,warming_level)%>%
  summarise(Pre_ssp1_BSL = mean(Pre_ssp1_BSL), 
            Pre_ssp1_BSL_low= mean(Pre_ssp1_BSL_low), 
            Pre_ssp1_BSL_up= mean(Pre_ssp1_BSL_up),
            Pre_ssp2_BSL = mean(Pre_ssp2_BSL), 
            Pre_ssp2_BSL_low= mean(Pre_ssp2_BSL_low),
            Pre_ssp2_BSL_up= mean(Pre_ssp2_BSL_up),
            Pre_ssp3_BSL= mean(Pre_ssp3_BSL),
            Pre_ssp3_BSL_low= mean(Pre_ssp3_BSL_low),
            Pre_ssp3_BSL_up= mean(Pre_ssp3_BSL_up),
            Pre_ssp5_BSL= mean(Pre_ssp5_BSL),
            Pre_ssp5_BSL_low= mean(Pre_ssp5_BSL_low),
            Pre_ssp5_BSL_up= mean(Pre_ssp5_BSL_up))
name = c("Pre_ssp1_BSL","Pre_ssp2_BSL",
         "Pre_ssp3_BSL","Pre_ssp5_BSL")
warming_deaths_all =do.call(rbind,lapply(split(warming_deaths_all,warming_deaths_all$rcp),function(g1){
  n = grep(substring(g1$rcp[1],1,4), name, value = TRUE)
  k = g1[,c("warming",n)]  
  colnames(k)[2] = "value"
  k$rcp = g1$rcp[1]
  return(k)
}))

kg = do.call(rbind,lapply(split(warming_deaths_all,warming_deaths_all$rcp),function(grs){
  k = summary(lm(grs$value ~grs$warming))$coefficients[2]
  print(paste("RCP",unique(grs$rcp),k))
}))
warming_deaths_warming_level =do.call(rbind,lapply(split(warming_deaths_warming_level,warming_deaths_warming_level$rcp),function(g1){
  n = grep(substring(g1$rcp[1],1,4), name, value = TRUE)
  k = g1[,c("warming_level",n)]  
  colnames(k)[2] = "value"
  k$rcp = g1$rcp[1]
  return(k)
}))
w2 = warming_deaths_warming_level[which(warming_deaths_warming_level$warming_level==2),]
print(paste("2:",min(w2$value),"-",max(w2$value)))
w4 = warming_deaths_warming_level[which(warming_deaths_warming_level$warming_level==4),]
print(paste("4:",min(w4$value),"-",max(w2$value)))
########################################################
#############用于Result part 2 描述的数据##############
geod = premor%>%group_by(rcp,Year,warming,geo)%>%
  summarise(Pre_ssp1_BSL_low= sum(Pre_ssp1_BSL_low)/sum(pop_ssp1_BSL), Pre_ssp1_BSL_up= sum(Pre_ssp1_BSL_up)/sum(pop_ssp1_BSL),
            Pre_ssp1_BSL = sum(Pre_ssp1_BSL)/sum(pop_ssp1_BSL), 
            Pre_ssp2_BSL_low= sum(Pre_ssp2_BSL_low)/sum(pop_ssp2_BSL), Pre_ssp2_BSL_up= sum(Pre_ssp2_BSL_up)/sum(pop_ssp2_BSL),
            Pre_ssp2_BSL = sum(Pre_ssp2_BSL)/sum(pop_ssp2_BSL), 
            Pre_ssp3_BSL_low= sum(Pre_ssp3_BSL_low)/sum(pop_ssp3_BSL),Pre_ssp3_BSL_up= sum(Pre_ssp3_BSL_up)/sum(pop_ssp3_BSL),
            Pre_ssp3_BSL= sum(Pre_ssp3_BSL)/sum(pop_ssp3_BSL),
            Pre_ssp5_BSL_low= sum(Pre_ssp5_BSL_low)/sum(pop_ssp5_BSL), Pre_ssp5_BSL_up= sum(Pre_ssp5_BSL_up)/sum(pop_ssp5_BSL),
            Pre_ssp5_BSL= sum(Pre_ssp5_BSL)/sum(pop_ssp5_BSL),
            Base=sum(Base_2022)/sum(pop_2022_BR2022))
#计算变化速率的整体基线
prebase =  premor[which(premor$Year==2022),]
prebase2022 = prebase%>%group_by(rcp,warming)%>%
  summarise(Base=sum(Base_2022)/sum(pop_2022_BR2022)*1000000)
print(paste("2022 baseline:",unique(prebase2022$Base),"per million people"))
#计算变化速率的区域基线
base_counrty = prebase%>%group_by(rcp,warming,geo)%>%
  summarise(Base=sum(Base_2022)/sum(pop_2022_BR2022)*1000000)
######################################
#############欧洲各地区热死亡随升温变化速率##############
geok = do.call(rbind,lapply(split(geod,geod$geo),function(gr){
  kg = do.call(rbind,lapply(split(gr,gr$rcp),function(grs){
    coln <- grep(substring(grs$rcp[1],1,4), names(grs), value = TRUE)
    k = data.frame(region=unique(grs$geo),rcp=unique(grs$rcp),
                   rate = round(summary(lm(grs[[coln[3]]] ~ grs$warming))$coefficients[2]*1000000,4),
                   rate.low = round(summary(lm(grs[[coln[1]]] ~ grs$warming))$coefficients[2]*1000000,4),
                   rate.up = round(summary(lm(grs[[coln[2]]] ~ grs$warming))$coefficients[2]*1000000,4))
    return(k)
  }))
  return(kg)
}))
################欧洲各地区热死亡人数随升温变化速率#############
geodd = premor%>%group_by(rcp,Year,warming,geo)%>%
  summarise(Pre_ssp1_BSL_low= sum(Pre_ssp1_BSL_low)/sum(pop_ssp1_BSL), Pre_ssp1_BSL_up= sum(Pre_ssp1_BSL_up)/sum(pop_ssp1_BSL),
            Pre_ssp1_BSL = sum(Pre_ssp1_BSL)/sum(pop_ssp1_BSL), 
            Pre_ssp2_BSL_low= sum(Pre_ssp2_BSL_low)/sum(pop_ssp2_BSL), Pre_ssp2_BSL_up= sum(Pre_ssp2_BSL_up)/sum(pop_ssp2_BSL),
            Pre_ssp2_BSL = sum(Pre_ssp2_BSL)/sum(pop_ssp2_BSL), 
            Pre_ssp3_BSL_low= sum(Pre_ssp3_BSL_low)/sum(pop_ssp3_BSL),Pre_ssp3_BSL_up= sum(Pre_ssp3_BSL_up)/sum(pop_ssp3_BSL),
            Pre_ssp3_BSL= sum(Pre_ssp3_BSL)/sum(pop_ssp3_BSL),
            Pre_ssp5_BSL_low= sum(Pre_ssp5_BSL_low)/sum(pop_ssp5_BSL), Pre_ssp5_BSL_up= sum(Pre_ssp5_BSL_up)/sum(pop_ssp5_BSL),
            Pre_ssp5_BSL= sum(Pre_ssp5_BSL)/sum(pop_ssp5_BSL),
            Base=sum(Base_2022)/sum(pop_2022_BR2022))
geokk = do.call(rbind,lapply(split(geodd,geod$geo),function(gr){
  kg = do.call(rbind,lapply(split(gr,gr$rcp),function(grs){
    coln <- grep(substring(grs$rcp[1],1,4), names(grs), value = TRUE)
    k = data.frame(region=unique(grs$geo),rcp=unique(grs$rcp),
                   rate = round(summary(lm((grs[[coln[3]]]-grs$Base)/grs$Base ~ grs$warming))$coefficients[2],4),
                   rate.low = round(summary(lm((grs[[coln[1]]]-grs$Base)/grs$Base ~ grs$warming))$coefficients[2],4),
                   rate.up = round(summary(lm((grs[[coln[2]]]-grs$Base)/grs$Base ~ grs$warming))$coefficients[2],4))
    return(k)
  }))
  return(kg)
}))
#############欧洲整体热死亡随升温变化速率##############
all = premor%>%group_by(rcp,Year,warming)%>%
  summarise(Pre_ssp1_BSL_low= sum(Pre_ssp1_BSL_low)/sum(pop_ssp1_BSL), Pre_ssp1_BSL_up= sum(Pre_ssp1_BSL_up)/sum(pop_ssp1_BSL),
            Pre_ssp1_BSL = sum(Pre_ssp1_BSL)/sum(pop_ssp1_BSL), 
            Pre_ssp2_BSL_low= sum(Pre_ssp2_BSL_low)/sum(pop_ssp2_BSL), Pre_ssp2_BSL_up= sum(Pre_ssp2_BSL_up)/sum(pop_ssp2_BSL),
            Pre_ssp2_BSL = sum(Pre_ssp2_BSL)/sum(pop_ssp2_BSL), 
            Pre_ssp3_BSL_low= sum(Pre_ssp3_BSL_low)/sum(pop_ssp3_BSL),Pre_ssp3_BSL_up= sum(Pre_ssp3_BSL_up)/sum(pop_ssp3_BSL),
            Pre_ssp3_BSL= sum(Pre_ssp3_BSL)/sum(pop_ssp3_BSL),
            Pre_ssp5_BSL_low= sum(Pre_ssp5_BSL_low)/sum(pop_ssp5_BSL), Pre_ssp5_BSL_up= sum(Pre_ssp5_BSL_up)/sum(pop_ssp5_BSL),
            Pre_ssp5_BSL= sum(Pre_ssp5_BSL)/sum(pop_ssp5_BSL),
            Base=sum(Base_2022)/sum(pop_2022_BR2022))
allk = do.call(rbind,lapply(split(all,all$rcp),function(grs){
  coln <- grep(substring(grs$rcp[1],1,4), names(grs), value = TRUE)
  k = data.frame(region="Europe",rcp=unique(grs$rcp),
                 rate = round(summary(lm((grs[[coln[3]]]-grs$Base)/grs$Base ~ grs$warming))$coefficients[2],4),
                 rate.low = round(summary(lm((grs[[coln[1]]]-grs$Base)/grs$Base ~ grs$warming))$coefficients[2],4),
                 rate.up = round(summary(lm((grs[[coln[2]]]-grs$Base)/grs$Base ~ grs$warming))$coefficients[2],4))
  return(k)
}))
library(reshape2)
library(scales)
gad =do.call(rbind,list(geok,allk))

gad$region = factor(gad$region,levels=
                      c("Europe","Southern Europe",
                        "Eastern Europe","Northern Europe","Western Europe" ))
g2c=ggplot(gad)+
  geom_linerange(aes(xmin=rate.low,xmax=rate.up,y=region,color=rcp),
                 position = position_dodge(width = 0.75),linewidth=0.7)+
  geom_pointrange(aes(x = rate, y = region, xmin=rate,xmax=rate,
                      size = region == "Europe",
                      color = rcp), fatten = 1.5, show.legend = TRUE,alpha=0.5,
                  position= position_dodge(width = 0.75))+
  scale_size_manual(values = c(0.3,0.6))+
  scale_color_manual(values = c(
    "#D1A46B","#BDAEAD","#930804","#313634"
  ))+theme_bw()+
  scale_y_discrete(position = "right",
                   labels=c("EU", "SEU", "EEU",
                            "NEU", "WEU")) +
  geom_hline(yintercept=c(1.5,2.5,3.5,4.5,5.5),size=.2,color="grey70",linetype=2)+
  #scale_shape_manual(values = c(15,21))+
  scale_x_continuous(expand=c(0.02,0.02),labels=percent)+
  labs(x ="Regional growth rate of heat-related\n mortality per capita with global warming",
       y = NULL)+
  theme(strip.background = element_rect(
    color="transparent", fill="grey80"),
    strip.text.x = element_text(
      size =9, color = "white",face = "bold" ), 
    panel.grid = element_blank(),
    axis.title.y= element_blank(),
    axis.title.x= element_text(color="black",hjust = 0.5,vjust=1,size =unit(9,"pt")),
    axis.text.x = element_text(color="black",hjust = 0.5,vjust=1,size =unit(9,"pt")),
    axis.text.y = element_text(color="black",size = unit(9,"pt")),
    legend.position="NA"
  ) 
ggsave(paste0(figout,"/fig2c.pdf"),g2c, width=6, height=14, units="cm", scale=1)

#############描述欧洲整体随升温速率死亡人数增加速率#############
allm = do.call(rbind,lapply(split(all,all$rcp),function(grs){
  coln <- grep(substring(grs$rcp[1],1,4), names(grs), value = TRUE)
  k = data.frame(region="Europe",rcp=unique(grs$rcp),
                 rate = round(summary(lm(grs[[coln[3]]] ~ grs$warming))$coefficients[2]*1000000,2),
                 rate.low = round(summary(lm(grs[[coln[1]]] ~ grs$warming))$coefficients[2]*1000000,2),
                 rate.up = round(summary(lm(grs[[coln[2]]] ~ grs$warming))$coefficients[2]*1000000,2))
  
  return(k)
}))
print(allm)#Heat-related deaths are projected to increase
#####################################################
g = premor%>%group_by(rcp,Year,warming)%>%
  summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL)/sum(pop_ssp1_BSL), 
            Pre_ssp1_BSL_low= sum(Pre_ssp1_BSL_low)/sum(pop_ssp1_BSL), Pre_ssp1_BSL_up= sum(Pre_ssp1_BSL_up)/sum(pop_ssp1_BSL),
            Pre_ssp2_BSL = sum(Pre_ssp2_BSL)/sum(pop_ssp2_BSL), 
            Pre_ssp2_BSL_low= sum(Pre_ssp2_BSL_low)/sum(pop_ssp2_BSL), Pre_ssp2_BSL_up= sum(Pre_ssp2_BSL_up)/sum(pop_ssp2_BSL),
            Pre_ssp3_BSL= sum(Pre_ssp3_BSL)/sum(pop_ssp3_BSL),
            Pre_ssp3_BSL_low= sum(Pre_ssp3_BSL_low)/sum(pop_ssp3_BSL),Pre_ssp3_BSL_up= sum(Pre_ssp3_BSL_up)/sum(pop_ssp3_BSL),
            Pre_ssp5_BSL= sum(Pre_ssp5_BSL)/sum(pop_ssp5_BSL),
            Pre_ssp5_BSL_low= sum(Pre_ssp5_BSL_low)/sum(pop_ssp5_BSL), Pre_ssp5_BSL_up= sum(Pre_ssp5_BSL_up)/sum(pop_ssp5_BSL),
            Base=sum(Base_2022)/sum(pop_2022_BR2022))

compar1 = do.call(rbind,lapply(split(g,g$rcp),function(r){
  coln <- grep(substring(r$rcp[1],1,4), names(r), value = TRUE)
  endcen = r[which(r$Year>=2045&r$Year<=2050),]
  nearcen = r[which(r$Year>=2025&r$Year<=2030),]
  ym = data.frame(rcp= unique(r$rcp),
                  value = mean(endcen[[coln[1]]])/mean(nearcen[[coln[1]]]),
                  low = mean(endcen[[coln[2]]])/mean(nearcen[[coln[1]]]),
                  up = mean(endcen[[coln[3]]])/mean(nearcen[[coln[1]]])
  )
  return(ym)
}))
compar2 = do.call(rbind,lapply(split(g,g$rcp),function(r){
  coln <- grep(substring(r$rcp[1],1,4), names(r), value = TRUE)
  endcen = r[which(r$Year>=2095&r$Year<=2100),]
  nearcen = r[which(r$Year>=2045&r$Year<=2050),]
  ym = data.frame(rcp= unique(r$rcp),
                  value = mean(endcen[[coln[1]]])/mean(nearcen[[coln[1]]]),
                  low = mean(endcen[[coln[2]]])/mean(nearcen[[coln[1]]]),
                  up = mean(endcen[[coln[3]]])/mean(nearcen[[coln[1]]])
  )
  return(ym)
}))
compar1$scenario = "V1"
compar2$scenario = "V2"
compar = do.call(rbind,list(compar1,compar2))

f2a = ggplot()+
  geom_errorbar(data = compar, aes(ymin =low, ymax=up,
                                   x=rcp,group=scenario),
                size=.5,width=0.4,
                position = position_dodge(width = 0.7))+
  geom_point(data=compar,aes(x=rcp,y=value,
                             color=scenario,shape = scenario),size=1.6,
             position = position_dodge(width = 0.7))+
  facet_wrap(~rcp, nrow = 1, scales = "free_x",strip.position = "bottom",
  )+
  theme_bw()+
  scale_shape_manual(values = c(15,21),
                     labels = c("Pre-mid-century comparison","Post-mid-century comparison"))+
  scale_color_manual(values = c("#b7b5a0","#44757a"),
                     labels = c("Pre-mid-century comparison","Post-mid-century comparison"))+
  scale_y_continuous(limits = c(0,4),labels = percent_format())+
  labs(
    y="Increase rate in heat-related\n mortality per capita",
    color = "",shape=""
  )+
  theme(strip.background = element_rect(
    color="transparent", fill="grey80"),
    strip.text.x = element_text(
      size =9, color = "white",face = "bold" ), 
    panel.grid = element_blank(),
    axis.title.x= element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y= element_text(color="black",hjust = 0.5,vjust=1,size =unit(9,"pt")),
    axis.text.y = element_text(color="black",size = unit(8,"pt")),
    legend.position="right"
  ) 
ggsave(paste0(figout,"/fig2a.pdf"),f2a, width=16, height=5, units="cm", scale=1)

rate = g[,c("Year","warming","rcp","Pre_ssp1_BSL","Pre_ssp2_BSL",
            "Pre_ssp3_BSL","Pre_ssp5_BSL")]
rate= do.call(rbind,lapply(split(rate,rate$rcp),function(r){
  coln <- grep(substring(r$rcp[1],1,4), names(r), value = TRUE)
  ym = data.frame(rcp= unique(r$rcp),
                  warming = r$warming,
                  value = r[[coln]]
  )
  return(ym)
}))

f2b = ggplot(rate,aes(x=warming,y=value*1000000))+#每万人增速
  geom_point(aes(color =rcp),size = 0.5,shape = 21,
             alpha=0.6)+
  #scale_shape_manual(values = c(15,21,5,10))+
  geom_smooth(aes(color = rcp),
              method = "loess", se = FALSE,size = 0.5)+
  scale_color_manual(values = c(
    "#D1A46B","#BDAEAD","#930804","#313634"
  ))+
  facet_wrap(~rcp, nrow = 1, scales = "free_x",strip.position = "top",
  )+
  theme_bw()+
  scale_x_continuous(expand=c(0,0),breaks = c(0.5,1,1.5,2,2.5,3,3.5,4),
                     expression(paste("Global warming (", degree, "C)")))+
  #scale_y_continuous(expand=c(0,0),limits = c(55000,100000))+
  labs(
    y="Heat-related mortality per million people",
    color = "",shape = "",linetype="",size=""
  )+#geom_abline(intercept = 0, slope = 1,color = "black")+
  theme(strip.background = element_blank(),
        strip.text.x = element_text(
          size =9, color = "black",face = "bold" ), 
        panel.grid = element_blank(),
        axis.title.x= element_text(color="black",hjust = 0.5,vjust=1,size =unit(9,"pt")),
        axis.text.x = element_text(color="black",size = unit(9,"pt")),
        axis.title.y= element_text(color="black",hjust = 0.5,vjust=1,size =unit(9,"pt")),
        axis.text.y = element_text(color="black",size = unit(9,"pt")),
        legend.position="top"
  ) 
ggsave(paste0(figout,"/fig2b.pdf"),f2b, width=15, height=10, units="cm", scale=1)
#################################计算老年人口比例############
agedata = do.call(rbind,lapply(split(premor,premor$rcp),function(ka){
  a = ka%>%group_by(rcp,age)%>%
    summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL), 
              Pre_ssp2_BSL = sum(Pre_ssp2_BSL), 
              Pre_ssp3_BSL= sum(Pre_ssp3_BSL),
              Pre_ssp5_BSL= sum(Pre_ssp5_BSL))
  sa = data.frame(rcp=unique(a$rcp),age=a$age,
                  ssp1 = a$Pre_ssp1_BSL/sum(a$Pre_ssp1_BSL),
                  ssp2 = a$Pre_ssp2_BSL/sum(a$Pre_ssp2_BSL),
                  ssp3 = a$Pre_ssp3_BSL/sum(a$Pre_ssp3_BSL),
                  ssp5 = a$Pre_ssp5_BSL/sum(a$Pre_ssp5_BSL))
  return(sa)
}))


##################Part 3 data extract##################################
fg = premor%>%group_by(rcp,Year,warming_level)%>%
  summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL)/sum(pop_ssp1_BSL),
            Pre_ssp2_BSL = sum(Pre_ssp2_BSL)/sum(pop_ssp2_BSL),
            Pre_ssp3_BSL = sum(Pre_ssp3_BSL)/sum(pop_ssp3_BSL),
            Pre_ssp5_BSL = sum(Pre_ssp5_BSL)/sum(pop_ssp5_BSL),
            
            Base_ssp1_BSL = sum(Base_ssp1_BSL)/sum(pop_ssp1_BSL),#气候效应
            Base_ssp2_BSL = sum(Base_ssp2_BSL)/sum(pop_ssp2_BSL),
            Base_ssp3_BSL = sum(Base_ssp3_BSL)/sum(pop_ssp3_BSL),
            Base_ssp5_BSL = sum(Base_ssp5_BSL)/sum(pop_ssp5_BSL),
            Pre_ssp1_BR2022=sum(Pre_ssp1_BR2022)/sum(pop_ssp1_BR2022),#老龄化效应
            Pre_ssp2_BR2022=sum(Pre_ssp2_BR2022)/sum(pop_ssp2_BR2022),
            Pre_ssp3_BR2022=sum(Pre_ssp3_BR2022)/sum(pop_ssp3_BR2022),
            Pre_ssp5_BR2022=sum(Pre_ssp5_BR2022)/sum(pop_ssp5_BR2022),
            Base_ssp1_BR2022 = sum(Base_ssp1_BR2022)/sum(pop_ssp1_BR2022),#气候效应
            Base_ssp2_BR2022 = sum(Base_ssp2_BR2022)/sum(pop_ssp2_BR2022),
            Base_ssp3_BR2022 = sum(Base_ssp3_BR2022)/sum(pop_ssp3_BR2022),
            Base_ssp5_BR2022 = sum(Base_ssp5_BR2022)/sum(pop_ssp5_BR2022),
            Base_2022_BSL = sum(Base_2022_BSL)/sum(pop_2022_BSL),
            Pre_2022_BSL = sum(Pre_2022_BSL)/sum(pop_2022_BSL),#人口效应
            Pre_2022 = sum(Pre_2022)/sum(pop_2022_BR2022),#人口效应
            Base_2022 = sum(Base_2022)/sum(pop_2022_BR2022)#总效应
  )

fg = fg%>%group_by(rcp,warming_level)%>%
  summarise(Pre_ssp1_BSL = mean(Pre_ssp1_BSL)*1000000,
            Pre_ssp2_BSL = mean(Pre_ssp2_BSL)*1000000,
            Pre_ssp3_BSL = mean(Pre_ssp3_BSL)*1000000,
            Pre_ssp5_BSL = mean(Pre_ssp5_BSL)*1000000,
            Base_ssp1_BSL = mean(Base_ssp1_BSL)*1000000,#气候效应
            Base_ssp2_BSL = mean(Base_ssp2_BSL)*1000000,
            Base_ssp3_BSL = mean(Base_ssp3_BSL)*1000000,
            Base_ssp5_BSL = mean(Base_ssp5_BSL)*1000000,
            Pre_ssp1_BR2022=mean(Pre_ssp1_BR2022)*1000000,#老龄化效应
            Pre_ssp2_BR2022=mean(Pre_ssp2_BR2022)*1000000,
            Pre_ssp3_BR2022=mean(Pre_ssp3_BR2022)*1000000,
            Pre_ssp5_BR2022=mean(Pre_ssp5_BR2022)*1000000,
            Base_ssp1_BR2022 = mean(Base_ssp1_BR2022)*1000000,#气候效应
            Base_ssp2_BR2022= mean(Base_ssp2_BR2022)*1000000,
            Base_ssp3_BR2022 = mean(Base_ssp3_BR2022)*1000000,
            Base_ssp5_BR2022 = mean(Base_ssp5_BR2022)*1000000,
            Base_2022_BSL =mean(Base_2022_BSL)*1000000,
            Pre_2022_BSL =mean(Pre_2022_BSL)*1000000,#人口效应
            Pre_2022 = mean(Pre_2022)*1000000,#人口效应
            Base_2022 = mean(Base_2022)*1000000#总效应
  )

rolfig = do.call(rbind,lapply(split(fg,fg$rcp),function(fg1){
  coln <- grep(substring(fg1$rcp[1],1,4), names(fg1), value = TRUE)
  ali = fg1[,c("warming_level",coln[1],"Base_2022")]
  colnames(ali) = c("warming","Pre","Base")
  ali$scenario = "all impacts"
  
  cli = fg1[,c("warming_level",coln[1],coln[2])]
  colnames(cli) = c("warming","Pre","Base")
  cli$scenario = "climate impacts"
  
  pli = fg1[,c("warming_level",coln[2],"Base_2022_BSL")]
  colnames(pli) = c("warming","Pre","Base")
  pli$scenario = "pop impacts"
  
  oli = fg1[,c("warming_level","Base_2022_BSL","Base_2022")]
  colnames(oli) = c("warming","Pre","Base")
  oli$scenario = "age impacts"
  
  fgnew = do.call(rbind,list(ali,cli,pli,oli))
  
  fgnew$scenario = factor(fgnew$scenario,
                          levels = c("all impacts", "climate impacts", "pop impacts", "age impacts"),
                          labels = c("all impacts"="Overall impacts",
                                     "climate impacts"="Impact of global warming",
                                     "pop impacts"="Impact of population size change",
                                     "age impacts"="Impact of population aging"))
  fgnew$rcp = substring(fg1$rcp[1],1,4)
  return(fgnew)
}))
fgnew = rolfig%>%group_by(warming,scenario)%>%summarise(Pre = mean(Pre),Base = mean(Base))
fgnew$side = fgnew$Pre>fgnew$Base

f4a = ggplot(fgnew)+geom_line(aes(x=warming,y=Pre),linetype=1,size=1,show.legend = T)+
  geom_line(aes(x=warming,y=Base),linetype=2,size=0.6,show.legend = T)+
  geom_ribbon(aes(x=warming,ymax=Pre,ymin=Base,
                  fill =side),alpha=0.8)+
  facet_wrap(~scenario, nrow = 2 ,scales = "free_y")+
  scale_fill_manual(values = c(
    "#8AABC4","#891F1F"
  ))+
  theme_bw()+
  scale_x_continuous(expand=c(0,0),
                     breaks = seq(min(fgnew$warming),max(fgnew$warming),0.5))+
  labs(
    y="Annual average number of heat-related deaths per million people",
    color = "",shape = "",linetype="",size=""
  )+
  theme(strip.background = element_blank(),
        strip.text.x = element_text(
          size =9, color = "black",face = "bold" ), 
        panel.grid = element_blank(),
        axis.title.x= element_text(color="black",hjust = 0.5,vjust=1,size =unit(9,"pt")),
        axis.text.x = element_text(color="black",size = unit(9,"pt")),
        axis.title.y= element_text(color="black",hjust = 0.5,vjust=1,size =unit(9,"pt")),
        axis.text.y = element_text(color="black",size = unit(9,"pt")),
        legend.position="top"
  ) 

ggsave(paste0(figout,"/fig4a.pdf"),f4a, width=15, height=12, units="cm", scale=1)

rofig = do.call(rbind,lapply(c(1.5,2,2.5,3,4),function(wl){
  fg4 = fgnew[which(fgnew$warming==wl),]
  
  fgnewb  =  fg4%>%group_by(scenario)%>%summarise(Pre = mean(Pre), Base = mean(Base))
  fgnewb$scenario = factor(fgnewb$scenario,levels = c("Impact of population aging","Impact of population size change",
                                                      "Impact of global warming","Overall impacts"
  ))
  fgnewb$diff = fgnewb$Pre-fgnewb$Base
  fgnewb$side = fgnewb$Pre>fgnewb$Base
  
  if(length(unique(fgnewb$side))>1){
    f4b = ggplot(fgnewb)+
      geom_col(aes(y=scenario,x=diff,fill=side),
               width = 0.5,alpha = 0.8,color="grey20",size=0.5 )+
      scale_fill_manual(values = c(
        "#8AABC4","#891F1F"))+
      geom_vline(xintercept = 0,size=0.5,linetype=5,color="grey30")+
      theme_bw()+
      labs(x= paste0("Deviation of annual average heat−related deaths\n per million people under global warming of " ,wl,"\u00B0C"),y="")+
      theme(strip.background = element_blank(),
            strip.text.x = element_text(
              size =2, color = "black",face = "bold" ), 
            panel.grid = element_blank(),
            axis.title.x= element_text(color="black",hjust = 0.5,vjust=1,size =unit(9,"pt")),
            axis.text.x = element_text(color="black",size = unit(9,"pt")),
            axis.title.y= element_blank(),
            axis.text.y = element_text(color="black",size = unit(9,"pt")),
            legend.position="") 
  }
  else{
    f4b = ggplot(fgnewb)+geom_col(aes(y=scenario,x=diff,fill=side),
                                  width = 0.5,alpha = 0.8,color="grey20",size=0.5 )+
      scale_fill_manual(values = c(
        "#891F1F"
      ))+
      geom_vline(xintercept = 0,size=0.5,linetype=5,color="grey30")+
      theme_bw()+
      labs(x= paste0("Deviation of annual average heat−related deaths\n per million people under global warming of " ,wl,"\u00B0C"),y="")+
      theme(strip.background = element_blank(),
            strip.text.x = element_text(
              size =2, color = "black",face = "bold" ), 
            panel.grid = element_blank(),
            axis.title.x= element_text(color="black",hjust = 0.5,vjust=1,size =unit(9,"pt")),
            axis.text.x = element_text(color="black",size = unit(9,"pt")),
            axis.title.y= element_blank(),
            axis.text.y = element_text(color="black",size = unit(9,"pt")),
            legend.position=""
      ) 
  }
  ggsave(paste0(figout,"/fig4b_warming",wl,".jpg"),f4b, width=15, height=5, units="cm", scale=1)
  ggsave(paste0(figout,"/fig4b_warming",wl,".pdf"),f4b, width=15, height=5, units="cm", scale=1)
  fgnewb$warming = wl
  return(fgnewb)
}))

f = do.call(rbind,lapply(split(rofig,rofig$warming),function(mk){
  mk$con = mk$diff/mk$diff[which(mk$scenario=="Overall impacts")]
  return(mk)
}))
f= subset(f,f$scenario!="Overall impacts")
f$warming = as.factor(f$warming)
g4c = ggplot(f)+geom_raster(aes(y=warming,x=scenario,fill=con*100))+
  scale_fill_gradientn(colours = c('#8AABC4',"#f2f3ec",
                                   '#891F1F'),
                       limits = c(-100, 100))+
  scale_x_discrete(
    labels=c("Impact of global warming"="GW", 
             "Impact of population size change"="PC",
             "Impact of population aging"="PA"))+
  scale_y_discrete(position = "right",
                   labels=c("1.5" = "Golbal warming of 1.5\u00B0C",
                            "2" = "Golbal warming of 2\u00B0C",
                            "2.5" = "Golbal warming of 2.5\u00B0C",
                            "3" = "Golbal warming of 3\u00B0C",
                            "4" = "Golbal warming of 4\u00B0C"))+
  theme_bw()+
  labs(x= "",y="",fill = "Effect of each driver(%)")+
  theme(strip.background = element_blank(),
        strip.text.x = element_text(
          size =2, color = "black",face = "bold" ), 
        panel.grid = element_blank(),
        axis.title.x= element_text(color="black",hjust = 0.5,vjust=0.5,size =unit(9,"pt")),
        axis.text.x = element_text(color="black",size = unit(9,"pt"),
                                   angle=0, vjust=0.5, hjust=0.5),
        axis.title.y= element_blank(),
        axis.text.y = element_text(color="black",size = unit(9,"pt")),
        legend.position="right",
        legend.title =element_text(color="black",size = unit(9,"pt"),angle=90),
        legend.key.size=unit(2, "cm"),  # 设置图例的大小
        legend.key.width=unit(0.2, "cm"),
        legend.text = element_text(color="black",size = unit(9,"pt"))
  ) 
ggsave(paste0(figout,"/fig4c.pdf"),g4c , width=8, height=16, units="cm", scale=1)
