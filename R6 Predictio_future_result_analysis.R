setwd("D:/ATtest/Europe/")
library(ggplot2)
library(dplyr)
prein = "Mortality_prediction_future_V1223"
file_names <- list.files(prein)
pattern = "Future_weekly_data"
matching_files <- grep(pattern, file_names, value = TRUE)

premor = do.call(rbind,lapply(matching_files,function(f){
  file = read.csv(file.path(prein,f),stringsAsFactors = F)
  file$age = strsplit(strsplit(f,"_")[[1]][5],".csv")[[1]][1]
  # for (k in colnames(file)){
  #   file[,k][is.nan(file[,k])] = 0
  #   file[,k][is.na(file[,k])] = 0
  # }
  file$scenario = paste0(file$rcp,file$member)
  #file = file[which(file$scenari!="rcp45r2i1p1"),]
  out = file%>%group_by(geo,year,age,rcp)%>%
    summarise(Humidex_mean=mean(Humidex_mean),DD_num= mean(DD_num),DN_num= mean(DN_num),
              Humidex_2022=mean(Humidex_2022),DD_2022= mean(DD_2022),DN_2022= mean(DN_2022),
              pop_2022 = unique(pop_2022),#2022年对应年龄段人口数
              pop_ssp1_BSL = unique(pop_ssp1_BSL),pop_ssp2_BSL = unique(pop_ssp2_BSL),#ssp人口情景-BSL人口结构情景的人口数量
              pop_ssp3_BSL = unique(pop_ssp3_BSL),pop_ssp4_BSL = unique(pop_ssp4_BSL),pop_ssp5_BSL = unique(pop_ssp5_BSL),
              pop_ssp1_BR2022 = unique(pop_ssp1_BR2022),pop_ssp2_BR2022 = unique(pop_ssp2_BR2022),#ssp人口情景-2022年人口结构，即人口基数变，人口结构不变
              pop_ssp3_BR2022 = unique(pop_ssp3_BR2022),pop_ssp4_BR2022 = unique(pop_ssp4_BR2022),pop_ssp5_BR2022 = unique(pop_ssp5_BR2022),
              death_base = unique(death_base),#2015-2019死亡人数均值
              pop_base = unique(pop_base),#2015-2019人口数量均值
              Pre_ssp1_BSL_low = quantile(Pre_ssp1_BSL,0.25),Pre_ssp1_BSL_up = quantile(Pre_ssp1_BSL,0.75),
              Pre_ssp1_BSL = mean(Pre_ssp1_BSL),#基于对应rcp情景-ssp21人口情景-BSL人口结构情景的死亡预测
              Pre_ssp2_BSL_low = quantile(Pre_ssp2_BSL,0.25),Pre_ssp2_BSL_up = quantile(Pre_ssp2_BSL,0.75),
              Pre_ssp2_BSL = mean(Pre_ssp2_BSL),#基于对应rcp情景-ssp2人口情景-BSL人口结构情景的死亡预测
              Pre_ssp3_BSL_low = quantile(Pre_ssp3_BSL,0.25),Pre_ssp3_BSL_up = quantile(Pre_ssp3_BSL,0.75),
              Pre_ssp3_BSL = mean(Pre_ssp3_BSL),#基于对应rcp情景-ssp3人口情景-BSL人口结构情景的死亡预测
              Pre_ssp4_BSL_low = quantile(Pre_ssp4_BSL,0.25),Pre_ssp4_BSL_up = quantile(Pre_ssp4_BSL,0.75),
              Pre_ssp4_BSL = mean(Pre_ssp4_BSL),#基于对应rcp情景-ssp4人口情景-BSL人口结构情景的死亡预测
              Pre_ssp5_BSL_low = quantile(Pre_ssp5_BSL,0.25),Pre_ssp5_BSL_up = quantile(Pre_ssp5_BSL,0.75),
              Pre_ssp5_BSL = mean(Pre_ssp5_BSL),#基于对应rcp情景-ssp5人口情景-BSL人口结构情景的死亡预测
              Pre_ssp1_BR2022_low = quantile(Pre_ssp1_BR2022,0.25),Pre_ssp1_BR2022_up = quantile(Pre_ssp1_BR2022,0.75),
              Pre_ssp1_BR2022 = mean(Pre_ssp1_BR2022),#基于对应rcp情景-ssp1人口情景-人口结构不变死亡预测
              Pre_ssp2_BR2022_low = quantile(Pre_ssp2_BR2022,0.25),Pre_ssp2_BR2022_up = quantile(Pre_ssp2_BR2022,0.75),
              Pre_ssp2_BR2022 = mean(Pre_ssp2_BR2022),#基于对应rcp情景-ssp2人口情景-人口结构不变死亡预测
              Pre_ssp3_BR2022_low = quantile(Pre_ssp3_BR2022,0.25),Pre_ssp3_BR2022_up = quantile(Pre_ssp3_BR2022,0.75),
              Pre_ssp3_BR2022 = mean(Pre_ssp3_BR2022),#基于对应rcp情景-ssp3人口情景-人口结构不变的死亡预测
              Pre_ssp4_BR2022_low = quantile(Pre_ssp4_BR2022,0.25),Pre_ssp4_BR2022_up = quantile(Pre_ssp4_BR2022,0.75),
              Pre_ssp4_BR2022 = mean(Pre_ssp4_BR2022),#基于对应rcp情景-ssp4人口情景-人口结构不变死亡预测
              Pre_ssp5_BR2022_low = quantile(Pre_ssp5_BR2022,0.25),Pre_ssp5_BR2022_up = quantile(Pre_ssp5_BR2022,0.75),
              Pre_ssp5_BR2022 = mean(Pre_ssp5_BR2022),#基于对应rcp情景-ssp5人口情景-人口结构不变的死亡预测
              Pre_2022_BSL_low = quantile(Pre_2022_BSL,0.25),Pre_2022_BSL_up = quantile(Pre_2022_BSL,0.75),
              Pre_2022_BSL = mean(Pre_2022_BSL),#基于rcp情景-人口b不变-人口结构BSL
              Pre_2022_low = quantile(Pre_2022,0.25),Pre_2022_up = quantile(Pre_2022,0.75),
              Pre_2022 = mean(Pre_2022),#基于rcp情景，人口与人口结构均不变的预测
              Base_ssp1_BSL_low = quantile(Base_ssp1_BSL,0.25),Base_ssp1_BSL_up = quantile(Base_ssp1_BSL,0.75),
              Base_ssp1_BSL = mean(Base_ssp1_BSL),#气候不变-ssp1人口情景-BSL人口结构情景
              Base_ssp2_BSL_low = quantile(Base_ssp2_BSL,0.25),Base_ssp2_BSL_up = quantile(Base_ssp2_BSL,0.75),
              Base_ssp2_BSL = mean(Base_ssp2_BSL),#气候不变-ssp2人口情景-BSL人口结构情景
              Base_ssp3_BSL_low = quantile(Base_ssp3_BSL,0.25),Base_ssp3_BSL_up = quantile(Base_ssp3_BSL,0.75),
              Base_ssp3_BSL = mean(Base_ssp3_BSL),#气候不变-ssp3人口情景-BSL人口结构情景
              Base_ssp4_BSL_low = quantile(Base_ssp4_BSL,0.25),Base_ssp4_BSL_up = quantile(Base_ssp4_BSL,0.75),
              Base_ssp4_BSL = mean(Base_ssp4_BSL),#气候不变-ssp2人口情景-BSL人口结构情景
              Base_ssp5_BSL_low = quantile(Base_ssp5_BSL,0.25),Base_ssp5_BSL_up = quantile(Base_ssp5_BSL,0.75),
              Base_ssp5_BSL = mean(Base_ssp5_BSL),#气候不变-ssp人口情景-BSL人口结构情景
              Base_ssp1_BR2022_low = quantile(Base_ssp1_BR2022,0.25),Base_ssp1_BR2022_up = quantile(Base_ssp1_BR2022,0.75),
              Base_ssp1_BR2022 = mean(Base_ssp1_BR2022),#气候不变-ssp1人口情景-人口结构不变
              Base_ssp2_BR2022_low = quantile(Base_ssp2_BR2022,0.25),Base_ssp2_BR2022_up = quantile(Base_ssp2_BR2022,0.75),
              Base_ssp2_BR2022 = mean(Base_ssp2_BR2022),#气候不变-ssp2人口情景-人口结构不变
              Base_ssp3_BR2022_low = quantile(Base_ssp3_BR2022,0.25),Base_ssp3_BR2022_up = quantile(Base_ssp3_BR2022,0.75),
              Base_ssp3_BR2022 = mean(Base_ssp3_BR2022),#气候不变-ssp3人口情景-人口结构不变
              Base_ssp4_BR2022_low = quantile(Base_ssp4_BR2022,0.25),Base_ssp4_BR2022_up = quantile(Base_ssp4_BR2022,0.75),
              Base_ssp4_BR2022 = mean(Base_ssp4_BR2022),#气候不变-ssp2人口情景-人口结构不变
              Base_ssp5_BR2022_low = quantile(Base_ssp5_BR2022,0.25),Base_ssp5_BR2022_up = quantile(Base_ssp5_BR2022,0.75),
              Base_ssp5_BR2022 = mean(Base_ssp5_BR2022),#气候不变-ssp5人口情景-人口结构不变
              Base_2022_BSL_low = quantile(Base_2022_BSL,0.25),Base_2022_BSL_up = quantile(Base_2022_BSL,0.75),
              Base_2022_BSL = mean(Base_2022_BSL),#基于rcp情景-人口b不变-人口结构BSL
              Base_2022_low =  quantile(Base_2022,0.25),Base_2022_low =  quantile(Base_2022,0.75),
              Base_2022 = mean(Base_2022)#气象，人口，结构均不变，可直接视作是2022年的热死亡人数
              )
  print(paste(f,"has been processed!"))
  return(out)
}))
outdir = "Result_V1225"
if(dir.exists(outdir)){
  print(paste(outdir,"has existed!"))
}else{
  dir.create(outdir)
}
write.csv(premor,file.path(outdir,"Mortality_prediction.csv"),row.names = F)
