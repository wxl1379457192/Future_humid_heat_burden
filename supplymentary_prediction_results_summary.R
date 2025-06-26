rm(list = ls())
gc()
setwd("D:/ATtest/Europe_version3/")
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
indir = "Heatmor_prediction_future_supply10years"
timelist = c("2000_2009","2001_2010","2002_2011","2003_2012","2004_2013",
             "2005_2014","2006_2015","2007_2016","2008_2017","2009_2018","2010_2019")
newdir = "Result"
if (dir.exists(newdir)){
  print("Output dir has existed!")
}else{
  dir.create(newdir)
}
# 打印文件读取进度的函数
print_progress <- function(file) {
  message("Reading file: ", file)
  return(fread(file))
}
warming = read.csv(file.path("D:/ATtest/Europe_version2/Result","Global_warming.csv"),stringsAsFactors =F)
for (t in timelist){
  filename = file.path(newdir,paste0("Heatmor_prediction_future_",t,".csv"))
  if (file.exists(filename)){
    print(paste(filename,"has exists!"))
  }else{
    filelist = list.files(path = indir,pattern = paste0("*", t, "\\.csv$"),full.names=TRUE)
    premor <- do.call(rbind,lapply(filelist,function(k){
      f = print_progress(k)
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
      }else{f1=NULL}
      return(f1)
    }))
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
    spag = premor%>%group_by(rcp,Year,warming_level,geo)%>%
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
    spag = spag%>%group_by(rcp,geo,warming_level)%>%
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
    allspag = premor%>%group_by(rcp,Year,warming_level)%>%
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
    allspag = allspag%>%group_by(rcp,warming_level)%>%
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
    allspag$geo = "Europe"
    all  = do.call(rbind,list(spag,allspag))
    k1 = all[which(all$warming_level==1.5),]
    k1 = k1%>%group_by(geo,warming_level)%>%
      summarise(value = mean(c(Pre_ssp1_BSL,Pre_ssp2_BSL,Pre_ssp3_BSL, Pre_ssp5_BSL), na.rm = TRUE), 
                low = mean(c(Pre_ssp1_BSL_low,Pre_ssp2_BSL_low,Pre_ssp3_BSL_low, Pre_ssp5_BSL_low), na.rm = TRUE),
                up = mean(c(Pre_ssp1_BSL_up,Pre_ssp2_BSL_up,Pre_ssp3_BSL_up, Pre_ssp5_BSL_up), na.rm = TRUE))
    k2 = all[which(all$warming_level==2),]
    k2 = k2%>%group_by(geo,warming_level)%>%
      summarise(value = mean(c(Pre_ssp2_BSL,Pre_ssp3_BSL, Pre_ssp5_BSL), na.rm = TRUE), 
                low = mean(c(Pre_ssp2_BSL_low,Pre_ssp3_BSL_low, Pre_ssp5_BSL_low), na.rm = TRUE),
                up = mean(c(Pre_ssp2_BSL_up,Pre_ssp3_BSL_up, Pre_ssp5_BSL_up), na.rm = TRUE))
    k3 = all[which(all$warming_level==3),]
    k3 = k3%>%group_by(geo,warming_level)%>%
      summarise(value = mean(c(Pre_ssp3_BSL, Pre_ssp5_BSL), na.rm = TRUE),
                low = mean(c(Pre_ssp3_BSL_low, Pre_ssp5_BSL_low), na.rm = TRUE),
                up = mean(c(Pre_ssp3_BSL_up, Pre_ssp5_BSL_up), na.rm = TRUE))
    k4 = all[which(all$warming_level==4),]
    k4 = k4%>%group_by(geo,warming_level)%>%
      summarise(value = Pre_ssp5_BSL, 
                low = Pre_ssp5_BSL_low,
                up = Pre_ssp5_BSL_up)
    
    outfile = do.call(rbind,list(k1,k2,k3,k4))
    fwrite(outfile,filename,row.names = F)
  }
}


###############
folder_path <- "D:/ATtest/Europe_version3/Heatmor_prediction_future_supply10years"
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = FALSE)
geo_codes <- regmatches(file_list, regexpr("_[A-Za-z0-9]{5}_", file_list))
geo_codes <- gsub("_", "", geo_codes) 
unique_geo_codes <-unique(geo_codes)

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
premor= premor[premor$geom %in%geo_codes, ]
spag = premor%>%group_by(rcp,Year,warming_level,geo)%>%
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
spag = spag%>%group_by(rcp,geo,warming_level)%>%
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
allspag = premor%>%group_by(rcp,Year,warming_level)%>%
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
allspag = allspag%>%group_by(rcp,warming_level)%>%
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
allspag$geo = "Europe"
all  = do.call(rbind,list(spag,allspag))
k1 = all[which(all$warming_level==1.5),]
k1 = k1%>%group_by(geo,warming_level)%>%
  summarise(value = mean(c(Pre_ssp1_BSL,Pre_ssp2_BSL,Pre_ssp3_BSL, Pre_ssp5_BSL), na.rm = TRUE), 
            low = mean(c(Pre_ssp1_BSL_low,Pre_ssp2_BSL_low,Pre_ssp3_BSL_low, Pre_ssp5_BSL_low), na.rm = TRUE),
            up = mean(c(Pre_ssp1_BSL_up,Pre_ssp2_BSL_up,Pre_ssp3_BSL_up, Pre_ssp5_BSL_up), na.rm = TRUE))
k2 = all[which(all$warming_level==2),]
k2 = k2%>%group_by(geo,warming_level)%>%
  summarise(value = mean(c(Pre_ssp2_BSL,Pre_ssp3_BSL, Pre_ssp5_BSL), na.rm = TRUE), 
            low = mean(c(Pre_ssp2_BSL_low,Pre_ssp3_BSL_low, Pre_ssp5_BSL_low), na.rm = TRUE),
            up = mean(c(Pre_ssp2_BSL_up,Pre_ssp3_BSL_up, Pre_ssp5_BSL_up), na.rm = TRUE))
k3 = all[which(all$warming_level==3),]
k3 = k3%>%group_by(geo,warming_level)%>%
  summarise(value = mean(c(Pre_ssp3_BSL, Pre_ssp5_BSL), na.rm = TRUE),
            low = mean(c(Pre_ssp3_BSL_low, Pre_ssp5_BSL_low), na.rm = TRUE),
            up = mean(c(Pre_ssp3_BSL_up, Pre_ssp5_BSL_up), na.rm = TRUE))
k4 = all[which(all$warming_level==4),]
k4 = k4%>%group_by(geo,warming_level)%>%
  summarise(value = Pre_ssp5_BSL, 
            low = Pre_ssp5_BSL_low,
            up = Pre_ssp5_BSL_up)

outfile = do.call(rbind,list(k1,k2,k3,k4))
fwrite(outfile,"Result/Heatmor_prediction_future_2010_2019.csv",row.names = F)



