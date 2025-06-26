rm(list = ls())
gc()
###############Prediction of heat related mortality from 2030-2100##############
library(dlnm)
library(splines)
library(patchwork)
library(zoo)
library(Epi)
library(lubridate)
library(dplyr)
library(reshape2)
library(data.table)
library(scales)
setwd("D:/ATtest/Europe_version3/")
preout = "Result"
if(dir.exists(preout)){
  print(paste(preout,"has existed!"))
}else{
  dir.create(preout)
}
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
agelist = c("0-15","15-65","65+")
daall = fread("Input_model_data/Input_data_all_v2.csv")
filter_list<- filter_geo(daall$geo)
pop = fread(file.path("D:/ATtest/Europe_version2/Input_model_data/Aux_pop_data_V2.csv"))
pop = pop[age_group!="TOTAL",]
daall= daall[daall$geo %in% filter_list, ]
daall =daall[age!="TOTAL",]
daall = merge(daall,pop,
              by.x = c("Year","geo","age"),
              by.y = c("Year","geo","age_group"))
daall = daall[complete.cases(daall),]
daall = daall[pop>0]

daall$age = as.factor(daall$age)
daall$gender_group = paste(daall$geo,daall$Year)
daall$IHD = daall$Heatday-daall$CHD
daall$IHN = daall$Heatnight-daall$CHN
daall$IHA = daall$Heatall-daall$CH

heatdeath = list()

for (a in agelist){
  print(a)
  data = daall[which(age==a&Year>=2010),]
  modeldir = paste0("Model/stratamodel_",a,"_ns_hum.rds")
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
  
  CDD = onebasis(da1$CHD,fun="strata",breaks=c(1,2,4))
  CDN = onebasis(da1$CHN,fun="strata",breaks=c(1,2,4))
  CDA = onebasis(da1$CH,fun="strata",breaks=c(1,2,4))
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
  preo = merge(preo,CDD_f,by.x="CHD",by.y = "CDD_num")
  preo = merge(preo,CDN_f,by.x="CHN",by.y = "CDN_num")
  preo = merge(preo,CDA_f,by.x="CH",by.y = "CDA_num")
  preo$rr.cdd[preo$rr.cdd<0]=0
  preo$rr.cdn[preo$rr.cdn<0]=0
  preo$rr.cda[preo$rr.cda<0]=0
  preo$rr.cdd.low[preo$rr.cdd.low<0]=0
  preo$rr.cdn.low[preo$rr.cdn.low<0]=0
  preo$rr.cda.low[preo$rr.cda.low<0]=0
  preo$rr.cdd.high[preo$rr.cdd.high<0]=0
  preo$rr.cdn.high[preo$rr.cdn.high<0]=0
  preo$rr.cda.high[preo$rr.cda.high<0]=0
  preo$morrisk = exp(preo$rr+preo$rr.cdd+preo$rr.cdn+preo$rr.cda)-1
  preo$morrisk.low = exp(preo$rr.low+preo$rr.cdd.low+preo$rr.cdn.low)-1
  preo$morrisk.high = exp(preo$rr.high+preo$rr.cdd.high+preo$rr.cdn.high+preo$rr.cda.high)-1
  
  preo$morrisk[which(preo$morrisk<0)] = 0
  preo$morrisk.low[which(preo$morrisk.low<0)] = 0
  preo$morrisk.high[which(preo$morrisk.high<0)] = 0
  preo$heat_predicted = preo$morrisk*preo$death
  preo$heat_predicted.low = preo$morrisk.low*preo$death
  preo$heat_predicted.high = preo$morrisk.high*preo$death
  preo = preo[which(Week>=22&Week<=35),]
  heatdeath[[a]] = preo%>%group_by(Year,geo,age)%>%summarise(heat_death = sum(heat_predicted),
                                                             heat_death.low = sum(heat_predicted.low),
                                                             heat_death.high = sum(heat_predicted.high),
                                                             pop= unique(pop),death = sum(death))
}

preda =  rbindlist(heatdeath)
preda$country = substring(preda$geo,1,2)
file = preda%>%group_by(Year,age,country)%>%summarise(heatdeath = round(sum(heat_death),0),
                                                      heatdeath.low = round(sum(heat_death.low),0),
                                                      heatdeath.high =round(sum(heat_death.high),0),
                                                      death = sum(death),pop = sum(pop))
file = file[which(file$Year<=2022),]


write.csv(file,paste0(preout,"/Mortality_prediction_history.csv"),row.names = F)


###############mortality of each country###############
file = fread("Result/Mortality_prediction_history.csv")

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
  geo = c("Southern Europe", "Western Europe", "Western Europe", "Eastern Europe", "Western Europe", "Southern Europe", "Eastern Europe", "Western Europe","Northern Europe", "Northern Europe", 
          "Southern Europe","Southern Europe", "Northern Europe", "Western Europe","Southern Europe",
          "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe", "Northern Europe",
          "Western Europe", "Northern Europe","Eastern Europe", "Southern Europe","Western Europe", 
          "Northern Europe", "Eastern Europe", "Southern Europe", "Eastern Europe", "Eastern Europe", 
          "Northern Europe",  "Southern Europe","Eastern Europe", "Northern Europe")
)
all = file%>%group_by(Year,country)%>%summarise(heatdeath = round(sum(heatdeath),0),
                                                heatdeath.low = round(sum(heatdeath.low),0),
                                                heatdeath.high =round(sum(heatdeath.high),0),
                                                death = sum(death),age = "TOTAL")
print(paste("Sum of deaths:",sum(all$heatdeath),"with 95%CI", sum(all$heatdeath.low),sum(all$heatdeath.high)))
print(paste("Country number:",length(unique(all$country))))
all = do.call(rbind,list(all,file))
all1 = subset(all,all$age =="TOTAL"|all$age =="65+")

print(round(sum(all1$heatdeath[which(all1$age=="65+")])/
              sum(all1$heatdeath[which(all1$age=="TOTAL")])*100,2))

all1 = all1%>%group_by(country,age)%>%
  summarise(death =sum(death),heatdeath = sum(heatdeath),heatdeath.low = sum(heatdeath.low),heatdeath.high = sum(heatdeath.high))
all1 = merge(all1,label,id = country)
all1$geo=  factor(all1$geo,ordered=TRUE,levels = c("Southern Europe","Eastern Europe","Western Europe","Northern Europe"))


all_o = subset(all1,all1$age=="TOTAL") 
all_o <- all_o %>% arrange(desc(heatdeath))%>%
  mutate(Country_name = factor(Country_name, levels =Country_name))

all_old = subset(all1,all1$age=="65+") 
g2= ggplot()+
  geom_linerange(data = all_o, aes(ymin =0, ymax=heatdeath, x=Country_name,color = geo),linewidth=1,alpha=0.5)+
  geom_linerange(data = all_old, aes(ymin =0, ymax=heatdeath, x=Country_name,color = geo),linewidth=2)+
  geom_point(data = all_o, aes(y=heatdeath, x=Country_name,fill = geo),shape = 21,color = "grey20",size=3,alpha=0.3)+
  #scale_shape_manual(values = c(21))+
  scale_fill_manual(values = c("#b5755b","#757876","#8db398","#dfbf7f"))+
  scale_color_manual(values = c("#b5755b","#757876","#8db398","#dfbf7f"))+
  theme_bw() +labs(y="Total number of heat-related death\n from 2010 to 2022 (weeks 22-35)")+
  scale_y_continuous(limits=c(0,125000),labels=comma)+
  theme(legend.position = "bottom",
        legend.background = element_rect(
          fill = "transparent", # ????ɫ
          colour = "transparent", # ????ɫ
          size = 1.5),
        legend.title=element_blank(),
        legend.text=element_text(color="black",hjust = 0.5,vjust=.50,size =unit(9,"pt")),
        panel.grid = element_blank(),
        axis.line=element_line(color="black",size=0.5),
        plot.title = element_text(color="black",hjust = 0,vjust=0,size =unit(9,"pt")),
        axis.title.x= element_blank(),
        axis.text.x = element_text(color="black",size = unit(9,"pt"),angle = 60,hjust=1,vjust=1),
        axis.title.y= element_text(color="black",size = unit(9,"pt")),
        axis.text.y = element_text(color="black",size = unit(9,"pt")),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))+
  guides(color = guide_legend(nrow =1),fill = guide_legend(nrow = 1) ) 
outdir = "Figures"
if(dir.exists(outdir)){
  print(paste(outdir,"has existed!"))
}else{
  dir.create(outdir)
}
ggsave(paste0(outdir,"/fig1d_summerMortality.pdf"),g2, width=22, height=9, units="cm", scale=1)

a1 =all_o %>%group_by(geo)%>%
  summarise(death =sum(death),heatdeath = sum(heatdeath),
            heatdeath.low = sum(heatdeath.low),heatdeath.high = sum(heatdeath.high))

a2 =all_old %>%group_by(geo)%>%
  summarise(death_old =sum(death),heatdeath_old = sum(heatdeath),
            heatdeath_old.low = sum(heatdeath.low),heatdeath_old.high = sum(heatdeath.high))

d = merge(a1,a2,by="geo")
d$old_ratio = d$heatdeath_old/d$heatdeath
d$old_ratio.low = d$heatdeath_old.low/d$heatdeath
d$old_ratio.high = d$heatdeath_old.high/d$heatdeath
print(d)
