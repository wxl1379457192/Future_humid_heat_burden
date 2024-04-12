setwd("D:/ATtest/Europe/")
library(ggplot2)
library(dplyr)
figout = "Figure_1225"
if (dir.exists(figout)){
  print("Output dir has existed!")
}else{
  dir.create(figout)
}
outdir = "Result_V1225"
premor = read.csv(file.path(outdir,"Mortality_prediction.csv"),stringsAsFactors = F)

dnnum = premor%>%group_by(rcp,year)%>%summarise(DD_num=mean(DD_num),DN_num=mean(DN_num),
                                                DD_2022=mean(DD_2022),
                                                DN_2022=mean(DN_2022))
dnnum$fdd = dnnum$DD_num/dnnum$DD_2022
dnnum$fdn = dnnum$DN_num/dnnum$DN_2022

warming = read.csv(file.path("Result","Global_warming.csv"),stringsAsFactors =F)
base = read.csv(file.path("Mortality_prediction_2022_V1223","Mortality_prediction_2022.csv"),stringsAsFactors = F)
premor$country = substring(premor$geo,1,2)
label = data.frame(country =  c("AL", "AT", "BE", "BG", "CH", "CY", "CZ", "DK", "EE", "EL", "ES", "FI", "FR", "HU", "IS", "IT", "LI", "LT", "LU", "LV",
                                "ME", "NL", "NO", "PL", "PT", "RO", "RS", "SE", "SK", "UK", "DE","TR","IE"),
                   Country_name = c("Albania", "Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Greece",
                                    "Spain", "Finland", "France", "Hungary", "Iceland", "Italy", "Liechtenstein", "Lithuania", "Luxembourg", "Latvia",
                                    "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Serbia", "Sweden", 
                                    "Slovakia", "United Kingdom","Germany","Turkey","Ireland"),
                   region =  c("Southeastern Europe", "Central Europe", "Western Europe", "Southeastern Europe", "Central Europe", "Southern Europe",
                               "Central Europe", "Northern Europe", "Northern Europe", "Southern Europe", "Southern Europe", "Northern Europe",
                               "Western Europe", "Central Europe", "Northern Europe", "Southern Europe", "Central Europe", "Northern Europe",
                               "Western Europe", "Northern Europe", "Southern Europe", "Western Europe", "Northern Europe", "Central Europe",
                               "Southern Europe", "Southeastern Europe", "Southern Europe", "Northern Europe", "Central Europe", "Western Europe",
                               "Central Europe","Southeastern Europe","Western Europe"))
premor = merge(premor,label,by="country")
premor = merge(premor,warming,by.x=c("year","rcp"),by.y =c("Year","Scenario"))
base = base[,c("geom","age","Predicted","Predicted.low","Predicted.up")]
colnames(base) = c("geo","age","Base","Base_low","Base_up")
premor = merge(premor,base)
########################################################
#############用于Result part 2 描述的数据##############
geod = premor%>%group_by(rcp,year,warming,region)%>%
  summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL), 
            Pre_ssp1_BSL_low= sum(Pre_ssp1_BSL_low), Pre_ssp1_BSL_up= sum(Pre_ssp1_BSL_up),
            Pre_ssp2_BSL = sum(Pre_ssp2_BSL), 
            Pre_ssp2_BSL_low= sum(Pre_ssp2_BSL_low), Pre_ssp2_BSL_up= sum(Pre_ssp2_BSL_up),
            Pre_ssp3_BSL= sum(Pre_ssp3_BSL),
            Pre_ssp3_BSL_low= sum(Pre_ssp3_BSL_low),Pre_ssp3_BSL_up= sum(Pre_ssp3_BSL_up),
            Pre_ssp4_BSL= sum(Pre_ssp4_BSL),
            Pre_ssp4_BSL_low= sum(Pre_ssp4_BSL_low), Pre_ssp4_BSL_up= sum(Pre_ssp4_BSL_up),
            Pre_ssp5_BSL= sum(Pre_ssp5_BSL),
            Pre_ssp5_BSL_low= sum(Pre_ssp5_BSL_low), Pre_ssp5_BSL_up= sum(Pre_ssp5_BSL_up),
            Base_2022 = sum(Base_2022),
            Base=sum(Base))
geok = do.call(rbind,lapply(split(geod,geod$region),function(gr){
  kg = do.call(rbind,lapply(split(gr,gr$rcp),function(grs){
    k = data.frame(region=unique(grs$region),rcp=unique(grs$rcp),
                   ssp1 = round(summary(lm(Pre_ssp1_BSL/Base~warming,grs))$coefficients[2],4),
                   ssp2 = round(summary(lm(Pre_ssp2_BSL/Base~warming,grs))$coefficients[2],4),
                   ssp3 = round(summary(lm(Pre_ssp3_BSL/Base~warming,grs))$coefficients[2],4),
                   ssp4 = round(summary(lm(Pre_ssp4_BSL/Base~warming,grs))$coefficients[2],4),
                   ssp5 = round(summary(lm(Pre_ssp5_BSL/Base~warming,grs))$coefficients[2],4))
    return(k)
  }))
  return(kg)
}))
geokl = do.call(rbind,lapply(split(geod,geod$region),function(gr){
  kg = do.call(rbind,lapply(split(gr,gr$rcp),function(grs){
    k = data.frame(region=unique(grs$region),rcp=unique(grs$rcp),
                   ssp1 = round(summary(lm(Pre_ssp1_BSL_low/Base~warming,grs))$coefficients[2],4),
                   ssp2 = round(summary(lm(Pre_ssp2_BSL_low/Base~warming,grs))$coefficients[2],4),
                   ssp3 = round(summary(lm(Pre_ssp3_BSL_low/Base~warming,grs))$coefficients[2],4),
                   ssp4 = round(summary(lm(Pre_ssp4_BSL_low/Base~warming,grs))$coefficients[2],4),
                   ssp5 = round(summary(lm(Pre_ssp5_BSL_low/Base~warming,grs))$coefficients[2],4))
    return(k)
  }))
  return(kg)
}))
geoku = do.call(rbind,lapply(split(geod,geod$region),function(gr){
  kg = do.call(rbind,lapply(split(gr,gr$rcp),function(grs){
    k = data.frame(region=unique(grs$region),rcp=unique(grs$rcp),
                   ssp1 = round(summary(lm(Pre_ssp1_BSL_up/Base~warming,grs))$coefficients[2],4),
                   ssp2 = round(summary(lm(Pre_ssp2_BSL_up/Base~warming,grs))$coefficients[2],4),
                   ssp3 = round(summary(lm(Pre_ssp3_BSL_up/Base~warming,grs))$coefficients[2],4),
                   ssp4 = round(summary(lm(Pre_ssp4_BSL_up/Base~warming,grs))$coefficients[2],4),
                   ssp5 = round(summary(lm(Pre_ssp5_BSL_up/Base~warming,grs))$coefficients[2],4))
    return(k)
  }))
  return(kg)
}))
library(reshape2)
geok = melt(geok,id=c("region","rcp"))
geokl = melt(geokl,id=c("region","rcp"),value.name = "low")
geoku = melt(geoku,id=c("region","rcp"),value.name = "up")
geok = merge(geok,geokl,by=c("region","rcp","variable"))
geok = merge(geok,geoku,by=c("region","rcp","variable"))

#############用于Result part 2 描述的数据##############
all = premor%>%group_by(rcp,year,warming)%>%
  summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL), 
            Pre_ssp1_BSL_low= sum(Pre_ssp1_BSL_low), Pre_ssp1_BSL_up= sum(Pre_ssp1_BSL_up),
            Pre_ssp2_BSL = sum(Pre_ssp2_BSL), 
            Pre_ssp2_BSL_low= sum(Pre_ssp2_BSL_low), Pre_ssp2_BSL_up= sum(Pre_ssp2_BSL_up),
            Pre_ssp3_BSL= sum(Pre_ssp3_BSL),
            Pre_ssp3_BSL_low= sum(Pre_ssp3_BSL_low),Pre_ssp3_BSL_up= sum(Pre_ssp3_BSL_up),
            Pre_ssp4_BSL= sum(Pre_ssp4_BSL),
            Pre_ssp4_BSL_low= sum(Pre_ssp4_BSL_low), Pre_ssp4_BSL_up= sum(Pre_ssp4_BSL_up),
            Pre_ssp5_BSL= sum(Pre_ssp5_BSL),
            Pre_ssp5_BSL_low= sum(Pre_ssp5_BSL_low), Pre_ssp5_BSL_up= sum(Pre_ssp5_BSL_up),
            Base_2022 = sum(Base_2022),
            Base=sum(Base))
allk = do.call(rbind,lapply(split(all,all$rcp),function(grs){
  k = data.frame(region="Europe",rcp=unique(grs$rcp),
                 ssp1 = round(summary(lm(Pre_ssp1_BSL/Base~warming,grs))$coefficients[2],4),
                 ssp2 = round(summary(lm(Pre_ssp2_BSL/Base~warming,grs))$coefficients[2],4),
                 ssp3 = round(summary(lm(Pre_ssp3_BSL/Base~warming,grs))$coefficients[2],4),
                 ssp4 = round(summary(lm(Pre_ssp4_BSL/Base~warming,grs))$coefficients[2],4),
                 ssp5 = round(summary(lm(Pre_ssp5_BSL/Base~warming,grs))$coefficients[2],4))
  return(k)
}))
allkl = do.call(rbind,lapply(split(all,all$rcp),function(grs){
  k = data.frame(region="Europe",rcp=unique(grs$rcp),
                 ssp1 = round(summary(lm(Pre_ssp1_BSL_low/Base~warming,grs))$coefficients[2],4),
                 ssp2 = round(summary(lm(Pre_ssp2_BSL_low/Base~warming,grs))$coefficients[2],4),
                 ssp3 = round(summary(lm(Pre_ssp3_BSL_low/Base~warming,grs))$coefficients[2],4),
                 ssp4 = round(summary(lm(Pre_ssp4_BSL_low/Base~warming,grs))$coefficients[2],4),
                 ssp5 = round(summary(lm(Pre_ssp5_BSL_low/Base~warming,grs))$coefficients[2],4))
  return(k)
}))
allku = do.call(rbind,lapply(split(all,all$rcp),function(grs){
  k = data.frame(region="Europe",rcp=unique(grs$rcp),
                 ssp1 = round(summary(lm(Pre_ssp1_BSL_up/Base~warming,grs))$coefficients[2],4),
                 ssp2 = round(summary(lm(Pre_ssp2_BSL_up/Base~warming,grs))$coefficients[2],4),
                 ssp3 = round(summary(lm(Pre_ssp3_BSL_up/Base~warming,grs))$coefficients[2],4),
                 ssp4 = round(summary(lm(Pre_ssp4_BSL_up/Base~warming,grs))$coefficients[2],4),
                 ssp5 = round(summary(lm(Pre_ssp5_BSL_up/Base~warming,grs))$coefficients[2],4))
  return(k)
}))
library(reshape2)
library(scales)
allk = melt(allk,id=c("region","rcp"))
allkl = melt(allkl,id=c("region","rcp"),value.name = "low")
allku = melt(allku,id=c("region","rcp"),value.name = "up")
allk = merge(allk,allkl,by=c("region","rcp","variable"))
allk = merge(allk,allku,by=c("region","rcp","variable"))
gad =do.call(rbind,list(geok,allk))

gad$region = factor(gad$region,levels=
                      c("Europe","Southeastern Europe","Southern Europe",
                        "Central Europe","Northern Europe","Western Europe"
                        ))
g2c=ggplot(gad)+
  geom_linerange(aes(xmin=low,xmax=up,y=region,color=variable,linetype=rcp),
                 position = position_dodge(width = 0.75),linewidth=0.5)+
  geom_pointrange(aes(x = value, y = region, xmin=value,xmax=value,
                      size = region == "Europe",
                      color = variable,shape=rcp), fatten = 1.5, show.legend = TRUE,alpha=0.5,
                  position= position_dodge(width = 0.75))+
  scale_size_manual(values = c(0.4,1))+
  scale_color_manual(values = c(
    "#780001","#d44c3c","#002f49","#669bbb","#beb18b"
  ))+theme_bw()+
  scale_y_discrete(position = "right",
                   labels=c("EU", "SEEU", "SEU", "CEU",
                            "NEU", "WEU")) +
  geom_hline(yintercept=c(1.5,2.5,3.5,4.5,5.5),size=.2,color="grey70",linetype=2)+
  scale_shape_manual(values = c(15,21))+
  scale_x_continuous(expand=c(0.02,0.02),labels=percent)+
  labs(x ="Regional growth rate of heat-related mortality with global warming",
       y = NULL)+
  theme(strip.background = element_rect(
    color="transparent", fill="grey80"),
    strip.text.x = element_text(
      size =8, color = "white",face = "bold" ), 
    panel.grid = element_blank(),
    axis.title.y= element_blank(),
    axis.title.x= element_text(color="black",hjust = 0.5,vjust=1,size =unit(8,"pt")),
    axis.text.x = element_text(color="black",hjust = 1,vjust=1,size =unit(8,"pt")),
    axis.text.y = element_text(color="black",size = unit(8,"pt")),
    legend.position="NA"
  ) 
ggsave(paste0(figout,"/fig2c.pdf"),g2c, width=6, height=13.3, units="cm", scale=1)

#############用于Result part 2 描述的数据##############
allm = do.call(rbind,lapply(split(all,all$rcp),function(grs){
  k = data.frame(region="Europe",rcp=unique(grs$rcp),
                 ssp1 = round(summary(lm(Pre_ssp1_BSL~warming,grs))$coefficients[2],4),
                 ssp2 = round(summary(lm(Pre_ssp2_BSL~warming,grs))$coefficients[2],4),
                 ssp3 = round(summary(lm(Pre_ssp3_BSL~warming,grs))$coefficients[2],4),
                 ssp4 = round(summary(lm(Pre_ssp4_BSL~warming,grs))$coefficients[2],4),
                 ssp5 = round(summary(lm(Pre_ssp5_BSL~warming,grs))$coefficients[2],4))
  return(k)
}))
allml= do.call(rbind,lapply(split(all,all$rcp),function(grs){
  k = data.frame(region="Europe",rcp=unique(grs$rcp),
                 ssp1 = round(summary(lm(Pre_ssp1_BSL_low~warming,grs))$coefficients[2],4),
                 ssp2 = round(summary(lm(Pre_ssp2_BSL_low~warming,grs))$coefficients[2],4),
                 ssp3 = round(summary(lm(Pre_ssp3_BSL_low~warming,grs))$coefficients[2],4),
                 ssp4 = round(summary(lm(Pre_ssp4_BSL_low~warming,grs))$coefficients[2],4),
                 ssp5 = round(summary(lm(Pre_ssp5_BSL_low~warming,grs))$coefficients[2],4))
  return(k)
}))
allmu = do.call(rbind,lapply(split(all,all$rcp),function(grs){
  k = data.frame(region="Europe",rcp=unique(grs$rcp),
                 ssp1 = round(summary(lm(Pre_ssp1_BSL_up~warming,grs))$coefficients[2],4),
                 ssp2 = round(summary(lm(Pre_ssp2_BSL_up~warming,grs))$coefficients[2],4),
                 ssp3 = round(summary(lm(Pre_ssp3_BSL_up~warming,grs))$coefficients[2],4),
                 ssp4 = round(summary(lm(Pre_ssp4_BSL_up~warming,grs))$coefficients[2],4),
                 ssp5 = round(summary(lm(Pre_ssp5_BSL_up~warming,grs))$coefficients[2],4))
  return(k)
}))
library(reshape2)
library(scales)
allm = melt(allm,id=c("region","rcp"))
allml = melt(allml,id=c("region","rcp"),value.name = "low")
allmu = melt(allmu,id=c("region","rcp"),value.name = "up")
allm = merge(allm,allml,by=c("region","rcp","variable"))
allm = merge(allm,allmu,by=c("region","rcp","variable"))


#####################################################
g = premor%>%group_by(rcp,year,warming)%>%
  summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL), 
            Pre_ssp1_BSL_low= sum(Pre_ssp1_BSL_low), Pre_ssp1_BSL_up= sum(Pre_ssp1_BSL_up),
            Pre_ssp2_BSL = sum(Pre_ssp2_BSL), 
            Pre_ssp2_BSL_low= sum(Pre_ssp2_BSL_low), Pre_ssp2_BSL_up= sum(Pre_ssp2_BSL_up),
            Pre_ssp3_BSL= sum(Pre_ssp3_BSL),
            Pre_ssp3_BSL_low= sum(Pre_ssp3_BSL_low),Pre_ssp3_BSL_up= sum(Pre_ssp3_BSL_up),
            Pre_ssp4_BSL= sum(Pre_ssp4_BSL),
            Pre_ssp4_BSL_low= sum(Pre_ssp4_BSL_low), Pre_ssp4_BSL_up= sum(Pre_ssp4_BSL_up),
            Pre_ssp5_BSL= sum(Pre_ssp5_BSL),
            Pre_ssp5_BSL_low= sum(Pre_ssp5_BSL_low), Pre_ssp5_BSL_up= sum(Pre_ssp5_BSL_up),
            Base_2022 = sum(Base_2022),
            Base=sum(Base))

compar = do.call(rbind,lapply(split(g,g$rcp),function(r){
  ym = data.frame(rcp= unique(r$rcp),
                  ssp1 = mean(r$Pre_ssp1_BSL[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp1_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp1.low = mean(r$Pre_ssp1_BSL_low[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp1_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp1.up = mean(r$Pre_ssp1_BSL_up[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp1_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp2 = mean(r$Pre_ssp2_BSL[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp2_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp2.low = mean(r$Pre_ssp2_BSL_low[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp2_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp2.up = mean(r$Pre_ssp2_BSL_up[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp2_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp3 = mean(r$Pre_ssp3_BSL[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp3_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp3.low = mean(r$Pre_ssp3_BSL_low[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp3_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp3.up = mean(r$Pre_ssp3_BSL_up[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp3_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp4 = mean(r$Pre_ssp4_BSL[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp4_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp4.low = mean(r$Pre_ssp4_BSL_low[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp4_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp4.up = mean(r$Pre_ssp4_BSL_up[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp4_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp5 = mean(r$Pre_ssp5_BSL[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp5_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp5.low = mean(r$Pre_ssp5_BSL_low[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp5_BSL[which(r$year>=2025&r$year<=2030)]),
                  ssp5.up = mean(r$Pre_ssp5_BSL_up[which(r$year>=2075&r$year<=2080)])/mean(r$Pre_ssp5_BSL[which(r$year>=2025&r$year<=2030)])
  )
  return(ym)
}))

library(tidyr)

df_long <- compar %>%
  pivot_longer(cols = starts_with("ssp"), 
               names_to = "scenario", 
               values_to = "value") %>%
  separate(scenario, into = c("scenario", "suffix"), sep = "\\.")
df1  = df_long[is.na(df_long$suffix),-c(3)]
df2 = df_long[which(df_long$suffix=="low"),-c(3)]
colnames(df2)[3] = "low"
df3 = df_long[which(df_long$suffix=="up"),-c(3)]
colnames(df3)[3] = "up"
compar = merge(df1,df2,by=c("rcp","scenario"))
compar = merge(compar,df3,by=c("rcp","scenario"))
rm(df_long,df1,df2,df3)
library(scales)
f2a = ggplot()+
  geom_errorbar(data = compar, aes(ymin =low, ymax=up,
                                   x=scenario,group=rcp),
                size=.5,width=0.4,
                position = position_dodge(width = 0.7))+
  geom_point(data=compar,aes(x=scenario,y=value,
                             color=rcp,shape = rcp),size=1.6,
             position = position_dodge(width = 0.7))+
  facet_wrap(~scenario, nrow = 1, scales = "free_x",strip.position = "bottom",
  )+
  theme_bw()+
  scale_shape_manual(values = c(15,21))+
  scale_color_manual(values = c("#b7b5a0","#44757a"))+
  scale_y_continuous(limits = c(0.8,4.5),labels = percent_format())+
  labs(
    y="Percentage change in heat-related deaths\n from 2025-2030 to 2075-2080",
    color = "",shape=""
  )+
  theme(strip.background = element_rect(
    color="transparent", fill="grey80"),
    strip.text.x = element_text(
      size =8, color = "white",face = "bold" ), 
    panel.grid = element_blank(),
    axis.title.x= element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y= element_text(color="black",hjust = 0.5,vjust=1,size =unit(8,"pt")),
    axis.text.y = element_text(color="black",size = unit(8,"pt")),
    legend.position="right"
  ) 
ggsave(paste0(figout,"/fig2a.pdf"),f2a, width=16, height=5, units="cm", scale=1)

rate = g[,c("year","warming","rcp","Pre_ssp1_BSL","Pre_ssp2_BSL",
            "Pre_ssp3_BSL","Pre_ssp4_BSL","Pre_ssp5_BSL")]
library(reshape2)
rate=reshape2::melt(rate,id = c("year","warming","rcp"))
rate$scenario = substring(rate$variable,5,8)
f2b = ggplot(rate,aes(x=warming,y=value))+
  geom_point(aes(color =scenario,shape= rcp,size=rcp),
             alpha=0.6)+
  scale_size_manual(values=c(0.5,0.7))+
  scale_shape_manual(values = c(15,21))+
  geom_smooth(aes(color = scenario,linetype=rcp),
              method = "lm", formula = y ~ x, se = FALSE,size = 0.5)+
  scale_color_manual(values = c(
    "#780001","#d44c3c","#002f49","#669bbb","#beb18b"
  ))+
  facet_wrap(~rcp, nrow = 1, scales = "free",strip.position = "top",
  )+
  theme_bw()+
  scale_x_continuous(expand=c(0,0),breaks = c(1.5,2,2.5,3,3.5,4),
                     expression(paste("Global warming (", degree, "C)")))+
  #scale_y_continuous(expand=c(0,0),limits = c(55000,100000))+
  labs(
    y="Total number of heat-related deaths",
    color = "",shape = "",linetype="",size=""
  )+geom_abline(intercept = 0, slope = 1,color = "black")+
  theme(strip.background = element_blank(),
        strip.text.x = element_text(
          size =9, color = "black",face = "bold" ), 
        panel.grid = element_blank(),
        axis.title.x= element_text(color="black",hjust = 0.5,vjust=1,size =unit(8,"pt")),
        axis.text.x = element_text(color="black",size = unit(8,"pt")),
        axis.title.y= element_text(color="black",hjust = 0.5,vjust=1,size =unit(8,"pt")),
        axis.text.y = element_text(color="black",size = unit(8,"pt")),
        legend.position="top"
  ) 
ggsave(paste0(figout,"/fig2b.pdf"),f2b, width=15, height=10, units="cm", scale=1)
#################################计算老年人口比例############
agedata = do.call(rbind,lapply(split(premor,premor$rcp),function(ka){
  
  a = ka%>%group_by(rcp,age)%>%
    summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL), 
              Pre_ssp2_BSL = sum(Pre_ssp2_BSL), 
              Pre_ssp3_BSL= sum(Pre_ssp3_BSL),
              Pre_ssp4_BSL= sum(Pre_ssp4_BSL),
              Pre_ssp5_BSL= sum(Pre_ssp5_BSL),
              Base_2022 = sum(Base_2022),Base = sum(Base))
  sa = data.frame(rcp=unique(a$rcp),age=a$age,
                  ssp1 = a$Pre_ssp1_BSL/sum(a$Pre_ssp1_BSL),
                  ssp2 = a$Pre_ssp2_BSL/sum(a$Pre_ssp2_BSL),
                  ssp3 = a$Pre_ssp3_BSL/sum(a$Pre_ssp3_BSL),
                  ssp4 = a$Pre_ssp4_BSL/sum(a$Pre_ssp4_BSL),
                  ssp5 = a$Pre_ssp5_BSL/sum(a$Pre_ssp5_BSL))
  return(sa)
}))
agedata = do.call(rbind,lapply(split(premor,premor$rcp),function(ka){
  
  f = ka%>%group_by(rcp,region,age)%>%
    summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL), 
              Pre_ssp2_BSL = sum(Pre_ssp2_BSL), 
              Pre_ssp3_BSL= sum(Pre_ssp3_BSL),
              Pre_ssp4_BSL= sum(Pre_ssp4_BSL),
              Pre_ssp5_BSL= sum(Pre_ssp5_BSL),
              Base_2022 = sum(Base_2022),Base = sum(Base))
  sa= do.call(rbind,lapply(split(f,f$region),function(a){
    sa = data.frame(rcp=unique(a$rcp),age=a$age,
                    ssp1 = a$Pre_ssp1_BSL/sum(a$Pre_ssp1_BSL),
                    ssp2 = a$Pre_ssp2_BSL/sum(a$Pre_ssp2_BSL),
                    ssp3 = a$Pre_ssp3_BSL/sum(a$Pre_ssp3_BSL),
                    ssp4 = a$Pre_ssp4_BSL/sum(a$Pre_ssp4_BSL),
                    ssp5 = a$Pre_ssp5_BSL/sum(a$Pre_ssp5_BSL))
  }))
  return(sa)
}))
################################################################################
spag = premor%>%group_by(rcp,geo)%>%
  summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL), 
            Pre_ssp1_BSL_low= sum(Pre_ssp1_BSL_low), Pre_ssp1_BSL_up= sum(Pre_ssp1_BSL_up),
            Pre_ssp2_BSL = sum(Pre_ssp2_BSL), 
            Pre_ssp2_BSL_low= sum(Pre_ssp2_BSL_low), Pre_ssp2_BSL_up= sum(Pre_ssp2_BSL_up),
            Pre_ssp3_BSL= sum(Pre_ssp3_BSL),
            Pre_ssp3_BSL_low= sum(Pre_ssp3_BSL_low),Pre_ssp3_BSL_up= sum(Pre_ssp3_BSL_up),
            Pre_ssp4_BSL= sum(Pre_ssp4_BSL),
            Pre_ssp4_BSL_low= sum(Pre_ssp4_BSL_low), Pre_ssp4_BSL_up= sum(Pre_ssp4_BSL_up),
            Pre_ssp5_BSL= sum(Pre_ssp5_BSL),
            Pre_ssp5_BSL_low= sum(Pre_ssp5_BSL_low), Pre_ssp5_BSL_up= sum(Pre_ssp5_BSL_up),
            Base_2022 = sum(Base_2022),Base = sum(Base))
name = c("Pre_ssp1_BSL","Pre_ssp2_BSL",
         "Pre_ssp3_BSL","Pre_ssp4_BSL","Pre_ssp5_BSL")
for (ydr in split(spag,spag$rcp)){
  for(n in name){
    k = ydr[,c("geo",n)]
    write.csv(k,file.path(figout,paste0("Predicted_total_heat_deaths_",
                                        unique(ydr$rcp),"_",substring(n,5,8),".csv")))
  }
}





####################################################
fg = premor%>%group_by(geo,rcp,year,warming)%>%
  summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL),Pre_ssp2_BSL = sum(Pre_ssp2_BSL), 
            Pre_ssp3_BSL= sum(Pre_ssp3_BSL),Pre_ssp4_BSL= sum(Pre_ssp4_BSL),
            Pre_ssp5_BSL= sum(Pre_ssp5_BSL),
            Base_ssp1_BSL = sum(Base_ssp1_BSL),Base_ssp2_BSL = sum(Base_ssp2_BSL),#气候效应
            Base_ssp3_BSL = sum(Base_ssp3_BSL),Base_ssp4_BSL = sum(Base_ssp4_BSL),
            Base_ssp5_BSL = sum(Base_ssp5_BSL),
            Pre_ssp1_BR2022=sum(Pre_ssp1_BR2022),Pre_ssp2_BR2022=sum(Pre_ssp2_BR2022),#老龄化效应
            Pre_ssp3_BR2022=sum(Pre_ssp3_BR2022),Pre_ssp4_BR2022=sum(Pre_ssp4_BR2022),
            Pre_ssp5_BR2022=sum(Pre_ssp5_BR2022),
            Pre_2022_BSL = sum(Pre_2022_BSL),#人口效应
            Base_2022 = sum(Base_2022)#总效应
  )
fg = premor%>%group_by(rcp,year,warming)%>%
  summarise(Pre_ssp1_BSL = sum(Pre_ssp1_BSL),Pre_ssp2_BSL = sum(Pre_ssp2_BSL), 
            Pre_ssp3_BSL= sum(Pre_ssp3_BSL),Pre_ssp4_BSL= sum(Pre_ssp4_BSL),
            Pre_ssp5_BSL= sum(Pre_ssp5_BSL),
            Base_ssp1_BSL = sum(Base_ssp1_BSL),Base_ssp2_BSL = sum(Base_ssp2_BSL),#气候效应
            Base_ssp3_BSL = sum(Base_ssp3_BSL),Base_ssp4_BSL = sum(Base_ssp4_BSL),
            Base_ssp5_BSL = sum(Base_ssp5_BSL),
            Pre_ssp1_BR2022=sum(Pre_ssp1_BR2022),Pre_ssp2_BR2022=sum(Pre_ssp2_BR2022),#老龄化效应
            Pre_ssp3_BR2022=sum(Pre_ssp3_BR2022),Pre_ssp4_BR2022=sum(Pre_ssp4_BR2022),
            Pre_ssp5_BR2022=sum(Pre_ssp5_BR2022),
            Pre_2022_BSL = sum(Pre_2022_BSL),#人口效应
            Base_2022 = sum(Base_2022),
            Base = sum(Base)#总效应
  )
rolfig = do.call(rbind,lapply(c("rcp45","rcp85"),function(rcp){
  fg1 = fg[which(fg$rcp==rcp),]
  a = do.call(rbind,lapply(c("Pre_ssp1_BSL","Pre_ssp2_BSL","Pre_ssp3_BSL",
                             "Pre_ssp4_BSL","Pre_ssp5_BSL"),function(ssp){
                               ali = fg1[,c("year",ssp,"Base_2022")]
                               colnames(ali) = c("year","Pre","Base")
                               ali$scenario = "all impacts"
                               Basename = paste0("Base",substring(ssp,4,12))
                               cli = fg1[,c("year",ssp,Basename)]
                               colnames(cli) = c("year","Pre","Base")
                               cli$scenario = "climate impacts"
                               pli = fg1[,c("year",ssp,"Pre_2022_BSL")]
                               colnames(pli) = c("year","Pre","Base")
                               pli$scenario = "pop impacts"
                               obaname = paste0(substring(ssp,1,9),"BR2022")
                               oli = fg1[,c("year",ssp,obaname)]
                               colnames(oli) = c("year","Pre","Base")
                               oli$scenario = "age impacts"
                               fgnew = do.call(rbind,list(ali,cli,pli,oli))
                               fgnew$side = fgnew$Pre>fgnew$Base
                               
                               fgnew$scenario = factor(fgnew$scenario,
                                                       levels = c("all impacts", "climate impacts", "pop impacts", "age impacts"),
                                                       labels = c("all impacts"="Overall impacts",
                                                                  "climate impacts"="Impact of global warming",
                                                                  "pop impacts"="Impact of population size change",
                                                                  "age impacts"="Impact of population aging"))
                               
                               if(length(unique(fgnew$side))>1){
                                 f4a = ggplot(fgnew)+geom_line(aes(x=year,y=Pre),linetype=1,size=1,show.legend = T)+
                                   geom_line(aes(x=year,y=Base),linetype=2,size=0.6,show.legend = T)+
                                   geom_ribbon(aes(x=year,ymax=Pre,ymin=Base,
                                                   fill =side),alpha=0.7)+
                                   facet_wrap(~scenario, nrow = 2)+
                                   scale_fill_manual(values = c(
                                     "#8AABC4","#891F1F"
                                   ))+
                                   theme_bw()+
                                   scale_x_continuous(expand=c(0,0),limits = c(2024,2080),breaks =seq(2025,2080,10))+
                                   #scale_y_continuous(expand=c(0,0),limits = c(55000,100000))+
                                   labs(
                                     y="Total number of heat-related deaths",
                                     color = "",shape = "",linetype="",size=""
                                   )+
                                   theme(strip.background = element_blank(),
                                         strip.text.x = element_text(
                                           size =9, color = "black",face = "bold" ), 
                                         panel.grid = element_blank(),
                                         axis.title.x= element_text(color="black",hjust = 0.5,vjust=1,size =unit(7,"pt")),
                                         axis.text.x = element_text(color="black",size = unit(7,"pt")),
                                         axis.title.y= element_text(color="black",hjust = 0.5,vjust=1,size =unit(7,"pt")),
                                         axis.text.y = element_text(color="black",size = unit(7,"pt")),
                                         legend.position="top"
                                   ) 
                               }else{
                                 f4a = ggplot(fgnew)+geom_line(aes(x=year,y=Pre),linetype=1,size=1,show.legend = T)+
                                   geom_line(aes(x=year,y=Base),linetype=2,size=0.6,show.legend = T)+
                                   geom_ribbon(aes(x=year,ymax=Pre,ymin=Base,
                                                   fill =side),alpha=0.7)+
                                   facet_wrap(~scenario, nrow = 2)+
                                   scale_fill_manual(values = c(
                                     "#891F1F"
                                   ))+
                                   theme_bw()+
                                   scale_x_continuous(expand=c(0,0),limits = c(2024,2080),breaks =seq(2025,2080,10))+
                                   #scale_y_continuous(expand=c(0,0),limits = c(55000,100000))+
                                   labs(
                                     y="Total number of heat-related deaths",
                                     color = "",shape = "",linetype="",size=""
                                   )+
                                   theme(strip.background = element_blank(),
                                         strip.text.x = element_text(
                                           size =9, color = "black",face = "bold" ), 
                                         panel.grid = element_blank(),
                                         axis.title.x= element_text(color="black",hjust = 0.5,vjust=1,size =unit(7,"pt")),
                                         axis.text.x = element_text(color="black",size = unit(7,"pt")),
                                         axis.title.y= element_text(color="black",hjust = 0.5,vjust=1,size =unit(7,"pt")),
                                         axis.text.y = element_text(color="black",size = unit(7,"pt")),
                                         legend.position="top"
                                   ) 
                               }
                               
                               ggsave(paste0(figout,"/fig4a_",rcp,"-",substring(ssp,5,8),".pdf"),f4a, width=15, height=12, units="cm", scale=1)
                               
                               
                               fgnewb  = fgnew%>%group_by(scenario)%>%summarise(Pre = sum(Pre), Base = sum(Base))
                               fgnewb$scenario = factor(fgnewb$scenario,levels = c("Impact of population aging",
                                                                                   "Impact of population size change",
                                                                                   "Impact of global warming",
                                                                                   "Overall impacts"
                               ))
                               fgnewb$diff = fgnewb$Pre-fgnewb$Base
                               fgnewb$side = fgnewb$Pre>fgnewb$Base
                               
                               if(length(unique(fgnewb$side))>1){
                                 f4b = ggplot(fgnewb)+geom_col(aes(y=scenario,x=diff/1000000,fill=side),
                                                               width = 0.5,alpha = 0.8,color="grey20",size=0.5 )+
                                   scale_fill_manual(values = c(
                                     "#8AABC4","#891F1F"
                                   ))+
                                   geom_vline(xintercept = 0,size=0.5,linetype=5,color="grey30")+
                                   theme_bw()+
                                   scale_x_continuous(expand=c(0,0),limits = c(-5,5))+
                                   labs(x= "Deviation of cumulative heat−related deaths from 2024 to 2080 (Millions)",y="")+
                                   theme(strip.background = element_blank(),
                                         strip.text.x = element_text(
                                           size =2, color = "black",face = "bold" ), 
                                         panel.grid = element_blank(),
                                         axis.title.x= element_text(color="black",hjust = 0.5,vjust=1,size =unit(7,"pt")),
                                         axis.text.x = element_text(color="black",size = unit(7,"pt")),
                                         axis.title.y= element_blank(),
                                         axis.text.y = element_text(color="black",size = unit(7,"pt")),
                                         legend.position=""
                                   ) 
                               }else{
                                 f4b = ggplot(fgnewb)+geom_col(aes(y=scenario,x=diff/1000000,fill=side),
                                                               width = 0.5,alpha = 0.8,color="grey20",size=0.5 )+
                                   scale_fill_manual(values = c(
                                     "#891F1F"
                                   ))+
                                   geom_vline(xintercept = 0,size=0.5,linetype=5,color="grey30")+
                                   theme_bw()+
                                   scale_x_continuous(expand=c(0,0),limits = c(-6,6))+
                                   labs(x= "Cumulative heat-related deaths (Millions)",y="")+
                                   theme(strip.background = element_blank(),
                                         strip.text.x = element_text(
                                           size =2, color = "black",face = "bold" ), 
                                         panel.grid = element_blank(),
                                         axis.title.x= element_text(color="black",hjust = 0.5,vjust=1,size =unit(7,"pt")),
                                         axis.text.x = element_text(color="black",size = unit(7,"pt")),
                                         axis.title.y= element_blank(),
                                         axis.text.y = element_text(color="black",size = unit(7,"pt")),
                                         legend.position=""
                                   ) 
                               }
                               
                               #scale_y_continuous(expand=c(0,0),limits = c(55000,100000))+
                               ggsave(paste0(figout,"/fig4b_",rcp,"-",substring(ssp,5,8),".pdf"),f4b, width=15, height=5, units="cm", scale=1)
                               fgnewb$rcp = rcp
                               fgnewb$ssp = substring(ssp,5,8)
                               return(fgnewb)
                               
                             }))
  return(a)
}))
rolfig$m = paste(rolfig$rcp,rolfig$ssp)
f = do.call(rbind,lapply(split(rolfig,rolfig$m),function(mk){
  mk$con = mk$diff/mk$diff[which(mk$scenario=="Overall impacts")]
  return(mk)
}))
f= subset(f,f$scenario!="Overall impacts")
g4c = ggplot(f)+geom_raster(aes(x=paste(rcp,ssp),y=scenario,fill=con*100))+
  scale_fill_gradientn(colours = c('#8AABC4',"#f2f3ec",
                                   '#891F1F'),
                       limits = c(-90, 90))+
  scale_y_discrete(position = "left",
                   labels=c("climate impacts"="Impact of global warming", 
                            "pop impacts"="Impact of population size change",
                            "age impacts"="Impact of population aging"))+
  scale_x_discrete(labels=c("RCP4.5\nSSP1", "RCP4.5\nSSP2", "RCP4.5\nSSP3", "RCP4.5\nSSP4", "RCP4.5\nSSP5", 
                            "RCP8.5\nSSP1", "RCP8.5\nSSP2", "RCP8.5\nSSP3", "RCP8.5\nSSP4", "RCP8.5\nSSP5"))+
  theme_bw()+
  labs(x= "",y="",fill = "Effect of each driver(%)")+
  theme(strip.background = element_blank(),
        strip.text.x = element_text(
          size =2, color = "black",face = "bold" ), 
        panel.grid = element_blank(),
        axis.title.x= element_text(color="black",hjust = 0.5,vjust=0.5,size =unit(7,"pt")),
        axis.text.x = element_text(color="black",size = unit(7,"pt"),
                                   angle=0, vjust=0.5, hjust=0.5),
        axis.title.y= element_blank(),
        axis.text.y = element_text(color="black",size = unit(7,"pt")),
        legend.position="right",
        legend.title =element_text(color="black",size = unit(7,"pt"),angle=90),
        legend.key.size=unit(0.5, "cm"),  # 设置图例的大小
        legend.key.width=unit(0.1, "cm"),
        legend.text = element_text(color="black",size = unit(7,"pt"))
  ) 
ggsave(paste0(figout,"/fig4c.pdf"),g4c , width=16, height=5, units="cm", scale=1)
# g4c = ggplot(rolfig)+geom_raster(aes(x=paste(rcp,ssp),y=scenario,fill=diff))+
#   scale_fill_gradientn(colours = c('#8AABC4',"#f2f3ec",
#                                    '#891F1F'),
#                        limits = c(-5500000, 5500000))+
#   scale_y_discrete(position = "left",
#                    labels=c("all impacts"="Overall impacts", 
#                             "climate impacts"="Impact of global warming", 
#                             "pop impacts"="Impact of population size change",
#                             "age impacts"="Impact of population aging"))+
#   scale_x_discrete(labels=c("RCP4.5\nSSP1", "RCP4.5\nSSP2", "RCP4.5\nSSP3", "RCP4.5\nSSP4", "RCP4.5\nSSP5", 
#                             "RCP8.5\nSSP1", "RCP8.5\nSSP2", "RCP8.5\nSSP3", "RCP8.5\nSSP4", "RCP8.5\nSSP5"))+
#   theme_bw()+
#   labs(x= "",y="")+
#   theme(strip.background = element_blank(),
#         strip.text.x = element_text(
#           size =2, color = "black",face = "bold" ), 
#         panel.grid = element_blank(),
#         axis.title.x= element_text(color="black",hjust = 0.5,vjust=0.5,size =unit(7,"pt")),
#         axis.text.x = element_text(color="black",size = unit(7,"pt"),
#                                    angle=0, vjust=0.5, hjust=0.5),
#         axis.title.y= element_blank(),
#         axis.text.y = element_text(color="black",size = unit(7,"pt")),
#         legend.position="right",
#         legend.title = element_blank(),
#         legend.key.size=unit(0.5, "cm"),  # 设置图例的大小
#         legend.key.width=unit(0.1, "cm"),
#         legend.text = element_text(color="black",size = unit(7,"pt"))
#   ) 
# ggsave(paste0(figout,"/fig4c.pdf"),g4c , width=16, height=5, units="cm", scale=1)





rol = do.call(rbind,lapply(split(rolfig,rolfig$rcp),function(k){
  k = do.call(rbind,lapply(split(k,k$ssp),function(k1){
    k1$contribution = k1$diff/k1$diff[which(k1$scenario=="all impacts")]
    return(k1)
  }))
  return(k)
}))

rolfig = do.call(rbind,lapply(c("rcp45","rcp85"),function(rcp){
  fg1 = fg[which(fg$rcp==rcp),]
  a = do.call(rbind,lapply(c("Pre_ssp1_BSL","Pre_ssp2_BSL","Pre_ssp3_BSL",
                             "Pre_ssp4_BSL","Pre_ssp5_BSL"),function(ssp){
                               ali = fg1[,c("year","warming",ssp,"Base_2022")]
                               colnames(ali) = c("year","warming","Pre","Base")
                               ali$scenario = "all impacts"
                               Basename = paste0("Base",substring(ssp,4,12))
                               cli = fg1[,c("year","warming",ssp,Basename)]
                               colnames(cli) = c("year","warming","Pre","Base")
                               cli$scenario = "climate impacts"
                               pli = fg1[,c("year","warming",ssp,"Pre_2022_BSL")]
                               colnames(pli) = c("year","warming","Pre","Base")
                               pli$scenario = "pop impacts"
                               obaname = paste0(substring(ssp,1,9),"BR2022")
                               oli = fg1[,c("year","warming",ssp,obaname)]
                               colnames(oli) = c("year","warming","Pre","Base")
                               oli$scenario = "age impacts"
                               fgnew = do.call(rbind,list(ali,cli,pli,oli))
                               fgnew$side = fgnew$Pre>fgnew$Base
                               fgnew$diff = fgnew$Pre-fgnew$Base
                               fgnew$scenario = factor(fgnew$scenario,levels = c("all impacts","climate impacts",
                                                                                 "pop impacts","age impacts"))
                               
                               fga = fgnew[which(fgnew$scenario=="all impacts"),]
                               fga = merge(fgnew,fga[,c("year","diff")],by="year")
                               fga$contribution =fga$diff.x/fga$diff.y
                               fga$rcp = rcp
                               fga$ssp = substring(ssp,5,8)
                               return(fga)
                               
                             }))
  
  return(a)
}))


k = rolfig%>%group_by(year,warming,scenario,rcp)%>%summarise(mean=mean(contribution),
                                        min=min(contribution),
                                        max=max(contribution))

ggplot(k)+geom_line(aes(x=year,y=mean,color=scenario))+
  facet_wrap(~rcp, nrow = 1, scales = "free_x",strip.position = "bottom")
  


# g1 =  premor%>%group_by(year)%>%summarise(Pre_ssp2 = sum(Pre_ssp2_BSL),
#                                           Pre_ssp3= sum(Pre_ssp3_BSL),
#                                           Pre_ssp5= sum(Pre_ssp5_BSL))
# g = merge(g,g1,by="year")
# g$PS2 = g$Pre_ssp2_BSL/g$Pre_ssp2
# g$PS2_low = g$Pre_ssp2_BSL_low/g$Pre_ssp2
# g$PS2_up = g$Pre_ssp2_BSL_up/g$Pre_ssp2
# g$PS3 = g$Pre_ssp3_BSL/g$Pre_ssp3
# g$PS3_low = g$Pre_ssp3_BSL_low/g$Pre_ssp3
# g$PS3_up = g$Pre_ssp3_BSL_up/g$Pre_ssp3
# g$PS5 = g$Pre_ssp5_BSL/g$Pre_ssp5
# g$PS5_low = g$Pre_ssp5_BSL_low/g$Pre_ssp5
# g$PS5_up = g$Pre_ssp5_BSL_up/g$Pre_ssp5
# write.csv(g,file.path(outdir,"Predicted_mortality_region.csv"),row.names = F)
# 
# ###################拆分影响因素####################
# premor$climate = premor$Pre_2022-premor$Base_2022 #pre_2022只改变气候数据，人口不变的
# premor$climate_low = premor$Pre_2022_low-premor$Base_2022
# premor$climate_up = premor$Pre_2022_up-premor$Base_2022
# premor$a_BSL= premor$Base_2022_BSL-premor$Base_2022#a只改变人口结构
# premor$a_BSL_low= premor$Base_2022_BSL_low-premor$Base_2022
# premor$a_BSL_up= premor$Base_2022_BSL_up-premor$Base_2022
# premor$a_NIRMIGR= premor$Base_2022_NIRMIGR-premor$Base_2022
# premor$a_NIRMIGR_low= premor$Base_2022_NIRMIGR_low-premor$Base_2022
# premor$a_NIRMIGR_up= premor$Base_2022_NIRMIGR_low-premor$Base_2022
# premor$a_NMIGR= premor$Base_2022_NMIGR-premor$Base_2022
# premor$a_NMIGR_low= premor$Base_2022_NMIGR_low-premor$Base_2022
# premor$a_NMIGR_up= premor$Base_2022_NMIGR_up-premor$Base_2022
# premor$cpa_ssp2 = premor$Pre_ssp2_BSL-premor$Base_2022#cpa是人口、人口结构、气候均改变
# premor$cpa_ssp2_up  = premor$Pre_ssp2_BSL_up-premor$Base_2022
# premor$cpa_ssp2_low  = premor$Pre_ssp2_BSL_low-premor$Base_2022
# premor$cpa_ssp3 = premor$Pre_ssp3_BSL-premor$Base_2022
# premor$cpa_ssp3_up  = premor$Pre_ssp3_BSL_up-premor$Base_2022
# premor$cpa_ssp3_low  = premor$Pre_ssp3_BSL_low-premor$Base_2022
# premor$cpa_ssp5 = premor$Pre_ssp5_BSL-premor$Base_2022
# premor$cpa_ssp5_up  = premor$Pre_ssp5_BSL_up-premor$Base_2022
# premor$cpa_ssp5_low  = premor$Pre_ssp5_BSL_low-premor$Base_2022
# 
# 
# premor$p_ssp2 = premor$Base_ssp2_BR2022-premor$Base_2022 #p只改变人口总数
# premor$p_ssp2_low = premor$Base_ssp2_BR2022_low-premor$Base_2022 
# premor$p_ssp2_up = premor$Base_ssp2_BR2022_up-premor$Base_2022 
# premor$p_ssp3 = premor$Base_ssp3_BR2022-premor$Base_2022
# premor$p_ssp3_low = premor$Base_ssp3_BR2022_low-premor$Base_2022 
# premor$p_ssp3_up = premor$Base_ssp3_BR2022_up-premor$Base_2022 
# premor$p_ssp5 = premor$Base_ssp5_BR2022-premor$Base_2022
# premor$p_ssp5_low = premor$Base_ssp5_BR2022_low-premor$Base_2022 
# premor$p_ssp5_up = premor$Base_ssp5_BR2022_up-premor$Base_2022 
# 
# premor$cp_ssp2 = premor$Pre_ssp2_BR2022-premor$Pre_2022- #cp人口和气候协同作用
#   premor$Base_ssp2_BR2022+premor$Base_2022
# premor$cp_ssp3 = premor$Pre_ssp3_BR2022-premor$Pre_2022-
#   premor$Base_ssp3_BR2022+premor$Base_2022
# premor$cp_ssp5 = premor$Pre_ssp5_BR2022-premor$Pre_2022-
#   premor$Base_ssp5_BR2022+premor$Base_2022
# 
# premor$ca_BSL = premor$Pre_2022_BSL-premor$Pre_2022- #ca 气候和人口结构协同作用
#   premor$Base_2022_BSL+premor$Base_2022
# premor$ca_NIRMIGR = premor$Pre_2022_NIRMIGR-premor$Pre_2022-
#   premor$Base_2022_NIRMIGR+premor$Base_2022
# premor$ca_NMIGR = premor$Pre_2022_NMIGR-premor$Pre_2022-
#   premor$Base_2022_NMIGR+premor$Base_2022
# 
# premor$pa_ssp2_BSL = premor$Base_ssp2_BSL-premor$Base_ssp2_BR2022- #pa 人口和人口结构协同作用
#   premor$Base_2022_BSL+premor$Base_2022 
# premor$pa_ssp3_BSL = premor$Base_ssp3_BSL-premor$Base_ssp3_BR2022-
#   premor$Base_2022_BSL+premor$Base_2022
# premor$pa_ssp5_BSL = premor$Base_ssp5_BSL-premor$Base_ssp5_BR2022-
#   premor$Base_2022_BSL+premor$Base_2022
# premor$pa_ssp2_NIRMIGR = premor$Base_ssp2_NIRMIGR-premor$Base_ssp2_BR2022-
#   premor$Base_2022_NIRMIGR+premor$Base_2022
# premor$pa_ssp3_NIRMIGR = premor$Base_ssp3_NIRMIGR-premor$Base_ssp3_BR2022-
#   premor$Base_2022_NIRMIGR+premor$Base_2022
# premor$pa_ssp5_NIRMIGR = premor$Base_ssp5_NIRMIGR-premor$Base_ssp5_BR2022-
#   premor$Base_2022_NIRMIGR+premor$Base_2022
# premor$pa_ssp2_NMIGR = premor$Base_ssp2_NMIGR-premor$Base_ssp2_BR2022-
#   premor$Base_2022_NMIGR+premor$Base_2022
# premor$pa_ssp3_NMIGR = premor$Base_ssp3_NMIGR-premor$Base_ssp3_BR2022-
#   premor$Base_2022_NMIGR+premor$Base_2022
# premor$pa_ssp5_NMIGR = premor$Base_ssp5_NMIGR-premor$Base_ssp5_BR2022-
#   premor$Base_2022_NMIGR+premor$Base_2022
# 
# 
# premor$syne_ssp2_BSL = premor$Pre_ssp2_BSL- premor$Pre_ssp2_BR2022- #气候，人口，人口结构协同
#   premor$Pre_2022_BSL-premor$Base_ssp2_BSL+premor$Base_ssp2_BR2022+
#   premor$Base_2022_BSL+premor$Pre_2022-premor$Base_2022
# premor$syne_ssp3_BSL = premor$Pre_ssp3_BSL- premor$Pre_ssp3_BR2022-
#   premor$Pre_2022_BSL-premor$Base_ssp3_BSL+premor$Base_ssp3_BR2022+
#   premor$Base_2022_BSL+premor$Pre_2022-premor$Base_2022
# premor$syne_ssp5_BSL = premor$Pre_ssp5_BSL- premor$Pre_ssp5_BR2022-
#   premor$Pre_2022_BSL-premor$Base_ssp5_BSL+premor$Base_ssp5_BR2022+
#   premor$Base_2022_BSL+premor$Pre_2022-premor$Base_2022
# premor$syne_ssp2_NIRMIGR = premor$Pre_ssp2_NIRMIGR- premor$Pre_ssp2_BR2022-
#   premor$Pre_2022_NIRMIGR-premor$Base_ssp2_NIRMIGR+premor$Base_ssp2_BR2022+
#   premor$Base_2022_NIRMIGR+premor$Pre_2022-premor$Base_2022
# premor$syne_ssp3_NIRMIGR = premor$Pre_ssp3_NIRMIGR- premor$Pre_ssp3_BR2022-
#   premor$Pre_2022_NIRMIGR-premor$Base_ssp3_NIRMIGR+premor$Base_ssp3_BR2022+
#   premor$Base_2022_NIRMIGR+premor$Pre_2022-premor$Base_2022
# premor$syne_ssp5_NIRMIGR = premor$Pre_ssp5_NIRMIGR- premor$Pre_ssp5_BR2022-
#   premor$Pre_2022_NIRMIGR-premor$Base_ssp5_NIRMIGR+premor$Base_ssp5_BR2022+
#   premor$Base_2022_NIRMIGR+premor$Pre_2022-premor$Base_2022
# premor$syne_ssp2_NMIGR = premor$Pre_ssp2_NMIGR- premor$Pre_ssp2_BR2022-
#   premor$Pre_2022_NMIGR-premor$Base_ssp2_NMIGR+premor$Base_ssp2_BR2022+
#   premor$Base_2022_NMIGR+premor$Pre_2022-premor$Base_2022
# premor$syne_ssp3_NMIGR = premor$Pre_ssp3_NMIGR- premor$Pre_ssp3_BR2022-
#   premor$Pre_2022_NMIGR-premor$Base_ssp3_NMIGR+premor$Base_ssp3_BR2022+
#   premor$Base_2022_NMIGR+premor$Pre_2022-premor$Base_2022
# premor$syne_ssp5_NMIGR = premor$Pre_ssp5_NMIGR- premor$Pre_ssp5_BR2022-
#   premor$Pre_2022_NMIGR-premor$Base_ssp5_NMIGR+premor$Base_ssp5_BR2022+
#   premor$Base_2022_NMIGR+premor$Pre_2022-premor$Base_2022
# premor$Humidex_diff = premor$Humidex_mean-premor$Humidex_2022
# 
# write.csv(premor,file.path(outdir,"Factor_effect_all.csv"),row.names = F)
#  
# 
# 
# ######
# premor = read.csv(file.path(outdir,"Factor_effect_all.csv"),stringsAsFactors = F)
# #############Fig 4##################
# premor = subset(premor,premor$year<=2090)
# rcp45list = paste("rcp45",seq(min(premor$warming[which(premor$rcp=="rcp45")]),
#                               max(premor$warming[which(premor$rcp=="rcp45")]),0.2))
# rcp85list = paste("rcp85",seq(min(premor$warming[which(premor$rcp=="rcp85")]),
#                               max(premor$warming[which(premor$rcp=="rcp85")]),0.2))
# premor$warm = paste(premor$rcp,premor$warming)
# premor = premor[which(premor$warm%in%rcp45list|premor$warm%in%rcp85list),]
# 
# p3 = premor%>%group_by(country,rcp,year,warming)%>%
#   summarise(Base_2022 = sum(Base_2022),
#             cpa_ssp2 = sum(cpa_ssp2),cpa_ssp2_low  =  sum(cpa_ssp2_low),cpa_ssp2_up =  sum(cpa_ssp2_up),
#             cpa_ssp3 = sum(cpa_ssp3),cpa_ssp3_low  =  sum(cpa_ssp3_low),cpa_ssp3_up =  sum(cpa_ssp3_up),
#             cpa_ssp5 = sum(cpa_ssp5),cpa_ssp5_low  =  sum(cpa_ssp5_low),cpa_ssp5_up =  sum(cpa_ssp5_up),
#             climate = sum(climate),climate_low = sum(climate_low),climate_up = sum(climate_up),
#             p_ssp2=sum(p_ssp2),p_ssp2_low  =  sum(p_ssp2_low),p_ssp2_up =  sum(cpa_ssp2_up),
#             p_ssp3=sum(p_ssp3),p_ssp3_low  =  sum(p_ssp3_low),p_ssp3_up =  sum(cpa_ssp3_up),
#             p_ssp5=sum(p_ssp5),p_ssp5_low  =  sum(p_ssp5_low),p_ssp5_up =  sum(cpa_ssp5_up),
#             a_BSL=sum(a_BSL), a_BSL_low=sum(a_BSL_low),a_BSL_up=sum(a_BSL_up),
#             a_NIRMIGR=sum(a_NIRMIGR),a_NIRMIGR_low=sum(a_NIRMIGR_low),a_NIRMIGR_up=sum(a_NIRMIGR_up),
#             a_NMIGR=sum(a_NMIGR),a_NMIGR_low=sum(a_NMIGR_low),a_NMIGR_up=sum(a_NMIGR_up),
#             cp_ssp2 = sum(cp_ssp2),cp_ssp3=sum(cp_ssp3),cp_ssp5=sum(cp_ssp5),
#             ca_BSL = sum(ca_BSL),ca_NIRMIGR = sum(ca_NIRMIGR),ca_NMIGR=sum(ca_NMIGR),
#             pa_ssp2_BSL = sum(pa_ssp2_BSL),pa_ssp3_BSL=sum(pa_ssp3_BSL),
#             pa_ssp5_BSL=sum(pa_ssp5_BSL),pa_ssp2_NIRMIGR = sum(pa_ssp2_NIRMIGR),
#             pa_ssp3_NIRMIGR = sum(pa_ssp3_NIRMIGR),pa_ssp5_NIRMIGR = sum(pa_ssp5_NIRMIGR),
#             pa_ssp2_NMIGR = sum(pa_ssp2_NMIGR),
#             pa_ssp3_NMIGR = sum(pa_ssp3_NMIGR),pa_ssp5_NMIGR = sum(pa_ssp5_NMIGR),
#             syne_ssp2_BSL = sum(syne_ssp2_BSL),syne_ssp3_BSL = sum(syne_ssp3_BSL),syne_ssp5_BSL = sum(syne_ssp5_BSL),
#             syne_ssp2_NIRMIGR = sum(syne_ssp2_NIRMIGR),syne_ssp3_NIRMIGR = sum(syne_ssp3_NIRMIGR),
#             syne_ssp5_NIRMIGR = sum(syne_ssp5_NIRMIGR),syne_ssp2_NMIGR = sum(syne_ssp2_NMIGR),
#             syne_ssp3_NMIGR = sum(syne_ssp3_NMIGR),syne_ssp5_NMIGR = sum(syne_ssp5_NMIGR),
#             Humidex_diff=mean(Humidex_diff))
# 
# library(reshape2)
# library(gghalves)
# p3$label = paste(p3$rcp,p3$warming)
# pd3 = melt(p3[which(p3$label=="rcp85 2.5"|p3$label=="rcp85 3.5"),
#               c("country","rcp","warming","climate","p_ssp3","a_BSL",
#                 "cp_ssp3","ca_BSL","pa_ssp3_BSL","syne_ssp3_BSL")],
#            id=c("country","rcp","warming"))
# pd3 = merge(pd3,p3[which(p3$label=="rcp85 3.5"|p3$label=="rcp85 2.5"),
#                    c("country","warming","Base_2022")])
# pd3$ratio = pd3$value/pd3$Base_2022*100
# 
# summary(p3[which(p3$label=="rcp85 3.5"),
#            c("climate","p_ssp3","a_BSL",
#              "cp_ssp3","ca_BSL","pa_ssp3_BSL","syne_ssp3_BSL")]/
#           p3$Base_2022[which(p3$label=="rcp85 3.5")])
# summary(p3[which(p3$label=="rcp85 2.5"),
#            c("climate","p_ssp3","a_BSL",
#              "cp_ssp3","ca_BSL","pa_ssp3_BSL","syne_ssp3_BSL")]/
#           p3$Base_2022[which(p3$label=="rcp85 2.5")])
# 
# color_palette <- colorRampPalette(c("#800000", "#e7d4d1","black"))(25)
# g2 = ggplot() + facet_wrap(~warming,ncol=2)+
#   geom_violin(data = pd3,aes(x = variable, y = ratio,fill =variable),
#               alpha=0.5,color="grey80",
#               position = position_dodge(width = 1),
#               draw_quantiles= c(0.5),scale= 'width',
#               show.legend = T) +
#   geom_boxplot(data = pd3,aes(x = variable, y = ratio),
#                color = 'white', width= 0.15,size= 0.8, fill= NA)+
#   geom_point(data = pd3,aes(x = variable, y = ratio,
#                             color=country,shape=country),
#              position = 'jitter',size= 0.5)+ 
#   scale_fill_manual(values = c("#23748b","#a8cfdd","#2e744b",
#                                "#e6c35c","#c0c0c0","#453028","#800000"),
#                     labels = c("CE:climate effect","PE:population effect","AE:aging effect",
#                                "CPIE:climate and population interaction effect",
#                                "CAIE:climate and aging interaction effect",
#                                "PAIE:population and aging interaction effect",
#                                "CPAIE:climate, population and aging interaction effect"
#                                )) +
#   scale_x_discrete(labels = c("CE", "PE", "AE", "CPIE","CAIE","PAIE","CPAIE")) +  # 修改 x 轴坐标文字
#   scale_color_manual(values = color_palette) +
#   scale_shape_manual(values = 1:25) +  # 手动指定形状
#   labs(x = '', y = 'Heat-related mortality ratio relative to baseline(2022, %)')+
#   theme(
#     plot.title = element_text(angle=0,size= unit(7,"pt")),
#     axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.text.y=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.title.x=element_text(size= unit(7,"pt")),
#     axis.title.y=element_text(size= unit(7,"pt")),
#     legend.position="top",
#     legend.title=element_text(size= unit(7,"pt")),
#     legend.text=element_text(size= unit(7,"pt")),
#     panel.background = element_rect(fill = 'transparent', color = 'black'),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_blank(),
#     axis.line.y = element_blank(),
#     legend.background = element_blank(),  # 去掉图例背景
#     legend.box = "horizontal",  # 图例水平排布
#     legend.spacing.x = unit(0.5, 'cm')  # 调整图例之间的间距
#   )
# 
# p4 = premor%>%group_by(rcp,year,warming)%>%
#   summarise(Base_2022 = sum(Base_2022),
#             cpa_ssp2 = sum(cpa_ssp2),cpa_ssp2_low  =  sum(cpa_ssp2_low),cpa_ssp2_up =  sum(cpa_ssp2_up),
#             cpa_ssp3 = sum(cpa_ssp3),cpa_ssp3_low  =  sum(cpa_ssp3_low),cpa_ssp3_up =  sum(cpa_ssp3_up),
#             cpa_ssp5 = sum(cpa_ssp5),cpa_ssp5_low  =  sum(cpa_ssp5_low),cpa_ssp5_up =  sum(cpa_ssp5_up),
#             climate = sum(climate),climate_low = sum(climate_low),climate_up = sum(climate_up),
#             p_ssp2=sum(p_ssp2),p_ssp2_low  =  sum(p_ssp2_low),p_ssp2_up =  sum(cpa_ssp2_up),
#             p_ssp3=sum(p_ssp3),p_ssp3_low  =  sum(p_ssp3_low),p_ssp3_up =  sum(cpa_ssp3_up),
#             p_ssp5=sum(p_ssp5),p_ssp5_low  =  sum(p_ssp5_low),p_ssp5_up =  sum(cpa_ssp5_up),
#             a_BSL=sum(a_BSL), a_BSL_low=sum(a_BSL_low),a_BSL_up=sum(a_BSL_up),
#             a_NIRMIGR=sum(a_NIRMIGR),a_NIRMIGR_low=sum(a_NIRMIGR_low),a_NIRMIGR_up=sum(a_NIRMIGR_up),
#             a_NMIGR=sum(a_NMIGR),a_NMIGR_low=sum(a_NMIGR_low),a_NMIGR_up=sum(a_NMIGR_up),
#             cp_ssp2 = sum(cp_ssp2),cp_ssp3=sum(cp_ssp3),cp_ssp5=sum(cp_ssp5),
#             ca_BSL = sum(ca_BSL),ca_NIRMIGR = sum(ca_NIRMIGR),ca_NMIGR=sum(ca_NMIGR),
#             pa_ssp2_BSL = sum(pa_ssp2_BSL),pa_ssp3_BSL=sum(pa_ssp3_BSL),
#             pa_ssp5_BSL=sum(pa_ssp5_BSL),pa_ssp2_NIRMIGR = sum(pa_ssp2_NIRMIGR),
#             pa_ssp3_NIRMIGR = sum(pa_ssp3_NIRMIGR),pa_ssp5_NIRMIGR = sum(pa_ssp5_NIRMIGR),
#             pa_ssp2_NMIGR = sum(pa_ssp2_NMIGR),
#             pa_ssp3_NMIGR = sum(pa_ssp3_NMIGR),pa_ssp5_NMIGR = sum(pa_ssp5_NMIGR),
#             syne_ssp2_BSL = sum(syne_ssp2_BSL),syne_ssp3_BSL = sum(syne_ssp3_BSL),syne_ssp5_BSL = sum(syne_ssp5_BSL),
#             syne_ssp2_NIRMIGR = sum(syne_ssp2_NIRMIGR),syne_ssp3_NIRMIGR = sum(syne_ssp3_NIRMIGR),
#             syne_ssp5_NIRMIGR = sum(syne_ssp5_NIRMIGR),syne_ssp2_NMIGR = sum(syne_ssp2_NMIGR),
#             syne_ssp3_NMIGR = sum(syne_ssp3_NMIGR),syne_ssp5_NMIGR = sum(syne_ssp5_NMIGR),
#             Humidex_diff=mean(Humidex_diff))
# 
# pd4 = melt(p4[which(p4$rcp=="rcp85"), c("rcp","year","warming",
#                                         "climate","a_BSL", "p_ssp3",
#                                         "cp_ssp3","ca_BSL","pa_ssp3_BSL","syne_ssp3_BSL")],
#            id=c("warming","year","rcp"))
# pd4a = pd4%>%group_by(rcp,year,warming)%>%summarise(all=sum(value))
# pd4 = merge(pd4,pd4a,id=c("rcp","year","warming"))
# pd4$precent = pd4$value/pd4$all
# #pd4$variable = factor(pd4$variable, levels = c("p_ssp3","a_BSL","climate"))
# #pd4=pd4[which(pd4$variable=="climate"|pd4$variable=="a_BSL"|pd4$variable=="p_ssp3"),]
# g3 = ggplot()+geom_tile(data=pd4,aes(x=warming,y=variable,fill=precent),
#                         alpha=0.8)+
#   scale_fill_gradientn(colors = c("#142125","#8cbcbd","gray95","#bf8f85", "#74271d"),
#                        breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1.3, 1.3))+
#   scale_x_continuous(expand = c(0.01, 0.01),breaks = seq(1.5,4.7,0.5)) + 
#   labs(x = 'Global warming')+
#   theme_bw()+
#   theme(
#     plot.title = element_text(angle=0,size= unit(7,"pt")),
#     axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.text.y=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.title.x=element_text(size= unit(7,"pt")),
#     axis.title.y=element_text(size= unit(7,"pt")),
#     legend.position="top",
#     legend.title=element_text(size= unit(7,"pt")),
#     legend.text=element_text(size= unit(7,"pt")),
#     panel.background = element_rect(fill = 'transparent', color = 'black'),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_blank(),
#     axis.line.y = element_blank(),
#     legend.background = element_blank(),  # 去掉图例背景
#     legend.box = "horizontal",  # 图例水平排布
#     legend.spacing.x = unit(0.5, 'cm')  # 调整图例之间的间距
#   )
# 
# figdir = "Figure_0912"
# ggsave(file.path(figdir,"fig4a.pdf"),g2, width=15, height=12, units="cm", scale=1)
# ggsave(file.path(figdir,"fig4b.pdf"),g3, width=15, height=7, units="cm", scale=1)
# 
# 
# 
# 
# pd4 = melt(p4[which(p4$rcp=="rcp85"), c("rcp","year","warming",
#                                         "climate","a_BSL", "p_ssp2",
#                                         "cp_ssp3","ca_BSL","pa_ssp2_BSL","syne_ssp2_BSL")],
#            id=c("warming","year","rcp"))
# pd4a = pd4%>%group_by(rcp,year,warming)%>%summarise(all=sum(value))
# pd4 = merge(pd4,pd4a,id=c("rcp","year","warming"))
# pd4$precent = pd4$value/pd4$all
# #pd4$variable = factor(pd4$variable, levels = c("p_ssp3","a_BSL","climate"))
# #pd4=pd4[which(pd4$variable=="climate"|pd4$variable=="a_BSL"|pd4$variable=="p_ssp3"),]
# g3 = ggplot()+geom_tile(data=pd4,aes(x=warming,y=variable,fill=precent),
#                         alpha=0.8)+
#   scale_fill_gradientn(colors = c("#142125","#8cbcbd","gray95","#bf8f85", "#74271d"),
#                        breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1.3, 1.3))+
#   scale_x_continuous(expand = c(0.01, 0.01),breaks = seq(1.5,4.7,0.5)) + 
#   labs(x = 'Global warming')+
#   theme_bw()+
#   theme(
#     plot.title = element_text(angle=0,size= unit(7,"pt")),
#     axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.text.y=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.title.x=element_text(size= unit(7,"pt")),
#     axis.title.y=element_text(size= unit(7,"pt")),
#     legend.position="top",
#     legend.title=element_text(size= unit(7,"pt")),
#     legend.text=element_text(size= unit(7,"pt")),
#     panel.background = element_rect(fill = 'transparent', color = 'black'),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_blank(),
#     axis.line.y = element_blank(),
#     legend.background = element_blank(),  # 去掉图例背景
#     legend.box = "horizontal",  # 图例水平排布
#     legend.spacing.x = unit(0.5, 'cm')  # 调整图例之间的间距
#   )
# ggsave(file.path(figdir,"figS11_rcp85_ssp2.pdf"),g3, width=15, height=7, units="cm", scale=1)
# 
# pd4 = melt(p4[which(p4$rcp=="rcp85"), c("rcp","year","warming",
#                                         "climate","a_BSL", "p_ssp5",
#                                         "cp_ssp3","ca_BSL","pa_ssp5_BSL","syne_ssp5_BSL")],
#            id=c("warming","year","rcp"))
# pd4a = pd4%>%group_by(rcp,year,warming)%>%summarise(all=sum(value))
# pd4 = merge(pd4,pd4a,id=c("rcp","year","warming"))
# pd4$precent = pd4$value/pd4$all
# #pd4$variable = factor(pd4$variable, levels = c("p_ssp3","a_BSL","climate"))
# #pd4=pd4[which(pd4$variable=="climate"|pd4$variable=="a_BSL"|pd4$variable=="p_ssp3"),]
# g3 = ggplot()+geom_tile(data=pd4,aes(x=warming,y=variable,fill=precent),
#                         alpha=0.8)+
#   scale_fill_gradientn(colors = c("#142125","#8cbcbd","gray95","#bf8f85", "#74271d"),
#                        breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1.3, 1.3))+
#   scale_x_continuous(expand = c(0.01, 0.01),breaks = seq(1.5,4.7,0.5)) + 
#   labs(x = 'Global warming')+
#   theme_bw()+
#   theme(
#     plot.title = element_text(angle=0,size= unit(7,"pt")),
#     axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.text.y=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.title.x=element_text(size= unit(7,"pt")),
#     axis.title.y=element_text(size= unit(7,"pt")),
#     legend.position="top",
#     legend.title=element_text(size= unit(7,"pt")),
#     legend.text=element_text(size= unit(7,"pt")),
#     panel.background = element_rect(fill = 'transparent', color = 'black'),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_blank(),
#     axis.line.y = element_blank(),
#     legend.background = element_blank(),  # 去掉图例背景
#     legend.box = "horizontal",  # 图例水平排布
#     legend.spacing.x = unit(0.5, 'cm')  # 调整图例之间的间距
#   )
# ggsave(file.path(figdir,"figS11_rcp85_ssp5.pdf"),g3, width=15, height=7, units="cm", scale=1)
# 
# 
# pd4 = melt(p4[which(p4$rcp=="rcp45"), c("rcp","year","warming",
#                                         "climate","a_BSL", "p_ssp2",
#                                         "cp_ssp3","ca_BSL","pa_ssp2_BSL","syne_ssp2_BSL")],
#            id=c("warming","year","rcp"))
# pd4a = pd4%>%group_by(rcp,year,warming)%>%summarise(all=sum(value))
# pd4 = merge(pd4,pd4a,id=c("rcp","year","warming"))
# pd4$precent = pd4$value/pd4$all
# #pd4$variable = factor(pd4$variable, levels = c("p_ssp3","a_BSL","climate"))
# #pd4=pd4[which(pd4$variable=="climate"|pd4$variable=="a_BSL"|pd4$variable=="p_ssp3"),]
# g3 = ggplot()+geom_tile(data=pd4,aes(x=warming,y=variable,fill=precent),
#                         alpha=0.8)+
#   scale_fill_gradientn(colors = c("#142125","#8cbcbd","gray95","#bf8f85", "#74271d"),
#                        breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1.3, 1.3))+
#   scale_x_continuous(expand = c(0.01, 0.01),breaks = seq(1.5,4.7,0.5)) + 
#   labs(x = 'Global warming')+
#   theme_bw()+
#   theme(
#     plot.title = element_text(angle=0,size= unit(7,"pt")),
#     axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.text.y=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.title.x=element_text(size= unit(7,"pt")),
#     axis.title.y=element_text(size= unit(7,"pt")),
#     legend.position="top",
#     legend.title=element_text(size= unit(7,"pt")),
#     legend.text=element_text(size= unit(7,"pt")),
#     panel.background = element_rect(fill = 'transparent', color = 'black'),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_blank(),
#     axis.line.y = element_blank(),
#     legend.background = element_blank(),  # 去掉图例背景
#     legend.box = "horizontal",  # 图例水平排布
#     legend.spacing.x = unit(0.5, 'cm')  # 调整图例之间的间距
#   )
# ggsave(file.path(figdir,"figS11_rcp45_ssp2.pdf"),g3, width=15, height=7, units="cm", scale=1)
# 
# 
# pd4 = melt(p4[which(p4$rcp=="rcp45"), c("rcp","year","warming",
#                                         "climate","a_BSL", "p_ssp3",
#                                         "cp_ssp3","ca_BSL","pa_ssp3_BSL","syne_ssp3_BSL")],
#            id=c("warming","year","rcp"))
# pd4a = pd4%>%group_by(rcp,year,warming)%>%summarise(all=sum(value))
# pd4 = merge(pd4,pd4a,id=c("rcp","year","warming"))
# pd4$precent = pd4$value/pd4$all
# #pd4$variable = factor(pd4$variable, levels = c("p_ssp3","a_BSL","climate"))
# #pd4=pd4[which(pd4$variable=="climate"|pd4$variable=="a_BSL"|pd4$variable=="p_ssp3"),]
# g3 = ggplot()+geom_tile(data=pd4,aes(x=warming,y=variable,fill=precent),
#                         alpha=0.8)+
#   scale_fill_gradientn(colors = c("#142125","#8cbcbd","gray95","#bf8f85", "#74271d"),
#                        breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1.3, 1.3))+
#   scale_x_continuous(expand = c(0.01, 0.01),breaks = seq(1.5,4.7,0.5)) + 
#   labs(x = 'Global warming')+
#   theme_bw()+
#   theme(
#     plot.title = element_text(angle=0,size= unit(7,"pt")),
#     axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.text.y=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.title.x=element_text(size= unit(7,"pt")),
#     axis.title.y=element_text(size= unit(7,"pt")),
#     legend.position="top",
#     legend.title=element_text(size= unit(7,"pt")),
#     legend.text=element_text(size= unit(7,"pt")),
#     panel.background = element_rect(fill = 'transparent', color = 'black'),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_blank(),
#     axis.line.y = element_blank(),
#     legend.background = element_blank(),  # 去掉图例背景
#     legend.box = "horizontal",  # 图例水平排布
#     legend.spacing.x = unit(0.5, 'cm')  # 调整图例之间的间距
#   )
# ggsave(file.path(figdir,"figS11_rcp45_ssp3.pdf"),g3, width=15, height=7, units="cm", scale=1)
# 
# 
# pd4 = melt(p4[which(p4$rcp=="rcp45"), c("rcp","year","warming",
#                                         "climate","a_BSL", "p_ssp5",
#                                         "cp_ssp5","ca_BSL","pa_ssp5_BSL","syne_ssp5_BSL")],
#            id=c("warming","year","rcp"))
# pd4a = pd4%>%group_by(rcp,year,warming)%>%summarise(all=sum(value))
# pd4 = merge(pd4,pd4a,id=c("rcp","year","warming"))
# pd4$precent = pd4$value/pd4$all
# #pd4$variable = factor(pd4$variable, levels = c("p_ssp3","a_BSL","climate"))
# #pd4=pd4[which(pd4$variable=="climate"|pd4$variable=="a_BSL"|pd4$variable=="p_ssp3"),]
# g3 = ggplot()+geom_tile(data=pd4,aes(x=warming,y=variable,fill=precent),
#                         alpha=0.8)+
#   scale_fill_gradientn(colors = c("#142125","#8cbcbd","gray95","#bf8f85", "#74271d"),
#                        breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-1.3, 1.3))+
#   scale_x_continuous(expand = c(0.01, 0.01),breaks = seq(1.5,4.7,0.5)) + 
#   labs(x = 'Global warming')+
#   theme_bw()+
#   theme(
#     plot.title = element_text(angle=0,size= unit(7,"pt")),
#     axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.text.y=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
#     axis.title.x=element_text(size= unit(7,"pt")),
#     axis.title.y=element_text(size= unit(7,"pt")),
#     legend.position="top",
#     legend.title=element_text(size= unit(7,"pt")),
#     legend.text=element_text(size= unit(7,"pt")),
#     panel.background = element_rect(fill = 'transparent', color = 'black'),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line.x = element_blank(),
#     axis.line.y = element_blank(),
#     legend.background = element_blank(),  # 去掉图例背景
#     legend.box = "horizontal",  # 图例水平排布
#     legend.spacing.x = unit(0.5, 'cm')  # 调整图例之间的间距
#   )
# ggsave(file.path(figdir,"figS11_rcp45_ssp5.pdf"),g3, width=15, height=7, units="cm", scale=1)