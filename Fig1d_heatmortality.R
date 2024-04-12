rm(list = ls())
gc()
#########绘制可获取年份因热致死人数###################
###########################################
setwd("D:/ATtest/Europe/DLMN_model_V0530")
library(dlnm)
library(splines)
library(ggplot2)
library(reshape2)
library(patchwork)
library(zoo)
library(Epi)
library(data.table)
library(dplyr)
library(scales)
outdir = "pooled_model_age_group_dataV2_ns"
da = read.csv(paste0("pooled_model_age_group_dataV2","/Inputdata_withinage.csv"),stringsAsFactors  = F)
da$week_num = as.integer(substring(da$week,7,8))
da = subset(da,da$week_num>=22&da$week_num<36)
old_model = readRDS(file.path(outdir,"stratamodel_65+.rds"))
print("65+ model has added")
data = subset(da,da$age=="65+")
data$week = as.integer(substring(data$week,7,8))
knots = quantile(data$Humidex_mean,c(80)/100,na.rm=T)
geolist = unique(da$geo)
data$gender_group = paste(data$geo,data$year)
lag = 4
lagnk = 2
cb= crossbasis(data$Humidex_mean,lag=lag,
               argvar = list(fun="ns",knots =  quantile(data$Humidex_mean,c(80)/100,na.rm=T), 
                             Boundary.knots = range(data$Humidex_mean,na.rm=T)),
               arglag= list(knots = logknots(lag, lagnk)),group = data$gender_group)
red <- crossreduce(cb,old_model,at=10:40)
MMT = red$predvar[which.min(red$RRfit)]

DD = onebasis(data$DD_num,fun="strata",breaks=c(1,4))
DN = onebasis(data$DN_num,fun="strata",breaks=c(1,4))

pre = crosspred(cb,old_model,cumul = T,
                at = data$Humidex_mean,cen=MMT,model.link = "log")
f =data.frame(cbind(pre$predvar,pre$matfit[,1],pre$matfit[,1] - 1.96 * pre$matse[,1],pre$matfit[,1] + 1.96 * pre$matse[,1])) 
colnames(f) <- c("Humidex_mean", "rr","rr.low","rr.high")
DDpre = crosspred(DD,old_model,
                  at = seq(0,7),cen=0,model.link = "log")
D_f = data.frame(cbind(as.integer(DDpre$predvar),DDpre$allfit,DDpre$allfit - 1.96 * DDpre$allse,DDpre$allfit + 1.96 * DDpre$allse)) 
colnames(D_f) <- c("DD_num", "rr.dd","rr.dd.low","rr.dd.high")
DNpre = crosspred(DN,old_model,
                  at = seq(0,7),cen=0,model.link = "log")#,model.link = "log"
DN_f = data.frame(cbind(as.integer(DNpre$predvar),DNpre$allfit,DNpre$allfit - 1.96 * DNpre$allse,DNpre$allfit + 1.96 * DNpre$allse)) 
colnames(DN_f) <- c("DN_num", "rr.dn","rr.dn.low","rr.dn.high")

preda = merge(data,f,by="Humidex_mean")
preda = merge(preda,D_f,by="DD_num")
preda = merge(preda,DN_f,by="DN_num")
preda$morrisk = exp(preda$rr+preda$rr.dd+preda$rr.dn)-1
preda$morrisk.low = exp(preda$rr.low+preda$rr.dd.low+preda$rr.dn.low)-1
preda$morrisk.high = exp(preda$rr.high+preda$rr.dd.high+preda$rr.dn.high)-1
preda$predicted = preda$morrisk*preda$death
preda$predicted.low = preda$morrisk.low*preda$death
preda$predicted.high = preda$morrisk.high*preda$death
preda$predicted[preda$predicted<0]=0
preda$predicted.low[preda$predicted.low<0]=0
preda$predicted.high[preda$predicted.high<0]=0
#################################16-65##########################
model = readRDS(file.path(outdir,"stratamodel_16-65.rds"))
data = subset(da,da$age=="15-65")
data$week = as.integer(substring(data$week,7,8))
knots = quantile(data$Humidex_mean,c(80)/100,na.rm=T)
geolist = unique(da$geo)
data$gender_group = paste(data$geo,data$year)
lag = 4
lagnk = 2
cb= crossbasis(data$Humidex_mean,lag=lag,
               argvar =list(fun="ns",knots =  quantile(data$Humidex_mean,c(80)/100,na.rm=T), 
                            Boundary.knots = range(data$Humidex_mean,na.rm=T)),
               arglag= list(knots = logknots(lag, lagnk)),group = data$gender_group)

DD = onebasis(data$DD_num,fun="strata",breaks=c(1,4))
DN = onebasis(data$DN_num,fun="strata",breaks=c(1,4))

pre = crosspred(cb,model,cumul = T,
                at = data$Humidex_mean,cen=MMT,model.link = "log")
f =data.frame(cbind(pre$predvar,pre$matfit[,1],pre$matfit[,1] - 1.96 * pre$matse[,1],pre$matfit[,1] + 1.96 * pre$matse[,1])) 
colnames(f) <- c("Humidex_mean", "rr","rr.low","rr.high")
DDpre = crosspred(DD,model,
                  at = seq(0,7),cen=0,model.link = "log")
D_f = data.frame(cbind(as.integer(DDpre$predvar),DDpre$allfit,DDpre$allfit - 1.96 * DDpre$allse,DDpre$allfit + 1.96 * DDpre$allse)) 
colnames(D_f) <- c("DD_num", "rr.dd","rr.dd.low","rr.dd.high")
DNpre = crosspred(DN,model,
                  at = seq(0,7),cen=0,model.link = "log")#,model.link = "log"
DN_f = data.frame(cbind(as.integer(DNpre$predvar),DNpre$allfit,DNpre$allfit - 1.96 * DNpre$allse,DNpre$allfit + 1.96 * DNpre$allse)) 
colnames(DN_f) <- c("DN_num", "rr.dn","rr.dn.low","rr.dn.high")


preda2 = merge(data,f,by="Humidex_mean")
preda2 = merge(preda2,D_f,by="DD_num")
preda2 = merge(preda2,DN_f,by="DN_num")
preda2$morrisk = exp(preda2$rr+preda2$rr.dd+preda2$rr.dn)-1
preda2$morrisk.low = exp(preda2$rr.low+preda2$rr.dd.low+preda2$rr.dn.low)-1
preda2$morrisk.high = exp(preda2$rr.high+preda2$rr.dd.high+preda2$rr.dn.high)-1
preda2$predicted = preda2$morrisk*preda2$death
preda2$predicted.low = preda2$morrisk.low*preda2$death
preda2$predicted.high = preda2$morrisk.high*preda2$death
preda2$predicted[preda2$predicted<0]=0
preda2$predicted.low[preda2$predicted.low<0]=0
preda2$predicted.high[preda2$predicted.high<0]=0
###########################Age:0-15##########################
model = readRDS(file.path(outdir,"stratamodel_0-15.rds"))
data = subset(da,da$age=="0-15")
data$week = as.integer(substring(data$week,7,8))
knots = quantile(data$Humidex_mean,c(80)/100,na.rm=T)
geolist = unique(da$geo)
data$gender_group = paste(data$geo,data$year)
lag = 4
lagnk = 2
cb= crossbasis(data$Humidex_mean,lag=lag,
               argvar =list(fun="ns",knots =  quantile(data$Humidex_mean,c(80)/100,na.rm=T), 
                            Boundary.knots = range(data$Humidex_mean,na.rm=T)),
               arglag= list(knots = logknots(lag, lagnk)),group = data$gender_group)

DD = onebasis(data$DD_num,fun="strata",breaks=c(1,4))
DN = onebasis(data$DN_num,fun="strata",breaks=c(1,4))

pre = crosspred(cb,model,cumul = T,
                at = data$Humidex_mean,cen=MMT,model.link = "log")
f =data.frame(cbind(pre$predvar,pre$matfit[,1],pre$matfit[,1] - 1.96 * pre$matse[,1],pre$matfit[,1] + 1.96 * pre$matse[,1])) 
colnames(f) <- c("Humidex_mean", "rr","rr.low","rr.high")
DDpre = crosspred(DD,model,
                  at = seq(0,7),cen=0,model.link = "log")
D_f = data.frame(cbind(as.integer(DDpre$predvar),DDpre$allfit,DDpre$allfit - 1.96 * DDpre$allse,DDpre$allfit + 1.96 * DDpre$allse)) 
colnames(D_f) <- c("DD_num", "rr.dd","rr.dd.low","rr.dd.high")
DNpre = crosspred(DN,model,
                  at = seq(0,7),cen=0,model.link = "log")#,model.link = "log"
DN_f = data.frame(cbind(as.integer(DNpre$predvar),DNpre$allfit,DNpre$allfit - 1.96 * DNpre$allse,DNpre$allfit + 1.96 * DNpre$allse)) 
colnames(DN_f) <- c("DN_num", "rr.dn","rr.dn.low","rr.dn.high")


preda3 = merge(data,f,by = "Humidex_mean")
preda3 = merge(preda3,D_f,by="DD_num")
preda3 = merge(preda3,DN_f,by="DN_num")
preda3$morrisk = exp(preda3$rr+preda3$rr.dd+preda3$rr.dn)-1
preda3$morrisk.low = exp(preda3$rr.low+preda3$rr.dd.low+preda3$rr.dn.low)-1
preda3$morrisk.high = exp(preda3$rr.high+preda3$rr.dd.high+preda3$rr.dn.high)-1
preda3$predicted = preda3$morrisk*preda3$death
preda3$predicted.low = preda3$morrisk.low*preda3$death
preda3$predicted.high = preda3$morrisk.high*preda3$death
preda3$predicted[preda3$predicted<0]=0
preda3$predicted.low[preda3$predicted.low<0]=0
preda3$predicted.high[preda3$predicted.high<0]=0

preda = do.call(rbind,list(preda,preda2,preda3))

##################Mortality data plot######################
preda$country = substring(preda$geom,1,2)
file = preda%>%group_by(year,week,age,country)%>%summarise(heatdeath = round(sum(predicted),0),
                                                           heatdeath.low =round(sum(predicted.low),0),
                                                           heatdeath.high =round(sum(predicted.high),0),
                                                           death = sum(death))
###############mortality of each country###############
label = data.frame(country =  c("AL", "AT", "BE", "BG", "CH", "CY", "CZ", "DK", "EE", "EL", "ES", "FI", "FR", "HU", "IS", "IT", "LI", "LT", "LU", "LV",
                                "ME", "NL", "NO", "PL", "PT", "RO", "RS", "SE", "SK", "UK"),
                   Country_name = c("Albania", "Austria", "Belgium", "Bulgaria", "Switzerland", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Greece",
                                    "Spain", "Finland", "France", "Hungary", "Iceland", "Italy", "Liechtenstein", "Lithuania", "Luxembourg", "Latvia",
                                    "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Serbia", "Sweden", "Slovakia", "United Kingdom"),
                   geo =  c("Southeastern Europe", "Central Europe", "Western Europe", "Southeastern Europe", "Central Europe", "Southern Europe",
                            "Central Europe", "Northern Europe", "Northern Europe", "Southern Europe", "Southern Europe", "Northern Europe",
                            "Western Europe", "Central Europe", "Northern Europe", "Southern Europe", "Central Europe", "Northern Europe",
                            "Western Europe", "Northern Europe", "Southern Europe", "Western Europe", "Northern Europe", "Central Europe",
                            "Southern Europe", "Southeastern Europe", "Southern Europe", "Northern Europe", "Central Europe", "Western Europe"))
file= subset(file,file$week>=22&file$week<=36)
all = file%>%group_by(year,week,country)%>%summarise(heatdeath = round(sum(heatdeath),0),
                                                     heatdeath.low = round(sum(heatdeath.low),0),
                                                     heatdeath.high = round(sum(heatdeath.high),0),
                                                     death = sum(death),age = "TOTAL")
all = do.call(rbind,list(all,file))
all1 = subset(all,all$age =="TOTAL"|all$age =="65+")
all4 = all1%>%group_by(age,year)%>%
  summarise(death =sum(death),heatdeath = sum(heatdeath),
            heatdeath.low = sum(heatdeath.low),heatdeath.high = sum(heatdeath.high))

print(round(sum(all4$heatdeath[which(all4$age=="65+")])/
              sum(all4$heatdeath[which(all4$age=="TOTAL")])*100,2))

all1 = all1%>%group_by(country,age)%>%
  summarise(death =sum(death),heatdeath = sum(heatdeath),
            heatdeath.low = sum(heatdeath.low),heatdeath.high = sum(heatdeath.high))
all1 = merge(all1,label,id = country)
all1$geo=  factor(all1$geo,ordered=TRUE,levels = c("Northern Europe","Southeastern Europe","Central Europe","Southern Europe","Western Europe"))


all_o = subset(all1,all1$age=="TOTAL") 
all_o <- all_o %>% arrange(desc(heatdeath))%>%
  mutate(Country_name = factor(Country_name, levels =Country_name))

all_old = subset(all1,all1$age=="65+") 
g2= ggplot()+
  geom_linerange(data = all_o, aes(ymin =0, ymax=heatdeath, x=Country_name),size=1.8,color="#e4e6e1")+
  geom_linerange(data = all_old, aes(ymin =0, ymax=heatdeath, x=Country_name),size=2,color="#989898")+
  geom_point(data = all_o, aes(y=heatdeath, x=Country_name,color= geo,fill=geo,shape=geo),size=2,alpha=0.8)+
  scale_shape_manual(values = c(15,16,17,18,19))+
  scale_fill_manual(values = c("#d5b48a","#a6944a","#858fac","#5b4837","#c9842e"))+
  scale_color_manual(values = c("#d5b48a","#a6944a","#858fac","#5b4837","#c9842e"))+
  theme_bw()+labs(x="Number of heat-related death (weeks 22-35)")+
  scale_y_continuous(limits=c(0,120000),labels=comma)+
  theme(legend.position = "top",
        legend.background = element_rect(
          fill = "transparent", # ????ɫ
          colour = "transparent", # ????ɫ
          size = 1.5),
        legend.title=element_blank(),
        legend.text=element_text(color="black",hjust = 0.5,vjust=.50,size =unit(8,"pt")),
        panel.grid = element_blank(),
        axis.line=element_line(color="black",size=0.5),
        plot.title = element_text(color="black",hjust = 0,vjust=0,size =unit(8,"pt")),
        axis.title.y= element_blank(),
        axis.text.x = element_text(color="black",size = unit(8,"pt"),angle = 60,hjust=1,vjust=1),
        axis.title.x= element_text(color="black",size = unit(8,"pt")),
        axis.text.y = element_text(color="black",size = unit(8,"pt")),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))+
  guides(color = guide_legend(nrow = 3),fill = guide_legend(nrow = 3) ) 
outdir = "D:/ATtest/Europe/Figure_1225"
ggsave(paste0(outdir,"/fig2d_summerMortality.pdf"),g2, width=18, height=8, units="cm", scale=1)

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


file2022= subset(file,file$year==2022)
death2022 = file2022%>%group_by(country)%>%summarise(heatdeath = round(sum(heatdeath),0),
                                               heatdeath.low = round(sum(heatdeath.low),0),
                                               heatdeath.high = round(sum(heatdeath.high),0),
                                               death = sum(death))
death2022 = merge(death2022,label,id = country)

age2022 = file2022%>%group_by(age)%>%summarise(heatdeath = round(sum(heatdeath),0),
                                                     heatdeath.low = round(sum(heatdeath.low),0),
                                                     heatdeath.high = round(sum(heatdeath.high),0),
                                                     death = sum(death))

age = file%>%group_by(country)%>%summarise(heatdeath = round(sum(heatdeath),0),
                                                     heatdeath.low = round(sum(heatdeath.low),0),
                                                     heatdeath.high = round(sum(heatdeath.high),0),
                                                     death = sum(death))
write.csv(age,paste0(outdir,"/figS1_heat-related_deaths_spatial.csv"),row.names = F)