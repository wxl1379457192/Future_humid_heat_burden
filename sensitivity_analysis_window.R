rm(list = ls())
gc()
###########################################
setwd("D:/ATtest/Europe_version2/Model")
library(data.table)
library(ggplot2)
library(dlnm)
library(dplyr)
threlist = c(90,95,99)
thredatalist = list()
do.call(rbind,lapply(threlist,function(i){
  if(i == 90){
    indata = fread(paste0("Inputdata_win14.csv"))
  }else{
    indata = fread(paste0("Inputdata_win14_",i,"th.csv"))
  }
  indata = indata[!is.na(indata$predictions),] 
  indata$predictions = round(indata$predictions,0)
  
  df = lapply(split(indata,indata$age),function(data){
    RMSE =sqrt(mean((data$death - data$predictions)^2))
    data.frame(age = unique(data$age),rmse = RMSE,label = i)
  })
  
  df = rbindlist(df)
  return(df)
}))
  
windowlist = c(10,14,30)
figlist = list()
for (i in windowlist){
  indata = fread(paste0("Inputdata_win",i,".csv"))
  data = indata[which(age =="65+"),]
  
  lag <- 4
  lagnk <-2
  cb= crossbasis(data$Hum,
                 lag=lag,
                 argvar = list(fun="ns",knots =  quantile(data$Hum,c(80)/100,na.rm=T), 
                               Boundary.knots = range(data$Hum,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group = data$gender_group)
  
  model =  readRDS(paste0("stratamodel_win",i,"_",data$age[1],"_ns_hum95th.rds"))
  red <- crossreduce(cb,model,at=10:40)
  df <- data.frame(
    humidex = rep(10:40, 1),
    age = rep(paste0("Age:",data$age[1]), each = 31),
    RR = c(red$RRfit),
    low = c(red$RRlow),
    high = c(red$RRhigh),
    win = i
  )
  figlist[[i]] <- df
}
figdata = rbindlist(figlist)

figdata$win = as.factor(figdata$win)
g1 = ggplot(figdata)+geom_point(aes(x = humidex, y = RR,color = win), size=0.4,position=position_dodge(width = 0.85)) +
  geom_linerange(aes(x = humidex, ymin = low, ymax = high,color = win), alpha = 0.5,
                 position=position_dodge(width = 0.85),show.legend = T) +
  theme_bw() +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),limits=c(0.90,1.6))+
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.x= element_text(color="black",hjust = 0.5,vjust=0,size =unit(9,"pt")),
        axis.text.x = element_text(color="black",size = unit(9,"pt")),
        axis.title.y= element_text(color="black",hjust = 0.5,vjust=0,size =unit(9,"pt")),
        axis.text.y = element_text(color="black",size = unit(9,"pt")),
        legend.position="bottom"
  ) +
  labs(
    x="Weekly average Humidex",
    y="Relative risk (RR)"
  )+
  scale_color_manual(values = c("grey30","#0e56a2","#a1d7ed"),
                     labels = c("10-days Window", "15-days Window", "30-days Window"),
                     name = "")


outdir = "D:/ATtest/Europe_version2/Figure"
if (dir.exists(outdir)){
  print("Output dir has existed!")
}else{
  dir.create(outdir)
}
ggsave(paste0(outdir,"/sensitivity_analysis_window.jpg"),g1, width=10, height=7, units="cm", scale=2)

continuous_heat = function(labelname, model,agename){
  index <- grep(labelname,names(coef(model)))
  coef = coef(model)[index]
  RReff = data.frame(num = seq(1,7,1))
  RReff$RR = 0 
  RReff$RR[which(RReff$num>=1&RReff$num<4)] = exp((coef[1])*(RReff$num[which(RReff$num>=1&RReff$num<4)]))
  RReff$RR[which(RReff$num>=4)] = exp((coef[2])*(RReff$num[which(RReff$num>=4)]))
  RReff$age = agename
  RReff$label = labelname
  return(RReff)
}


threlist = c(90,95,99)
thredatalist = list()
for (i in threlist){
  if(i == 90){
    indata = fread(paste0("Inputdata_win14.csv"))
    modelname =  paste0("stratamodel_win",data$window[1],"_",data$age[1],"_ns.rds")
    labname = "Hum"
  }else{
    indata = fread(paste0("Inputdata_win14_",i,"th.csv"))
    modelname = paste0("stratamodel_win",data$window[1],"_",data$age[1],"_ns_hum",i,"th.rds")
    labname = paste0("Hum_",i,"th")
  }
  
  data = indata[which(age =="65+"),]
  
  lag <- 4
  lagnk <-2
  
  cb= crossbasis(data$Hum,
                 lag=lag,
                 argvar = list(fun="ns",knots =  quantile(data$Hum,c(80)/100,na.rm=T), 
                               Boundary.knots = range(data$Hum,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group = data$gender_group)
  
  model =  readRDS(modelname)
  red <- crossreduce(cb,model,at=10:40)
  df <- data.frame(
    humidex = rep(10:40, 1),
    age = rep(paste0("Age:",data$age[1]), each = 31),
    RR = c(red$RRfit),
    low = c(red$RRlow),
    high = c(red$RRhigh),
    thr = i 
  )
  thredatalist[[i]] <- df
}
thredata = rbindlist(thredatalist)
thredata$thr = as.factor(thredata$thr)
g2 = ggplot(thredata)+geom_point(aes(x = humidex, y = RR,color = thr), size=0.5,position=position_dodge(width = 0.7)) +
  geom_linerange(aes(x = humidex, ymin = low, ymax = high,color = thr), alpha = 0.5,
                 position=position_dodge(width = 0.7),show.legend = T) +
  theme_bw() +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),limits=c(0.90,1.7))+
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.x= element_text(color="black",hjust = 0.5,vjust=0,size =unit(9,"pt")),
        axis.text.x = element_text(color="black",size = unit(9,"pt")),
        axis.title.y= element_text(color="black",hjust = 0.5,vjust=0,size =unit(9,"pt")),
        axis.text.y = element_text(color="black",size = unit(9,"pt")),
        legend.position="bottom"
  ) +
  labs(
    x="Weekly average Humidex",
    y="Relative risk (RR)"
  )+
  scale_color_manual(values = c("grey30","#ad1a2a","#e18133"),
                     labels = c("90th", "95th", "99th"),
                     name = "")

ggsave(paste0(outdir,"/sensitivity_analysis_threshold.jpg"),g2, width=10, height=7, units="cm", scale=2)



##############mean min max comparation################
threlist = c("mean","min","max")
thredatalist = list()
for (i in threlist){
  if(i =="mean"){
    indata = fread(paste0("Inputdata_win14_95th.csv"))
    data = indata[which(age =="65+"),]
    modelname =  paste0("stratamodel_win",data$window[1],"_",data$age[1],"_ns_hum95th.rds")
    labname = "Hum"
  }else{
    indata = fread(paste0("Inputdata_win14_hum",i,".csv"))
    data = indata[which(age =="65+"),]
    modelname = paste0("stratamodel_win",data$window[1],"_",data$age[1],"_ns_hum",i,".rds")
    labname = paste0("Hum_",i)
  }
  
 
  val = data[!is.na(data$predictions),]
  print(sqrt(mean((val$death - val$predictions)^2)))
  lag <- 4
  lagnk <-2
  
  cb= crossbasis(data[[labname]],
                 lag=lag,
                 argvar = list(fun="ns",knots =  quantile(data[[labname]],c(80)/100,na.rm=T), 
                               Boundary.knots = range(data[[labname]],na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group = data$gender_group)
  
  model =  readRDS(modelname)
  red <- crossreduce(cb,model,at=round(min(data[[labname]]),0):round(max(data[[labname]]),0))
  MMT = red$predvar[which.min(red$RRfit)]
  df <- data.frame(
    humidex = rep(round(min(data[[labname]]),0):round(max(data[[labname]]),0), 1),
    age = rep(paste0("Age:",data$age[1]), each =round(max(data[[labname]]),0)-round(min(data[[labname]]),0)+1),
    RR = c(red$RRfit),
    low = c(red$RRlow),
    high = c(red$RRhigh),
    thr = i ,
    MMT = MMT
  )
  print(paste(i,MMT))
  thredatalist[[i]] <- df
}
thredata = rbindlist(thredatalist)
thredata$thr = as.factor(thredata$thr)
thredata$humidex = thredata$humidex-thredata$MMT
g3 = ggplot(thredata)+geom_point(aes(x = humidex, y = RR,color = thr), size=0.5,position=position_dodge(width = 0.7)) +
  geom_linerange(aes(x = humidex, ymin = low, ymax = high,color = thr), alpha = 0.5,
                 position=position_dodge(width = 0.7),show.legend = T) +
  theme_bw() +
  scale_color_manual(values = c("grey30","#b31609","#0aa6bb"))+
  scale_x_continuous(expand = c(0,0),limits=c(-20,20))+
  scale_y_continuous(expand = c(0,0),limits=c(0.90,1.5))+
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.title.x= element_text(color="black",hjust = 0.5,vjust=0,size =unit(9,"pt")),
        axis.text.x = element_text(color="black",size = unit(9,"pt")),
        axis.title.y= element_text(color="black",hjust = 0.5,vjust=0,size =unit(9,"pt")),
        axis.text.y = element_text(color="black",size = unit(9,"pt")),
        legend.position="bottom"
  ) +
  labs(
    x="Weekly average Humidex",
    y="Relative risk (RR)"
  )
outdir = "D:/ATtest/Europe_version2/Figure_0721"
if (dir.exists(outdir)){
  print("Output dir has existed!")
}else{
  dir.create(outdir)
}
ggsave(paste0(outdir,"/sensitivity_analysis_minmax.jpg"),g3, width=16, height=10, units="cm", scale=1)




threlist = c(90,95,99)

condata = list()
for (i in threlist){
  if(i == 90){
    indata = fread(paste0("Inputdata_win14.csv"))
    modelname =  paste0("stratamodel_win",data$window[1],"_",data$age[1],"_ns.rds")
  }else{
    indata = fread(paste0("Inputdata_win14_",i,"th.csv"))
    modelname = paste0("stratamodel_win",data$window[1],"_",data$age[1],"_ns_hum",i,"th.rds")
  }
  
  data = indata[which(age =="65+"),]
  
  model =  readRDS(modelname)
  condf = do.call(rbind,lapply(c("CDD","CDN","CDA","UDD","UDN","UDA"),function(l){
    continuous_heat(l, model,"65+")}))
  condf$thrd = i
  condata[[i]] = condf
}
condata = rbindlist(condata)

condata$thrd = as.factor(condata$thrd)
ggplot(condata,aes(x=num,y=RR,color = thrd))+
  facet_wrap(~label, nrow = 4,scales = "free_y") +
  theme_bw()+
  geom_bar(aes(fill=thrd),stat="identity",position = 'dodge',width=0.6)+
  theme(panel.grid = element_blank(),
        axis.text.x =element_text(angle =0,hjust =0.5,vjust = 0.5))+
  xlab(NULL) +
  theme(panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
        axis.title.y=element_text(size=unit(9,"pt")),
        axis.title.x=element_text(size=unit(9,"pt")),
        legend.text=element_text(size=unit(9,"pt")))+
  geom_vline(xintercept=c(7.5,14.5),size=.5)+
  labs(y="Absolute risk (%)",x="Duration (Day)")+
  scale_x_discrete(labels = absrr$num)+
  theme(legend.position="top")

































