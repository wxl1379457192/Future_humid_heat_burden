rm(list = ls())
gc()
###########################################
setwd("D:/ATtest/Europe")
library(dlnm)
library(splines)
library(ggplot2)
library(reshape2)
library(patchwork)
library(zoo)
library(Epi)
library(data.table)
modeldir = "DLMN_model_V0530/pooled_model_age_group_dataV2_ns"
outdir = "Figure_1225"
if (dir.exists(outdir)){
  print("Output dir has existed!")
}else{
  dir.create(outdir)
}
da = read.csv(paste0("DLMN_model_V0530/pooled_model_age_group_dataV2/Inputdata_withinage.csv"),stringsAsFactors  = F)
da = subset(da,da$year>=2015&da$year<=2019)
# PARAMETERS FOR THE LAG-RESPONSE FUNCTION

da$week_num = as.integer(substring(da$week,7,8))
da = subset(da,da$week_num>=22&da$week_num<36)
#da$pop = da$pop/max(da$pop)
lag <- 4
lagnk <-2
#######################################
#######################################
data = subset(da,da$age=="65+")
data$death[which(data$death==0)]=1
data$age = as.factor(data$age)

cb= crossbasis(data$Humidex_mean,
               lag=lag,
               argvar = list(fun="ns",knots =  quantile(data$Humidex_mean,c(80)/100,na.rm=T), 
                             Boundary.knots = range(data$Humidex_mean,na.rm=T)),
               arglag= list(knots = logknots(lag, lagnk)),
               group = data$gender_group)

DD = onebasis(data$DD_num,fun="strata",breaks=c(1,4))
DN = onebasis(data$DN_num,fun="strata",breaks=c(1,4))

model <-  readRDS(file.path(modeldir,"stratamodel_65+.rds"))

#############################
data2 = subset(da,da$age=="15-65")
data2$death[which(data2$death==0)]=1
data2$age = as.factor(data2$age)

cb2= crossbasis(data2$Humidex_mean,
                lag=lag,
                argvar = list(fun="ns",knots =  quantile(data$Humidex_mean,c(80)/100,na.rm=T), 
                              Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                arglag= list(knots = logknots(lag, lagnk)),
                group = data2$gender_group)
DD2 = onebasis(data2$DD_num,fun="strata",breaks=c(1,4))
DN2 = onebasis(data2$DN_num,fun="strata",breaks=c(1,4))

model2 <-  readRDS(file.path(modeldir,"stratamodel_16-65.rds"))

##############model for age 0-15#####################
data3 = subset(da,da$age=="0-15")
data3$death[which(data3$death==0)]=1
data3$age = as.factor(data3$age)

cb3= crossbasis(data3$Humidex_mean,lag=lag,
                argvar =list(fun="ns",knots =  quantile(data$Humidex_mean,c(80)/100,na.rm=T), 
                             Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                arglag= list(knots = logknots(lag, lagnk)),
                group = data3$gender_group)

DD3 = onebasis(data3$DD_num,fun="strata",breaks=c(1,4))
DN3 = onebasis(data3$DN_num,fun="strata",breaks=c(1,4))

model3 <- readRDS(file.path(modeldir,"stratamodel_0-15.rds"))

red <- crossreduce(cb,model,at=10:40)
MMT = red$predvar[which.min(red$RRfit)]

red3 <- crossreduce(cb3,model3,at=10:40,cen = MMT)
red2 <- crossreduce(cb2,model2,at=10:40,cen =MMT)

red <- crossreduce(cb,model,at=10:40,cen = MMT)
print(MMT)
df <- data.frame(
  humidex = rep(10:40, 1),
  age = rep(c("Age:0-15", "Age:16-65", "Age:65+"), each = 31),
  RR = c(red3$RRfit, red2$RRfit, red$RRfit),
  low = c(red3$RRlow, red2$RRlow, red$RRlow),
  high = c(red3$RRhigh, red2$RRhigh, red$RRhigh)
)


g = ggplot(df) +
  geom_line(aes(x = humidex, y = RR), color = "#496c88",size=0.4) +
  geom_ribbon(aes(x = humidex, ymin = low, ymax = high), alpha = 0.4,fill="#a5b6c5") +
  facet_wrap(~age, nrow = 1) +
  theme_bw() +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0),limits=c(0.90,1.5))+
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
ggsave(paste0(outdir,"/fig1a.pdf"),g, width=12, height=6, units="cm", scale=1)
#######extract the effect of day heat##########
DDindex <- grep("DD",names(coef(model)))
coef_DD = coef(model)[DDindex]
DNindex <- grep("DN",names(coef(model)))
coef_DN = coef(model)[DNindex]
RReff = data.frame(num = seq(1,7,1))
RReff$RR_DD = 0 
RReff$RR_DD[which(RReff$num>=1&RReff$num<4)] = exp((coef_DD[1])*(RReff$num[which(RReff$num>=1&RReff$num<4)]))
RReff$RR_DD[which(RReff$num>=4)] = exp((coef_DD[2])*(RReff$num[which(RReff$num>=4)]))
RReff$RR_DN = 0 
RReff$RR_DN[which(RReff$num>=1&RReff$num<4)] = exp((coef_DN[1])*(RReff$num[which(RReff$num>=1&RReff$num<4)]))
RReff$RR_DN[which(RReff$num>=4)] = exp((coef_DN[2])*(RReff$num[which(RReff$num>=4)]))
RReff$label = "65+"

DDindex <- grep("DD",names(coef(model2)))
coef_DD = coef(model2)[DDindex]
DNindex <- grep("DN",names(coef(model2)))
coef_DN = coef(model2)[DNindex]
RReff1 = data.frame(num = seq(1,7,1))
RReff1$RR_DD = 0 
RReff1$RR_DD[which(RReff$num>=1&RReff$num<4)] = exp(coef_DD[1]*(RReff1$num[which(RReff$num>=1&RReff$num<4)]))
RReff1$RR_DD[which(RReff1$num>=4)] = exp((coef_DD[2])*(RReff1$num[which(RReff1$num>=4)]))
RReff1$RR_DN = 0 
RReff1$RR_DN[which(RReff$num>=1&RReff$num<4)] = exp((coef_DN[1])*(RReff1$num[which(RReff$num>=1&RReff$num<4)]))
RReff1$RR_DN[which(RReff1$num>=4)] = exp((coef_DN[2])*(RReff1$num[which(RReff1$num>=4)]))
RReff1$label = "16-65"

DDindex <- grep("DD",names(coef(model3)))
coef_DD = coef(model3)[DDindex]
DNindex <- grep("DN",names(coef(model3)))
coef_DN = coef(model3)[DNindex]
RReff2 = data.frame(num = seq(1,7,1))
RReff2$RR_DD = 0 
RReff2$RR_DD[which(RReff$num>=1&RReff$num<4)] = exp(coef_DD[1]*(RReff2$num[which(RReff$num>=1&RReff$num<4)]))
RReff2$RR_DD[which(RReff2$num>=4)] = exp((coef_DD[2])*(RReff2$num[which(RReff2$num>=4)]))
RReff2$RR_DN = 0 
RReff2$RR_DN[which(RReff$num>=1&RReff$num<4)] = exp((coef_DN[1])*(RReff2$num[which(RReff$num>=1&RReff$num<4)]))
RReff2$RR_DN[which(RReff2$num>=4)] = exp((coef_DN[2])*(RReff2$num[which(RReff2$num>=4)]))
RReff2$label = "0-15"

RR = do.call(rbind,list(RReff,RReff1,RReff2))
# Merge the data frames by age and num
#df <- do.call(rbind,list(DDeff,DNeff))
RR$RR_DD =  round(RR$RR_DD*100,1)
RR$RR_DN =  round(RR$RR_DN*100,1)
RR$xlab = paste0(RR$label,":",RR$num," days")

absrr = melt(RR,id=c("num","label","xlab"))
absrr$value = absrr$value-100
absrr$value[absrr$value<0] = 0.1
g2b = ggplot(absrr,aes(x=xlab,y=value))+
  scale_fill_manual(values = c("#e7dfec","#553d66"))+
  theme_bw()+
  geom_bar(aes(fill=variable),stat="identity",position = 'dodge',width=0.6)+
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
library(ggplot2)
library(reshape2)
library(patchwork)

#combined_plot <- g2a / g2b + plot_layout(guides = 'collect',heights = c(3, 1)) & 
#  theme(legend.position="bottom")

ggsave(paste0(outdir,"/fig1b.pdf"),g2b, width=12, height=8, units="cm", scale=1)


#######################################
#######################heat-lag map##############################

pred.humidex= crosspred(cb3, model3,cen=MMT, bylag=0.2)
df <- data.frame(pred.humidex$matRRfit)
df$Humidex = rownames(df)
df.long <- melt(df, variable.name = "Lag", value.name = "RR")
df.long$variable = as.numeric(gsub("[^[:digit:].]", "", df.long$Lag))
df.long$Humidex = as.numeric(df.long$Humidex)
dfplot = df.long[which(df.long$Humidex>=10),]
dfplot$RR = round(dfplot$RR,4)
dfplot$group<- cut(dfplot$RR, breaks = c(seq(min(dfplot$RR), 1, length.out =5), 
                                         seq(1, max(dfplot$RR), length.out =5)[-1]), 
                   include.lowest = TRUE)


pred.humidex= crosspred(cb2, model2,cen=MMT, bylag=0.2)
df2 <- data.frame(pred.humidex$matRRfit)
df2$Humidex = rownames(df2)
df2.long <- melt(df2, variable.name = "Lag", value.name = "RR")
df2.long$variable = as.numeric(gsub("[^[:digit:].]", "", df2.long$Lag))
df2.long$Humidex = as.numeric(df2.long$Humidex)
dfplot2 = df2.long[which(df2.long$Humidex>=10),]
dfplot2$RR = round(dfplot2$RR,4)
dfplot2$group<- cut(dfplot2$RR, breaks = c(seq(min(dfplot2$RR), 1, length.out = 5), 
                                           seq(1, max(dfplot2$RR), length.out = 5)[-1]), 
                    include.lowest = TRUE)

pred.humidex= crosspred(cb, model,cen=MMT, bylag=0.2)
df3 <- data.frame(pred.humidex$matRRfit)
df3$Humidex = rownames(df3)
df3.long <- melt(df3, variable.name = "Lag", value.name = "RR")
df3.long$variable = as.numeric(gsub("[^[:digit:].]", "", df3.long$Lag))
df3.long$Humidex = as.numeric(df3.long$Humidex)
dfplot3 = df3.long[which(df3.long$Humidex>=10),]
dfplot3$RR = round(dfplot3$RR,4)
dfplot3$group<- cut(dfplot3$RR, breaks = c(seq(min(dfplot3$RR), 1, length.out = 5), 
                                           seq(1, max(dfplot3$RR), length.out = 5)[-1]), 
                    include.lowest = TRUE)

g1 = ggplot(dfplot, aes(x = Humidex, y =as.numeric(variable),
                        z = RR)) + 
  geom_tile(aes(fill = RR),show.legend = T)+
  scale_fill_gradient2(low = "#0072a3",mid="#feffec",high ="#DA0303",
                       midpoint = 1,breaks =seq(0.97,1.4,0.03))+
  labs(title = "Age group: 0-15", y = "") + 
  theme_bw()+scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))+
  theme(
    plot.title = element_text(angle=0,size= unit(7,"pt")),
    axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
    axis.text.y=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
    axis.title.x=element_blank(),
    axis.title.y=element_text(size= unit(7,"pt")),
    legend.position="bottom",
    legend.title=element_text(size= unit(7,"pt"),angle=0,hjust=0.5,vjust=0.5),
    legend.text=element_text(size= unit(7,"pt")),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 0.1))

g2 = ggplot(dfplot2, aes(x = Humidex, y =as.numeric(variable),
                         z = RR)) + 
  geom_tile(aes(fill = RR),show.legend = T)+
  scale_fill_gradient2(low = "#0072a3",mid="#feffec",high ="#DA0303",
                       midpoint = 1,breaks =seq(0.97,1.4,0.03))+
  labs(title = "Age group: 16-65", y = "Lag (weeks)") + 
  theme_bw()+scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))+
  theme(
    plot.title = element_text(angle=0,size= unit(7,"pt")),
    axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
    axis.text.y=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
    axis.title.x=element_blank(),
    axis.title.y=element_text(size= unit(7,"pt")),
    legend.position="bottom",
    legend.title=element_text(size= unit(7,"pt")),
    legend.text=element_text(size= unit(7,"pt")),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )

g3 = ggplot(dfplot3, aes(x = Humidex, y =as.numeric(variable),
                         z = RR)) + 
  geom_tile(aes(fill = RR),show.legend = T)+
  scale_fill_gradient2(low = "#0072a3",mid="#feffec",high ="#DA0303",
                       midpoint = 1,breaks =seq(0.97,1.4,0.03))+
  labs(x = "Weekly average Humidex (??)", y = "",
       title = "Age group: 65+") + 
  theme_bw()+scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))+
  theme(
    plot.title = element_text(angle=0,size= unit(7,"pt")),
    axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
    axis.text.y=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(7,"pt")),
    axis.title.x=element_text(size= unit(7,"pt")),
    axis.title.y=element_text(size= unit(7,"pt")),
    legend.position="bottom",
    legend.title=element_text(size= unit(7,"pt")),
    legend.text=element_text(size= unit(7,"pt")),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank()
  )
library(gridExtra)
library(grid)
g <- ggplotGrob(g1)$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
g1 <- g1 + theme(legend.position = "none")
g2 <- g2 + theme(legend.position = "none")
g3 <- g3 + theme(legend.position = "none")
p= grid.arrange(g1, g2, g3, legend, nrow = 4, heights = c(3, 3, 3, 0.5))
ggsave(paste0(outdir,"/fig1c.pdf"),p, width=6, height=12, units="cm", scale=1)
