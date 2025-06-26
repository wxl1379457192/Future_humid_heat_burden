rm(list = ls())
gc()
###########################################
setwd("D:/ATtest/Europe_version3")
library(dlnm)
library(splines)
library(ggplot2)
library(reshape2)
library(patchwork)
library(zoo)
library(Epi)
library(data.table)
outdir = "Figures"
if (dir.exists(outdir)){
  print("Output dir has existed!")
}else{
  dir.create(outdir)
}
indir = "Input_model_data"
pop = fread(file.path("D:/ATtest/Europe_version2",indir,"Aux_pop_data_V2.csv"))
modeldir = "Model"
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
#######################################
#######################################
dfex = function(data,modelname){
  data = data[Year>=2010&Year<=2019,]
  lag <- 4
  lagnk <-2
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  
  cb= crossbasis(data$Hum,
                 lag=lag,
                 argvar = list(fun="ns",knots =  quantile(data$Hum,c(80)/100,na.rm=T), 
                               Boundary.knots = range(data$Hum,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group = data$gender_group)
  model <-  readRDS(modelname)
  
  red <- crossreduce(cb,model,at=-1:56)
  MMT = red$predvar[which.min(red$RRfit)]
  red <- crossreduce(cb,model,at=-1:56,cen = MMT)
  df <- data.frame(
    humidex = rep(-1:56, 1),
    age = rep(paste0("Age:",data$age[1]), each = 58),
    RR = c(red$RRfit),
    low = c(red$RRlow),
    high = c(red$RRhigh)
  )
  print(paste(data$age[1],MMT))
  return(df)
}

indata = fread(file.path(indir,paste0("Input_data_all_v2.csv")))
filter_list<- filter_geo(indata$geo)
pop = pop[age_group!="TOTAL",]
indata= indata[indata$geo %in% filter_list, ]
indata =indata[age!="TOTAL",]
indata = merge(indata,pop,
               by.x = c("Year","geo","age"),
               by.y = c("Year","geo","age_group"))
indata = indata[complete.cases(indata),]
indata = indata[pop>0]

df1 = do.call(rbind,lapply(split(indata,indata$age),function(data){
  modelname = paste0(modeldir,"/stratamodel_",data$age[1],"_ns_hum.rds")
  return(dfex(data,modelname))
}))


g = ggplot(df1) +
  geom_line(aes(x = humidex, y = RR), color = "#B4281E",size=0.4) +
  geom_ribbon(aes(x = humidex, ymin = low, ymax = high), alpha = 0.2,fill="#A49b90") +
  facet_wrap(~age, nrow = 1) +
  theme_bw() +
  scale_x_continuous(expand = c(0,0))+
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40",linewidth = 0.4) + 
  # geom_hline(yintercept = 1.1, linetype = "dashed", color = "grey40") + 
  #scale_y_continuous(expand = c(0,0),limits=c(0.70,1.5))+
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.title.x= element_text(color="black",hjust = 0.5,vjust=0,size =unit(9,"pt")),
        axis.text.x = element_text(color="black",size = unit(9,"pt")),
        axis.title.y= element_text(color="black",hjust = 0.5,vjust=0,size =unit(9,"pt")),
        axis.text.y = element_text(color="black",size = unit(9,"pt")),
        legend.position="bottom",
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  labs(
    x=bquote("Weekly average Humidex (" * degree * "C)"),
    y="Relative risk (RR)"
  )
ggsave(paste0(outdir,"/fig1a.pdf"),g, width=14, height=6, units="cm", scale=1)
#######extract the effect of day heat##########
dhex = function(data,model){
  data = data[Year>=2010&Year<=2019,]
  data$death_rate[which(data$death_rate==0)]=0.000001
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  model <-  readRDS(model)
  summary_model <- summary(model)
  
  # 提取参数名中包含"CDD"的行
  coefd = do.call(rbind,lapply(c("CDD","CDN","CDA","UDD","UDN","UDA"),function(n){
    CDDindex <- grep(n,names(coef(model)))
    coef_CDD =coef(model)[CDDindex]
    CDD_se <- summary_model$coefficients[CDDindex, "Std. Error"]
    if(length(coef_CDD)>0){
      cd = data.frame(var = n, coef = coef_CDD,se = CDD_se,
                      lower = coef_CDD - 1.96 * CDD_se,
                      upper = coef_CDD + 1.96 * CDD_se,
                      day = c("1 day","2-3 days",">3 days"))
      print(n)
      return(cd)
    }
    else{return(NULL)}
  }))
  coefd$label =unique(data$age)
  return(coefd)
}
indata =indata[age!="TOTAL",]
RR = do.call(rbind,lapply(split(indata,indata$age),function(data){
  modelname = paste0(modeldir,"/stratamodel_",data$age[1],"_ns_hum.rds")
  print(modelname)
  k = dhex(data,modelname)
  return(k)
}))

nsample = do.call(rbind,lapply(split(indata,indata$age),function(data){
  modelname = paste0(modeldir,"/stratamodel_",data$age[1],"_ns_hum.rds")
  model <-  readRDS(modelname)
  nobs = nobs(model)
  return(data.frame(age = unique(data$age),samples = nobs))
}))


k = lapply(split(RR,RR$day),function(rd){
  do.call(rbind,lapply(c("DD","DN","DA"),function(k){
    rdd = subset(rd, grepl(k, var))
  }))
})

df = do.call(rbind,k)
# Merge the data frames by age and num
#df <- do.call(rbind,list(DDeff,DNeff))
df$RR = (exp(df$coef)-1) *100
df$RR_lower = (exp(df$lower)-1) *100
df$RR_upper = (exp(df$upper)-1) *100
df$dn = substring(df$var,2,3)
df$xlab = paste(df$label,":", df$dn)

df$risk = ifelse(df$RR>0,"Increase","Decrease")
df$con = substring(df$var,1,1)

library(ggalt)
library(ggtext)
library(ggplot2)

df$dn <- factor(df$dn, levels = c("DD", "DN", "DA"))
df$day = factor(df$day,levels = c("1 day","2-3 days",">3 days"))
df$con <- factor(df$con)
df$label <- factor(df$label)
df$con = ifelse(df$con=="C","Consecutive","Non-consecutive")
df$RR[is.na(df$RR)] <- 0
g2b =ggplot(df, aes(x = dn, y = RR, fill = risk, alpha = day)) +
  scale_fill_manual(values = c("#4f3b21", "#CF221F")) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.8) +  # Bar chart with dodge
  theme_bw() +
  facet_wrap(~ con + label, scales = "free_y", ncol = 3)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.1) + 
  geom_errorbar(aes(ymin = RR_lower, ymax = RR_upper), 
                width = 0.2,  # Adjust the width of error bars
                position = position_dodge(width = 0.8),  # Ensure the error bars match the bar positions
                size = 0.3,color = "black") +  # Set error bar color
  scale_alpha_manual(values = c(0.2, 0.6, 1)) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    panel.border = element_rect(fill = NA, color = "black", size = 1, linetype = "solid"),
    axis.title.y = element_text(size = unit(9, "pt")),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = unit(9, "pt")),
    legend.position = "bottom"
  ) +
  labs(y = "Absolute risk (%)", x = "Duration (Day)") +
  theme(
    strip.background = element_blank(),  # Remove panel background
    strip.placement = "outside",
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )

library(reshape2)
library(patchwork)

#combined_plot <- g2a / g2b + plot_layout(guides = 'collect',heights = c(3, 1)) & 
#  theme(legend.position="bottom")
ggsave(paste0(outdir,"/fig1b.pdf"),g2b, width=14.5, height=10, units="cm", scale=1)
#######################################
#######################heat-lag map##############################
conex = function(data,model){
  data = data[Year>=2010&Year<=2019,]
  lag <- 4
  lagnk <-2
  data$death_rate[which(data$death_rate==0)]=0.000001
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  
  cb= crossbasis(data$Hum,
                 lag=lag,
                 argvar = list(fun="ns",knots =  quantile(data$Hum,c(80)/100,na.rm=T), 
                               Boundary.knots = range(data$Hum,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group = data$gender_group)
  
  model <-  readRDS(model)
  
  pred.humidex= crosspred(cb, model,bylag=0.2)
  df <- data.frame(pred.humidex$matRRfit)
  df$Humidex = rownames(df)
  df.long <- melt(df, variable.name = "Lag", value.name = "RR")
  df.long$variable = as.numeric(gsub("[^[:digit:].]", "", df.long$Lag))
  df.long$Humidex = as.numeric(df.long$Humidex)
  dfplot = df.long[which(df.long$Humidex>=-10),]
  dfplot$RR = round(dfplot$RR,4)
  dfplot$group<- cut(dfplot$RR, breaks = c(seq(min(dfplot$RR), 1, length.out =5), 
                                           seq(1, max(dfplot$RR), length.out =5)[-1]), 
                     include.lowest = TRUE)
  dfplot$age = paste("Age:",unique(data$age))
  return(dfplot)
}

lagdata = do.call(rbind,lapply(split(indata,indata$age),function(data){
  modelname = paste0(modeldir,"/stratamodel_",data$age[1],"_ns_hum.rds")
  print(modelname)
  k = conex(data,modelname)
  return(k)
}))
lagdata$RR = lagdata$RR
g1 = ggplot(lagdata, aes(x = Humidex, y =as.numeric(variable),
                         z = RR)) + 
  geom_tile(aes(fill = RR),show.legend = T)+
  facet_grid(age~.) +
  scale_fill_gradient2(low = "#082567",mid="#f2f5f9",high ="#DA0303",
                       midpoint = 1)+
  theme_bw()+scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))+
  labs(x = bquote("Weekly average Humidex (" * degree * "C)"),y = "Week")+
  theme(
    plot.title = element_text(angle=0,size= unit(9,"pt")),
    axis.text.x=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(9,"pt")),
    axis.text.y=element_text(angle=0,hjust=0.5,vjust=0.5,size= unit(9,"pt")),
    axis.title.y=element_text(size= unit(9,"pt")),
    axis.title.x=element_text(size= unit(9,"pt")),
    legend.position="right",
    legend.title=element_text(size= unit(9,"pt"),angle=0,hjust=0.5,vjust=0.5),
    legend.text=element_text(size= unit(9,"pt")),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    strip.background = element_blank(),
    plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")
  )+
  guides(fill = guide_colorbar(barwidth = 0.3, barheight = 12))

ggsave(paste0(outdir,"/fig1c.pdf"),g1 , width=9, height=15, units="cm", scale=1)
