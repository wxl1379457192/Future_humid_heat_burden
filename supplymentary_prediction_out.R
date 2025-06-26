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

#######################################
dfex = function(data,modelname){
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
  model <-  readRDS(modelname)
  
  red <- crossreduce(cb,model,at=-1:40)
  MMT = red$predvar[which.min(red$RRfit)]
  red <- crossreduce(cb,model,at=-1:40,cen = MMT)
  df <- data.frame(
    humidex = rep(-1:40, 1),
    age = rep(paste0("Age:",data$age[1]), each = 42),
    RR = c(red$RRfit),
    low = c(red$RRlow),
    high = c(red$RRhigh),
    MMH= MMT
  )
  df$num = nrow(data)
  print(paste(data$age[1],MMT))
  return(df)
}
timelist = c("2000_2009","2001_2010","2002_2011","2003_2012","2004_2013",
             "2005_2014","2006_2015","2007_2016","2008_2017","2009_2018","2010_2019")


all = do.call(rbind,lapply(timelist,function(x){
  data1 = fread(file.path("Model_supply_10year",paste0("Inputdata_",x,".csv")))
  df1 = do.call(rbind,lapply(split(data1,data1$age),function(data){
    modelname = paste0("Model_supply_10year","/stratamodel_",x,"_",data$age[1],"_ns.rds")
    return(dfex(data,modelname))
  }))
  df1$type= x
  return(df1)
}))

#df = do.call(rbind,list(df2,df3,df4))
k = all[,c("age","type","MMH","num")]
k = unique(k)
df65 = all[all$age=="Age:65+",]
all$age[which(all$age=="Age:0-15")] = "Age:0-14"
all$age[which(all$age=="Age:15-65")] = "Age:15-64"
all$type = gsub("_","-",all$type)
library(RColorBrewer)
mycolor2<-brewer.pal(11, "Spectral")

g2 = ggplot(all) +
  geom_line(aes(x = humidex, y = RR,color = type),size=0.4) +
  geom_ribbon(aes(x = humidex, ymin = low, ymax = high,fill= type), alpha = 0.05) +
  facet_wrap(~age, nrow = 1) +
  # geom_vline(aes(xintercept = MMH,color = type),alpha=0.3)+
  theme_bw() +
  scale_color_manual(values=mycolor2)+
  scale_fill_manual(values=mycolor2)+
  scale_x_continuous(expand = c(0,0))+
  labs(
    x=bquote("Weekly average Humidex (" * degree * "C)"),
    y="Relative risk (RR)"
  )+
  geom_hline(yintercept  = 1, linetype = "dashed", color = "grey40",linewidth = 0.4) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(color = "black",size = 9, face = "bold"),
        panel.grid = element_blank(),
        axis.title= element_text(color="black",hjust = 0.5,vjust=0,size =unit(9,"pt"), face = "bold"),
        axis.text.x = element_text(color="black",size = unit(9,"pt")),
        axis.text.y = element_text(color="black",size = unit(9,"pt")),
        legend.position="right",
        legend.title = element_blank(), 
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) 
ggsave(paste0("Figures","/fig1a_supply_10win.jpg"),g2, width=20, height=9, units="cm", scale=1)




sldf<- 
  do.call(rbind,lapply(split(all,all$age),function(a){
    adf = do.call(rbind,lapply(split(a,a$type),function(at){
      df_left <- at %>% filter(humidex <= unique(MMH))
      df_right <- at %>% filter(humidex > unique(MMH))
      model_left <- lm(RR ~ humidex, data = df_left)
      model_right <- lm(RR ~ humidex, data = df_right)
      
      slope_df = data.frame(age = unique(at$age),type = unique(at$type), MMH=unique(at$MMH),
                            slope_left= coef(model_left)[2],
                            slope_right = coef(model_right)[2],
                            start = strsplit(at$type,"_")[[1]][1],
                            end = strsplit(at$type,"_")[[1]][2])
      return(slope_df)
    }))
    return(adf)
  }))
library(grid)
library(scales)

sldf = melt(sldf,id= c("age","type","MMH","start","end"))
sldf$start=as.numeric(sldf$start)
g = ggplot(data = sldf)+geom_line(aes(y= abs(value),x =start +5,color= age))+
  geom_point(aes(y= abs(value),x = start+5,color= age))+
  facet_wrap(~variable, nrow = 1,
             labeller = as_labeller(c("slope_left" = "Cold Zone", "slope_right" = "Hot Zone"))) +
  theme_bw() +
  scale_x_continuous(
    breaks = seq(min(sldf$start) + 5, max(sldf$start) + 5, by = 1),  # 每年显示一个刻度
  ) +
  labs(
    x = "Year",
    y = "Absolute slope of curve"
  ) +
  scale_color_manual(values = c("gray35","#b51224","#c0aaac")) +
  theme(strip.text = element_text(color = "black",size = 9, face = "bold"),
        strip.background = element_blank(),  # 子图标题背景透明
        panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(color = "black",angle = 45, hjust = 1, size = unit(9, "pt")),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        axis.title = element_text(color = "black",size = 9, face = "bold"), 
        axis.text = element_text(color = "black",size = 9),
        legend.title = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank() )

ggsave(paste0("Figures","/figSlope_supply_10win.jpg"),g, width=15, height=8, units="cm", scale=1)


k$age[which(k$age=="Age:0-15")] = "Age:0-14"
k$age[which(k$age=="Age:15-64")] = "Age:15-64"
k$age[which(k$age=="Age:65+")] = "Age:65+"


k$type = gsub("_","-",k$type)
g1 = ggplot(data = k, aes(x = type, y = MMH, fill = age)) +
  geom_col(position = "dodge", width = 0.7) +  # 使用 "dodge" 进行分组显示
  scale_y_continuous(
    labels = function(x) paste0(x, " \u00b0C")  # 添加 "℃"
  ) +
  labs(
    x = "Type",  # x轴标签
    y = "MMH"    # y轴标签
  ) +
  theme_minimal() +  # 使用简洁的主题
  theme(
    axis.title.x= element_blank(),
    axis.text.x = element_text(color = "black",angle = 45, hjust =0.7,size = unit(9,"pt")),  # 旋转x轴标签，使其更易读
    axis.title=element_text( color = "black",hjust = 0.5,size = unit(9,"pt"), face = "bold"),
    axis.text.y = element_text( color = "black",hjust = 0,size = unit(9,"pt")),  # 旋转x轴标签，使其更易读
    legend.title = element_blank(),  # 移除legend的标题
    legend.position = "top",  # 调整legend的位置
  ) +
  scale_fill_manual(values = c("gray35","#b51224","#c0aaac")) 
ggsave(paste0("Figures","/figMMH_supply_10win.jpg"),g1, width=15, height=7, units="cm", scale=1)


























