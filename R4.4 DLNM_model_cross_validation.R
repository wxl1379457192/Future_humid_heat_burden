rm(list = ls())
gc()
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
# PARAMETERS FOR THE LAG-RESPONSE FUNCTION
lag <- 4
lagnk <- 2

outdir = "pooled_model_age_group_dataV2_valdation"
dir.create(outdir)
da = read.csv("pooled_model_age_group_dataV2/Inputdata_withinage.csv",stringsAsFactors  = F)
da = subset(da,da$year>=2015&da$year<=2019)
da$week_num = as.integer(substring(da$week,7,8))
k1 = split(da,da$age)[[1]]
knots = quantile(k1$Humidex_mean,c(80)/100,na.rm=T)
da = subset(da,da$week_num>=22&da$week_num<36)
########################################################################
cross_validation = function(da,outdir,year){
  if (file.exists(paste0(outdir,"/validation_",year,".csv"))){
    p= read.csv(paste0(outdir,"/validation_",year,".csv"),stringsAsFactors = F)
  }else{
    valda = da[which(da$year==year),]
    da2 = da[which(da$year!=year),]
    geolist = unique(da2$geom)
    valda =  valda[which(valda$geom%in%geolist),]

    data = subset(da2,da2$age=="65+")
    data$week_num = as.integer(substring(data$week,7,8))
    data$death[which(data$death==0)]=1
    data$age = as.factor(data$age)
    data$gender_group = paste(data$geom,data$year)
    cb= crossbasis(data$Humidex_mean,
                   lag=lag,
                   argvar = list(fun="ns",knots = knots, 
                                 Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                   arglag= list(knots = logknots(lag, lagnk)),
                   group = data$gender_group)
    
    DD = onebasis(data$DD_num,fun="strata",breaks=c(1,4))
    DN = onebasis(data$DN_num,fun="strata",breaks=c(1,4))
    dfn = length(unique(data$year))-1
    
    filename = paste0(outdir,"/validation_model_65+_",year,".rds")
    if(file.exists(filename)){
      model = readRDS(filename)
    }else{
      
      model <-  glm(death ~ cb + DD + DN + ns(year, df = dfn)+ns(week_num, df = 3)+ log(pop) + geom,
                    family = quasipoisson(link="log"), data = data, na.action="na.exclude")
      saveRDS(model,filename)
    }
    
    
    valdata = subset(valda,valda$age=="65+")
    valdata$week_num = as.integer(substring(valdata$week,7,8))
    valdata$age = as.factor(valdata$age)
    valdata$gender_group = paste(valdata$geom,valdata$year)
    cb= crossbasis(valdata$Humidex_mean,lag=lag,
                   argvar = list(fun="ns",knots = knots,
                                 Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                   arglag= list(knots = logknots(lag, lagnk)),group = valdata$gender_group)
    DD = onebasis(valdata$DD_num,fun="strata",breaks=c(1,4))
    DN = onebasis(valdata$DN_num,fun="strata",breaks=c(1,4))
    valdata$pred <- predict(model,type="response",newdata = valdata)
    
    ##################################################
    
    data = subset(da2,da2$age=="15-65")
    data$week_num = as.integer(substring(data$week,7,8))
    data$death[which(data$death==0)]=1
    data$age = as.factor(data$age)
    data$gender_group = paste(data$geom,data$year)
    dfn = length(unique(data$year))-1
    cb= crossbasis(data$Humidex_mean,lag=lag,
                   argvar =list(fun="ns",knots = knots,
                                Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                   arglag= list(knots = logknots(lag, lagnk)),group = data$gender_group)
    DD = onebasis(data$DD_num,fun="strata",breaks=c(1,4))
    DN = onebasis(data$DN_num,fun="strata",breaks=c(1,4))
    filename = paste0(outdir,"/validation_model_16-65_",year,".rds")
    if(file.exists(filename)){
      model = readRDS(filename)
    }else{
      model <- glm(death ~ cb + DD + DN + ns(year, df = dfn)+ns(week_num, df = 3)+ log(pop) + geom ,
                   family = quasipoisson(link="log"), data = data, na.action="na.exclude")
      saveRDS(model,filename)
    }
    
    
    valdata2 = subset(valda,valda$age=="15-65")
    valdata2$week_num = as.integer(substring(valdata2$week,7,8))
    valdata2$age = as.factor(valdata2$age)
    valdata2$gender_group = paste(valdata2$geom,valdata2$year)
    cb= crossbasis(valdata2$Humidex_mean,lag=lag,
                   argvar = list(fun="ns",knots = knots,
                                 Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                   arglag= list(knots = logknots(lag, lagnk)),group = valdata2$gender_group)
    DD = onebasis(valdata2$DD_num,fun="strata",breaks=c(1,4))
    DN = onebasis(valdata2$DN_num,fun="strata",breaks=c(1,4))
    valdata2$pred <- predict(model,type="response",newdata = valdata2)
    
    ##############model for age 0-15#####################
    ##################################################
    data = subset(da2,da2$age=="0-15")
    data$week_num = as.integer(substring(data$week,7,8))
    data$death[which(data$death==0)]=1
    data$age = as.factor(data$age)
    data$gender_group = paste(data$geom,data$year)
    dfn = length(unique(data$year))-1
    cb= crossbasis(data$Humidex_mean,lag=lag,
                   argvar = list(fun="ns",knots = knots,
                                 Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                   arglag= list(knots = logknots(lag, lagnk)),group = data$gender_group)
    
    DD = onebasis(data$DD_num,fun="strata",breaks=c(1,4))
    DN = onebasis(data$DN_num,fun="strata",breaks=c(1,4))
    #model = readRDS(paste0(outdir,"/validation_model_0-15.rds"))
    filename = paste0(outdir,"/validation_model_0-15_",year,".rds")
    if(file.exists(filename)){
      model = readRDS(filename)
    }else{
      model <- glm(death ~ cb + DD + DN + ns(year, df = dfn)+ns(week_num, df = 3)+ log(pop) + geom ,
                   family = quasipoisson(link="log"), data = data, na.action="na.exclude")
      saveRDS(model,filename)
      
    }
    valdata3 = subset(valda,valda$age=="0-15")
    valdata3$week_num = as.integer(substring(valdata3$week,7,8))
    valdata3$age = as.factor(valdata3$age)
    valdata3$gender_group = paste(valdata3$geom,valdata3$year)
    cb= crossbasis(valdata3$Humidex_mean,lag=lag,
                   argvar = list(fun="ns",knots = knots,
                                 Boundary.knots = range(data$Humidex_mean,na.rm=T)),
                   arglag= list(knots = logknots(lag, lagnk)),group = valdata3$gender_group)
    DD = onebasis(valdata3$DD_num,fun="strata",breaks=c(1,4))
    DN = onebasis(valdata3$DN_num,fun="strata",breaks=c(1,4))
    valdata3$pred <- predict(model,type="response",newdata = valdata3)
    p= do.call(rbind,list(valdata,valdata2,valdata3))
    write.csv(p,paste0(outdir,"/validation_",year,".csv"))
  }
  return(p)
}

predata = do.call(rbind,lapply(c(2015,2016,2017,2018,2019),function(i){
  p = cross_validation(da,outdir,i)
  #p = read.csv(paste0(outdir,"/validation_",i,".csv"),stringsAsFactors = F)
  print(paste(i,"has been processed!!"))
  return(p)
}))

color_range <- c("grey70","#899ebd","#427996","#003153","#4e5a7d")
predata$year = as.factor(predata$year)
predata = predata[complete.cases(predata),]
g1 = ggplot(predata)+geom_point(aes(x=pred,y=death,color=year),alpha=0.2,size=0.3)+
  scale_color_manual(values = color_range)+
  scale_x_continuous(breaks = seq(0,1000,250),limits = c(0,1000))+
  scale_y_continuous(breaks = seq(0,1000,250),limits = c(0,1000))+
  theme_bw()+
  labs(x="Predicted number of mortality",y = "Observed number of mortality")+
  theme(legend.position ="right",
        legend.background = element_rect(
          fill = "transparent", # ????ɫ
          colour = "transparent", # ????ɫ
          size = 0.5),
        legend.title=element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(linetype = "dashed", color="grey70"),
        #axis.line=element_line(color="black",size=0.5),
        plot.title = element_text(color="black",hjust = 0,vjust=0,size =unit(8,"pt")),
        axis.title.x= element_text(color="black",size = unit(10,"pt")),
        axis.text.x = element_text(color="black",size = unit(8,"pt")),
        axis.title.y= element_text(color="black",size = unit(10,"pt")),
        axis.text.y = element_text(color="black",size = unit(8,"pt")),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))+
  guides(color = guide_legend(
    override.aes = list(
      size = 2  # 调整legend中点的大小
    )
  ))

setwd("D:/ATtest/Europe")
ggsave(paste0("Figure_1225","/FigS4_CVpoint.pdf"),g1, width=14, height=10, units="cm", scale=1)

model.lm<-lm(formula = death ~ pred, data = predata)
summary(model.lm)
l <- list(r2 = format(summary(model.lm)$r.squared, digits = 2),
          p = format(summary(model.lm)$coefficients[2,4], digits = 10))

rmse = function(a,b){
  s = (a-b)^2
  rmse = sqrt(sum(s)/length(s))
  return(rmse)
}
print(paste("Population R2 =", format(summary(model.lm)$r.squared, digits = 2),",RMSE = ",rmse(predata$pred,predata$death)))

valdata = do.call(rbind,lapply(split(predata,predata$year),function(y){
  y = do.call(rbind,lapply(split(y,y$geo),function(g){
    model = lm(formula = death ~ pred, data = g)
    val = data.frame(geo = unique(g$geo),year = unique(g$year),R2 = summary(model)$r.squared,
                     p_value = round(summary(model)$coefficients[2,4],10),
                     RMSE = rmse(g$pred,g$death))
    return(val)
  }))
  return(y)
}))
summary(valdata)
library(reshape2)
library(ggsignif)
library(ggprism)
#val = melt(valdata[,c("geo","year","R2","RMSE")],id=c("geo","year"))

g2a = ggplot(valdata,aes(x=year,y=R2))+
  stat_boxplot(aes(color=year),geom="errorbar",width=0.2,size=0.5)+
  geom_boxplot(outlier.shape=NA,show.legend = T,outlier.size=0.1,aes(color=year),
               shape=2,outlier.stroke = 0.1, outlier.alpha = 45,
               notch = F,notchwidth = 0.2,
               fill="grey95",width=0.6,cex=0.5)+
  geom_jitter(aes(color=year),width=.25,size=0.01,alpha=0.2)+ 
  theme_classic()+
  ylim(c(0.3, 1))+
  #facet_wrap( ~ variable, scales = "free_y")+
  scale_color_manual(values = c("grey70","#899ebd","#427996",
                                "#003153","#4e5a7d"))+
  theme_prism(base_line_size = 0.1,
              base_fontface = "plain",
              base_family = "serif")+
  theme(legend.position = "",
        panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_line(colour = "transparent"),
        legend.title=element_text(color="black",size = unit(7, "pt")),
        plot.title = element_text(color="black",hjust = 0,vjust=0,size = unit(7, "pt")),
        axis.title.x= element_text(color="black",hjust=0.5,size = unit(7, "pt")),
        axis.text.x = element_text(color="black",hjust=0.5,size = unit(7, "pt")),
        axis.title.y=element_text(color="black",hjust=0.5,size = unit(7, "pt")),
        axis.text.y= element_text(color="black",hjust=1,size = unit(7, "pt")),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        panel.background=element_rect(fill = "transparent",colour = NA))+
  labs(y="R2",x="Year")

g2b = ggplot(valdata,aes(x=year,y=RMSE))+
  stat_boxplot(aes(color=year),geom="errorbar",width=0.2,size=0.5)+
  geom_boxplot(outlier.shape=NA,show.legend = T,outlier.size=0.1,aes(color=year),
               shape=2,outlier.stroke = 0.1, outlier.alpha = 45,
               notch = F,notchwidth = 0.2,
               fill="grey95",width=0.6,cex=0.5)+
  geom_jitter(aes(color=year),width=.25,size=0.01,alpha=0.1)+ 
  theme_classic()+
  ylim(c(0,30))+
  #facet_wrap( ~ variable, scales = "free_y")+
  scale_color_manual(values = c("grey70","#899ebd","#427996",
                                "#003153","#4e5a7d"))+
  theme_prism(base_line_size = 0.1,
              base_fontface = "plain",
              base_family = "serif")+
  theme(legend.position = "",
        panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_line(colour = "transparent"),
        legend.title=element_text(color="black",size = unit(7, "pt")),
        plot.title = element_text(color="black",hjust = 0,vjust=0,size = unit(7, "pt")),
        axis.title.x= element_text(color="black",hjust=0.5,size = unit(7, "pt")),
        axis.text.x = element_text(color="black",hjust=0.5,size = unit(7, "pt")),
        axis.title.y= element_text(color="black",hjust=0.5,size = unit(7, "pt")),
        axis.text.y= element_text(color="black",hjust=1,size = unit(7, "pt")),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        panel.background=element_rect(fill = "transparent",colour = NA))+
  labs(y="RMSE",x="Year")

combined_plot <- g2a * g2b + plot_layout(guides = 'collect',widths = c(1, 1))
ggsave(paste0("Figure_1225","/FigS4_CVboxplot.pdf"),combined_plot, width=14, height=6, units="cm", scale=1)























