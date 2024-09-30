rm(list = ls())
gc()
###########################################
setwd("D:/ATtest/Europe_version2")
library(dlnm)
library(splines)
library(ggplot2)
library(reshape2)
library(patchwork)
library(zoo)
library(Epi)
library(data.table)
modeldir = "Model"
outdir = "Figure"
if (dir.exists(outdir)){
  print("Output dir has existed!")
}else{
  dir.create(outdir)
}

indir = "Input_model_data"
pop = fread(file.path(indir,"Aux_pop_data.csv"))
# 定义一个函数来筛选 geo 字段
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

modelval = function(d1){
  lag <- 4
  lagnk <-2
  cb= crossbasis(d1$Hum,
                 lag=lag,
                 argvar = list(fun="ns",knots =  quantile(data$Hum,c(80)/100,na.rm=T), 
                               Boundary.knots = range(data$Hum,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group = d1$gender_group)
  
  CDD = onebasis(d1$CHD_95th,fun="strata",breaks=c(1,2,4))
  CDN = onebasis(d1$CHN_95th,fun="strata",breaks=c(1,2,4))
  CDA = onebasis(d1$CH_95th,fun="strata",breaks=c(1,2,4))
  UDD = onebasis(d1$Heatday_95th-d1$CHD_95th,fun="strata",breaks=c(1,2,4))
  UDN = onebasis(d1$Heatnight_95th - d1$CHN_95th,fun="strata",breaks=c(1,2,4))
  UDA = onebasis(d1$Heatall_95th - d1$CH_95th,fun="strata",breaks=c(1,2,4))
  m1 <-  glm(death ~ cb + CDD + CDN+ CDA + UDD + UDN+ UDA + ns(Year, df = 4)+ ns(Week, df = 3)+ geo  +log(pop),
             family = quasipoisson(link="log"), data = d1, na.action="na.exclude")
  saveRDS(m1,paste0(outdir,"/stratamodel_win",d1$window[1],"_",d1$age[1],"_ns_hum95th_",min(d1$Year),"_",max(d1$Year),".rds"))
  cof = crosspred(cb,m1,by=1)
  cof = data.frame(coef(cof))
  cof$id = rownames(cof)
  return(cof)
}

modelval_tem = function(d1){
  lag <- 4
  lagnk <-2
  cb= crossbasis(d1$tem,
                 lag=lag,
                 argvar = list(fun="ns",knots =  quantile(data$tem,c(80)/100,na.rm=T), 
                               Boundary.knots = range(data$tem,na.rm=T)),
                 arglag= list(knots = logknots(lag, lagnk)),
                 group = d1$gender_group)
  
  CDD = onebasis(d1$CHD_95th_tem,fun="strata",breaks=c(1,2,4))
  CDN = onebasis(d1$CHN_95th_tem,fun="strata",breaks=c(1,2,4))
  CDA = onebasis(d1$CH_95th_tem,fun="strata",breaks=c(1,2,4))
  UDD = onebasis(d1$Heatday_95th_tem-d1$CHD_95th_tem,fun="strata",breaks=c(1,2,4))
  UDN = onebasis(d1$Heatnight_95th_tem - d1$CHN_95th_tem,fun="strata",breaks=c(1,2,4))
  UDA = onebasis(d1$Heatall_95th_tem - d1$CH_95th_tem,fun="strata",breaks=c(1,2,4))
  m1 <-  glm(death ~ cb + CDD + CDN+ CDA + UDD + UDN+ UDA + ns(Year, df = 4)+ ns(Week, df = 3)+ geo +log(pop),
             family = quasipoisson(link="log"), data = d1, na.action="na.exclude")
  saveRDS(m1,paste0(outdir,"/stratamodel_win",d1$window[1],"_",d1$age[1],"_ns_tem95th_",min(d1$Year),"_",max(d1$Year),".rds"))
  
  cof = crosspred(cb,m1,by=1)
  cof = data.frame(coef(cof))
  cof$id = rownames(cof)
  return(cof)
}

indata = fread(file.path(indir,paste0("Input_data_window14.csv")))
filter_list<- filter_geo(indata$geo)

pop = pop[age_group!="TOTAL",]
indata= indata[indata$geo %in% filter_list, ]
indata =indata[age!="TOTAL",]
indata = indata[complete.cases(indata),]
indata = merge(indata,pop,
               by.x = c("Year","geo","age"),
               by.y = c("Year","geo","age_group"))
indata$death_rate = indata$death/indata$pop*10000


humlist = lapply(split(indata,indata$age),function(data){
  data$death_rate[which(data$death_rate==0)]=0.000001
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  d1= data[Year>=2010&Year<=2014,]
  cof3 = modelval(d1)
  d2= data[Year>=2015&Year<=2019,]
  cof4 = modelval(d2)
  
  colnames(cof3)[1] = "Y1"
  colnames(cof4)[1] = "Y2"
  
  cof = merge(cof3,cof4,by = "id")
  RMSE = sqrt(sum((cof$Y1-cof$Y2)^2)/nrow(cof))
  MADs = abs(sum(cof$Y1-cof$Y2)/nrow(cof))
  k= data.frame(age = unique(data$age),RMSE = RMSE, MADs = MADs)
  print(paste("Hum-",data$age[1],": RMSE=",RMSE,"MADs=",MADs))
  cof$age =  unique(data$age)
  
  return(cof)
})

templist = lapply(split(indata,indata$age),function(data){
  data$death_rate[which(data$death_rate==0)]=0.000001
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  d1= data[Year>=2010&Year<=2014,]
  cof3 = modelval_tem(d1)
  d2= data[Year>=2015&Year<=2019,]
  cof4 = modelval_tem(d2)
  
  colnames(cof3)[1] = "Y1"
  colnames(cof4)[1] = "Y2"
  
  cof = merge(cof3,cof4,by = "id")
  
  RMSE = sqrt(sum((cof$Y1-cof$Y2)^2)/nrow(cof))
  MADs = abs(sum(cof$Y1-cof$Y2)/nrow(cof))
  k= data.frame(age = unique(data$age),RMSE = RMSE, MADs = MADs)
  print(paste("TEM-",data$age[1],": RMSE=",RMSE,"MADs=",MADs))
  cof$age =  unique(data$age)
  return(cof)
})
t1 = do.call(rbind,templist)
fwrite(t1,"Result/robutness_validation_tem.csv",row.names = F)
h1 = do.call(rbind,humlist)
fwrite(h1,"Result/robutness_validation_hum.csv",row.names = F)
t1 = fread("Result/robutness_validation_tem.csv",stringsAsFactors = F)
h1 = fread("Result/robutness_validation_hum.csv",stringsAsFactors = F)
t1$var = "temp"
h1$var = "hum"
all = rbind(t1,h1)



R2<-function(x,y){
  xm<-mean(x)
  ssres<-sum((xm-x)^2)
  ssreg<-sum((y-x)^2)
  return(1-ssreg/ssres)
}
rmse = function(a,b){
  s = (a-b)^2
  rmse = sqrt(mean(s))
  return(rmse)
}
bootstrap_validation = function(d1, n_bootstrap){
  lag <- 4
  lagnk <-2
  results <- list()
  d1 = d1[!is.na(d1$death_rate),]

  for (i in 1:n_bootstrap) {
    
    max_repeats <- 30 # 最大重复次数
    count <- 0  # 初始化计数器
    
    repeat {
      count <- count + 1  # 增加计数器
      
      # 打印当前循环次数
      print(paste("当前循环次数:", count))
      
      # 创建自助样本
      bootstrap_indices <- lapply(split(1:nrow(d1), d1$gender_group), function(indices) {
        sample(indices, replace = TRUE)
      })
      
      d1_bootstrap <- do.call(rbind, lapply(bootstrap_indices, function(indices) {
        d1[indices, ]
      }))
      
      # 尝试生成 crossbasis，如果出错则重新采样
      cb <- tryCatch({
        crossbasis(d1_bootstrap$Hum,
                   lag = lag,
                   argvar = list(fun = "ns", knots = quantile(d1_bootstrap$Hum, c(80) / 100, na.rm = T), 
                                 Boundary.knots = range(d1_bootstrap$Hum, na.rm = T)),
                   arglag = list(knots = logknots(lag, lagnk)),
                   group = d1_bootstrap$gender_group)
      }, error = function(e) {
        print(paste("Error in iteration", count, ":", e$message))
        NULL # 如果出错，返回 NULL
      })
      
      # 如果 cb 不是 NULL，则跳出 repeat 循环
      if (!is.null(cb)) {
        print("Modeling start:")
        break
      }
      
      # 如果达到最大重复次数，则跳出循环
      if (count >= max_repeats) {
        print("达到最大重复次数，退出循环")
        break
      }
    }
    
    CDD = onebasis(d1_bootstrap$CHD_95th,fun="strata",breaks=c(1,2,4))
    CDN = onebasis(d1_bootstrap$CHN_95th,fun="strata",breaks=c(1,2,4))
    CDA = onebasis(d1_bootstrap$CH_95th,fun="strata",breaks=c(1,2,4))
    UDD = onebasis(d1_bootstrap$Heatday_95th-d1_bootstrap$CHD_95th,fun="strata",breaks=c(1,2,4))
    UDN = onebasis(d1_bootstrap$Heatnight_95th - d1_bootstrap$CHN_95th,fun="strata",breaks=c(1,2,4))
    UDA = onebasis(d1_bootstrap$Heatall_95th - d1_bootstrap$CH_95th,fun="strata",breaks=c(1,2,4))
    m1 <-  glm(death~ cb + CDD + CDN+ CDA + UDD + UDN+ UDA + ns(Year, df = 4)+ ns(Week, df = 3)+ geo +log(pop),
               family = quasipoisson(link="log"), data = d1_bootstrap, na.action="na.exclude")
    
    d1$predictions <- predict(m1, newdata = d1, type = "response")
    d1 = d1[!is.na(d1$predictions),]
    RMSE <- sqrt(mean((d1$death - d1$predictions)^2))
    R2 <- R2(d1$death,d1$predictions)
    results[[i]] <- data.frame(bootnum = i, rmse = RMSE,r2 = R2)
    print(paste(i,":",RMSE," R2:",R2))
  }
  
  re = rbindlist(results)
  return(re)
}
bootstrap_validation_temp = function(d1, n_bootstrap){
  lag <- 4
  lagnk <-2
  results <- list()
 # d1 = d1[!is.na(d1$death_rate),]
  for (i in 1:n_bootstrap) {
    # 创建自助样本
    print("Sampling start:")
    
    max_repeats <- 30 # 最大重复次数
    count <- 0  # 初始化计数器
    
    repeat {
      count <- count + 1  # 增加计数器
      
      # 打印当前循环次数
      print(paste("当前循环次数:", count))
      
      # 创建自助样本
      bootstrap_indices <- lapply(split(1:nrow(d1), d1$gender_group), function(indices) {
        sample(indices, replace = TRUE)
      })
      
      d1_bootstrap <- do.call(rbind, lapply(bootstrap_indices, function(indices) {
        d1[indices, ]
      }))
      
      # 尝试生成 crossbasis，如果出错则重新采样
      cb <- tryCatch({
        crossbasis(d1_bootstrap$Hum,
                   lag = lag,
                   argvar = list(fun = "ns", knots = quantile(d1_bootstrap$Hum, c(80) / 100, na.rm = T), 
                                 Boundary.knots = range(d1_bootstrap$Hum, na.rm = T)),
                   arglag = list(knots = logknots(lag, lagnk)),
                   group = d1_bootstrap$gender_group)
      }, error = function(e) {
        print(paste("Error in iteration", count, ":", e$message))
        NULL # 如果出错，返回 NULL
      })
      
      # 如果 cb 不是 NULL，则跳出 repeat 循环
      if (!is.null(cb)) {
        print("Modeling start:")
        break
      }
      
      # 如果达到最大重复次数，则跳出循环
      if (count >= max_repeats) {
        print("达到最大重复次数，退出循环")
        break
      }
    }
    
    
    CDD = onebasis(d1_bootstrap$CHD_95th_tem,fun="strata",breaks=c(1,2,4))
    CDN = onebasis(d1_bootstrap$CHN_95th_tem,fun="strata",breaks=c(1,2,4))
    CDA = onebasis(d1_bootstrap$CH_95th_tem,fun="strata",breaks=c(1,2,4))
    UDD = onebasis(d1_bootstrap$Heatday_95th_tem-d1_bootstrap$CHD_95th_tem,fun="strata",breaks=c(1,2,4))
    UDN = onebasis(d1_bootstrap$Heatnight_95th_tem - d1_bootstrap$CHN_95th_tem,fun="strata",breaks=c(1,2,4))
    UDA = onebasis(d1_bootstrap$Heatall_95th_tem - d1_bootstrap$CH_95th_tem,fun="strata",breaks=c(1,2,4))
    m1 <-  glm(death~ cb + CDD + CDN+ CDA + UDD + UDN+ UDA + ns(Year, df = 4)+ ns(Week, df = 3)+ geo +log(pop),
               family = quasipoisson(link="log"), data = d1_bootstrap, na.action="na.exclude")
    
    d1$predictions <- predict(m1, newdata = d1, type = "response")
    d1 = d1[!is.na(d1$predictions),]
    RMSE <- sqrt(mean((d1$death - d1$predictions)^2))
    R2 <- R2(d1$death,d1$predictions)
    results[[i]] <- data.frame(bootnum = i, rmse = RMSE,r2 = R2)
    print(paste(i,":",RMSE," R2:",R2))
  }
  
  re = rbindlist(results)
  return(re)
}

humlist = lapply (split(indata,indata$age),function(data){
  data$death_rate[which(data$death_rate==0)]=0.000001
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  d1= data[Year>=2010&Year<=2019,]
  k =  do.call(rbind,lapply(seq(1,10),function(i){
    r = bootstrap_validation(d1,3)
    return(r)}))
  k$age = data$age[1]
  return(k)
})
temlist = lapply (split(indata,indata$age),function(data){
  data$death_rate[which(data$death_rate==0)]=0.000001
  data$age = as.factor(data$age)
  data$gender_group = paste(data$geo,data$Year)
  d1= data[Year>=2010&Year<=2019,]
  k =  do.call(rbind,lapply(seq(1,10),function(i){
    r = bootstrap_validation_temp(d1,3)
    return(r)}))
  k$age = data$age[1]
  return(k)
})


bootval = rbindlist(humlist)

write.csv(bootval,file.path(outdir,"bootstrap_validation.csv"))
bootval2 = rbindlist(temlist)
write.csv(bootval2,file.path(outdir,"bootstrap_validation_tem.csv"))

library(data.table)
library(dplyr)
bootval =  fread(file.path(outdir,"bootstrap_validation.csv"))
bootval%>%group_by(age)%>%summarise(rmse = mean(rmse),r2 = mean(r2))
bootval = melt(bootval[,-c(1,2)],id = "age")
bootval2 =  fread(file.path(outdir,"bootstrap_validation_tem.csv"))
bootval2%>%group_by(age)%>%summarise(rmse = mean(rmse),r2 = mean(r2))
bootval2 = melt(bootval2[,-c(1,2)],id = "age")
bootval$var = "Humidex"
bootval2$var = "Air temperature"
ball = do.call(rbind,list(bootval,bootval2))
test_results <- ball %>% 
  group_by(age, variable) %>% 
  summarise(p_value = t.test(value ~ var)$p.value)

# Merge the test results with the original data for plotting
ball <- ball %>% 
  left_join(test_results, by = c("age", "variable"))

p <-ggplot(ball)+geom_boxplot(aes(x=variable,y=value,color = var), 
                              width = 0.4, position=position_dodge(0.5), outlier.shape = NA, linewidth =0.3)+
  geom_point(aes(x=variable, y=value, color=var), position=position_jitterdodge(jitter.width=0.2, 
                                                                                dodge.width=0.5), alpha = 0.3,size=0.6) + 
  facet_wrap(variable~age, nrow = 2,scales = "free") +
  theme_bw() +
  scale_x_discrete(labels = c("rmse" = "RMSE", "r2" = expression(R^2)))+
  scale_color_manual(values= c("#a49b90","#CF221F"))+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        panel.grid = element_blank(),
        axis.title.x= element_blank(),
        axis.text.x = element_text(color="black",size = unit(9,"pt")),
        axis.title.y= element_blank(),
        axis.text.y = element_text(color="black",size = unit(9,"pt")),
        legend.position="top",legend.title = element_blank(),
        legend.text = element_text(color = "black", size = unit(9, "pt"))
  )


ggsave(paste0(outdir,"/Boostval_compariation.pdf"),p, width=16, height=12, units="cm", scale=1)

ggsave(paste0(outdir,"/Boostval_compariation.jpg"),p, width=16, height=12, units="cm", scale=1)

indata%>%group_by(age)%>%summarise(mean = mean(death))


