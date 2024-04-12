###########################################
library(ggplot2)
setwd("D:/ATtest/Europe")

pop = read.csv("pop_structure_projected/proj_19rp3_linear.csv",stringsAsFactors = F)
pop = subset(pop,pop$age!="TOTAL")
pop = subset(pop,pop$sex=="T")
pop$age[which(pop$age=="Y_LT1")] = "Y0"
pop$age[which(pop$age=="Y_GE100")] = "Y100"
pop$age = as.numeric(gsub("Y","",pop$age))
breaks <- c(-Inf,15, 65, Inf)
# Create labels for the age groups
labels <- c("<15", "16-65", "65+")

# Use cut() to create the age groups
pop$age_group <- cut(pop$age, breaks = breaks, labels = labels)
library(dplyr)
p = pop%>%
  group_by(projection,sex,unit,geo,TIME_PERIOD,age_group)%>%
  summarise(pop = sum(OBS_VALUE))
p$geo_l1 = substring(p$geo,1,3)
p$geo_l2 = substring(p$geo,1,4)
p$geo_l0 = substring(p$geo,1,2)
p1 =  p%>%
  group_by(projection,sex,unit,geo_l1,TIME_PERIOD,age_group)%>%
  summarise(pop = sum(pop))
colnames(p1)[4] = "geo"
p2 =  p%>%
  group_by(projection,sex,unit,geo_l2,TIME_PERIOD,age_group)%>%
  summarise(pop = sum(pop))
colnames(p2)[4] = "geo"
p3 =  p%>%
  group_by(projection,sex,unit,geo_l0,TIME_PERIOD,age_group)%>%
  summarise(pop = sum(pop))
colnames(p3)[4] = "geo"
p = p[,-c(8,9,10)]
p = do.call(rbind,list(p,p1,p2))
sum = p%>%group_by(projection,geo,TIME_PERIOD)%>%
  summarise(sum = sum(pop))

p = merge(p,sum,by=c("projection","geo","TIME_PERIOD"))
p$ratio = p$pop/p$sum
p=subset(p,p$projection=="BSL")

#pop structure projection in UK##########
ukp = read.csv("pop_structure_projected/proj_UK_country_level.csv",stringsAsFactors = F)
library(tidyverse)
ukp <- ukp %>%
  pivot_longer(cols = c(England, Northern.Ireland, Scotland, Wales),
               names_to = "country", values_to = "pop") %>%
  select(Projected.Year, country, age,pop)%>%
  mutate(pop = as.numeric(gsub(",", "",pop)))

label = read.csv("pop_structure_projected/NUTS_levels3_label.csv",stringsAsFactors = F)
label = subset(label,label$CNTR_CODE=="UK")
l1 = subset(label,label$LEVL_CODE==1)
l1$l0 = "England"
l1$l0[which(l1$NAME_LATN=="Scotland")] = "Scotland"
l1$l0[which(l1$NAME_LATN=="Wales")] = "Wales"
l1$l0[which(l1$NAME_LATN=="Northern Ireland")] = "Northern.Ireland"

ukp = do.call(rbind,lapply(split(ukp,ukp$Projected.Year),function(y){
  y= do.call(rbind,lapply(split(y,y$country),function(c){
    c$sum = as.numeric(c$pop[which(c$age=="Total")])
    c$ratio = c$pop/c$sum
    return(c)
  }))
  return(y)
}))
ukp = subset(ukp,ukp$age!="Total")
ukp$age[which(ukp$age== "\"0-15\"")]=1
ukp$age[which(ukp$age=="\"16-65\"")]=16
ukp$age[which(ukp$age=="\"65+\"")]=66
ukp$age=as.numeric(ukp$age)
# Create labels for the age groups
labels <- c("<15", "16-65", "65+")
breaks <- c(-Inf,15, 65, Inf)
ukp$age_group <- cut(ukp$age, breaks = breaks, labels = labels)
uk = merge(ukp,l1[,c("FID_1","l0")],by.x="country",by.y="l0")
colnames(uk)=c("country","TIME_PERIOD","age","pop","sum","ratio","age_group","geo")
pcon = do.call(rbind,list(p[,c("geo","TIME_PERIOD","age_group","pop","ratio")],
                          uk[,c("geo","TIME_PERIOD","age_group","pop","ratio")]))

write.csv(pcon,"pop_structure_projected/proj_pop_age_proportion.csv",row.names = F)



