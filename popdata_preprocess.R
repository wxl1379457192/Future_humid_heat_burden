rm(list = ls())
gc()
setwd("D:/ATtest/Europe_version2")
library(data.table)

popdir = "popdata"
popL2 = fread(file.path(popdir,"estat_demo_r_pjangroup.tsv"),header = TRUE, data.table = FALSE)
library(tidyr)
library(dplyr)
popL2 <- popL2%>%
  separate('freq,unit,sex,age,geo\\TIME_PERIOD', into = c("freq", "units", "sex","age","geo"), sep = ",")
setDT(popL2)
popL2[, age_group:= fcase(
  grepl("Y_LT5|Y5-9|Y10-14", age), "0-15",
  grepl("Y15-19|Y20-24|Y25-29|Y30-34|Y35-39|Y40-44|Y45-49|Y50-54|Y55-59|Y60-64", age), "15-65",
  grepl("Y65-69|Y70-74|Y75-79|Y80-84|Y_GE85", age), "65+",
  grepl("TOTAL", age), "TOTAL",
  default = NA_character_
)]
popL2 <-popL2 %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"), # 选择所有以"19"或"20"开头的列
    names_to = "Year", # 设置新的年份列名
    values_to = "pop"  # 设置新的数值列名
  )
popL2 = popL2[which(popL2$sex=="T"),]
popL2 = popL2[!is.na(popL2$age_group),]
setDT(popL2)
popL2=popL2[which(popL2$pop!=":"),]
popL2$pop = as.numeric(popL2$pop)
popL2_new <- popL2[, .(pop = sum(pop)), by = .(Year, geo,age_group)]
rm(popL2)


popL3 = fread(file.path(popdir,"estat_demo_r_pjangrp3.tsv"),header = TRUE, data.table = FALSE)
popL3 <- popL3%>%
  separate('freq,sex,unit,age,geo\\TIME_PERIOD', into = c("freq", "sex", "units","age","geo"), sep = ",")
popL3 = popL3[which(popL3$sex=="T"),]
setDT(popL3)
popL3[, age_group:= fcase(
  grepl("Y_LT5|Y5-9|Y10-14", age), "0-15",
  grepl("Y15-19|Y20-24|Y25-29|Y30-34|Y35-39|Y40-44|Y45-49|Y50-54|Y55-59|Y60-64", age), "15-65",
  grepl("Y65-69|Y70-74|Y75-79|Y80-84|Y_GE85", age), "65+",
  grepl("TOTAL", age), "TOTAL",
  default = NA_character_
)]
popL3 <-popL3 %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"), # 选择所有以"19"或"20"开头的列
    names_to = "Year", # 设置新的年份列名
    values_to = "pop"  # 设置新的数值列名
  )
popL3 = popL3[!is.na(popL3$age_group),]
setDT(popL3)
popL3=popL3[which(popL3$pop!=":"),]
popL3$pop = as.numeric(popL3$pop)
popL3_new <- popL3[, .(pop = sum(pop)), by = .(Year, geo,age_group)]
rm(popL3)
gc()
#用NUTS_level3 2014年的人口比例 对NUTS_LEVEL2 2010-2013年的人口做加权处理
poppro = popL3_new[which(popL3_new$Year=="2014"),]
popall = poppro[which(poppro$age_group=="TOTAL"),]
colnames(popall)[4] = "all"
poppro = poppro[!which(poppro$age_group=="TOTAL"),]
poppro = merge(poppro,popall[,c("Year","geo","all")],by = c("Year","geo"))
poppro$pro = poppro$pop/poppro$all
popL2_new$Year = as.numeric(popL2_new$Year)
popL2_new = popL2_new[which(popL2_new$Year<2014),]
popL2_new$level = nchar(popL2_new$geo)
popL2 = popL2_new[which(popL2_new$level==4),]
poppro$level = nchar(poppro$geo)
poppro = poppro[which(poppro$level>4),]
poppro$l4 = substring(poppro$geo,1,4)
poppro$Year = as.numeric(poppro$Year)
colnames(poppro)[2] = "NUTS_LEVEL3"
popL3_add = merge(popL2,poppro[,c("NUTS_LEVEL3","l4","pro","age_group")],
                  by.x =c("geo","age_group"),by.y = c("l4","age_group"),
                  allow.cartesian = TRUE)
popL3_add$pop = popL3_add$pop*popL3_add$pro

popL3_add = popL3_add[,c("Year","NUTS_LEVEL3","age_group","pop")]
colnames(popL3_add)[2] = "geo"
popL3_add$Year = as.numeric(popL3_add$Year)
popL2_new = popL2_new[,c("Year","geo","age_group","pop")]
popf = rbind(popL3_new,popL3_add,popL2_new)
popf = popf[which(popf$Year>=2000),]

outdir2 = "Input_model_data"
if (!file.exists(outdir2)){
  dir.create(outdir2)
}
fwrite(popf, file.path(outdir2,"Aux_pop_data.csv"))



