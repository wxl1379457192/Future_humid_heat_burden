
pop = read.csv("D:/ATtest/Europe/Auxdata/2015-2019_age_group_pop.csv",stringsAsFactors  = F)
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
da = read.csv("all_data_weekly_V2.csv",stringsAsFactors  = F)
cumsum_reset <- function(x) {
  reset_idx <- which(x == 0)
  cumsum_x <- cumsum(x)
  cumsum_x[-(reset_idx)] - cumsum_x[reset_idx[-1]]
}
#dd = subset(dd,dd$year>=2015&dd$year<=2019)
#da =dd
da = subset(da,da$age!="TOTAL")
da$gender_group = interaction(da$geo,da$year)
pop$age[which(pop$age=="Y15-64")] = "15-65"
pop$age[which(pop$age=="Y_GE65")] = "65+"
pop$age[which(pop$age=="Y_LT15")] = "0-15"
# PARAMETERS FOR THE LAG-RESPONSE FUNCTION
da =do.call(rbind,lapply(split(da,da$age),function(aged){
  agepop = subset(pop,pop$age==unique(aged$age))
  agepop = agepop[,c("TIME_PERIOD","OBS_VALUE","geo","age")]
  colnames(agepop) = c("year","pop","geom","age")
  aged = do.call(rbind,lapply(split(aged,aged$geom),function(dg){
    geopop = subset(agepop,agepop$geom==unique(dg$geom))
    dg = merge(dg,geopop,by=c("year","geom","age"))
    dg = do.call(rbind,lapply(split(dg,dg$year),function(dgy){
      dgy$DN_num[which(dgy$DD_num<=3&dgy$DN_num>0)] =
        dgy$DN_num[which(dgy$DD_num<=3&dgy$DN_num>0)]+
        dgy$DD_num[which(dgy$DD_num<=3&dgy$DN_num>0)]
      dgy$DD_num[which(dgy$DD_num<=3&dgy$DN_num>0)] = 0
      dgy$mixHeat = dgy$DD_num+dgy$DN_num
      dgy$cumheat= 0
      dgy$cumheat[which(dgy$mixHeat==7)] = 1
      zeros <- which(dgy$cumheat == 0)
      dgy$Conheat <- rep(0, length(dgy$cumheat))
      for (i in 1:length(dgy$cumheat)) {
        if (i == 1) {
          dgy$Conheat[i] <- dgy$cumheat[i]
        } else {
          if (i %in% zeros) {
            dgy$Conheat[i] <- 0
          } else {
            dgy$Conheat[i] <- dgy$Conheat[i-1] + dgy$cumheat[i]
          }
        }
      }
      
      dgy$DN_num_tem[which(dgy$DD_num_tem<=3&dgy$DN_num_tem>0)] =
        dgy$DN_num_tem[which(dgy$DD_num_tem<=3&dgy$DN_num_tem>0)]+
        dgy$DD_num_tem[which(dgy$DD_num_tem<=3&dgy$DN_num_tem>0)]
      dgy$DD_num_tem[which(dgy$DD_num_tem<=3&dgy$DN_num_tem>0)] = 0
      dgy$mixHeat_tem = dgy$DD_num_tem+dgy$DN_num_tem
      dgy$cumheat_tem= 0
      dgy$cumheat_tem[which(dgy$mixHeat_tem==7)] = 1
      zeros_tem <- which(dgy$cumheat_tem == 0)
      dgy$Conheat_tem <- rep(0, length(dgy$cumheat_tem))
      for (i in 1:length(dgy$cumheat_tem)) {
        if (i == 1) {
          dgy$Conheat_tem[i] <- dgy$cumheat_tem[i]
        } else {
          if (i %in% zeros_tem) {
            dgy$Conheat_tem[i] <- 0
          } else {
            dgy$Conheat_tem[i] <- dgy$Conheat_tem[i-1] + dgy$cumheat_tem[i]
          }
        }
      }
      return(dgy)
    }))
    print(paste(unique(dg$age),unique(dg$geo)))
    return(dg)
  }))
  return(aged)
}))
outdir = "pooled_model_age_group_dataV2"
if (dir.exists(outdir)==F){dir.create(outdir)
}else{print("This path has been exists")}
write.csv(da,paste0(outdir,"/Inputdata_withinage.csv"),row.names = F)
