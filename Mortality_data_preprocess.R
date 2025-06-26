rm(list = ls())
gc()
setwd("D:/ATtest/Europe_version3")
library(lubridate)
library(data.table)
library(tidyr)

mortality_process = function(mortality_path){
  mortality <- fread(mortality_path, 
                     select = c("TIME_PERIOD", "age", "sex","geo","OBS_VALUE"), 
                     stringsAsFactors = FALSE)
  
  mortality[, `:=` (Year = as.numeric(substring(TIME_PERIOD, 1, 4)),
                    Week = as.numeric(substring(TIME_PERIOD, 7, 8)))]
  mortality[, age := fcase(
    grepl("LT5|Y5-9|Y10-14", age), "0-15",
    grepl("Y15-19|Y20-24|Y25-29|Y30-34|Y35-39|Y40-44|Y45-49|Y50-54|Y55-59|Y60-64", age), "15-65",
    grepl("Y65-69|Y70-74|Y75-79|Y80-84|Y85-89|Y_GE90", age), "65+",
    grepl("TOTAL", age), "TOTAL",
    default = NA_character_
  )]
  mortality_new <- mortality[, .(death = sum(OBS_VALUE)), by = .(geo, Year, Week, sex, age)]
  mortality_new <- mortality_new[!is.na(age) & sex == "T:Total"]
  mortality_new$geo= sub(":.*", "", mortality_new$geo)
  mor_new = do.call(rbind,lapply(split(mortality_new,mortality_new$geo),function(g){
    if(max(g$Year)<2022){
      repnum = 2022-max(g$Year)
      if(repnum>1){
        gd = do.call(rbind,lapply(range(0,repnum),function(r){
          k = g[which(g$Year ==max(g$Year)),]
          k$Year = 2022-r
          return(k)
        }))
        gd = do.call(rbind,list(g,gd))
      }else{
        gd = g[which(g$Year ==max(g$Year)),]
        gd$Year = 2022
        gd = do.call(rbind,list(g,gd))
      }
      return(gd)
    }else{
      return(g)
    }
  }))
  
  mor1 <- mor_new[geo %like% "DE" & geo != "DE"]
  mor2 <- mor_new[!geo %like% "DE"]
  mor1_all <- mor_new[geo == "DE"]
  mor1new = do.call(rbind,lapply(split(mor1_all,mor1_all$Year),function(y){
    y2 = mor1[which(mor1$Year==unique(y$Year)),]
    y  = do.call(rbind,lapply(split(y,y$Week),function(w){
      w2 = y2[which(y2$Week==unique(w$Week)),]
      yd = data.frame(geo=unique(w$geo),Year = unique(w$Year),Week =  unique(w$Week),
                      sex = unique(w$sex),age="0-15",
                      death = w$death[which(w$age=="TOTAL")]-w$death[which(w$age=="15-65")]-
                        w$death[which(w$age=="65+")])
      w = do.call(rbind,list(w,yd))
      w$prop = w$death/w$death[which(w$age=="TOTAL")]
      w2n = do.call(rbind,lapply(unique(w$age),function(a){
        data.frame(geo=w2$geo,Year = w2$Year,Week =  w2$Week,sex = w2$sex,
                   age = rep(a,nrow(w2)), death = w2$death*w$prop[which(w$age==a)])
      }))
      return(w2n)
    }))
    return(y)
  }))
  mortality_new <- rbindlist(list(mor1new, mor2), use.names = TRUE, fill = TRUE)
  return(mortality_new)
}
mortality_batch_storage = function(mortalitydata,Year,outdir){
  if (file.exists(morname)){
    print(paste(morname," has existed"))
  }else{
    m <- mortalitydata[Year == Year,]
    fwrite(m, morname)
  }
}
mortality_path = "D:/ATtest/Europe_version2/Mortality_data/estat_demo_r_mweek3_en.csv" #Mortality data
mortality_new = mortality_process(mortality_path)


outdir = "D:/ATtest/Europe_version2/Mortality_data"
if (!file.exists(outdir)){
  dir.create(outdir)
}
for (m in split(mortality_new,mortality_new$Year)){
  morname = file.path(outdir,paste0("Weekly_mortality_totalage_",unique(m$Year),".csv"))
  fwrite(m, morname)
}
