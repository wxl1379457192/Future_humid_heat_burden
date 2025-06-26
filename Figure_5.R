rm(list = ls())
gc()
setwd("D:/ATtest/Europe_version3/")
library(ggplot2)
library(dplyr)
library(purrr)
library(data.table)
figout = "Figures"
indir = "Heatmor_prediction_future_withadaptation_USD_V2"
filelist = list.files(path = indir,pattern = "\\.csv$",full.names=TRUE)
# 打印文件读取进度的函数
print_progress <- function(file) {
  message("Reading file: ", file)
  return(fread(file))
}

newdir = "Heatmor_prediction_future_withadaptation_USD_agg"
if (dir.exists(newdir)){
  print("Output dir has existed!")
}else{
  dir.create(newdir)
}
premor <- do.call(rbind,lapply(filelist,function(k){
  filename = file.path(newdir,strsplit(k,"/")[[1]][2])
  if (!file.exists(filename)){
    f = print_progress(k)
    if(is.numeric(f$Pre_death_ada)==T){
      f1 = f%>%group_by(rcp,Year,geo,age,adaptation_rate)%>%
        summarise(Humidex = mean(Humidex_mean),
                  pop = mean(proj_pop_age),
                  death_history = mean(death_history),
                  Base_death =quantile(Base_death_ada,0.5),
                  Pre_death_low= quantile(Pre_death_ada,0.25), Pre_death_up= quantile(Pre_death_ada,0.75),
                  Pre_death =quantile(Pre_death_ada,0.5),
                  Pre_death_noada_low= quantile(Pre_death_noada,0.25), Pre_death_noada_up= quantile(Pre_death_noada,0.75),
                  Pre_death_noada =quantile(Pre_death_noada,0.5)
        )
      fwrite(f1,filename,row.names = F)
    }
    else{f1=NULL}
  }
  else{
    f1 = fread(filename)
  }
  return(f1)
}))
fwrite(premor,file.path("Result","Heatmor_prediction_future_withadaptation.csv"),row.names = F)

premor = fread(file.path("Result","Heatmor_prediction_future_withadaptation.csv"),stringsAsFactors = F)
warming = read.csv(file.path("D:/ATtest/Europe_version2/Result","Global_warming.csv"),stringsAsFactors =F)
base = read.csv(file.path("Result","Mortality_prediction_history.csv"),stringsAsFactors = F)
premor$country = substring(premor$geo,1,2)
colnames(premor)[3] = "geom"
label = data.frame(
  country = c("AL", "AT", "BE", "BG", "CH",
              "CY", "CZ","DE", "DK", "EE",
              "EL", "ES", "FI", "FR", "HR",
              "HU", "IS", "IT", "LI", "LT", 
              "LU", "LV","ME", "MT","NL", 
              "NO", "PL", "PT", "RO", "RS", 
              "SE", "SI","SK", "UK"),
  Country_name = c("Albania", "Austria", "Belgium", "Bulgaria", "Switzerland",
                   "Cyprus", "Czech Republic", "Germany","Denmark", "Estonia", 
                   "Greece","Spain", "Finland", "France", "Croatia",
                   "Hungary", "Iceland", "Italy", "Liechtenstein", "Lithuania",
                   "Luxembourg","Latvia","Montenegro","Malta", "Netherlands",
                   "Norway", "Poland", "Portugal", "Romania", "Serbia",
                   "Sweden", "Slovenia","Slovakia", "United Kingdom"),
  geo = c("Southern Europe", "Western Europe", "Western Europe", "Eastern Europe", "Western Europe", 
          "Southern Europe", "Eastern Europe", "Western Europe","Northern Europe", "Northern Europe", 
          "Southern Europe","Southern Europe", "Northern Europe", "Western Europe","Southern Europe",
          "Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe", "Northern Europe",
          "Western Europe", "Northern Europe","Eastern Europe", "Southern Europe","Western Europe", 
          "Northern Europe", "Eastern Europe", "Southern Europe", "Eastern Europe", "Eastern Europe", 
          "Northern Europe",  "Southern Europe","Eastern Europe", "Northern Europe")
)
premor = merge(premor,label,by="country")
premor = merge(premor,warming,by.x=c("Year","rcp"),by.y =c("Year","Scenario"))
spaggeo <- premor %>%
  group_by(rcp,geo,Year, warming, adaptation_rate) %>%
  summarise(Pre_death = (sum(Pre_death))/sum(pop)*1000000,
            Pre_death_low= (sum(Pre_death_low))/sum(pop)*1000000,
            Pre_death_up= (sum(Pre_death_up))/sum(pop)*1000000,
            Pre_death_noada = sum(Pre_death_noada)/sum(pop)*1000000,
            Pre_death_noada_low = sum(Pre_death_noada_low)/sum(pop)*1000000,
            Pre_death_noada_up = sum(Pre_death_noada_up)/sum(pop)*1000000
  )

spag = premor%>%group_by(rcp,Year,warming,adaptation_rate)%>%
  summarise(Pre_death = (sum(Pre_death))/sum(pop)*1000000,
            Pre_death_low= (sum(Pre_death_low))/sum(pop)*1000000,
            Pre_death_up= (sum(Pre_death_up))/sum(pop)*1000000,
            Pre_death_noada = sum(Pre_death_noada)/sum(pop)*1000000,
            Pre_death_noada_low = sum(Pre_death_noada_low)/sum(pop)*1000000,
            Pre_death_noada_up = sum(Pre_death_noada_up)/sum(pop)*1000000
            )



spag$warming_level = round(spag$warming,1)
spag <- spag %>%
  group_by(rcp, warming_level, adaptation_rate) %>%
  summarise(
    Pre_death = mean(Pre_death),
    Pre_death_low = mean(Pre_death_low),
    Pre_death_up = mean(Pre_death_up),
    Pre_death_noada = mean(Pre_death_noada),
    Pre_death_noada_low = mean(Pre_death_noada_low),
    Pre_death_noada_up = mean(Pre_death_noada_up)
  )
spag = spag[which(spag$warming_level>=1.1&spag$warming_level<=4),]




spagada <- spag[, c("rcp", "warming_level", "adaptation_rate",
                    "Pre_death", "Pre_death_low", "Pre_death_up")]

spagada <- spagada[which(spagada$adaptation_rate %in% c(0, 0.05, 0.1, 0.25, 0.5)), ]

spagnoada <- spag[, c("rcp", "warming_level",
                      "Pre_death_noada", "Pre_death_noada_low", "Pre_death_noada_up")]
colnames(spagnoada)[3:5] <- c("Pre_death", "Pre_death_low", "Pre_death_up")
spagnoada <- unique(spagnoada)
spagnoada$adaptation_rate <- "No adaptation"
spagada$adaptation_rate=as.factor(spagada$adaptation_rate)
spag <- bind_rows(spagada, spagnoada)

chinese_colors <- c( 
  "No adaptation"="grey80",
  "0" = "#b61624",           
  "0.05" = "#faedcd",         
  "0.1" = "#a8dadc",          
  "0.25" = "#457b9d",        
  "0.5" = "#1d3557"          
)
g = ggplot(data = spag) +geom_ribbon(aes(x = warming_level, 
        ymin = Pre_death_low, 
        ymax = Pre_death_up, 
        fill = as.factor(adaptation_rate)),
    alpha = 0.1,
    show.legend = T
  ) +
  geom_line(
    aes(x = warming_level, 
        y = Pre_death, 
        color = as.factor(adaptation_rate)),
    size = 0.5
  ) +
  geom_line(
    data = filter(spag, adaptation_rate == "No adaptation"),
    aes(x = warming_level, y = Pre_death),
    color = "black",  # 直接指定颜色
    size = 0.6,  # 加粗
    linetype = "dashed"
  ) +
  facet_wrap(~rcp, nrow = 1,scales = "free_x",strip.position = "bottom") +
  scale_fill_manual(
    name = "Adaptation rate",
    values = chinese_colors,
    labels =  c("No adaptation"="No adapation", "0" ="0%", 
                "0.05" ="5%", "0.1" ="10%","0.25" = "25%", 
                "0.5" ="50%")
  ) +
  scale_color_manual(
    name = "Adaptation rate",
    values = chinese_colors,
    labels = c("No adaptation"="No adapation", "0" ="0%", 
               "0.05" ="5%", "0.1" ="10%","0.25" = "25%", 
               "0.5" ="50%")
  ) +
  theme_bw()+
  theme(
    strip.text.x = element_text(
      size =9, color = "black",face = "bold" ), 
    panel.grid = element_blank(),
    axis.title.x= element_text(color="black",hjust = 0.5,vjust=1,size =unit(9,"pt")),
    axis.title.y= element_text(color="black",hjust = 0.5,vjust=1,size =unit(9,"pt")),
    axis.text.x = element_text(color="black",hjust = 0.5,vjust=1,size =unit(9,"pt")),
    axis.text.y = element_text(color="black",size = unit(9,"pt")),
    legend.position="top"
  ) +
  labs(
    x = "Warming Level (°C)",
    y = "Annual heat-related deaths per million people"
  ) +
  guides(
    color = guide_legend(nrow = 1),
    fill = guide_legend(nrow = 1)   
  )
ggsave(paste0(figout,"/fig5.pdf"),g, width=15, height=10, units="cm", scale=1)
ggsave(paste0(figout,"/fig5.jpg"),g, width=15, height=10, units="cm", scale=1)



#######死亡率降低情况描述#########
spag = premor%>%group_by(rcp,Year,warming,adaptation_rate)%>%
  summarise(Pre_death = (sum(Pre_death))/sum(pop)*1000000,
            Pre_death_low= (sum(Pre_death_low))/sum(pop)*1000000,
            Pre_death_up= (sum(Pre_death_up))/sum(pop)*1000000,
            Pre_death_noada = sum(Pre_death_noada)/sum(pop)*1000000,
            Pre_death_noada_low = sum(Pre_death_noada_low)/sum(pop)*1000000,
            Pre_death_noada_up = sum(Pre_death_noada_up)/sum(pop)*1000000
  )

spag$warming_level = round(spag$warming,1)
spag <- spag %>%
  group_by(rcp, warming_level, adaptation_rate) %>%
  summarise(
    Pre_death = mean(Pre_death),
    Pre_death_low = mean(Pre_death_low),
    Pre_death_up = mean(Pre_death_up),
    Pre_death_noada = mean(Pre_death_noada),
    Pre_death_noada_low = mean(Pre_death_noada_low),
    Pre_death_noada_up = mean(Pre_death_noada_up)
  )
spag<- spag[which(spag$adaptation_rate %in% c(0, 0.05, 0.1, 0.25, 0.5)), ]
spag = spag[which(spag$warming_level>=1.1&spag$warming_level<=4),]
warming2 = spag[which(spag$warming_level==2&spag$Pre_death!=0),]
warming1.5 = spag[which(spag$warming_level==1.5&spag$Pre_death!=0),]
spag1 =spag%>%mutate(Pre_death = (Pre_death - Pre_death_noada)/Pre_death_noada,
                    Pre_death_low = (Pre_death_low - Pre_death_noada_low)/Pre_death_noada_low,
                    Pre_death_up = (Pre_death_up - Pre_death_noada_up)/Pre_death_noada_up)%>%
  select(-Pre_death_noada, -Pre_death_noada_low, -Pre_death_noada_up)

baseline=spag1[which(spag1$adaptation_rate==0&spag1$Pre_death!=0),]
adap50=spag1[which(spag1$adaptation_rate==0.5&spag1$Pre_death!=0),]

baseline%>%
  group_by(rcp) %>%
  summarise(min=max(Pre_death)*100,max = min(Pre_death)*100)

#######死亡率降低情况描述#########
spag = premor%>%group_by(rcp,Year,warming,geom,adaptation_rate)%>%
  summarise(Pre_death = (sum(Pre_death))/sum(pop)*1000000,
            Pre_death_low= (sum(Pre_death_low))/sum(pop)*1000000,
            Pre_death_up= (sum(Pre_death_up))/sum(pop)*1000000,
            Pre_death_noada = sum(Pre_death_noada)/sum(pop)*1000000,
            Pre_death_noada_low = sum(Pre_death_noada_low)/sum(pop)*1000000,
            Pre_death_noada_up = sum(Pre_death_noada_up)/sum(pop)*1000000
  )

spag$warming_level = round(spag$warming,1)
spag <- spag %>%
  group_by(rcp, warming_level, adaptation_rate,geom) %>%
  summarise(
    Pre_death = mean(Pre_death),
    Pre_death_low = mean(Pre_death_low),
    Pre_death_up = mean(Pre_death_up),
    Pre_death_noada = mean(Pre_death_noada),
    Pre_death_noada_low = mean(Pre_death_noada_low),
    Pre_death_noada_up = mean(Pre_death_noada_up)
  )
spag =spag%>%mutate(Pre_death = (Pre_death - Pre_death_noada)/Pre_death_noada,
                     Pre_death_low = (Pre_death_low - Pre_death_noada_low)/Pre_death_noada_low,
                     Pre_death_up = (Pre_death_up - Pre_death_noada_up)/Pre_death_noada_up)%>%
  select(-Pre_death_noada, -Pre_death_noada_low, -Pre_death_noada_up)
spag$Pre_death[is.nan(spag$Pre_death)] = 0 
spag<- spag[which(spag$adaptation_rate %in% c(0, 0.05, 0.1, 0.25, 0.5)), ]
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
shpfile = file.path("D:/ATtest/Europe_version2/Figure_250119", "output_NUTS3.shp")
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = 3035)
country_abbr <- data.frame(
  NAME = c("Spain", "France", "Switzerland", "Germany", "Italy", 
           "Poland", "Finland", 
           "Greece", "Turkey", "Egypt"),
  ABBR = c("ESP", "FRA", "CHE", "DEU", "ITA", 
           "POL",  "FIN",
           "GRC", "TUR", "EGY"),
  stringsAsFactors = FALSE
)
world <- world %>% left_join(country_abbr, by = c("name" = "NAME"))
figout = "Figures"
for (w in c(1.5,3)){
  shp_file = spag[which(spag$warming_level==w),]
  NUT = st_read(shpfile)
  shp_file = merge(NUT,shp_file,by.x = "NUTS_ID",by.y="geom")
  shp_file <- st_transform(shp_file, crs = 3035)
  shp_file$Pre_death = -shp_file$Pre_death
  g = ggplot() +
    geom_sf(data = world, fill = "grey96", color = "grey50", size = 2, alpha = 1) +
    geom_sf(data = shp_file, aes(fill = cut(Pre_death*100,
                                            breaks = c(-5, 0, 5,  10, 15, 20,25,30,50,75,100))), #
            color = "white", size = 0.1) +
    geom_sf(data = world, fill = "transparent", color = "grey50", size = 2, alpha = 0) +
    geom_sf_text(data = world %>% filter(!is.na(ABBR)), aes(label = ABBR), 
                 size = unit(5, "pt"), color = "black", check_overlap = TRUE) +
    coord_sf(xlim = c(2200000, 7000000), ylim = c(1500000, 5400000)) +
    scale_fill_manual(name = "Decrease in projected heat-related excess deaths\n(per million people)",
                      values = c("#d9ed92","#85e48c","#99e98c","#76c893", "#52b691",
                                 "#34A0A4","#168AAd","#1a759f","#1e6091","#184e77"),
                      labels = c("<0","0-5","5-10","11-15","16-20","21-25",
                                 "26-30","31-50","51-75","75-100"),
                      drop = FALSE,guide = guide_legend(title.position = "left", title.hjust = 0.5)) +  # Keep all categories in the legend
    theme(panel.background = element_rect(fill = "#f2fcff"),
          panel.grid.major = element_line(color = "grey93"),
          legend.key.size = unit(3, "cm"),  # Adjust legend size
          legend.key.width = unit(0.5, "cm"),  # Adjust legend width
          legend.title = element_text(color = "black", size = unit(18, "pt"), angle = 90),
          legend.text = element_text(size = unit(18, "pt")),
          axis.title = element_blank(),
          axis.text = element_text(color = "black", size = unit(18, "pt")),
          plot.margin = margin(1, 1, 1, 1),
          plot.title = element_text(size = unit(18, "pt")),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(size = unit(18, "pt"), face = "bold")) +
    facet_grid(adaptation_rate~rcp              # 将标签放在图的左右和上下侧
    )
    ggsave(paste0(figout,"/fig5b_warming",w,".jpg"),g, width=30, height=50, units="cm", scale=2,limitsize=F)
}








