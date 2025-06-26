library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(dplyr)
library(data.table)
setwd("D:/ATtest/Europe_version3/")

shpfile = file.path("D:/ATtest/Europe_version2/Figure_250119", "output_NUTS3.shp")
filelist  =c("Predicted_total_heat_deaths_ssp585_Warming_1.5.csv","Predicted_total_heat_deaths_ssp585_Warming_2.csv",
             "Predicted_total_heat_deaths_ssp585_Warming_3.csv","Predicted_total_heat_deaths_ssp585_Warming_4.csv")
labellist = c("Global warming of 1.5\u00B0C","Global warming of 2.0\u00B0C",
              "Global warming of 3.0\u00B0C","Global warming of 4.0\u00B0C")
# 读取全球国家边界
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = 3035)

country_abbr <- data.frame(
  NAME = c("Portugal", "Spain", "France", "Switzerland", "Germany", "Italy", 
           "Romania", "Poland", "Denmark", "Sweden", "Finland", "Latvia", 
           "Greece", "Bulgaria", "Ukraine", "Turkey", "Egypt", "Iceland", "Ireland"),
  ABBR = c("PRT", "ESP", "FRA", "CHE", "DEU", "ITA", 
           "ROU", "POL", "DNK", "SWE", "FIN", "LVA", 
           "GRC", "BGR", "UKR", "TUR", "EGY", "ISL", "IRL"),
  stringsAsFactors = FALSE
)
world <- world %>% left_join(country_abbr, by = c("name" = "NAME"))

figout = "Figures"
all_data <- data.frame()
for (i in seq(1,4)){
  shp_file <-fread(file.path(figout,filelist[i]))
  colnames(shp_file)[4] ="value"
  shp_file = shp_file[,-c(1)]
  NUT = st_read(shpfile)
  shp_file = merge(NUT,shp_file,by.x = "NUTS_ID",by.y="geom")
  shp_file <- st_transform(shp_file, crs = 3035)
  shp_file$source <- labellist[i]
  all_data <- rbind(all_data, shp_file)
}


f = ggplot() +
  geom_sf(data = world, fill = "grey96", color = "grey50", size = 2, alpha = 1) +
  geom_sf(data = all_data, aes(fill = cut(value,breaks = c(-0.1, 25,  50,  75,  100, 200, 400, 600, 800, 
                                                           1000, 1500, 3000))), #
          color = "white", size = 0.1) +
  geom_sf(data = world, fill = "transparent", color = "grey50", size = 2, alpha = 0) +
  geom_sf_text(data = world %>% filter(!is.na(ABBR)), aes(label = ABBR), 
               size = unit(5, "pt"), color = "black", check_overlap = TRUE) +
  coord_sf(xlim = c(2200000, 7000000), ylim = c(1500000, 5400000)) +
  scale_fill_manual(name = "Projected heat-related excess deaths\n(per million people)",
                    values = c("#4798b3","#7ab8cc","#b9d5de","#d4e7ee","#fcf2da",
                               "#f0d6d0", "#dfaea3", "#b4604e", "#800000", "#430f02", "#1f0001"),
                    labels = c("0-25", "26-50", "51-75","76-100","101-200","201-400","401-600",
                               "601-800","801-1000","1001-1500", ">1500"),
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
  facet_wrap(~source)
ggsave(paste0(figout,"/fig3.pdf"),f, width=24, height=27, units="cm", scale=2)
ggsave(paste0(figout,"/fig3.jpg"),f, width=24, height=27, units="cm", scale=2)

#The projected heat-related excess deaths per 10,000 people
