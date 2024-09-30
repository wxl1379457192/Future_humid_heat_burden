library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(dplyr)
setwd("D:/ATtest/Europe_version2/")
dir = "Result"
filelist  =c("ssp126.shp","ssp245.shp","ssp337.shp","ssp585.shp")
labellist = c("SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5")
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

figout = "Figure_0721"
all_data <- data.frame()
for (i in seq(1,4)){
  shp_file <- st_read(file.path(dir,filelist[i]))
  colnames(shp_file)[12] ="value"
  shp_file <- st_transform(shp_file, crs = 3035)
  shp_file$source <- labellist[i]
  all_data <- rbind(all_data, shp_file)
}


f = ggplot() +
  geom_sf(data = world, fill = "grey96", color = "grey50", size = 2, alpha = 1) +
  geom_sf(data = all_data, aes(fill = cut(value, breaks = c(0, 25, 50, 100, 150, 200, 300, 500, 700, 1500, 3000, 7000))), 
          color = "white", size = 0.1) +
  geom_sf(data = world, fill = "transparent", color = "grey50", size = 2, alpha = 0) +
  geom_sf_text(data = world %>% filter(!is.na(ABBR)), aes(label = ABBR), 
               size = unit(5, "pt"), color = "black", check_overlap = TRUE) +
  scale_fill_manual(name = "Projected heat-related excess deaths\n(per 10,000 people)",
                    values = c("#4798b3","#7ab8cc","#b9d5de","#d4e7ee","#eaf3f6",
                               "#f0d6d0", "#dfaea3", "#b4604e", "#800000", "#430f02", "#1f0001"),
                    labels = c("0-25", "26-50", "51-100","101-150","151-200","201-300",
                               "301-500","501-700","701-1500", "1501-3000", "3001-7000"),
                    drop = FALSE,guide = guide_legend(title.position = "left", title.hjust = 0.5)) +  # Keep all categories in the legend
  coord_sf(xlim = c(2200000, 7000000), ylim = c(1500000, 5400000)) +
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
ggsave(paste0(figout,"/fig3_V2.pdf"),f, width=24, height=27, units="cm", scale=2)
ggsave(paste0(figout,"/fig3_V2.jpg"),f, width=24, height=27, units="cm", scale=2)

#The projected heat-related excess deaths per 10,000 people
