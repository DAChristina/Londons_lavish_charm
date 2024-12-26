# 2023 Geo spatial data for London Maps (Including Expensive zones, ehe~)
library(tidyverse)

# DO NOT FILTER SF DATA BY USING dplyr!
UK_spdf <- sf::read_sf(dsn = "raw_data/gadm41_GBR_shp/gadm41_GBR_4.shp")
UK_spdf_sf <- sf::st_as_sf(UK_spdf, coords = c("longitude", "latitude"), crs = '4326')

G_LDN <- UK_spdf_sf[UK_spdf_sf$NAME_2 %in% c('Greater London'), ]
G_LDN_sf <- sf::st_as_sf(G_LDN, coords = c("longitude", "latitude"), crs = '4326')

# Casually create a map by using ggplot2, ehe~
# ggplot(data = G_LDN_sf ) +
#   geom_sf() +
#   xlab("Longitude") + ylab("Latitude") +
#   theme_bw()


# London Workplace Zone Classification (LWZC)
# SOURCE: https://data.london.gov.uk/dataset/london-workplace-zone-classification
G_LDN_WZ <- readxl::read_excel("raw_data/LWZC Classification.xls") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(LA_name=str_replace(LA_name, "Westminster", "City of Westminster"),
                Expensiveness = case_when(
                  SubGroup == "FF" ~ 0,
                  SubGroup == "A1" ~ 8,
                  SubGroup == "A2" ~ 11,
                  SubGroup == "B1" ~ 1,
                  SubGroup == "B2" ~ 2,
                  SubGroup == "C1" ~ 9,
                  SubGroup == "C2" ~ 10,
                  SubGroup == "D1" ~ 6,
                  SubGroup == "D2" ~ 7,
                  SubGroup == "D3" ~ 5,
                  SubGroup == "E1" ~ 4,
                  SubGroup == "E2" ~ 3
                )
  ) %>% 
  dplyr::group_by(LA_name, SubGroup, Subgroup_description, Expensiveness) %>%
  dplyr::summarise(countt = n()) %>%
  dplyr::ungroup()

# Check name consistency in GADM database AND gov data:
# NeedToBeCorrected_gov <- G_LDN_WZ[!G_LDN_WZ$LA_name %in% G_LDN$NAME_3,]
# unique(NeedToBeCorrected_gov$LA_name)
# NeedToBeCorrected_GADM <- G_LDN[!G_LDN$NAME_3 %in% G_LDN_WZ$LA_name,]
# unique(NeedToBeCorrected_GADM$NAME_3)

# Find the most dominant subgroup for each boroughs
Expensiveness_G_LDN <- G_LDN_WZ %>%
  group_by(LA_name) %>%
  filter(countt == max(countt)) %>%
  #top_n(n = 3, wt = countt) %>%
  ungroup()

# London Local Authority name
# SOURCE: https://geoportal.statistics.gov.uk/datasets/42af123c4663466496dafb4c8fcb0c82_0/explore
# LA_names <- read.csv("raw_data/Local_Authority_Districts_(December_2022)_Names_and_Codes_in_the_United_Kingdom.csv")


Comm_merged <- merge(G_LDN_sf, Expensiveness_G_LDN, by.x = 'NAME_3', by.y = 'LA_name', all.x = TRUE) %>% 
  #replace_na(list(Expensiveness = 'Not Targeted')) %>% 
  #filter(Expensiveness != 'Not Targeted') %>% 
  mutate(Expensiveness = as.numeric(Expensiveness))

# Leaflet
mypalette <- leaflet::colorNumeric(palette='Reds',
                          domain=c(Comm_merged$Expensiveness),
                          na.color="transparent",
                          reverse = TRUE)
mypalette(c(0,120))

mytext1 <- paste(
  'District: ', Comm_merged$NAME_3,'<br/>', 
  'Desc: ', Comm_merged$Subgroup_description, '<br/>', 
  sep="") %>%
  lapply(htmltools::HTML)

map1 <- leaflet::leaflet(Comm_merged) %>% 
  leaflet::addTiles()  %>% 
  leaflet::setView( lat=51.5033, lng=-0.1195, zoom=10) %>%
  leaflet::addPolygons(stroke = T, color = "white", weight = 0.5,
                       fillOpacity = 0.5,
                       fillColor = ~mypalette(Expensiveness),
                       label = mytext1,
                       labelOptions = leaflet::labelOptions(
                         style = list('font-weight' = 'normal', padding = '3px 8px'),
                         direction = 'auto')) %>% 
  leaflet::addLegend('bottomright', pal = mypalette, values = ~Expensiveness,
            title = 'Expensive',
            opacity = 1)

map1

htmlwidgets::saveWidget(map1, file = "outputs/interactive_map_expensiveness.html",
                        selfcontained = TRUE)
