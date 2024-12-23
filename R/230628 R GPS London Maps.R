# 2023 Geo spatial data for London Maps (Including Expensive zones, ehe~)
library(tidyverse)
library(rgdal)
library(sf)
library(readxl)
library(leaflet)
################################################################################
# 1. INTRO: Load the Geo spatial file
# TRIAL to use a shapefile (*.shp) files first!
################################################################################

# 1. Unzip file coz' file downloaded from GADM is in *.zip
# UK_shp_zip = '/home/ron/Downloads/2023 LPDP GUIDES & Documents/2023 London Maps/gadm41_GBR_shp.zip'
# UK_shp_out = '/home/ron/Downloads/2023 LPDP GUIDES & Documents/2023 London Maps/gadm41_GBR_shp'
# unzip (UK_shp_zip, exdir=UK_shp_out)

# 2. Load
UK_spdf <- sf::read_sf(dsn = "raw_data/gadm41_GBR_shp/gadm41_GBR_4.shp")

# summary(UK_spdf) # tells you the max and min coordinates, the kind of projection in use
# length(UK_spdf) # how many regions you have

head(UK_spdf@data)
# glimpse(UK_spdf)

# Compare NAME 1 vs. NAME 2, just being curious to find LONDON
unique(UK_spdf$NAME_1) # Name 1 = 4 constituent countries
unique(UK_spdf$NAME_2) # Name 2 = seems like regencies & cities

unique(UK_spdf[grepl('London', UK_spdf$NAME_2), ]$NAME_2)
# London is here (NAME_2) under the name of "Greater London"

# Filter GPS data to Greater London
G_LDN <- UK_spdf[UK_spdf$NAME_2 %in% c('Greater London'), ]

# Casually create a map by using ggplot2, ehe~
ggplot() +
  geom_polygon(data = G_LDN,
               aes( x = long, y = lat, group = group),
               fill="#69b3a2", color="white") +
  theme_void()

# NEXT:
# U have to combine *.csv OR *.xlsx data to *.shp (CANNOT BE RUN VICE-VERSA)!!!
# Convert the 'SpatialPolygonsDataFrame' to 'sf' object first!

# Combine the df to *.shp data (CANNOT BE RUN VICE-VERSA)!!!
# Convert the 'SpatialPolygonsDataFrame' to 'sf' object first:
# Recall GBR_spdf <- st_read(dsn = GBR_shp_path_LINUX)

# Notes: DO NOT Filter sf Data!!!!
UK_spdf_sf <- sf::st_as_sf(UK_spdf, coords = c("longitude", "latitude"), crs = '4326')
glimpse(GBR_spdf_sf)



################################################################################
# 2. Load & Data wrangling for Greater London areas,
# Specifically I want to highlight the expensive zones

# Workflow:
# 1. Create/load the df
################################################################################

# First of all, I didn't know that the gov have the data that I needed O.o
# SOURCE: https://data.london.gov.uk/dataset/london-workplace-zone-classification
# London Workplace Zone Classification (LWZC)

G_LDN_WZ_Path_LINUX = 'raw_data/LWZC Classification.xls'
G_LDN_WZ <- read_excel(G_LDN_WZ_Path_LINUX)
head(G_LDN_WZ)
unique(G_LDN_WZ$'LA name')
unique(G_LDN$NAME_3) # Name 3 = seems like boroughs/administrative divisions/districts
names(G_LDN_WZ) <- gsub(" ", "_", names(G_LDN_WZ))
head(G_LDN_WZ)

# Check name consistency in GADM database AND gov data:
NeedToBeCorrected_gov <- G_LDN_WZ[!G_LDN_WZ$LA_name %in% G_LDN$NAME_3,]
unique(NeedToBeCorrected_gov$LA_name)

NeedToBeCorrected_GADM <- G_LDN[!G_LDN$NAME_3 %in% G_LDN_WZ$LA_name,]
unique(NeedToBeCorrected_GADM$NAME_3)
# 'Westminster' --> 'City of Westminster'
# But we'll keep it for later since modifying *.shp files need library(sp)
# Now, we only change the gov data so both files can be merged!

# FILTER & data wrangling the gov file:
G_LDN_WZ <- G_LDN_WZ %>% 
  # filter('LA name' != '???') %>% # We'll do the filter if necesary ;)
  mutate(LA_name=str_replace(LA_name, "Westminster", "City of Westminster"))

unique(G_LDN_WZ$LA_name)
# For now, the data is "City of Westminster", but we will change it back later ;)

# Find Group & SubGroup classification,
# I'll classify it by myself based on categories of Groups & Subgroups

head(G_LDN_WZ)

Group_desc <- G_LDN_WZ %>%
  group_by(Group, Group_description, SubGroup, Subgroup_description) %>%
  summarise(countt = n()) %>%
  view()

# Only 12 categories found!
# How about we create new variable called 'Expensiveness' (5 point Likert scale)
# 0 = Unclassified, 5 = very expensive
G_LDN_WZ <- G_LDN_WZ %>%
  mutate(Expensiveness = case_when(
    SubGroup == "FF" ~ 0,
    SubGroup == "A1" ~ 3,
    SubGroup == "A2" ~ 1,
    SubGroup == "B1" ~ 5,
    SubGroup == "B2" ~ 5,
    SubGroup == "C1" ~ 2,
    SubGroup == "C2" ~ 2,
    SubGroup == "D1" ~ 4,
    SubGroup == "D2" ~ 3,
    SubGroup == "D3" ~ 4,
    SubGroup == "E1" ~ 5,
    SubGroup == "E2" ~ 5
  )) %>%
  view()


head(G_LDN_WZ)
unique(G_LDN_WZ$Group_description)
unique(G_LDN_WZ$Subgroup_description)

# Hmm, since each boroughs contains many variations of Group,
# How about we group the df by LA_name, and state the group by the dominant one? 
grouped_G_LDN_WZ <- G_LDN_WZ %>%
  group_by(LA_name, Subgroup_description, Expensiveness) %>%
  summarise(countt = n()) %>%
  ungroup() %>%
  view()

# Find the most dominant subgroup for each boroughs
Expensiveness_G_LDN <- grouped_G_LDN_WZ %>%
  group_by(LA_name) %>%
  filter(countt == max(countt)) %>%
  #top_n(n = 3, wt = countt) %>%
  ungroup() %>%
  view()

# 3. Combine Expensiveness_G_LDN to *.shp data (CANNOT BE RUN VICE-VERSA)!!!
# Convert the 'SpatialPolygonsDataFrame' to 'sf' object first:
G_LDN_sf <- st_as_sf(G_LDN, coords = c("longitude", "latitude"), crs = '4326')
# view(G_LDN_sf)
glimpse(G_LDN_sf)

Comm_merged <- merge(G_LDN_sf, Expensiveness_G_LDN, by.x = 'NAME_3', by.y = 'LA_name', all.x = TRUE) %>% 
  #replace_na(list(Expensiveness = 'Not Targeted')) %>% 
  #filter(Expensiveness != 'Not Targeted') %>% 
  mutate(Expensiveness = as.numeric(Expensiveness))

view(Comm_merged)
glimpse(Comm_merged)

################################################################################
# 3.1. LOAD interactive maps (LEAFLET), map1
################################################################################
# 3.1. for hovered label!



# Leaflet TRIAL!
mypalette <- colorNumeric(palette='Reds',
                          domain=c(Comm_merged$Expensiveness),
                          na.color="transparent")
mypalette(c(0,10))

mytext1 <- paste(
  'District: ', Comm_merged$NAME_3,'<br/>', 
  'Desc: ', Comm_merged$Subgroup_description, '<br/>', 
  sep="") %>%
  lapply(htmltools::HTML)

map1 <- leaflet(Comm_merged) %>% 
  addTiles()  %>% 
  setView( lat=51.5033, lng=-0.1195, zoom=9) %>%
  addPolygons( stroke = F, fillOpacity = 0.5,
               fillColor = ~mypalette(Expensiveness),
               label = mytext1,
               labelOptions = labelOptions( 
                 style = list('font-weight' = 'normal', padding = '3px 8px'), 
                 textsize = '15px', 
                 direction = 'auto')) %>% 
  addLegend('bottomright', pal = mypalette, values = ~Expensiveness,
            title = 'Expensive',
            # labFormat = labelFormat(prefix = "$"),
            opacity = 1)

map1
