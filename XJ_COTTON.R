library(tidyverse)
library(raster) #masks dlyr::select
library(stars)
library(sf)
library(mapchina)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(openxlsx)
#library(parallel) do not execute; fried CPU 


##------------------------------------------------------------------------------
## DATA SOURCE: Kang, X., et al. (2023). The 10-m cotton maps in Xinjiang, China during 2018–2021. Scientific Data, 10(1), 688.
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
##DATA PREPARATION
##------------------------------------------------------------------------------
target_crs <- st_crs(4326)

df_china_sf <- china%>%
  filter(Name_Province=="新疆维吾尔自治区")%>%
  mutate(Status = as.factor("Status"))%>%
  rename(Code_Prefecture=Code_Perfecture)

xinjiang_boundaries <- st_transform(df_china_sf, crs = target_crs)

xinjiang_boundary_outer <- df_china_sf %>%
  st_union() %>%           
  st_buffer(0)             

xinjiang_sf <- st_as_sf(st_sfc(xinjiang_boundary_outer))

#load in QGIS prepared shape files
raster_cotton_2018 <- raster("xj_cotton10/2018/cotton_2018_merged.tif")

plot(raster_cotton_2018)

crs(raster_cotton_2018)

cotton_sf <- read_sf("xj_cotton10/2018/cotton_2018_shapefiles.shp")

plot(cotton_sf)

st_crs(xinjiang_boundaries)
st_crs(cotton_sf)

summary(cotton_sf)
head(cotton_sf)

ggplot()+
  geom_sf(data = df_china_sf, fill = NA) +
  geom_sf(data = cotton_sf, fill = "darkgreen", alpha = 0.8) +
  theme_void()

##------------------------------------------------------------------------------
## COTTON PRODUCTION DUMMY AT COUNTY LEVEL; VISUALISATION AND INSPECTION
##------------------------------------------------------------------------------

overlaps <- st_intersects(df_china_sf, cotton_sf, sparse = FALSE)

df_china_sf$overlap_dummy <- apply(overlaps, 1, function(x) ifelse(any(x), 1, 0))

ggplot() +
  geom_sf(data = df_china_sf, aes(fill = as.factor(overlap_dummy)), color = "black") +
  geom_sf(data = cotton_sf, fill = "darkgreen", color = "darkgreen",
          alpha = 0.8) +
  scale_fill_manual(values = c("0" = "lightgray", "1" = "darkgray"), guide = FALSE) +
  labs(
    title = "Cotton fields in cotton producing counties in Xinjiang"
  ) +
  theme(legend.position = "bottom") +
  theme_void()

df_cotton_dummy <- st_drop_geometry(df_china_sf) %>%
  dplyr::select(Code_County, overlap_dummy) %>%
  rename("cotton_producing_county" = "overlap_dummy")

write.xlsx(df_cotton_dummy, "xj_cotton_county_dummy.xlsx")
