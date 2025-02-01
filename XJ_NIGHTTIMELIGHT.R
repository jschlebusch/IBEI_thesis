library(sf)
library(raster)
library(stars)
library(tidyverse)
library(mapchina)
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(viridis)
library(rasterVis)
library(openxlsx)

###-----------------------------------------------------------------------------
### NIGHTTIME LIGHT EMISSION
###-----------------------------------------------------------------------------

##------------------------------------------------------------------------------
## DATA SOURCE: Zhong, X., Yan, Q., & Li, G. (2022). Development of time series of nighttime light dataset of China (2000− 2020). Journal of Global Change Data & Discovery, 3, 416-424.
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
##DATA PREPARATION
##------------------------------------------------------------------------------
#read data
df_china_sf <- china%>%
  filter(Name_Province=="新疆维吾尔自治区")%>%
  mutate(Status = as.factor("Status"))%>%
  rename(Code_Prefecture=Code_Perfecture)

raster_nightlight_2000 <- raster("nightlight_china/EANTLI_like_2000/EANTLI_like_2000.tif")
raster_nightlight_2016 <- raster("nightlight_china/EANTLI_like_2016/EANTLI_like_2016.tif")

cellStats(raster_nightlight_2000, 'mean')
cellStats(raster_nightlight_2016, 'mean')

plot(raster_nightlight_2000)

png("raster_nighlight_2016.png", width = 800, height = 800)
plot(raster_nightlight_2016)
dev.off()

#average nightlight per county
target_crs <- st_crs(4326)

xinjiang_boundaries <- st_transform(df_china_sf, crs = target_crs)

nighttime_light_data_2000 <- projectRaster(raster_nightlight_2000, crs = target_crs$proj4string)
nighttime_light_data_2016 <- projectRaster(raster_nightlight_2016, crs = target_crs$proj4string)

#-------------------------------------------------------------------------------
#export for QGIS
output_filename <- "nighttime_data_2000.tif"
writeRaster(nighttime_light_data_2000, filename=output_filename, format="GTiff")
output_filename <- "nighttime_light_data_2016.tif"
writeRaster(nighttime_light_data_2016, filename=output_filename, format="GTiff")

xinjiang_boundary_outer <- df_china_sf %>%
  st_union() %>%           
  st_buffer(0)             

xinjiang_sf <- st_as_sf(st_sfc(xinjiang_boundary_outer))

st_write(xinjiang_sf, "xinjiang_boundary.shp", delete_layer = TRUE)

#re-load in QGIS prepared data to create visualisations here for consitency in appearance
map_2000 <- raster("xinjiang_nightlight_2000.tif")
map_2016 <- raster("xinjiang_nightlight_2016.tif")

#convert into data frame to be handled by ggplot2
map_2000_df <- as.data.frame(map_2000, xy = TRUE)
map_2016_df <- as.data.frame(map_2016, xy = TRUE)

#-------------------------------------------------------------------------------

#calculate variable of interest average nighttime light values per county
crs(nighttime_light_data_2000)
crs(nighttime_light_data_2016)

raster(nighttime_light_data_2000)
raster(nighttime_light_data_2016)
st_as_sf(xinjiang_boundaries)

nightlight_values_2000 <- raster::extract(nighttime_light_data_2000, xinjiang_boundaries, fun = mean)
nightlight_values_2016 <- raster::extract(nighttime_light_data_2016, xinjiang_boundaries, fun = mean, na.rm = TRUE)

df_china_sf$nightlight_mean_2000 <- nightlight_values_2000
df_china_sf <- df_china_sf %>%
  mutate(nightlight_mean_2000 = ifelse(is.na(nightlight_mean_2000), 0, nightlight_mean_2000))
summary(df_china_sf$nightlight_mean_2000)

df_china_sf$nightlight_mean_2016 <- nightlight_values_2016
summary(df_china_sf$nightlight_mean_2016)

df_nightlight_av <- st_drop_geometry(df_china_sf) %>%
  select(c(Code_County, nightlight_mean_2000, nightlight_mean_2016))

write.xlsx(df_nightlight_av, "xj_nighttimelight_av_00_16.xlsx")

##------------------------------------------------------------------------------
##PLOTTING AND VISUAL INSPECTION
##------------------------------------------------------------------------------

# 2000
levelplot(map_2000, 
          main = "Raster Plot Nightlight 2000",
          col.regions = viridis::viridis(100),
          xlab = "Longitude",
          ylab = "Latitude")

ggplot() +
  geom_raster(data = map_2000_df, aes(x = x, y = y, fill = xinjiang_nightlight_2000)) +
  geom_sf(data = xinjiang_sf, fill = NA, color = "white") +
  scale_fill_viridis_c() +
  coord_sf() +
  theme_void() +
  labs(title = "Raster Plot Nightlight 2000",
       fill = "Radiance")

color_for_zero <- viridis::viridis(100)[1]

ggplot() +
  geom_raster(data = map_2000_df, aes(x = x, y = y, fill = xinjiang_nightlight_2000)) +
  geom_sf(data = xinjiang_sf, fill = NA, color = "white") +
  scale_fill_viridis_c(na.value = color_for_zero, limits = c(0, max(map_2000_df$xinjiang_nightlight_2000, na.rm = TRUE))) +
  coord_sf() +
  theme_void() +
  theme(panel.background = element_rect(fill = color_for_zero, color = NA)) +
  labs(title = "Raster Plot Nightlight 2000",
       fill = "Radiance")

ggplot() +
  geom_raster(data = map_2000_df, aes(x = x, y = y, fill = xinjiang_nightlight_2000)) +
  geom_sf(data = xinjiang_sf, fill = NA, color = "white") +
  scale_fill_viridis_c(na.value = color_for_zero, limits = c(0, 75)) +
  coord_sf() +
  theme_void() +
  theme(panel.background = element_rect(fill = color_for_zero, color = NA)) +
  labs(title = "Raster Plot Nightlight 2000",
       fill = "Radiance")

ggplot(data = df_china_sf) +
  geom_sf(aes(fill = nightlight_mean_2000), color = NA) +  
  scale_fill_viridis_c(option = "magma", limits = c(0,50), na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Av. Nighttime Light Emission per County",
    fill = "Av. Radiance"
  )+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

# 2016
levelplot(map_2016, 
          main = "Raster Plot Nightlight 2016",
          col.regions = viridis::viridis(100),
          xlab = "Longitude",
          ylab = "Latitude")

ggplot() +
  geom_raster(data = map_2016_df, aes(x = x, y = y, fill = xinjiang_nightlight_2016_1)) +
  geom_sf(data = xinjiang_sf, fill = NA, color = "white") +
  scale_fill_viridis_c() +
  coord_sf() +
  theme_void() +
  labs(title = "Raster Plot Nightlight 2016",
       fill = "Radiance")

ggplot(data = df_china_sf) +
  geom_sf(aes(fill = nightlight_mean_2016), color = NA) +  
  scale_fill_viridis_c(option = "magma", limits = c(0,50), na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Av. Nighttime Light Emission per County",
    fill = "Av. Radiance"
  )+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))


###-----------------------------------------------------------------------------
### NIGHTTIME LIGHT EMISSION AS MEASUREMENT OF DEVELOPMENT
###-----------------------------------------------------------------------------
