library(tidyverse)
library(ggplot2)
library(ggspatial)
library(sf)
library(units)
library(mapchina)
library(openxlsx)

###-----------------------------------------------------------------------------
### COAL AND MINERALS
###-----------------------------------------------------------------------------

##------------------------------------------------------------------------------
##DATA SOURCE: Neudtaedter, E. R. et al. (2023). Compilation of Geospatial Data (GIS) for the Mineral Industries and Related Infrastructure of the People's Republic of China [Data set]. U.S. Geological Survey.
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
##DATA PREPARATION
##------------------------------------------------------------------------------

#read layers from geodatabase
gdb_path <- "CHN_GIS.gdb/CHN_GIS.gdb"
layers <- st_layers(gdb_path)
layer_names <- layers$name
print(layer_names)

for (layer in layer_names) {
  assign(layer, st_read(gdb_path, layer = layer))
}

#dams
str(CHN_Infra_Dams)
summary(as.factor(CHN_Infra_Dams$ADMIN1))

df_dams_sf <- CHN_Infra_Dams %>%
  select(2:5, latitude, longitude, Shape) %>%
  filter(ADMIN1 == "Xinjiang Uygur Autonomous Region") %>%
  mutate(type = factor("Dam")) %>%
  rename_all(tolower)

summary(df_dams_sf)
st_crs(df_dams_sf)

#power stations
str(CHN_Infra_Power_Stations)
summary(as.factor(CHN_Infra_Power_Stations$ADM1))

df_powerstations_sf <- CHN_Infra_Power_Stations%>%
  select(1:3, 5, DsgAttr02, DsgAttr03, DsgAttr04, latitude, longitude, Shape) %>%
  filter(ADM1 == "Xinjiang Uygur Autonomous Region") %>%
  mutate(type = factor("Power Station")) %>%
  rename_all(tolower)

summary(df_powerstations_sf)  
st_crs(df_powerstations_sf)

duplicates <- st_join(df_dams_sf, df_powerstations_sf, join = st_intersects)
duplicates <- duplicates[!is.na(duplicates$longitude.y),]
print(duplicates)

df_powerstations_sf <- df_powerstations_sf[!(df_powerstations_sf$longitude %in% duplicates$longitude.y & df_powerstations_sf$latitude %in% duplicates$latitude.y),]

summary(df_powerstations_sf)

#mineral deposits
str(CHN_Mineral_Deposits)
summary(as.factor(CHN_Mineral_Deposits$ADM1))

df_mineral_deposits_sf <- CHN_Mineral_Deposits %>%
  select(1:5, 7, DsgAttr01, Shape) %>%
  filter(ADM1 == "Xinjiang Uygur") %>% 
  mutate(type = factor("Mineral Deposit")) %>%
  rename_all(tolower)

summary(df_mineral_deposits_sf)
st_crs(df_mineral_deposits_sf)

#mineral exploration sites
str(CHN_Mineral_Exploration)
summary(as.factor(CHN_Mineral_Exploration$AMD1))

df_mineral_exploration_sf <- CHN_Mineral_Exploration %>%
  select(1:2, DsgAttr01, LocOpStat, Country, AMD1, Latitude, Longitude, OwnerName, Shape) %>%
  filter(AMD1 == "Xinjiang") %>%
  mutate(type = factor("Exploration Site")) %>%
  rename_all(tolower)

summary(df_mineral_exploration_sf)
st_crs(df_mineral_exploration_sf)

#mineral facilities
str(CHN_Mineral_Facilities)
summary(as.factor(CHN_Mineral_Facilities$ADM1))

df_mineral_facilities_sf <- CHN_Mineral_Facilities %>%
  select(1:7, ADM1, Latitude, Longitude, OwnerName1, OperateNam, Shape) %>%
  rename(ownername = OwnerName1) %>%
  filter(ADM1 == "Xinjiang") %>%
  mutate(type = factor("Mineral Facilities")) %>%
  rename_all(tolower)

summary(df_mineral_facilities_sf)
st_crs(df_mineral_facilities_sf)

#coal
str(CHN_Mineral_Resources_Coal)
summary(as.factor(CHN_Mineral_Resources_Coal$ADM1))

df_mineral_coal_sfpoly <- CHN_Mineral_Resources_Coal %>%
  select(1:2, Shape_Length, Shape_Area, Shape) %>%
  filter(ADM1 == "Xinjiang") %>%
  mutate(type = factor("Coal")) %>%
  rename_all(tolower)

summary(df_mineral_coal_sfpoly)
st_crs(df_mineral_coal_sfpoly)

st_write(df_mineral_coal_sfpoly, "xj_coal.geojson")

#complete sf data frame with all points
df_resources_combined_sf <- bind_rows(
  df_dams_sf,
  df_mineral_deposits_sf,
  df_mineral_exploration_sf,
  df_mineral_facilities_sf,
  df_powerstations_sf
)

df_resources_combined_sf <- df_resources_combined_sf %>%
  select(-country, -admin1, -adm1, -amd1)

summary(df_resources_combined_sf)

#administrative units in china
df_china_sf <- china%>%
  filter(Name_Province=="新疆维吾尔自治区")%>%
  mutate(Status = as.factor("Status"))%>%
  rename(Code_Prefecture=Code_Perfecture)

#resources in counties

df_resources_counties_sf <- st_join(df_resources_combined_sf, df_china_sf, join = st_within)

points_outside <- df_resources_counties_sf %>% filter(is.na(Code_County))
df_resources_counties_sf <- df_resources_counties_sf %>% filter(!is.na(Code_County))

st_write(df_resources_counties_sf, "xj_mineral sites_counties.geojson")

#visualisation
ggplot() +
  geom_sf(data = df_china_sf) +
  geom_sf(data = df_resources_counties_sf, size = 2, aes(shape = type)) +
  theme_minimal() +
  labs(title = "Sites on the Province Map") +
  theme_void()

ggplot() +
  geom_sf(data = df_china_sf) +
  geom_sf(data = df_mineral_coal_sfpoly, fill = "lightblue", color = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Coal on the Province Map") +
  theme_void()

##------------------------------------------------------------------------------
##COUNTY DATA
##------------------------------------------------------------------------------

df_site_type_counts <- st_drop_geometry(df_resources_counties_sf) %>%
  group_by(Code_County, type) %>%
  summarise(type_count = n(), .groups = "drop")

df_sites_per_county_pivot <- df_site_type_counts %>%
  pivot_wider(names_from = type, values_from = type_count, values_fill = 0)

df_sites_per_county$total_sites_count <- rowSums(df_sites_per_county_pivot[,-1])

df_area <- st_drop_geometry(df_china_sf)%>%
  select(Code_County, Area)

df_sites_per_county <- df_sites_per_county %>%
  left_join(df_area, by = "Code_County")

df_sites_per_county$density <- df_sites_per_county$total_sites_count/df_sites_per_county$Area

df_sites_per_county <- df_sites_per_county %>%
  select(-Area)

summary(df_sites_per_county)
barplot(df_sites_per_county$total_sites_count)

write.xlsx(df_sites_per_county, "xj_mineral sites_counties_count.xlsx")

##------------------------------------------------------------------------------
##PLOTTING AND VISUAL INSPECTION
##------------------------------------------------------------------------------

map_data_resources <- df_china_sf %>%
  left_join(df_sites_per_county, by = "Code_County")

ggplot(data = map_data_resources) +
  geom_sf(aes(fill = total_sites_count), color = NA) +  
  scale_fill_viridis_c(option = "mako", limits = c(0,40), na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Resource sites per county",
    fill = "No. of sites"
  )+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

ggplot(data = map_data_resources) +
  geom_sf(aes(fill = Power_Station), color = NA) +  
  scale_fill_viridis_c(option = "mako", na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Resource sites per county",
    fill = "No. of sites"
  )+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

ggplot(data = map_data_resources) +
  geom_sf(aes(fill = Mineral_Facilities), color = NA) +  
  scale_fill_viridis_c(option = "mako", na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Resource sites per county",
    fill = "No. of sites"
  )+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

###-----------------------------------------------------------------------------
### OIL
###-----------------------------------------------------------------------------

##------------------------------------------------------------------------------
##DATA SOURCE: PRIO-PETRODATA
##------------------------------------------------------------------------------

df_petrodata_sf <- read_sf("PETRODATA/Petrodata_Onshore_V1.2.shp") %>%
  filter(COUNTRY == "China")

ggplot()+
  geom_sf(data = df_petrodata_sf)

st_crs(df_petrodata_sf)

df_china_sf <- china%>%
  filter(Name_Province=="新疆维吾尔自治区")%>%
  mutate(Status = as.factor("Status"))%>%
  rename(Code_Prefecture=Code_Perfecture)

st_crs(df_china_sf)

ggplot()+
  geom_sf(data = df_petrodata_sf, fill = "red")+
  geom_sf(data = df_china_sf, fill = NA) +
  theme_void()

xinjiang_boundary_outer <- df_china_sf %>%
  st_union() %>%           
  st_buffer(0)             

xinjiang_sf <- st_as_sf(st_sfc(xinjiang_boundary_outer))

within_xinjiang_sf <- st_within(df_petrodata_sf, xinjiang_sf, sparse = FALSE)

df_within_xinjiang_sf <- as.data.frame(within_xinjiang_sf)

df_petrodata_sf <- df_petrodata_sf %>%
  mutate(id = row_number())

df_petro_xinjiang_sf <- df_petrodata_sf %>%
  mutate(within = apply(df_within_xinjiang_sf, 1, all)) %>%
  filter(within) %>%
  select(-within)

ggplot()+
  geom_sf(data = df_petro_xinjiang_sf, fill = "red")+
  geom_sf(data = df_china_sf, fill = NA) +
  theme_void()

overlap <- st_intersects(df_china_sf, df_petro_xinjiang_sf, sparse = FALSE)

df_china_sf$petro_county <- apply(overlap, 1, function(x) ifelse(any(x), 1, 0))

ggplot() +
  geom_sf(data = df_china_sf, aes(fill = as.factor(petro_county)), color = "black") +
  geom_sf(data = df_petro_xinjiang_sf, fill = "black", color = "black",
          alpha = 0.8) +
  scale_fill_manual(values = c("0" = "lightgray", "1" = "darkgray"), guide = FALSE) +
  labs(
    title = "Oil fields in counties in Xinjiang"
  ) +
  theme(legend.position = "bottom") +
  theme_void()

df_petro_dummy <- st_drop_geometry(df_china_sf) %>%
  select(Code_County, petro_county)

write.xlsx(df_petro_dummy, "xj_petro_dummy.xlsx")


