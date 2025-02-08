library(readxl)
library(haven)
library(foreign)
# for plotting
library(ggplot2)
library(ggspatial)
#library(patchwork)
#library(scico)
# for data wrangling
library(tidyverse)
library(dplyr)
library(openxlsx)
library(units)
#library(jsonlite)
library(sf)
library(geosphere)
library(stringr)
#shapefiles for China
library(mapchina)
#for translation
library(deeplr)

select <- dplyr::select

library(lme4)

library(MCMCglmm)
library(MCMCpack)
#library(mice)
library(coda)
#library(brms)
#library(rstanarm)
#library(rjags)
#library(runjags)

library(stargazer)

set.seed(123)

##------------------------------------------------------------------------------
##DATA PREPARATION
##------------------------------------------------------------------------------

#shapefiles for administrative units in China
df_china_sf <- china%>%
  filter(Name_Province=="新疆维吾尔自治区")%>%
  mutate(Status = as.factor("Status"))%>%
  rename(Code_Prefecture=Code_Perfecture)
summary(as.factor(df_china_sf$Name_Province))
st_crs(df_china_sf)

#data frame with all cultural sites
# df_cult_sites<-read.dbf("AllCulturalSites.dbf")
# str(df_cult_sites)
# summary(df_cult_sites$Latest.Mon
# )
#spatial features of all cultural sites
df_cult_sf<-read_sf("AllCulturalSites.shp")%>%
  mutate(Status = as.factor(Status))%>%
  filter(Status != "Duplicate")%>%
  mutate(Status = as.factor(case_when(
    as.character(Status) == "Destroyed" ~ "Destroyed",
    as.character(Status) %in% c("Significantly damaged", "Significantly Damages") ~ "Significantly Damaged",
    as.character(Status) %in% c("Slightly damaged", "Slightly Damaged") ~ "Slightly Damaged",
    TRUE ~ as.character(Status)
  )))
st_crs(df_cult_sf)

# df_cult_sites<-df_cult_sites%>%
#   mutate(Status = as.factor(Status))%>%
#   filter(Status != "Duplicate")%>%
#   mutate(Status = as.factor(case_when(
#     as.character(Status) == "Destroyed" ~ "Destroyed",
#     as.character(Status) %in% c("Significantly damaged", "Significantly Damages") ~ "Significantly Damaged",
#     as.character(Status) %in% c("Slightly damaged", "Slightly Damaged") ~ "Slightly Damaged",
#     TRUE ~ as.character(Status)
#   )))
# 
# duplicate_sites<- table(df_cult_sf$Number)[table(df_cult_sf$Number) > 1]
# print(duplicate_sites)

#distance to regional capital
urumqi_coords_point <- c(87.6169, 43.8266) #check coord ref system

df_cult_sf$distance_to_urumqi <- apply(df_cult_sf, 1, function(row) {
  point <- c(as.numeric(row['Longitude']), as.numeric(row['Latitude']))
  distHaversine(point, urumqi_coords_point)
})

df_cult_sf$distance_to_urumqi <- df_cult_sf$distance_to_urumqi/1000 #this can be done with units package
summary(df_cult_sf$distance_to_urumqi)
hist(df_cult_sf$distance_to_urumqi)

df_cult_in_county_sf <- st_join(df_china_sf, df_cult_sf, join = st_intersects, left = FALSE)
duplicate_cult_in_county<- table(df_cult_in_county_sf$Number)[table(df_cult_in_county_sf$Number) > 1]
print(duplicate_cult_in_county)

df_cult_in_county_sf <- df_cult_in_county_sf %>% distinct(Number, .keep_all = TRUE)

df_sites_distance <- as.data.frame(st_drop_geometry(df_cult_in_county_sf))%>%
  select(starts_with("Code"), 
         "Number",
         "Type",
         "Status.y",
         "Earliest M",
         "Latest Mon",
         "Latest Ima",
         "distance_to_urumqi")%>%
  rename("status" = "Status.y")%>%
  mutate(year = as.factor(str_replace(`Latest Mon`, ".*-(\\d{2})", "20\\1")),
         year_earliest = as.factor(str_replace(`Earliest M`, ".*-(\\d{2})", "20\\1")))%>%
  mutate(destroyed_dummy = as.factor(ifelse(status == "Destroyed", 1, 0)))

summary(df_sites_distance)
summary(df_sites_distance$distance_to_urumqi)
hist(df_sites_distance$distance_to_urumqi)
densityplot(df_sites_distance$distance_to_urumqi)
summary(df_sites_distance$year)
summary(df_sites_distance$year_earliest)

#counts per county
df_cult_code <- st_drop_geometry(df_cult_in_county_sf) %>%
  select(Number, Code_County)

df_cult_sf <- df_cult_sf %>%
  left_join(df_cult_code, by = "Number")

df_cult_counts <- st_drop_geometry(df_cult_sf)%>%
  group_by(Code_County) %>%
  summarize(Cult_Count = n())

#relative amounts
df_cult_relative_counts <- st_drop_geometry(df_cult_sf) %>%
  group_by(Code_County) %>%
  summarize(
    TotalSites = n(),
    DestroyedSites = sum(Status == "Destroyed"),
    SignificantlyDamagedSites = sum(Status == "Significantly Damaged"),
    SlightlyDamagedSites = sum(Status == "Slightly Damaged"),
    UnclearSites = sum(Status == "Unclear"),
    UndamagedSites = sum(Status == "Undamaged")
  ) %>%
  mutate(
    DestroyedRelative = DestroyedSites / TotalSites,
    SignificantlyDamagedRelative = SignificantlyDamagedSites / TotalSites,
    SlightlyDamagedRelative = SlightlyDamagedSites / TotalSites,
    UnclearSitesRelative = UnclearSites / TotalSites,
    UndamagedSitesRelative = UndamagedSites / TotalSites,
    CombinedRelative = (DestroyedSites + SignificantlyDamagedSites) / TotalSites
  )

write.xlsx(df_cult_relative_counts, "xj_relative_destruction.xlsx")

#English county and prefecture names, for reference
##UNTIL L. 134 NOT TO BE RE-RUN - TRANSLATION VOLUME RESTRICTIONS
languages<-available_languages2(auth_key = "*****")

df_china_sf$Name_County_English <- translate2(text = df_china_sf$Name_County, 
                                               source_lang = "ZH",
                                               target_lang = "EN",
                                               auth_key = "*****")

#excel file with codes and county names in English for reference and basis for data collection from Xinjiang statistical yearbook
df_names_and_codes <- st_drop_geometry(df_china_shp)%>%
  select(c("Code_County", "Code_Prefecture", "Name_County_English"))
write.xlsx(df_names_and_codes, file = "county_names_and_codes.xlsx")

#demographic data (for 2016)
df_demographics_2016 <- read_xlsx("xj_population_16.xlsx") %>%
  filter(!is.na(Total)) %>%
  mutate(across(4:10, as.numeric))%>%
  mutate(uyghur_relative = Uyghur / Total,
         han_relative = Han / Total)%>%
  mutate(Code_County = as.character(Code_County))

summary(df_demographics_2016$uyghur_relative)
summary(df_demographics_2016$han_relative)
hist(df_demographics_2016$uyghur_relative)
hist(df_demographics_2016$han_relative)

duplicate_count_demo<- table(df_demographics_2016$Code_County)[table(df_demographics_2016$Code_County) > 1]
print(duplicate_count_demo)

#measures of ethnic diversity
df_diversity <- read.xlsx("xj_ethnicdiv_16.xlsx")%>%
  select(c(Code_County, Fractionalisation, Polarisation)) %>%
  mutate(Code_County = as.character(Code_County))
summary(df_diversity)

#demographic change
df_demographic_change <- read.xlsx("xj_han_change_91_16.xlsx") %>%
  mutate(Code_County = as.character(Code_County))
summary(df_demographic_change)

#XPCC presence and population
df_bingtuan_2016 <- read.xlsx("xj_bingtuan_change_16.xlsx") %>%
  select(c(1,3:7)) %>%
  rename(bingtuan_population = Bingtuan.Population_2016) %>%
  mutate(Code_County = as.character(Code_County))
summary(df_bingtuan_2016)

#tourism - at prefecture level
df_tourism_2016 <- read.xlsx("xj_tourism_16.xlsx") %>%
  select(c(1,4)) %>%
  filter(!is.na(Code_Prefecture)) %>%
  mutate(Code_Prefecture = as.character(Code_Prefecture))
summary(df_tourism_2016)

#government expenditure and revenue
df_gov_finance_2016 <- read.xlsx("xj_gov_expenditure_16.xlsx") %>%
  select(c(1,4,5)) %>%
  filter(!is.na(revenue)) %>%
  mutate(rev_ex_ratio = revenue / expenditure) %>%
  mutate(Code_County = as.character(Code_County))
summary(df_gov_finance_2016)

#all attacks from the Global Terrorism Database
df_attacks<-read_xlsx("globalterrorismdb_0522dist.xlsx")%>%
  filter(country_txt == "China")%>%
  filter(provstate %in% c("Xinjiang", "Xinjiang Uyghur"))%>%
  filter(iyear > 2000)

#as sf to match with counties via coordinates
attacks_sf <- st_as_sf(df_attacks, coords = c("longitude", "latitude"), crs = 4326) #check coordinate reference system
attacks_per_county <- st_join(attacks_sf, df_china_sf, join = st_within)
attacks_count <- attacks_per_county %>%
  group_by(Code_County) %>%
  summarize(attacks = n())

#as regular data frame
df_attacks_count <- st_drop_geometry(attacks_count)
df_attacks_count <- as.data.frame(df_attacks_count)
summary(df_attacks_count)
barplot(df_attacks_count$attacks)

duplicate_attacks <- table(df_attacks_count$Code_County)[table(df_attacks_count$Code_County) > 1]
print(duplicate_attacks)

write.xlsx(df_attacks_count, "xj_attacks_per_county.xlsx")

#detention centers
df_detention_sf <- read_sf("CampDataset_v1.shp")%>%
  rename(latitude = Lat,
         longitude = Long)

st_crs(df_detention_sf)

duplicate_camps<- table(df_detention_sf$Number)[table(df_detention_sf$Number) > 1]
print(duplicate_camps)

df_detention_county_sf <- st_join(df_detention_sf, df_china_sf, join = st_within)

df_county_counts_detention_sf <- df_detention_county_sf%>%
  group_by(Code_County) %>%
  summarize(detention_count = n())

df_county_counts_detention <- st_drop_geometry(df_county_counts_detention_sf)

summary(df_county_counts_detention)
hist(df_county_counts_detention$detention_count)

write.xlsx(df_county_counts_detention, "xj_detention_per_county.xlsx")

#resource sites
df_resources_sf <- read_sf("xj_mineral sites_counties.geojson")
st_crs(df_resources_sf)

df_mineral_facilities_sf <- df_resources_sf %>%
  filter(type == "Mineral_Facilities")

#distance between heritage sites and resources
distances_cult_res <- st_distance(df_cult_sf, df_resources_sf)
distance_matrix_cult_res <- as.matrix(distances_cult_res)
min_distances <- apply(distance_matrix_cult_res, 1, min)
df_cult_sf$distance_to_resource <- min_distances/1000

summary(df_cult_sf$distance_to_resource)
hist(df_cult_sf$distance_to_resource)
hist(log(df_cult_sf$distance_to_resource))

distances_cult_minf <- st_distance(df_cult_sf, df_mineral_facilities_sf)
distance_matrix_cult_minf <- as.matrix(distances_cult_minf)
min_distances_cmf <- apply(distance_matrix_cult_minf, 1, min)
df_cult_sf$distance_to_mineral_facility <- min_distances_cmf/1000

summary(df_cult_sf$distance_to_mineral_facility)
hist(df_cult_sf$distance_to_mineral_facility)

#coal reserves
df_coal_sf <- read_sf("xj_coal.geojson")%>%
  select(3:6) 

df_coal_sf <- st_transform(df_coal_sf, crs = 4326)

summary(df_coal_sf)

#location of heritage sites on coal reserves
cult_coal <- st_join(df_cult_sf, df_coal_sf, join = st_within)

cult_coal <- st_drop_geometry(cult_coal) %>%
  mutate(in_coal = ifelse(is.na(shape_length), 0, 1)) %>%
  select(Number, in_coal) %>%
  distinct(Number, .keep_all = TRUE)

summary(cult_coal)

df_cult_sf <- df_cult_sf %>%
  left_join(cult_coal, by = "Number") 
df_cult_sf$in_coal <- as.factor(df_cult_sf$in_coal)

summary(df_cult_sf)

#resource sites per county excl. coal reserves
df_county_counts_resources <- read.xlsx("xj_mineral sites_counties_count.xlsx") %>%
  rename(resource_density = density,
         resource_sites_count = total_sites_count)

#county area covered by coal reserve
coal_union <- st_union(df_coal_sf)
df_china_sf$county_area <- st_area(df_china_sf)

intersection_county_coal <- st_intersection(df_china_sf, coal_union)
intersection_county_coal$intersection_area <- st_area(intersection_county_coal)

intersection_summary <- intersection_county_coal %>%
  group_by(Code_County) %>%
  summarise(total_intersection_area = sum(intersection_area))

intersection_summary <- st_drop_geometry(intersection_summary)

df_china_sf <- df_china_sf %>%
  left_join(intersection_summary, by = "Code_County") 

df_china_sf$county_area <- units::set_units(df_china_sf$county_area, km^2)

df_china_sf$total_intersection_area <- units::set_units(df_china_sf$total_intersection_area, km^2)

df_china_sf <- df_china_sf %>%
  mutate(county_area = as.numeric(county_area),
         total_intersection_area = as.numeric(total_intersection_area))

df_china_sf <- df_china_sf %>%
  mutate(total_intersection_area = replace_na(total_intersection_area, 0))

df_china_sf <- df_china_sf %>%
  mutate(share_area_covered = total_intersection_area / county_area)

summary(df_china_sf$share_area_covered)
hist(df_china_sf$share_area_covered)

#counties with oil reserves
df_oil <- read.xlsx("xj_petro_dummy.xlsx")
summary(df_oil)

#cotton production
df_cotton <- read.xlsx("xj_cotton_county_dummy.xlsx")
summary(df_cotton)

#nighttime light emission and change 2000 to 2016
df_nighttimelight <- read.xlsx("xj_nighttimelight_av_00_16.xlsx") %>%
  mutate(across(2:3, as.numeric))%>%
  mutate(nightlight_mean_2000 = nightlight_mean_2000 + 0.001,
         nightlight_mean_2016 = nightlight_mean_2016 + 0.001) %>%
  mutate(nightlight_change = nightlight_mean_2016 - nightlight_mean_2000)
summary(df_nighttimelight)

#toponyms

df_toponyms <- read.xlsx("xj_toponyms.xlsx")%>%
  rename_all(tolower)

##------------------------------------------------------------------------------
##EDA DATA
##------------------------------------------------------------------------------

#for Visuals

#data for plots

map_data <- df_china_sf %>%
  left_join(df_cult_relative_counts, by = "Code_County")
map_data_2 <- df_china_sf %>%
  left_join(df_demographics_2016, by = "Code_County")
map_data_3 <- df_china_sf %>%
  left_join(df_attacks_count, by = "Code_County")
map_data_4 <- df_china_sf %>%
  left_join(df_county_counts_detention, by = "Code_County")
map_data_5 <- df_china_sf %>%
  left_join(df_county_counts_resources, by = "Code_County")
map_data_6 <- df_china_sf %>%
  left_join(df_oil, by = "Code_County")
map_data_7 <- df_china_sf %>%
  left_join(df_diversity, by = "Code_County")
map_data_8 <- df_china_sf %>%
  left_join(df_demographic_change, by = "Code_County")
map_data_9 <- df_china_sf %>%
  left_join(df_bingtuan_2016, by = "Code_County")
map_data_10 <- df_china_sf %>%
  left_join(df_tourism_2016, by = "Code_Prefecture")
map_data_11 <- df_china_sf %>%
  left_join(df_gov_finance_2016, by = "Code_County")
map_data_12 <- df_china_sf %>%
  left_join(df_cotton, by = "Code_County")
map_data_13 <- df_china_sf %>%
  left_join(df_nighttimelight, by = "Code_County")

##------------------------------------------------------------------------------
##COMPLETE DATA SET
##------------------------------------------------------------------------------

#site level
df_cult_complete <- st_drop_geometry(df_cult_sf) %>%
  select(Number, Type, Code_County, Prefecture, Status, distance_to_urumqi, distance_to_resource, distance_to_mineral_facility, in_coal) %>%
  rename_all(tolower)

#county level data
df_county_complete <- st_drop_geometry(map_data)%>%
  dplyr::select(c(Code_County, Code_Prefecture, Area, Density, 
         ends_with("Sites"), 
         ends_with("Relative"),
         share_area_covered)) %>%
  rename(share_area_covered_coal = share_area_covered)

df_county_complete <- df_county_complete %>%
  left_join(df_demographics_2016, by = "Code_County") %>%
  left_join(df_attacks_count, by = "Code_County") %>%
  left_join(df_county_counts_detention, by = "Code_County") %>%
  left_join(df_county_counts_resources, by = "Code_County")%>%
  left_join(df_oil, by = "Code_County") %>%
  left_join(df_diversity, by = "Code_County") %>%
  left_join(df_demographic_change, by = "Code_County") %>%
  left_join(df_bingtuan_2016, by = "Code_County") %>%
  left_join(df_tourism_2016, by = "Code_Prefecture") %>%
  left_join(df_gov_finance_2016, by = "Code_County") %>%
  left_join(df_cotton, by = "Code_County") %>%
  left_join(df_nighttimelight, by = "Code_County") %>%
  mutate_all(~ as.numeric(as.character(.)))

df_county_complete <- df_county_complete %>%
  dplyr::select(-c(`County Name`, Year)) %>%
  mutate(attacks = replace_na(attacks, 0)) %>%
  rename_all(tolower)

df_county_complete <- df_county_complete %>%
  mutate(across(starts_with("code_"), as.character)) %>%
  mutate(across(ends_with("_dummy"), as.factor)) %>%
  mutate(petro_county = as.factor(petro_county),
         cotton_producing_county = as.factor(cotton_producing_county))

df_county_complete <- df_county_complete %>%
  left_join(df_toponyms, by = "code_county")

summary(df_county_complete)

df_county_complete <- df_county_complete %>%
  mutate(any_se = destroyedrelative + change_relative,
         any_se_destr_damages = combinedrelative + change_relative)


openxlsx::write.xlsx(df_county_complete, "xj_countyvars.xlsx")

df_county_complete_naomit <- df_county_complete %>%
  mutate(across(28:35, ~ ifelse(is.na(.), 0, .)))

df_county_complete_naomit <- na.omit(df_county_complete_naomit) %>%
  filter(totalsites >= 5) %>%
  mutate(urumqi_dummy = as.factor(case_when(
    as.character(code_prefecture) == "6501" ~ 1,
    TRUE ~ 0
  )))
summary(df_county_complete_naomit)

hist(df_county_complete_naomit$destroyedrelative)
hist(df_county_complete_naomit$combinedrelative)
hist(log(df_county_complete_naomit$share_area_covered_coal))

hist(df_county_complete_naomit$han_relative)
hist(df_county_complete_naomit$relative_change_han_pp)

hist(df_county_complete_naomit$polarisation)
hist(df_analysis$fractionalisation)

hist(df_analysis$rev_ex_ratio)


  
#both levels
df_analysis <- df_cult_complete %>%
  left_join(df_county_complete, by = "code_county") %>%
  filter(totalsites >= 5) %>%
  mutate(across(starts_with("code"), as.factor),
         prefecture = as.factor(prefecture))%>%
  mutate(across(c(mineral_facilities, power_station, exploration_site, 
                  dam, mineral_deposit, resource_sites_count, resource_density), 
                ~ replace_na(., 0))) %>%
  mutate(destroyed_dummy = as.factor(case_when(
    as.character(status) == "Destroyed" ~ 1,
    TRUE ~ 0
  ))) %>%
  mutate(destroyed_damaged_dummy = as.factor(case_when(
    as.character(status) %in% c("Destroyed", "Significantly Damaged") ~ 1,
    TRUE ~ 0
  ))) %>%
  mutate(in_urumqi_dummy = as.factor(case_when(
    as.character(code_prefecture) == "6501" ~ 1,
    TRUE ~ 0
  )))

summary(df_analysis)
nlevels(df_analysis$code_county)

hist(log(df_analysis_bayes$distance_to_urumqi))
hist(log(df_analysis_bayes$distance_to_resource))
hist(log(df_analysis_bayes$distance_to_mineral_facility))

ggplot(df_analysis_bayes, aes(x = in_coal)) +
  geom_bar() +
  labs(title = "location in coal reserve") +
  xlab(" ") +
  ylab("Count") + 
  theme_clean()


df_analysis_bayes <- na.omit(df_analysis) # bayesian model issue with NAs; only few - just rm


write.xlsx(df_analysis, "SE_XJ_analysis.xlsx")
##------------------------------------------------------------------------------
##VISUALISATIONS
##------------------------------------------------------------------------------

#cultural heritage sites
ggplot()+
  geom_sf(data = df_china_sf, fill = NA)+
  geom_point(data = df_cult_sf, mapping = aes(x=Longitude, y=Latitude, shape = Status, colour = Status))+
  geom_sf(data = df_coal_sf, fill = "lightblue", alpha = 0.2) +
  scale_color_viridis_d(option = "rocket", alpha = 0.5) +
  coord_sf()+
  labs(
    title = "Status of individual sites on coal reserves"
  ) +
  theme_void() +
  theme(legend.position = "bottom")
ggsave("indiv_sites_map.png", width = 8, height = 6)

ggplot(df_analysis_bayes, aes(x = status)) +
  geom_bar() +
  labs(title = "Count of Status") +
  xlab("Status") +
  ylab("Count") + 
  theme_clean()

ggplot(data = map_data) +
  geom_sf(aes(fill = DestroyedRelative), color = NA) +  
  scale_fill_viridis_c(option = "magma", na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Relative Amount of Destroyed Cultural Sites per County",
    fill = "Destroyed Relative"
  )+
  theme(legend.position = "bottom")

ggplot(data = map_data) +
  geom_sf(aes(fill = ifelse(TotalSites < 4, NA, DestroyedRelative)), color = NA) + 
  scale_fill_viridis_c(option = "magma", na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Relative Amount of Destroyed Cultural Sites per County",
    fill = "Destroyed Relative"
  )+
  theme(legend.position = "bottom")

ggplot(data = map_data) +
  geom_sf(aes(fill = ifelse(TotalSites < 5, NA, CombinedRelative)), color = NA) +  
  scico::scale_fill_scico(palette = "lajolla", na.value = alpha("grey", 0.5)) +
 # scale_fill_viridis_c(option = "magma", limits = c(0,1), na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Relative Amount of Destroyed and Significantly Damaged Cultural Sites per County",
    fill = "Destroyed and Significantly Damaged Sites"
  )+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
ggsave("cultural sites_hm.png", width = 8, height = 6)

ggplot(data = map_data) +
  geom_sf(aes(fill = UndamagedSitesRelative), color = NA) +  
  scale_fill_viridis_c(option = "magma", na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Relative Amount of Undamaged Sites",
    fill = "Undamaged Sites"
  )+
  theme(legend.position = "bottom")

#demographics
ggplot(data = map_data_2) +
  geom_sf(aes(fill = uyghur_relative), color = NA) +  
  scale_fill_viridis_c(option = "mako", limits = c(0,1), na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Relative Uyghur population per county, 2015",
    fill = "Relative No. of Uyghurs"
  )+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
ggsave("demographics_2005_hm.png", width = 8, height = 6)

ggplot(data = map_data_7) +
  geom_sf(aes(fill = Polarisation), color = NA) +  
  scale_fill_viridis_c(option = "magma", limits = c(0,1), na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Ethnic polarisation per county, 2016",
    fill = "Degree of ethnic polarisation"
  )+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))


#attacks
ggplot(data = map_data_3) +
  geom_sf(aes(fill = attacks), color = NA) +  
  scale_fill_viridis_c(option = "viridis", limits = c(0,10), na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Attacks since 2000, per County",
    fill = "Number of attacks"
  )+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
ggsave("attacks since 2000_hm.png", width = 8, height = 6)

#detention centers
ggplot(data = map_data_4) +
  geom_sf(aes(fill = detention_count)) +
  scale_fill_viridis_c(option = "magma", na.value = alpha("grey", 0.5)) +
  theme_void() +
  labs(
    title = "No. of detention centres per county",
    fill = "Number of centres"
  ) +
  theme(legend.position = "bottom")

#resources
ggplot()+
  geom_sf(data = df_china_sf, fill = NA)+
  geom_sf(data = df_resources_sf, mapping = aes(shape = type, colour = type))+
  scale_color_viridis_d(option = "rocket", alpha = 0.5) +
  coord_sf()+
  labs(
    title = "Location of resource sites in Xinjiang"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

ggplot(data = map_data_5) +
  geom_sf(aes(fill = resource_sites_count)) +
  scico::scale_fill_scico(palette = "lajolla", na.value = alpha("grey", 0.5)) +
  theme_void() +
  labs(
    title = "No. of resource sites per county",
    fill = "Number of sites"
  ) +
  theme(legend.position = "bottom")

ggplot(data = df_china_sf) +
  geom_sf(aes(fill = share_area_covered)) +
  scico::scale_fill_scico(palette = "lajolla", na.value = alpha("grey", 0.5)) +
  theme_void() +
  labs(
    title = "Share covered by coal reserve",
    fill = "Share"
  ) +
  theme(legend.position = "bottom")

#increase in Han
ggplot(data = map_data_8) +
  geom_sf(aes(fill = relative_change_han_pp), color = NA) +  
  scale_fill_viridis_c(option = "viridis", na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Increase in Han by 2016 against 1991 baseline, PP",
    fill = "Relative No. of Uyghurs"
  )+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))
ggsave("demographics_2005_hm.png", width = 8, height = 6)



#distance and status per individual site
#distance to Urumqi
aov(df_sites_distance$distance_to_urumqi ~ df_sites_distance$status) %>%
  summary()

kruskal.test(df_sites_distance$distance_to_urumqi ~ df_sites_distance$status)

wilcox.test(distance_to_urumqi ~ destroyed_dummy , data=df_sites_distance)

df_sites_distance%>%
  ggplot(aes(x = status, y = distance_to_urumqi)) +
  geom_boxplot() +
  labs(x = "Status", y = "Distance to Urumqi") +
  facet_wrap(~ year, scale = "free") +
  labs(
    title = "Status of all sites and distance to Urumqi") +
  theme_minimal() +
  coord_flip()
ggsave("status_distance_bp.png", width = 8, height = 6)

df_sites_distance%>%
  ggplot(aes(x = distance_to_urumqi)) +
  geom_histogram() +
  facet_wrap(~ Type) +
  theme_minimal()

df_sites_distance%>%
  filter(str_starts(Type, "Relics"))%>%
  ggplot(aes(x = distance_to_urumqi)) +
  geom_histogram() +
  facet_wrap(~ status) +
  labs(
    title = "Status of relicts and distance to Urumqi") +
  theme_minimal()
ggsave("relicts_status_distance_hist.png", width = 8, height = 6)

df_sites_distance%>%
  filter(str_starts(Type, "Religio"))%>%
  ggplot(aes(x = distance_to_urumqi)) +
  geom_histogram() +
  facet_wrap(~ status) +
  labs(
    title = "Status of rel. sites and distance to Urumqi") +
  theme_minimal()
ggsave("religious_status_distance_hist.png", width = 8, height = 6)

#distance to Resources
aov(df_cult_complete$distance_to_resource ~ df_cult_complete$Status) %>%
  summary()

kruskal.test(df_cult_complete$distance_to_resource ~ df_cult_complete$Status)

df_cult_complete %>%
  ggplot(aes(x = distance_to_resource)) +
  geom_histogram() +
  facet_wrap(~ Status) +
  theme_minimal()

aov(df_cult_complete$distance_to_mineral_facility ~ df_cult_complete$Status) %>%
  summary()

kruskal.test(df_cult_complete$distance_to_mineral_facility ~ df_cult_complete$Status)

df_cult_complete %>%
  ggplot(aes(x = distance_to_mineral_facility)) +
  geom_histogram() +
  facet_wrap(~ Status) +
  theme_minimal()

df_cult_complete %>%
  select(Status, in_coal) %>%
  table () %>%
  chisq.test()

#correlation tests at the county level:
#normality tests
relative_vars <- grep("elative$", names(df_county_complete), value = TRUE)
test_vars <- c(relative_vars, "attacks", "Area", "Density", "TotalSites", "share_area_covered")

shapiro_results <- lapply(test_vars, function(var) {
  shapiro_test <- shapiro.test(df_county_complete[[var]])
  list(Variable = var,
       W_statistic = shapiro_test$statistic,
       p_value = shapiro_test$p.value,
       Normality = ifelse(shapiro_test$p.value >= 0.05, "Normal", "Not Normal"))
})

df_shapiro_results <- do.call(rbind, shapiro_results)
print(df_shapiro_results)

#correlation tests
#Spearman's Rank Test
spearmantest_pairs <- list(
  c("uyghur_relative","CombinedRelative"),
  c("uyghur_relative","DestroyedRelative"),
  c("attacks","DestroyedRelative"),
  c("Area","DestroyedRelative"),
  c("total_sites_count", "DestroyedRelative"),
  c("Mineral_Facilities", "DestroyedRelative"),
  c("share_area_covered", "DestroyedRelative")
)

results <- list()
for (pair in spearmantest_pairs) {
  test_result <- cor.test(df_county_complete[[pair[1]]], df_county_complete[[pair[2]]], method = "spearman", exact = FALSE, alternative = "greater")
  results[[paste(pair[1], "_vs_", pair[2])]] <- test_result
}

for (key in names(results)) {
  cat(key, ":\n")
  print(results[[key]])
  cat("\n")
}

#Kendall's tau (because Newson (2002): Parameters behind “nonparametric” statistics, p. 47)
kendalltest_pairs <- list(
  c("CombinedRelative", "uyghur_relative"),
  c("DestroyedRelative", "uyghur_relative"),
  c("DestroyedRelative", "han_relative"),
  c("DestroyedRelative", "attacks"),
  c("DestroyedRelative", "Area"),
  c("DestroyedRelative", "share_area_covered")
)

results_kendall <- list()
for (pair in kendalltest_pairs) {
  test_result_kendall <- cor.test(df_county_complete[[pair[1]]], df_county_complete[[pair[2]]], method = "kendall")
  results[[paste(pair[1], "_vs_", pair[2])]] <- test_result_kendall
}

for (key in names(results)) {
  cat(key, ":\n")
  print(results[[key]])
  cat("\n")
}


##------------------------------------------------------------------------------
## ANALYSIS
##------------------------------------------------------------------------------

# FREQUENTIST ------------------------------------------------------------------

# PROBIT - INDIV SITES LEVEL

m1_logit <- glm(destroyed_dummy ~ distance_to_urumqi +
                  distance_to_mineral_facility +
                  in_coal +
                  in_urumqi_dummy,
                family = binomial(link="probit"),
                data = df_analysis)

summary(m1_logit)


# LINEAR - COUNTY VARIANCE

m1_linear <- lm(destroyedrelative ~ attacks +
                  #bingtuan_dummy +
                  detention_count +
                  #resource_sites_count +
                  mineral_deposit +
                  #petro_county +
                  #share_area_covered_coal +
                  cotton_producing_county +
                  number_beds +
                  nightlight_change +
                  uyghur_relative +
                  relative_change_han_pp,
  data = df_analysis)

summary(m1_linear)

m1_linear_DDS <- lm(destroyedrelative ~ attacks +
                  bingtuan_dummy +
                  detention_count +
                  #resource_sites_count +
                  mineral_deposit +
                  #petro_county +
                  #share_area_covered_coal +
                  #cotton_producing_county +
                  number_beds +
                  nightlight_change +
                  fractionalisation +
                  #uyghur_relative,
                  #relative_change_han_pp +
                  revenue,
                data = df_county_complete_naomit)

summary(m1_linear_DDS)

m1_linear_DDS_weighted <- lm(destroyedrelative ~ attacks +
                               bingtuan_dummy +
                               detention_count +
                               #resource_sites_count +
                               #mineral_deposit +
                               petro_county +
                               #share_area_covered_coal +
                               #cotton_producing_county +
                               #number_beds +
                               #nightlight_change +
                               uyghur_relative +
                               #relative_change_han_pp +
                               revenue +
                               urumqi_dummy,
                             data = df_county_complete_naomit,
                             weights = totalsites)

summary(m1_linear_DDS_weighted)

# HIERARCHICAL

m1_hierarchical <- glmer(destroyed_dummy ~ distance_to_urumqi +
                           distance_to_mineral_facility +
                           in_coal +
                           in_urumqi_dummy +
                           attacks +
                           bingtuan_dummy +
                           detention_count +
                           #resource_sites_count +
                           mineral_deposit +
                           #petro_county +
                           #share_area_covered_coal +
                           #cotton_producing_county +
                           number_beds +
                           #nightlight_change +
                           #uyghur_relative,
                           relative_change_han_pp +
                           (1 | code_county),
                         family = binomial(link = "probit"),
                         data = df_analysis)

summary(m1_hierarchical)

# BAYESIAN ---------------------------------------------------------------------

# PROBIT - INDIV. SITES LEVEL

#destruction
m1_probit_bayesian <- MCMCprobit(
  destroyed_dummy ~ distance_to_urumqi +
    distance_to_mineral_facility +
    in_coal +
    in_urumqi_dummy,
  data = df_analysis_bayes,
  burnin = 1000,   
  mcmc = 10000,    
  thin = 10,      
  seed = 123,      
  b0 = 0.5,         
  B0 = 0.01     
)

summary(m1_probit_bayesian)

plot(m1_probit_bayesian)

#destruction and damage
m2_probit_bayesian <- MCMCprobit(
  destroyed_damaged_dummy ~ distance_to_urumqi +
    distance_to_mineral_facility +
    in_coal +
    in_urumqi_dummy,
  data = df_analysis_bayes,
  burnin = 1000,   
  mcmc = 10000,    
  thin = 10,      
  seed = 123,      
  b0 = 0.5,         
  B0 = 0.01     
)

summary(m2_probit_bayesian)

plot(m2_probit_bayesian)

# LINEAR - COUNTY VARIANCE

# no NAs

m1_linear_bayesian <- MCMCglmm(destroyedrelative ~ attacks +
                          bingtuan_dummy, data=df_analysis)

summary(m1_linear_bayesian)


priors <- list(R = list(V = 1, nu = 0.002), 
               G = list(G1 = list(V = 1, nu = 0.002))) 

# m1_linear_bayesian <- MCMCglmm(fixed = destroyedrelative ~ attacks +
#     bingtuan_dummy +
#     revenue +
#     resource_sites_count +
#     #petro_county +
#     #share_area_covered_coal +
#     cotton_producing_county +
#     number_beds +
#     nightlight_change +
#     uyghur_relative +
#     relative_change_han_pp,
#   data = df_analysis_bayes,
#   family = "gaussian",
#   nitt = 14000,
#   burnin = 4000,
#   thin = 10)
# 
# summary(m1_bayesian)
# 
# samples <- m1_bayesian$Sol
# mcmc_samples <- as.mcmc(samples)
# plot(mcmc_samples)

# cross-sectional level of destruction
m1_linear_bayesian_DDS <- MCMCglmm(fixed = destroyedrelative ~ attacks +
                          bingtuan_dummy +
                          #rev_ex_ratio +
                          resource_sites_count +
                          petro_county +
                          #mineral_deposit +
                          #share_area_covered_coal +
                          #cotton_producing_county +
                          number_beds +
                          #nightlight_change +
                          uyghur_relative +
                          #relative_change_han_pp +
                          area +
                          urumqi_dummy,
                          #density,
                        data = df_county_complete_naomit,
                        family = "gaussian",
                        nitt = 20000,
                        burnin = 4000,
                        thin = 10)

summary(m1_linear_bayesian_DDS)


#diagnostics
posterior_samples_m1_linear_bayesian <- m1_linear_bayesian_DDS$Sol

mcmc_samples_m1_linear_bayesian <- as.mcmc(posterior_samples_m1_linear_bayesian)

plot(mcmc_samples_m1_linear_bayesian)

autocorr.plot(m1_linear_bayesian_DDS$Sol)

effectiveSize(m1_linear_bayesian_DDS$Sol)

heidel.diag(m1_linear_bayesian_DDS$Sol)

predicted <- predict(m1_linear_bayesian_DDS)
plot(predicted, df_county_complete_naomit$destroyedrelative)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m1_linear_bayes <- apply(posterior_samples_m1_linear_bayesian, 2, function(x) mean(x > 0))

prob_less_than_0_m1_linear_bayes <- apply(posterior_samples_m1_linear_bayesian, 2, function(x) mean(x < 0))

probabilities_m1_linear_bayes <- data.frame(
  Coefficient = colnames(posterior_samples_m1_linear_bayesian),
  Prob_Greater_Than_0 = prob_greater_than_0_m1_linear_bayes,
  Prob_Less_Than_0 = prob_less_than_0_m1_linear_bayes
)

print(probabilities_m1_linear_bayes)

# cross-sectional level of destruction, weighted
df_county_complete_naomit_weighted <- df_county_complete_naomit[rep(1:nrow(df_county_complete_naomit), times=round(0.5*df_county_complete_naomit$totalsites)), ]

m1_linear_bayesian_DDS_weighted <- MCMCglmm(fixed = destroyedrelative ~ attacks +
                              bingtuan_dummy +
                              #rev_ex_ratio +
                              #resource_sites_count +
                              #petro_county +
                              mineral_deposit +
                              #share_area_covered_coal +
                              #cotton_producing_county +
                              number_beds +
                              #nightlight_change +
                              uyghur_relative*density +
                              relative_change_han_pp +
                              area +
                              urumqi_dummy,
                            #density,
                            data = df_county_complete_naomit_weighted,
                            family = "gaussian",
                            nitt = 20000,
                            burnin = 4000,
                            thin = 10)

summary(m1_linear_bayesian_DDS_weighted)

posterior_samples_m1_linear_bayesian_weighted <- m1_linear_bayesian_DDS_weighted$Sol

mcmc_samples_m1_linear_bayesian_weighted <- as.mcmc(posterior_samples_m1_linear_bayesian_weighted)

plot(mcmc_samples_m1_linear_bayesian_weighted)

autocorr.plot(m1_linear_bayesian_DDS_weighted$Sol)

#probabilities of coefficients
prob_greater_than_0_m1_linear_bayes_weighted <- apply(posterior_samples_m1_linear_bayesian_weighted, 2, function(x) mean(x > 0))

prob_less_than_0_m1_linear_bayes_weighted <- apply(posterior_samples_m1_linear_bayesian_weighted, 2, function(x) mean(x < 0))

probabilities_m1_linear_bayes_weighted <- data.frame(
  Coefficient = colnames(posterior_samples_m1_linear_bayesian_weighted),
  Prob_Greater_Than_0 = prob_greater_than_0_m1_linear_bayes_weighted,
  Prob_Less_Than_0 = prob_less_than_0_m1_linear_bayes_weighted
)

print(probabilities_m1_linear_bayes_weighted)











hist(df_county_complete_naomit_weighted$area)








# HIERARCHICAL PROBIT

model <- brm(
  destroyed_dummy ~ distance_to_urumqi +
    distance_to_mineral_facility +
    in_coal +
    in_urumqi_dummy +
    (1 | code_county),  
  data = df_analysis_bayes,
  family = bernoulli(),  
  prior = c(
    prior(normal(0, 1), class = "b"),  # Prior for fixed effects
    prior(normal(0, 5), class = "sd")  # Prior for random effects standard deviations
  ),
  iter = 10000,    
  warmup = 1000,    
  thin = 10,       
  seed = 123
)

# Check the model summary
summary(model)

