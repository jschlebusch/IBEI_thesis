library(tidyverse)
library(mapchina)
library(ggplot2)
library(openxlsx)

###-----------------------------------------------------------------------------
### TOPONYM CHANGES
###-----------------------------------------------------------------------------

##------------------------------------------------------------------------------
## DATA SOURCE: HUMAN RIGHTS WATCH (PERSONAL COMMUNICATION)
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
## READ DATA
##------------------------------------------------------------------------------
df_admins <- read.csv("full_admin_final_2021.csv") 
df_namechanges <- read.csv("Xinjiang village names dataset.csv")
df_china_sf <- china%>%
  filter(Name_Province=="新疆维吾尔自治区")%>%
  mutate(Status = as.factor("Status"))%>%
  rename(Code_Prefecture=Code_Perfecture)

##------------------------------------------------------------------------------
## PREPARE COUNTY-LEVEL VARIABLES
##------------------------------------------------------------------------------

df_admins_xj <- df_admins %>%
  filter(province=="新疆维吾尔自治区")

df_cities_in_counties <- df_admins_xj %>%
  group_by(county) %>%
  summarise(num_cities = n())

# by county name
df_namechanges_in_counties <- df_namechanges %>%
  group_by(county) %>%
  summarise(num_renamed = n())
  
df_cities_and_changes <- df_cities_in_counties %>%
  left_join(df_namechanges_in_counties, by = "county")

df_cities_and_changes <- df_cities_and_changes %>%
  filter(county != "") %>%
  mutate(num_renamed = ifelse(is.na(num_renamed), 0, num_renamed),
         change_relative = num_renamed / num_cities)
summary(df_cities_and_changes$change_relative)
hist(df_cities_and_changes$change_relative)

#by county code
df_namechanges <- df_namechanges %>%
  mutate(Code_County = substr(geoid, 1, 6)) %>%
  select(c(city, county, town, year, before, after, Code_County)) %>%
  rename(Name_County = county)

df_namechanges_in_counties_2 <- df_namechanges %>%
  group_by(Code_County) %>%
  summarise(num_renamed_2 = n())

# bring it all together
df_counties <- df_china_sf %>%
  select(Code_County, Name_County)

df_cities_and_changes <- df_cities_and_changes %>%
  rename(Name_County = county)

df_counties <- df_counties %>%
  left_join(df_cities_and_changes, by = "Name_County")
summary(df_counties$change_relative)

df_counties <- df_counties %>%
  left_join(df_namechanges_in_counties_2, by = "Code_County") 

df_counties <- df_counties %>%
  mutate(num_renamed_2 = ifelse(is.na(num_renamed_2), 0, num_renamed_2),
         change_relative_2 = num_renamed_2 / num_cities)

df_toponyms_clean <- st_drop_geometry(df_counties)

summary(df_counties$change_relative_2)

class(df_counties)

write.xlsx(df_toponyms_clean, "xj_toponyms.xlsx")

# some visualisations

ggplot(data = df_counties) +
  geom_sf(aes(fill = change_relative), color = NA) +  
  scale_fill_viridis_c(option = "magma", na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Relative Amount of Renamed Cities per County",
    fill = "Relative Toponym Change"
  )+
  theme(legend.position = "bottom")

ggplot(data = df_counties) +
  geom_sf(aes(fill = change_relative_2), color = NA) +  
  scale_fill_viridis_c(option = "magma", na.value = alpha("grey", 0.5)) +  
  theme_void() +
  labs(
    title = "Relative Amount of Renamed Cities per County",
    fill = "Relative Toponym Change"
  )+
  theme(legend.position = "bottom")





