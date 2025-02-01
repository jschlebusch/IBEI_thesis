library(tidyverse)
library(ggplot2)
library(haven)
library(openxlsx)

###-----------------------------------------------------------------------------
### DEMOGRAPHICS OF XINJIANG
###-----------------------------------------------------------------------------

##------------------------------------------------------------------------------
## SOURCE: XINJIANG STATISTICAL YEARBOOK
##------------------------------------------------------------------------------

## Ethnic diversity indices per county, 2016 data, based on XSYB 2017 ----------

df_pop_2016 <- read.xlsx("xj_population_16_rev.xlsx") %>%
  filter(!is.na(Code_County)) %>%
  filter(!is.na(Total)) %>%
  mutate(Other = Total - rowSums(select(., 5:10)))

summary(df_pop_2016)

# Ethnic fractionalisation per county (transformation of Herfindahl concentration index)

df_fractionalisation_2016 <- df_pop_2016 %>%
  rowwise() %>%
  mutate(
    Fractionalisation = 1 - sum((c_across(5:11) / Total)^2, na.rm = TRUE)
  ) %>%
  ungroup()

# Ethnic polarisation per county (Reynal-Querol 2002, Montalvo and Reynal-Querol 2005)

df_polarisation_2016 <- df_pop_2016 %>%
  rowwise() %>%
  mutate(
    Polarisation = 1 - sum((((0.5 - (c_across(5:11) / Total)) / 0.5)^2) * (c_across(5:11) / Total), na.rm = TRUE)
  ) %>%
  ungroup()

df_diversity_2016 <- df_fractionalisation_2016 %>%
  select(c(1:3, Fractionalisation))
df_diversity_2016$Polarisation <- df_polarisation_2016$Polarisation

write.xlsx(df_diversity_2016, "xj_ethnicdiv_2016_rev.xlsx")

## Relative change in Han population per county, 1991 - 2016, based on XSYB 1992 and 2017

df_pop_1991 <- read.xlsx("xj_population_91.xlsx")%>%
  filter(!is.na(Code_County)) %>%
  filter(!is.na(Total)) %>%
  mutate(Other = Total - rowSums(select(., 5:10)))

summary(df_pop_1991)

# change against 1991 baseline; absolute 

total_pop_2016 <- sum(df_pop_2016$Total)
total_pop_1991 <- sum(df_pop_1991$Total)

relative_increase <- (total_pop_2016 - total_pop_1991) / total_pop_1991 * 100

df_pop_merged <- merge(df_pop_1991, df_pop_2016, by = "Code_County", suffixes = c("_1991", "_2016"))

df_pop_merged$relative_change_han <- (df_pop_merged$Han_2016 - df_pop_merged$Han_1991) / df_pop_merged$Han_1991 * 100

df_han_change <- df_pop_merged %>%
  select(c(Code_County, relative_change_han))

# change in share against 1991 baseline

df_pop_1991$han_relative <- df_pop_1991$Han / df_pop_1991$Total * 100

df_pop_2016$han_relative <- df_pop_2016$Han / df_pop_2016$Total * 100

df_pop_relative_merged <- merge(df_pop_1991, df_pop_2016, by = "Code_County", suffixes = c("_1991", "_2016"))

df_pop_relative_merged$relative_change_han_pp <- df_pop_relative_merged$han_relative_2016 - df_pop_relative_merged$han_relative_1991

df_han_change_relative <- df_pop_relative_merged %>%
  select(c(Code_County, relative_change_han_pp))

df_han_change_clean <- df_han_change %>%
  left_join(df_han_change_relative, by = "Code_County")

write.xlsx(df_han_change_clean, "xj_han_change_91_16.xlsx")

## Ethnic Diversity indexes 1991

# Ethnic fractionalisation per county (transformation of Herfindahl concentration index)

df_fractionalisation_1991 <- df_pop_1991 %>%
  rowwise() %>%
  mutate(
    Fractionalisation = 1 - sum((c_across(5:11) / Total)^2, na.rm = TRUE)
  ) %>%
  ungroup()

# Ethnic polarisation per county (Reynal-Querol 2002, Montalvo and Reynal-Querol 2005)

df_polarisation_1991 <- df_pop_1991 %>%
  rowwise() %>%
  mutate(
    Polarisation = 1 - sum((((0.5 - (c_across(5:11) / Total)) / 0.5)^2) * (c_across(5:11) / Total), na.rm = TRUE)
  ) %>%
  ungroup()

df_diversity_1991 <- df_fractionalisation_1991 %>%
  select(c(1:3, Fractionalisation))
df_diversity_1991$Polarisation <- df_polarisation_1991$Polarisation

write.xlsx(df_diversity_1991, "xj_ethnicdiv_1991.xlsx")


## Bingtuan Population ---------------------------------------------------------

df_bingtuan_2016 <- read.xlsx("xj_bingtuan_16.xlsx") %>%
  filter(!is.na(Code_County))

summary(df_bingtuan_2016)

df_bingtuan_2016 <- df_bingtuan_2016 %>%
  mutate(bingtuan_dummy = case_when(
    Bingtuan.Population == 0 ~ 0,
    Bingtuan.Population > 0 ~ 1
  ))

summary(as.factor(df_bingtuan_2016$bingtuan_dummy))

df_bingtuan_2000 <- read.xlsx("xj_bingtuan_2000.xlsx") %>%
  filter(!is.na(Code_County))

df_bingtuan_merged <- merge(df_bingtuan_2000, df_bingtuan_2016, by = "Code_County", suffixes = c("_2000", "_2016"))

df_bingtuan_merged$bingtuan_change <- df_bingtuan_merged$Bingtuan.Population_2016 - df_bingtuan_merged$Bingtuan.Population_2000

df_bingtuan_merged <- df_bingtuan_merged %>%
  mutate(bingtuan_change_dummy = case_when(
    bingtuan_change > 0 ~ 1,
    bingtuan_change <= 0 ~ 0 
  )) %>%
  mutate(bingtuan_new_dummy = case_when(
    Bingtuan.Population_2000 == 0 & Bingtuan.Population_2016 > 0 ~ 1,
    TRUE ~ 0
  ))

df_bingtuan_clean <- df_bingtuan_merged %>%
  select(c(Code_County, County.Name_2016, Bingtuan.Population_2016, bingtuan_dummy, bingtuan_change, bingtuan_change_dummy, bingtuan_new_dummy))

summary(df_bingtuan_clean)  

write.xlsx(df_bingtuan_clean, "xj_bingtuan_change_16.xlsx")


