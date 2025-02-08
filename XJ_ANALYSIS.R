library(tidyverse)
library(MCMCglmm)
library(MCMCpack)

set.seed(123)

##---- DATA PREPARATION --------------------------------------------------------
df_countyvars <- openxlsx::read.xlsx("xj_countyvars.xlsx") %>%
  mutate(petro_county = as.numeric(as.character(petro_county)),
         bingtuan_dummy = as.numeric(as.character(bingtuan_dummy)),
         bingtuan_change_dummy = as.numeric(as.character(bingtuan_change_dummy)),
         bingtuan_new_dummy = as.numeric(as.character(bingtuan_new_dummy)),
         cotton_producing_county = as.numeric(as.character(cotton_producing_county)),
         name_county = as.factor(name_county))
summary(df_countyvars)

df_analysis <- df_county_complete %>%
  mutate(across(28:35, ~ ifelse(is.na(.), 0, .)))

df_analysis <- na.omit(df_analysis) %>%
  mutate(urumqi_dummy = as.factor(case_when(
    as.character(code_prefecture) == "6501" ~ 1,
    TRUE ~ 0
  )))
summary(df_analysis)

##---- ANALYSIS ----------------------------------------------------------------

# ANY FORM OF SE, SIMPLE CROSS-SECTIONAL ANALYSIS

m1_se <- MCMCglmm(any_se ~ relative_change_han_pp,
                  data = df_analysis)

summary(m1_se)

m2_se <- MCMCglmm(any_se ~ attacks,
                           data = df_analysis)

summary(m2_se)

prior <- list(R = list(V = 1, nu = 0.002)) 

# full controls, dem. eng. as pop. change; any SE
m3_se_full_c <- MCMCglmm(any_se ~ relative_change_han_pp +
                                  attacks +
                                  polarisation + 
                                  resource_sites_count +
                                  share_area_covered_coal +
                                  petro_county +
                                  cotton_producing_county +
                                  nightlight_mean_2016 + 
                                  bingtuan_dummy +
                                  number_beds +
                                  detention_count +
                                  rev_ex_ratio +
                                  urumqi_dummy,
                                  data = df_analysis,
                                  family = "gaussian",
                                  prior = prior,
                                  nitt = 20000,
                                  burnin = 4000,
                                  thin = 10)

summary(m3_se_full_c)


#diagnostics
posterior_samples_m3 <- m3_se_full_c$Sol

mcmc_samples_m3 <- as.mcmc(posterior_samples_m3)

plot(mcmc_samples_m3)

autocorr.plot(m3_se_full_c$Sol)

effectiveSize(m3_se_full_c$Sol)

heidel.diag(m3_se_full_c$Sol)

predicted <- predict(m3_se_full_c)
plot(predicted, df_analysis$any_se)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m3 <- apply(posterior_samples_m3, 2, function(x) mean(x > 0))

prob_less_than_0_m3 <- apply(posterior_samples_m3, 2, function(x) mean(x < 0))

probabilities_m3 <- data.frame(
  Coefficient = colnames(posterior_samples_m3),
  Prob_Greater_Than_0 = prob_greater_than_0_m3,
  Prob_Less_Than_0 = prob_less_than_0_m3
)

print(probabilities_m3)


# full controls, dem. eng. as nightlight change; any SE
m4_se_full_c <- MCMCglmm(any_se ~ nightlight_change +
                           attacks +
                           polarisation + 
                           resource_sites_count +
                           share_area_covered_coal +
                           petro_county +
                           cotton_producing_county +
                           nightlight_mean_2016 + 
                           bingtuan_dummy +
                           number_beds +
                           detention_count +
                           rev_ex_ratio +
                           urumqi_dummy,
                         data = df_analysis,
                         family = "gaussian",
                         prior = prior,
                         nitt = 20000,
                         burnin = 4000,
                         thin = 10)

summary(m4_se_full_c)


#diagnostics
posterior_samples_m4 <- m4_se_full_c$Sol

mcmc_samples_m4 <- as.mcmc(posterior_samples_m4)

plot(mcmc_samples_m4)

autocorr.plot(m4_se_full_c$Sol)

effectiveSize(m4_se_full_c$Sol)

heidel.diag(m4_se_full_c$Sol)

predicted <- predict(m4_se_full_c)
plot(predicted, df_analysis$any_se)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m4 <- apply(posterior_samples_m4, 2, function(x) mean(x > 0))

prob_less_than_0_m4 <- apply(posterior_samples_m4, 2, function(x) mean(x < 0))

probabilities_m4 <- data.frame(
  Coefficient = colnames(posterior_samples_m4),
  Prob_Greater_Than_0 = prob_greater_than_0_m4,
  Prob_Less_Than_0 = prob_less_than_0_m4
)

print(probabilities_m4)

# ATTACHING WEIGHTS BY TOTAL NUMBER OF SITES 

df_analysis_weighted <- df_analysis[rep(1:nrow(df_analysis), df_analysis$totalsites), ]

# demographic engineering as han change
m5_se_full_cW <- MCMCglmm(any_se ~ relative_change_han_pp +
                           attacks +
                           polarisation + 
                           resource_sites_count +
                           share_area_covered_coal +
                           petro_county +
                           cotton_producing_county +
                           nightlight_mean_2016 + 
                           bingtuan_dummy +
                           number_beds +
                           detention_count +
                           rev_ex_ratio +
                           urumqi_dummy,
                         data = df_analysis_weighted,
                         family = "gaussian",
                         prior = prior,
                         nitt = 20000,
                         burnin = 4000,
                         thin = 10)

summary(m5_se_full_cW)


#diagnostics
posterior_samples_m5 <- m5_se_full_cW$Sol

mcmc_samples_m5 <- as.mcmc(posterior_samples_m5)

plot(mcmc_samples_m5)

autocorr.plot(m5_se_full_cW$Sol)

effectiveSize(m5_se_full_cW$Sol)

heidel.diag(m3_se_full_cW$Sol)

predicted <- predict(m5_se_full_cW)
plot(predicted, df_analysis_weighted$any_se)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m5 <- apply(posterior_samples_m5, 2, function(x) mean(x > 0))

prob_less_than_0_m5 <- apply(posterior_samples_m5, 2, function(x) mean(x < 0))

probabilities_m5 <- data.frame(
  Coefficient = colnames(posterior_samples_m5),
  Prob_Greater_Than_0 = prob_greater_than_0_m5,
  Prob_Less_Than_0 = prob_less_than_0_m5
)

print(probabilities_m5)


# demographic engineering as nighttime light emission change

m6_se_full_cW <- MCMCglmm(any_se ~ nightlight_change +
                           attacks +
                           polarisation + 
                           resource_sites_count +
                           share_area_covered_coal +
                           petro_county +
                           cotton_producing_county +
                           nightlight_mean_2016 + 
                           bingtuan_dummy +
                           number_beds +
                           detention_count +
                           rev_ex_ratio +
                           urumqi_dummy,
                         data = df_analysis_weighted,
                         family = "gaussian",
                         prior = prior,
                         nitt = 20000,
                         burnin = 4000,
                         thin = 10)

summary(m6_se_full_cW)


#diagnostics
posterior_samples_m6 <- m6_se_full_cW$Sol

mcmc_samples_m6 <- as.mcmc(posterior_samples_m6)

plot(mcmc_samples_m6)

autocorr.plot(m6_se_full_cW$Sol)

effectiveSize(m6_se_full_cW$Sol)

heidel.diag(m6_se_full_cW$Sol)

predicted <- predict(m6_se_full_cW)
plot(predicted, df_analysis_weighted$any_se)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m6 <- apply(posterior_samples_m6, 2, function(x) mean(x > 0))

prob_less_than_0_m6 <- apply(posterior_samples_m6, 2, function(x) mean(x < 0))

probabilities_m6 <- data.frame(
  Coefficient = colnames(posterior_samples_m6),
  Prob_Greater_Than_0 = prob_greater_than_0_m6,
  Prob_Less_Than_0 = prob_less_than_0_m6
)

print(probabilities_m6)
