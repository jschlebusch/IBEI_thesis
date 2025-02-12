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

df_analysis <- df_countyvars %>%
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
                                  #cotton_producing_county +
                                  nightlight_mean_2016 + 
                                  bingtuan_dummy +
                                  #number_beds +
                                  detention_count +
                                  #rev_ex_ratio +
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

df_analysis <- df_analysis %>%
  mutate(destroyedrelative_weighted = destroyedrelative*totalsites)

summary(df_analysis$destroyedrelative_weighted)

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


## ONLY DESTROYED SITES

m7_destruction_full_c <- MCMCglmm(destroyedrelative ~ relative_change_han_pp +
                           attacks +
                           polarisation + 
                           resource_sites_count +
                           share_area_covered_coal +
                           petro_county +
                           #cotton_producing_county +
                           nightlight_mean_2016 + 
                           bingtuan_dummy +
                           #number_beds +
                           detention_count +
                           #rev_ex_ratio +
                           urumqi_dummy,
                         data = df_analysis,
                         family = "gaussian",
                         prior = prior,
                         nitt = 20000,
                         burnin = 4000,
                         thin = 10)

summary(m7_destruction_full_c)


#diagnostics
posterior_samples_m7 <- m7_destruction_full_c$Sol

mcmc_samples_m7 <- as.mcmc(posterior_samples_m7)

plot(mcmc_samples_m7)

autocorr.plot(m7_destruction_full_c$Sol)

effectiveSize(m7_destruction_full_c$Sol)

heidel.diag(m7_destruction_full_c$Sol)

predicted <- predict(m7_destruction_full_c)
plot(predicted, df_analysis$destroyedrelative)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m7 <- apply(posterior_samples_m7, 2, function(x) mean(x > 0))

prob_less_than_0_m7 <- apply(posterior_samples_m7, 2, function(x) mean(x < 0))

probabilities_m7 <- data.frame(
  Coefficient = colnames(posterior_samples_m7),
  Prob_Greater_Than_0 = prob_greater_than_0_m7,
  Prob_Less_Than_0 = prob_less_than_0_m7
)

print(probabilities_m7)


# weighted destruction INDEX

m8_destruction_full_cW <- MCMCglmm(destroyedrelative_weighted ~ relative_change_han_pp +
                                    attacks +
                                    polarisation + 
                                    resource_sites_count +
                                    share_area_covered_coal +
                                    petro_county +
                                    #cotton_producing_county +
                                    nightlight_mean_2016 + 
                                    bingtuan_dummy +
                                    #number_beds +
                                    detention_count +
                                    #rev_ex_ratio +
                                    urumqi_dummy,
                                  data = df_analysis,
                                  family = "gaussian",
                                  prior = prior,
                                  nitt = 20000,
                                  burnin = 4000,
                                  thin = 10)

summary(m8_destruction_full_cW)


#diagnostics
posterior_samples_m8 <- m8_destruction_full_cW$Sol

mcmc_samples_m8 <- as.mcmc(posterior_samples_m8)

plot(mcmc_samples_m8)

autocorr.plot(m8_destruction_full_cW$Sol)

effectiveSize(m8_destruction_full_cW$Sol)

heidel.diag(m8_destruction_full_cW$Sol)

predicted <- predict(m8_destruction_full_cW)
plot(predicted, df_analysis$destroyedrelative_weighted)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m8 <- apply(posterior_samples_m8, 2, function(x) mean(x > 0))

prob_less_than_0_m8 <- apply(posterior_samples_m8, 2, function(x) mean(x < 0))

probabilities_m8 <- data.frame(
  Coefficient = colnames(posterior_samples_m8),
  Prob_Greater_Than_0 = prob_greater_than_0_m8,
  Prob_Less_Than_0 = prob_less_than_0_m8
)

print(probabilities_m8)


## COUNTY-LEVEL FIXED EFFECTS

df_analysis <- df_analysis %>%
  mutate(code_county = as.factor(code_county))

m9_destruction_full_cFE <- MCMCglmm(destroyedrelative ~ relative_change_han_pp +
                                    attacks +
                                    polarisation + 
                                    resource_sites_count +
                                    share_area_covered_coal +
                                    petro_county +
                                    #cotton_producing_county +
                                    nightlight_mean_2016 + 
                                    bingtuan_dummy +
                                    #number_beds +
                                    detention_count +
                                    #rev_ex_ratio +
                                    urumqi_dummy +
                                    code_county,
                                  data = df_analysis,
                                  family = "gaussian",
                                  prior = prior,
                                  nitt = 20000,
                                  burnin = 4000,
                                  thin = 10)

summary(m9_destruction_full_cFE)

#diagnostics
posterior_samples_m9 <- m9_destruction_full_cFE$Sol

mcmc_samples_m9 <- as.mcmc(posterior_samples_m9)

plot(mcmc_samples_m9)

autocorr.plot(m9_destruction_full_cFE$Sol)

effectiveSize(m9_destruction_full_cFE$Sol)

heidel.diag(m9_destruction_full_cFE$Sol)

predicted <- predict(m9_destruction_full_cFE)
plot(predicted, df_analysis$destroyedrelative)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m9 <- apply(posterior_samples_m9, 2, function(x) mean(x > 0))

prob_less_than_0_m9 <- apply(posterior_samples_m9, 2, function(x) mean(x < 0))

probabilities_m9 <- data.frame(
  Coefficient = colnames(posterior_samples_m9),
  Prob_Greater_Than_0 = prob_greater_than_0_m9,
  Prob_Less_Than_0 = prob_less_than_0_m9
)

print(probabilities_m9)


## COUNTY-LEVEL RANDOM EFFECTS

prior_normal <- list(
  B = list(mu = rep(0, 11), V = diag(1e6, 11)),  # Normal
  R = list(V = 1, nu = 0.002),  
  G = list(G1 = list(V = 1, nu = 1))  
)

m10_destruction_full_cRE <- MCMCglmm(destroyedrelative ~ relative_change_han_pp +
                                    attacks +
                                    polarisation + 
                                    resource_sites_count +
                                    share_area_covered_coal +
                                    petro_county +
                                    #cotton_producing_county +
                                    nightlight_mean_2016 + 
                                    bingtuan_dummy +
                                    #number_beds +
                                    detention_count +
                                    #rev_ex_ratio +
                                    urumqi_dummy,
                                  random = ~ code_county,
                                  data = df_analysis,
                                  family = "gaussian",
                                  prior = prior_normal,
                                  nitt = 20000,
                                  burnin = 4000,
                                  thin = 10)

summary(m10_destruction_full_cRE)


#diagnostics
posterior_samples_m10 <- m10_destruction_full_cRE$Sol

mcmc_samples_m10 <- as.mcmc(posterior_samples_m10)

plot(mcmc_samples_m10)

autocorr.plot(m10_destruction_full_cRE$Sol)

effectiveSize(m10_destruction_full_cRE$Sol)

heidel.diag(m10_destruction_full_cRE$Sol)

predicted <- predict(m10_destruction_full_cRE)
plot(predicted, df_analysis$destroyedrelative)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m10 <- apply(posterior_samples_m10, 2, function(x) mean(x > 0))

prob_less_than_0_m10 <- apply(posterior_samples_m10, 2, function(x) mean(x < 0))

probabilities_m10 <- data.frame(
  Coefficient = colnames(posterior_samples_m10),
  Prob_Greater_Than_0 = prob_greater_than_0_m10,
  Prob_Less_Than_0 = prob_less_than_0_m10
)

print(probabilities_m10)




