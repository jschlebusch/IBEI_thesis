library(tidyverse)
library(MCMCglmm)
library(MCMCpack)
library(modelsummary)

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

df_analysis <- df_analysis %>%
  mutate(xj_region_dummy = as.factor(ifelse(code_prefecture %in% c("6528","6529","6530","6531","6532"), "south", "north")))

summary(df_analysis$xj_region_dummy)

df_analysis <- df_analysis %>%
  mutate(se_sites = destroyedsites + num_renamed,
         exposure = totalsites + num_cities)

summary(df_analysis$se_sites)
hist(df_analysis$se_sites)

summary(df_analysis$exposure)
hist(df_analysis$exposure)
##---- ANALYSIS ----------------------------------------------------------------

##---- POISSON -----------------------------------------------------------------

#---- DESTRUCTION --------------------------------------------------------------

## DEMOGRAPHIC ENGINEERING

# demographic engineering - han change - full controls
m1_priors_poisson <- list(B = list(mu = c(rep(0, 13), 1),  
                              V = diag(c(rep(1e6, 13), 1e-6))))  
m1_poisson <- MCMCglmm(destroyedsites ~ relative_change_han_pp +
                           polarisation + 
                           resource_sites_count +
                           share_area_covered_coal +
                           petro_county +
                           nightlight_mean_2016 + 
                           bingtuan_dummy +
                           cotton_producing_county +
                           number_beds +
                           rev_ex_ratio +
                           detention_count +
                           urumqi_dummy +
                           log(totalsites),  
                         data = df_analysis,
                         family = "poisson",
                         prior = m1_priors_poisson,  
                         nitt = 25000,
                         burnin = 5000,
                         thin = 10)
summary(m1_poisson)

# diagnostics
posterior_samples_m1p <- m1_poisson$Sol

mcmc_samples_m1p <- as.mcmc(posterior_samples_m1p)

plot(mcmc_samples_m1p)

autocorr.plot(m1_poisson$Sol)

effectiveSize(m1_poisson$Sol)

heidel.diag(m1_poisson$Sol)

predicted <- predict(m1_poisson)
plot(predicted, df_analysis$destroyedsites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m1p <- apply(posterior_samples_m1p, 2, function(x) mean(x > 0))

prob_less_than_0_m1p <- apply(posterior_samples_m1p, 2, function(x) mean(x < 0))

probabilities_m1p <- data.frame(
  Coefficient = colnames(posterior_samples_m1p),
  Prob_Greater_Than_0 = prob_greater_than_0_m1p,
  Prob_Less_Than_0 = prob_less_than_0_m1p
)

print(probabilities_m1p)


# demographic engineering - han change - adjusted controls
m1b_priors_poisson <- list(B = list(mu = c(rep(0, 11), 1),  
                                   V = diag(c(rep(1e6, 11), 1e-6))))  
m1b_poisson <- MCMCglmm(destroyedsites ~ relative_change_han_pp +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         #bingtuan_dummy +
                         #cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(totalsites),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m1b_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m1b_poisson)

# diagnostics
posterior_samples_m1bp <- m1b_poisson$Sol

mcmc_samples_m1bp <- as.mcmc(posterior_samples_m1bp)

plot(mcmc_samples_m1bp)

autocorr.plot(m1b_poisson$Sol)

effectiveSize(m1b_poisson$Sol)

heidel.diag(m1b_poisson$Sol)

predicted <- predict(m1b_poisson)
plot(predicted, df_analysis$destroyedsites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m1bp <- apply(posterior_samples_m1bp, 2, function(x) mean(x > 0))

prob_less_than_0_m1bp <- apply(posterior_samples_m1bp, 2, function(x) mean(x < 0))

probabilities_m1bp <- data.frame(
  Coefficient = colnames(posterior_samples_m1bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m1bp,
  Prob_Less_Than_0 = prob_less_than_0_m1bp
)

print(probabilities_m1bp)


#demographic engineering - industrialisation - full controls
m2_priors_poisson <- list(B = list(mu = c(rep(0, 13), 1),  
                                   V = diag(c(rep(1e6, 13), 1e-6))))  
m2_poisson <- MCMCglmm(destroyedsites ~ nightlight_change +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         bingtuan_dummy +
                         cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(totalsites),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m2_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m2_poisson)

# diagnostics
posterior_samples_m2p <- m2_poisson$Sol

mcmc_samples_m2p <- as.mcmc(posterior_samples_m2p)

plot(mcmc_samples_m2p)

autocorr.plot(m2_poisson$Sol)

effectiveSize(m2_poisson$Sol)

heidel.diag(m2_poisson$Sol)

predicted <- predict(m2_poisson)
plot(predicted, df_analysis$destroyedsites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m2p <- apply(posterior_samples_m2p, 2, function(x) mean(x > 0))

prob_less_than_0_m2p <- apply(posterior_samples_m2p, 2, function(x) mean(x < 0))

probabilities_m2p <- data.frame(
  Coefficient = colnames(posterior_samples_m2p),
  Prob_Greater_Than_0 = prob_greater_than_0_m2p,
  Prob_Less_Than_0 = prob_less_than_0_m2p
)

print(probabilities_m2p)


#demographic engineering - industrialisation - adjusted controls
m2b_priors_poisson <- list(B = list(mu = c(rep(0, 11), 1),  
                                   V = diag(c(rep(1e6, 11), 1e-6))))  
m2b_poisson <- MCMCglmm(destroyedsites ~ nightlight_change +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(totalsites),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m2b_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m2b_poisson)

# diagnostics
posterior_samples_m2bp <- m2b_poisson$Sol

mcmc_samples_m2bp <- as.mcmc(posterior_samples_m2bp)

plot(mcmc_samples_m2bp)

autocorr.plot(m2b_poisson$Sol)

effectiveSize(m2b_poisson$Sol)

heidel.diag(m2b_poisson$Sol)

predicted <- predict(m2b_poisson)
plot(predicted, df_analysis$destroyedsites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m2bp <- apply(posterior_samples_m2bp, 2, function(x) mean(x > 0))

prob_less_than_0_m2bp <- apply(posterior_samples_m2bp, 2, function(x) mean(x < 0))

probabilities_m2bp <- data.frame(
  Coefficient = colnames(posterior_samples_m2bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m2bp,
  Prob_Less_Than_0 = prob_less_than_0_m2bp
)

print(probabilities_m2bp)




## HUBS OF RESISTANCE

# attacks - full controls
m3_priors_poisson <- list(B = list(mu = c(rep(0, 13), 1),  
                                   V = diag(c(rep(1e6, 13), 1e-6))))  
m3_poisson <- MCMCglmm(destroyedsites ~ attacks +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         bingtuan_dummy +
                         cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(totalsites),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m3_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m3_poisson)

# diagnostics
posterior_samples_m3p <- m3_poisson$Sol

mcmc_samples_m3p <- as.mcmc(posterior_samples_m3p)

plot(mcmc_samples_m3p)

autocorr.plot(m3_poisson$Sol)

effectiveSize(m3_poisson$Sol)

heidel.diag(m3_poisson$Sol)

predicted <- predict(m3_poisson)
plot(predicted, df_analysis$destroyedsites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m3p <- apply(posterior_samples_m3p, 2, function(x) mean(x > 0))

prob_less_than_0_m3p <- apply(posterior_samples_m3p, 2, function(x) mean(x < 0))

probabilities_m3p <- data.frame(
  Coefficient = colnames(posterior_samples_m3p),
  Prob_Greater_Than_0 = prob_greater_than_0_m3p,
  Prob_Less_Than_0 = prob_less_than_0_m3p
)

print(probabilities_m3p)


# attacks - adjusted controls
m3b_priors_poisson <- list(B = list(mu = c(rep(0, 11), 1),  
                                   V = diag(c(rep(1e6, 11), 1e-6))))  
m3b_poisson <- MCMCglmm(destroyedsites ~ attacks +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(totalsites),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m3b_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m3b_poisson)

# diagnostics
posterior_samples_m3bp <- m3b_poisson$Sol

mcmc_samples_m3bp <- as.mcmc(posterior_samples_m3bp)

plot(mcmc_samples_m3bp)

autocorr.plot(m3b_poisson$Sol)

effectiveSize(m3b_poisson$Sol)

heidel.diag(m3b_poisson$Sol)

predicted <- predict(m3b_poisson)
plot(predicted, df_analysis$destroyedsites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m3bp <- apply(posterior_samples_m3bp, 2, function(x) mean(x > 0))

prob_less_than_0_m3bp <- apply(posterior_samples_m3bp, 2, function(x) mean(x < 0))

probabilities_m3bp <- data.frame(
  Coefficient = colnames(posterior_samples_m3bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m3bp,
  Prob_Less_Than_0 = prob_less_than_0_m3bp
)

print(probabilities_m3bp)


# Region - full controls
m4_priors_poisson <- list(B = list(mu = c(rep(0, 13), 1),  
                                   V = diag(c(rep(1e6, 13), 1e-6))))  
m4_poisson <- MCMCglmm(destroyedsites ~ xj_region_dummy +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         bingtuan_dummy +
                         cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(totalsites),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m4_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m4_poisson)

# diagnostics
posterior_samples_m4p <- m4_poisson$Sol

mcmc_samples_m4p <- as.mcmc(posterior_samples_m4p)

plot(mcmc_samples_m4p)

autocorr.plot(m4_poisson$Sol)

effectiveSize(m4_poisson$Sol)

heidel.diag(m4_poisson$Sol)

predicted <- predict(m4_poisson)
plot(predicted, df_analysis$destroyedsites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m4p <- apply(posterior_samples_m4p, 2, function(x) mean(x > 0))

prob_less_than_0_m4p <- apply(posterior_samples_m4p, 2, function(x) mean(x < 0))

probabilities_m4p <- data.frame(
  Coefficient = colnames(posterior_samples_m4p),
  Prob_Greater_Than_0 = prob_greater_than_0_m4p,
  Prob_Less_Than_0 = prob_less_than_0_m4p
)

print(probabilities_m4p)


# Region - adjusted controls
m4b_priors_poisson <- list(B = list(mu = c(rep(0, 11), 1),  
                                   V = diag(c(rep(1e6, 11), 1e-6))))  
m4b_poisson <- MCMCglmm(destroyedsites ~ xj_region_dummy +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(totalsites),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m4b_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m4b_poisson)

# diagnostics
posterior_samples_m4bp <- m4b_poisson$Sol

mcmc_samples_m4bp <- as.mcmc(posterior_samples_m4bp)

plot(mcmc_samples_m4bp)

autocorr.plot(m4b_poisson$Sol)

effectiveSize(m4b_poisson$Sol)

heidel.diag(m4b_poisson$Sol)

predicted <- predict(m4b_poisson)
plot(predicted, df_analysis$destroyedsites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m4bp <- apply(posterior_samples_m4bp, 2, function(x) mean(x > 0))

prob_less_than_0_m4bp <- apply(posterior_samples_m4bp, 2, function(x) mean(x < 0))

probabilities_m4bp <- data.frame(
  Coefficient = colnames(posterior_samples_m4bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m4bp,
  Prob_Less_Than_0 = prob_less_than_0_m4bp
)

print(probabilities_m4bp)


## DEMOGRAPHIC ENGINEERING AND RESISTANCE 

# both - full controls

m5_priors_poisson <- list(B = list(mu = c(rep(0, 14), 1),  
                                   V = diag(c(rep(1e6, 14), 1e-6))))  
m5_poisson <- MCMCglmm(destroyedsites ~ relative_change_han_pp +
                         attacks +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         bingtuan_dummy +
                         cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(totalsites),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m5_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m5_poisson)

# diagnostics
posterior_samples_m5p <- m5_poisson$Sol

mcmc_samples_m5p <- as.mcmc(posterior_samples_m5p)

plot(mcmc_samples_m5p)

autocorr.plot(m5_poisson$Sol)

effectiveSize(m5_poisson$Sol)

heidel.diag(m5_poisson$Sol)

predicted <- predict(m5_poisson)
plot(predicted, df_analysis$destroyedsites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m5p <- apply(posterior_samples_m5p, 2, function(x) mean(x > 0))

prob_less_than_0_m5p <- apply(posterior_samples_m5p, 2, function(x) mean(x < 0))

probabilities_m5p <- data.frame(
  Coefficient = colnames(posterior_samples_m5p),
  Prob_Greater_Than_0 = prob_greater_than_0_m5p,
  Prob_Less_Than_0 = prob_less_than_0_m5p
)

print(probabilities_m5p)


# both - adjusted controls

m5b_priors_poisson <- list(B = list(mu = c(rep(0, 12), 1),  
                                   V = diag(c(rep(1e6, 12), 1e-6))))  
m5b_poisson <- MCMCglmm(destroyedsites ~ relative_change_han_pp +
                         attacks +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(totalsites),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m5b_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m5b_poisson)

# diagnostics
posterior_samples_m5bp <- m5b_poisson$Sol

mcmc_samples_m5bp <- as.mcmc(posterior_samples_m5bp)

plot(mcmc_samples_m5bp)

autocorr.plot(m5b_poisson$Sol)

effectiveSize(m5b_poisson$Sol)

heidel.diag(m5b_poisson$Sol)

predicted <- predict(m5b_poisson)
plot(predicted, df_analysis$destroyedsites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m5bp <- apply(posterior_samples_m5bp, 2, function(x) mean(x > 0))

prob_less_than_0_m5bp <- apply(posterior_samples_m5bp, 2, function(x) mean(x < 0))

probabilities_m5bp <- data.frame(
  Coefficient = colnames(posterior_samples_m5bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m5bp,
  Prob_Less_Than_0 = prob_less_than_0_m5bp
)

print(probabilities_m5bp)


#---- TOPONYM CHANGES ----------------------------------------------------------

## DEMOGRAPHIC ENGINEERING

# demographic engineering - han change - full controls
m6_priors_poisson <- list(B = list(mu = c(rep(0, 13), 1),  
                                   V = diag(c(rep(1e6, 13), 1e-6))))  
m6_poisson <- MCMCglmm(num_renamed ~ relative_change_han_pp +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         bingtuan_dummy +
                         cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(num_cities),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m6_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m6_poisson)

# diagnostics
posterior_samples_m6p <- m6_poisson$Sol

mcmc_samples_m6p <- as.mcmc(posterior_samples_m6p)

plot(mcmc_samples_m6p)

autocorr.plot(m6_poisson$Sol)

effectiveSize(m6_poisson$Sol)

heidel.diag(m6_poisson$Sol)

predicted <- predict(m6_poisson)
plot(predicted, df_analysis$num_renamed)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m6p <- apply(posterior_samples_m6p, 2, function(x) mean(x > 0))

prob_less_than_0_m6p <- apply(posterior_samples_m6p, 2, function(x) mean(x < 0))

probabilities_m6p <- data.frame(
  Coefficient = colnames(posterior_samples_m6p),
  Prob_Greater_Than_0 = prob_greater_than_0_m6p,
  Prob_Less_Than_0 = prob_less_than_0_m6p
)

print(probabilities_m6p)


# demographic engineering - han change - adjusted controls
m6b_priors_poisson <- list(B = list(mu = c(rep(0, 11), 1),  
                                    V = diag(c(rep(1e6, 11), 1e-6))))  
m6b_poisson <- MCMCglmm(num_renamed ~ relative_change_han_pp +
                          polarisation + 
                          resource_sites_count +
                          share_area_covered_coal +
                          petro_county +
                          nightlight_mean_2016 + 
                          #bingtuan_dummy +
                          #cotton_producing_county +
                          number_beds +
                          rev_ex_ratio +
                          detention_count +
                          urumqi_dummy +
                          log(num_cities),  
                        data = df_analysis,
                        family = "poisson",
                        prior = m6b_priors_poisson,  
                        nitt = 25000,
                        burnin = 5000,
                        thin = 10)
summary(m6b_poisson)

# diagnostics
posterior_samples_m6bp <- m6b_poisson$Sol

mcmc_samples_m6bp <- as.mcmc(posterior_samples_m6bp)

plot(mcmc_samples_m6bp)

autocorr.plot(m6b_poisson$Sol)

effectiveSize(m6b_poisson$Sol)

heidel.diag(m6b_poisson$Sol)

predicted <- predict(m6b_poisson)
plot(predicted, df_analysis$num_renamed)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m6bp <- apply(posterior_samples_m6bp, 2, function(x) mean(x > 0))

prob_less_than_0_m6bp <- apply(posterior_samples_m6bp, 2, function(x) mean(x < 0))

probabilities_m6bp <- data.frame(
  Coefficient = colnames(posterior_samples_m6bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m6bp,
  Prob_Less_Than_0 = prob_less_than_0_m6bp
)

print(probabilities_m6bp)


#demographic engineering - industrialisation - full controls
m7_priors_poisson <- list(B = list(mu = c(rep(0, 13), 1),  
                                   V = diag(c(rep(1e6, 13), 1e-6))))  
m7_poisson <- MCMCglmm(num_renamed ~ nightlight_change +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         bingtuan_dummy +
                         cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(num_cities),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m7_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m7_poisson)

# diagnostics
posterior_samples_m7p <- m7_poisson$Sol

mcmc_samples_m7p <- as.mcmc(posterior_samples_m7p)

plot(mcmc_samples_m7p)

autocorr.plot(m7_poisson$Sol)

effectiveSize(m7_poisson$Sol)

heidel.diag(m7_poisson$Sol)

predicted <- predict(m7_poisson)
plot(predicted, df_analysis$num_renamed)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m7p <- apply(posterior_samples_m7p, 2, function(x) mean(x > 0))

prob_less_than_0_m7p <- apply(posterior_samples_m7p, 2, function(x) mean(x < 0))

probabilities_m7p <- data.frame(
  Coefficient = colnames(posterior_samples_m7p),
  Prob_Greater_Than_0 = prob_greater_than_0_m7p,
  Prob_Less_Than_0 = prob_less_than_0_m7p
)

print(probabilities_m7p)


#demographic engineering - industrialisation - adjusted controls
m7b_priors_poisson <- list(B = list(mu = c(rep(0, 11), 1),  
                                    V = diag(c(rep(1e6, 11), 1e-6))))  
m7b_poisson <- MCMCglmm(num_renamed ~ nightlight_change +
                          polarisation + 
                          resource_sites_count +
                          share_area_covered_coal +
                          petro_county +
                          nightlight_mean_2016 + 
                          number_beds +
                          rev_ex_ratio +
                          detention_count +
                          urumqi_dummy +
                          log(num_cities),  
                        data = df_analysis,
                        family = "poisson",
                        prior = m7b_priors_poisson,  
                        nitt = 25000,
                        burnin = 5000,
                        thin = 10)
summary(m7b_poisson)

# diagnostics
posterior_samples_m7bp <- m7b_poisson$Sol

mcmc_samples_m7bp <- as.mcmc(posterior_samples_m7bp)

plot(mcmc_samples_m7bp)

autocorr.plot(m7b_poisson$Sol)

effectiveSize(m7b_poisson$Sol)

heidel.diag(m7b_poisson$Sol)

predicted <- predict(m7b_poisson)
plot(predicted, df_analysis$num_renamed)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m7bp <- apply(posterior_samples_m7bp, 2, function(x) mean(x > 0))

prob_less_than_0_m7bp <- apply(posterior_samples_m7bp, 2, function(x) mean(x < 0))

probabilities_m7bp <- data.frame(
  Coefficient = colnames(posterior_samples_m7bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m7bp,
  Prob_Less_Than_0 = prob_less_than_0_m7bp
)

print(probabilities_m7bp)




## HUBS OF RESISTANCE

# attacks - full controls
m8_priors_poisson <- list(B = list(mu = c(rep(0, 13), 1),  
                                   V = diag(c(rep(1e6, 13), 1e-6))))  
m8_poisson <- MCMCglmm(num_renamed ~ attacks +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         bingtuan_dummy +
                         cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(num_cities),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m8_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m8_poisson)

# diagnostics
posterior_samples_m8p <- m8_poisson$Sol

mcmc_samples_m8p <- as.mcmc(posterior_samples_m8p)

plot(mcmc_samples_m8p)

autocorr.plot(m8_poisson$Sol)

effectiveSize(m8_poisson$Sol)

heidel.diag(m8_poisson$Sol)

predicted <- predict(m8_poisson)
plot(predicted, df_analysis$num_renamed)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m8p <- apply(posterior_samples_m8p, 2, function(x) mean(x > 0))

prob_less_than_0_m8p <- apply(posterior_samples_m8p, 2, function(x) mean(x < 0))

probabilities_m8p <- data.frame(
  Coefficient = colnames(posterior_samples_m8p),
  Prob_Greater_Than_0 = prob_greater_than_0_m8p,
  Prob_Less_Than_0 = prob_less_than_0_m8p
)

print(probabilities_m8p)


# attacks - adjusted controls
m8b_priors_poisson <- list(B = list(mu = c(rep(0, 11), 1),  
                                    V = diag(c(rep(1e6, 11), 1e-6))))  
m8b_poisson <- MCMCglmm(num_renamed ~ attacks +
                          polarisation + 
                          resource_sites_count +
                          share_area_covered_coal +
                          petro_county +
                          nightlight_mean_2016 + 
                          number_beds +
                          rev_ex_ratio +
                          detention_count +
                          urumqi_dummy +
                          log(num_cities),  
                        data = df_analysis,
                        family = "poisson",
                        prior = m8b_priors_poisson,  
                        nitt = 25000,
                        burnin = 5000,
                        thin = 10)
summary(m8b_poisson)

# diagnostics
posterior_samples_m8bp <- m8b_poisson$Sol

mcmc_samples_m8bp <- as.mcmc(posterior_samples_m8bp)

plot(mcmc_samples_m8bp)

autocorr.plot(m8b_poisson$Sol)

effectiveSize(m8b_poisson$Sol)

heidel.diag(m8b_poisson$Sol)

predicted <- predict(m8b_poisson)
plot(predicted, df_analysis$num_renamed)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m8bp <- apply(posterior_samples_m8bp, 2, function(x) mean(x > 0))

prob_less_than_0_m8bp <- apply(posterior_samples_m8bp, 2, function(x) mean(x < 0))

probabilities_m8bp <- data.frame(
  Coefficient = colnames(posterior_samples_m8bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m8bp,
  Prob_Less_Than_0 = prob_less_than_0_m8bp
)

print(probabilities_m8bp)


# Region - full controls
m9_priors_poisson <- list(B = list(mu = c(rep(0, 13), 1),  
                                   V = diag(c(rep(1e6, 13), 1e-6))))  
m9_poisson <- MCMCglmm(num_renamed ~ xj_region_dummy +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         bingtuan_dummy +
                         cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(num_cities),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m9_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m9_poisson)

# diagnostics
posterior_samples_m9p <- m9_poisson$Sol

mcmc_samples_m9p <- as.mcmc(posterior_samples_m9p)

plot(mcmc_samples_m9p)

autocorr.plot(m9_poisson$Sol)

effectiveSize(m9_poisson$Sol)

heidel.diag(m9_poisson$Sol)

predicted <- predict(m9_poisson)
plot(predicted, df_analysis$num_renamed)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m9p <- apply(posterior_samples_m9p, 2, function(x) mean(x > 0))

prob_less_than_0_m9p <- apply(posterior_samples_m9p, 2, function(x) mean(x < 0))

probabilities_m9p <- data.frame(
  Coefficient = colnames(posterior_samples_m9p),
  Prob_Greater_Than_0 = prob_greater_than_0_m9p,
  Prob_Less_Than_0 = prob_less_than_0_m9p
)

print(probabilities_m9p)


# Region - adjusted controls
m9b_priors_poisson <- list(B = list(mu = c(rep(0, 11), 1),  
                                    V = diag(c(rep(1e6, 11), 1e-6))))  
m9b_poisson <- MCMCglmm(num_renamed ~ xj_region_dummy +
                          polarisation + 
                          resource_sites_count +
                          share_area_covered_coal +
                          petro_county +
                          nightlight_mean_2016 + 
                          number_beds +
                          rev_ex_ratio +
                          detention_count +
                          urumqi_dummy +
                          log(num_cities),  
                        data = df_analysis,
                        family = "poisson",
                        prior = m9b_priors_poisson,  
                        nitt = 25000,
                        burnin = 5000,
                        thin = 10)
summary(m9b_poisson)

# diagnostics
posterior_samples_m9bp <- m9b_poisson$Sol

mcmc_samples_m9bp <- as.mcmc(posterior_samples_m9bp)

plot(mcmc_samples_m9bp)

autocorr.plot(m9b_poisson$Sol)

effectiveSize(m9b_poisson$Sol)

heidel.diag(m9b_poisson$Sol)

predicted <- predict(m9b_poisson)
plot(predicted, df_analysis$num_renamed)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m9bp <- apply(posterior_samples_m9bp, 2, function(x) mean(x > 0))

prob_less_than_0_m9bp <- apply(posterior_samples_m9bp, 2, function(x) mean(x < 0))

probabilities_m9bp <- data.frame(
  Coefficient = colnames(posterior_samples_m9bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m9bp,
  Prob_Less_Than_0 = prob_less_than_0_m9bp
)

print(probabilities_m9bp)


## DEMOGRAPHIC ENGINEERING AND RESISTANCE 

# both - full controls

m10_priors_poisson <- list(B = list(mu = c(rep(0, 14), 1),  
                                   V = diag(c(rep(1e6, 14), 1e-6))))  
m10_poisson <- MCMCglmm(num_renamed ~ relative_change_han_pp +
                         attacks +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         bingtuan_dummy +
                         cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(num_cities),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m10_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m10_poisson)

# diagnostics
posterior_samples_m10p <- m10_poisson$Sol

mcmc_samples_m10p <- as.mcmc(posterior_samples_m10p)

plot(mcmc_samples_m10p)

autocorr.plot(m10_poisson$Sol)

effectiveSize(m10_poisson$Sol)

heidel.diag(m10_poisson$Sol)

predicted <- predict(m10_poisson)
plot(predicted, df_analysis$num_renamed)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m10p <- apply(posterior_samples_m10p, 2, function(x) mean(x > 0))

prob_less_than_0_m10p <- apply(posterior_samples_m10p, 2, function(x) mean(x < 0))

probabilities_m10p <- data.frame(
  Coefficient = colnames(posterior_samples_m10p),
  Prob_Greater_Than_0 = prob_greater_than_0_m10p,
  Prob_Less_Than_0 = prob_less_than_0_m10p
)

print(probabilities_m10p)


# both - adjusted controls

m10b_priors_poisson <- list(B = list(mu = c(rep(0, 12), 1),  
                                    V = diag(c(rep(1e6, 12), 1e-6))))  
m10b_poisson <- MCMCglmm(num_renamed ~ relative_change_han_pp +
                          attacks +
                          polarisation + 
                          resource_sites_count +
                          share_area_covered_coal +
                          petro_county +
                          nightlight_mean_2016 + 
                          number_beds +
                          rev_ex_ratio +
                          detention_count +
                          urumqi_dummy +
                          log(num_cities),  
                        data = df_analysis,
                        family = "poisson",
                        prior = m10b_priors_poisson,  
                        nitt = 25000,
                        burnin = 5000,
                        thin = 10)
summary(m10b_poisson)

# diagnostics
posterior_samples_m10bp <- m10b_poisson$Sol

mcmc_samples_m10bp <- as.mcmc(posterior_samples_m10bp)

plot(mcmc_samples_m10bp)

autocorr.plot(m10b_poisson$Sol)

effectiveSize(m10b_poisson$Sol)

heidel.diag(m10b_poisson$Sol)

predicted <- predict(m10b_poisson)
plot(predicted, df_analysis$num_renamed)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m10bp <- apply(posterior_samples_m10bp, 2, function(x) mean(x > 0))

prob_less_than_0_m10bp <- apply(posterior_samples_m10bp, 2, function(x) mean(x < 0))

probabilities_m10bp <- data.frame(
  Coefficient = colnames(posterior_samples_m10bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m10bp,
  Prob_Less_Than_0 = prob_less_than_0_m10bp
)

print(probabilities_m10bp)


#---- ANY SE ----------------------------------------------------------

## DEMOGRAPHIC ENGINEERING

# demographic engineering - han change - full controls
m11_priors_poisson <- list(B = list(mu = c(rep(0, 13), 1),  
                                   V = diag(c(rep(1e6, 13), 1e-6))))  
m11_poisson <- MCMCglmm(se_sites ~ relative_change_han_pp +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         bingtuan_dummy +
                         cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(exposure),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m11_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m11_poisson)

# diagnostics
posterior_samples_m11p <- m11_poisson$Sol

mcmc_samples_m11p <- as.mcmc(posterior_samples_m11p)

plot(mcmc_samples_m11p)

autocorr.plot(m11_poisson$Sol)

effectiveSize(m11_poisson$Sol)

heidel.diag(m11_poisson$Sol)

predicted <- predict(m11_poisson)
plot(predicted, df_analysis$se_sites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m11p <- apply(posterior_samples_m11p, 2, function(x) mean(x > 0))

prob_less_than_0_m11p <- apply(posterior_samples_m11p, 2, function(x) mean(x < 0))

probabilities_m11p <- data.frame(
  Coefficient = colnames(posterior_samples_m11p),
  Prob_Greater_Than_0 = prob_greater_than_0_m11p,
  Prob_Less_Than_0 = prob_less_than_0_m11p
)

print(probabilities_m11p)


# demographic engineering - han change - adjusted controls
m11b_priors_poisson <- list(B = list(mu = c(rep(0, 11), 1),  
                                    V = diag(c(rep(1e6, 11), 1e-6))))  
m11b_poisson <- MCMCglmm(se_sites ~ relative_change_han_pp +
                          polarisation + 
                          resource_sites_count +
                          share_area_covered_coal +
                          petro_county +
                          nightlight_mean_2016 + 
                          #bingtuan_dummy +
                          #cotton_producing_county +
                          number_beds +
                          rev_ex_ratio +
                          detention_count +
                          urumqi_dummy +
                          log(exposure),  
                        data = df_analysis,
                        family = "poisson",
                        prior = m11b_priors_poisson,  
                        nitt = 25000,
                        burnin = 5000,
                        thin = 10)
summary(m11b_poisson)

# diagnostics
posterior_samples_m11bp <- m11b_poisson$Sol

mcmc_samples_m11bp <- as.mcmc(posterior_samples_m11bp)

plot(mcmc_samples_m11bp)

autocorr.plot(m11b_poisson$Sol)

effectiveSize(m11b_poisson$Sol)

heidel.diag(m11b_poisson$Sol)

predicted <- predict(m11b_poisson)
plot(predicted, df_analysis$se_sites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m11bp <- apply(posterior_samples_m11bp, 2, function(x) mean(x > 0))

prob_less_than_0_m11bp <- apply(posterior_samples_m11bp, 2, function(x) mean(x < 0))

probabilities_m11bp <- data.frame(
  Coefficient = colnames(posterior_samples_m11bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m11bp,
  Prob_Less_Than_0 = prob_less_than_0_m11bp
)

print(probabilities_m11bp)


#demographic engineering - industrialisation - full controls
m12_priors_poisson <- list(B = list(mu = c(rep(0, 13), 1),  
                                   V = diag(c(rep(1e6, 13), 1e-6))))  
m12_poisson <- MCMCglmm(se_sites ~ nightlight_change +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         bingtuan_dummy +
                         cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(exposure),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m12_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m12_poisson)

# diagnostics
posterior_samples_m12p <- m12_poisson$Sol

mcmc_samples_m12p <- as.mcmc(posterior_samples_m12p)

plot(mcmc_samples_m12p)

autocorr.plot(m12_poisson$Sol)

effectiveSize(m12_poisson$Sol)

heidel.diag(m12_poisson$Sol)

predicted <- predict(m12_poisson)
plot(predicted, df_analysis$se_sites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m12p <- apply(posterior_samples_m12p, 2, function(x) mean(x > 0))

prob_less_than_0_m12p <- apply(posterior_samples_m12p, 2, function(x) mean(x < 0))

probabilities_m12p <- data.frame(
  Coefficient = colnames(posterior_samples_m12p),
  Prob_Greater_Than_0 = prob_greater_than_0_m12p,
  Prob_Less_Than_0 = prob_less_than_0_m12p
)

print(probabilities_m12p)


#demographic engineering - industrialisation - adjusted controls
m12b_priors_poisson <- list(B = list(mu = c(rep(0, 11), 1),  
                                    V = diag(c(rep(1e6, 11), 1e-6))))  
m12b_poisson <- MCMCglmm(se_sites ~ nightlight_change +
                          polarisation + 
                          resource_sites_count +
                          share_area_covered_coal +
                          petro_county +
                          nightlight_mean_2016 + 
                          number_beds +
                          rev_ex_ratio +
                          detention_count +
                          urumqi_dummy +
                          log(exposure),  
                        data = df_analysis,
                        family = "poisson",
                        prior = m12b_priors_poisson,  
                        nitt = 25000,
                        burnin = 5000,
                        thin = 10)
summary(m12b_poisson)

# diagnostics
posterior_samples_m12bp <- m12b_poisson$Sol

mcmc_samples_m12bp <- as.mcmc(posterior_samples_m12bp)

plot(mcmc_samples_m12bp)

autocorr.plot(m12b_poisson$Sol)

effectiveSize(m12b_poisson$Sol)

heidel.diag(m12b_poisson$Sol)

predicted <- predict(m12b_poisson)
plot(predicted, df_analysis$se_sites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m12bp <- apply(posterior_samples_m12bp, 2, function(x) mean(x > 0))

prob_less_than_0_m12bp <- apply(posterior_samples_m12bp, 2, function(x) mean(x < 0))

probabilities_m12bp <- data.frame(
  Coefficient = colnames(posterior_samples_m12bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m12bp,
  Prob_Less_Than_0 = prob_less_than_0_m12bp
)

print(probabilities_m12bp)




## HUBS OF RESISTANCE

# attacks - full controls
m13_priors_poisson <- list(B = list(mu = c(rep(0, 13), 1),  
                                   V = diag(c(rep(1e6, 13), 1e-6))))  
m13_poisson <- MCMCglmm(se_sites ~ attacks +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         bingtuan_dummy +
                         cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(exposure),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m13_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m13_poisson)

# diagnostics
posterior_samples_m13p <- m13_poisson$Sol

mcmc_samples_m13p <- as.mcmc(posterior_samples_m13p)

plot(mcmc_samples_m13p)

autocorr.plot(m13_poisson$Sol)

effectiveSize(m13_poisson$Sol)

heidel.diag(m13_poisson$Sol)

predicted <- predict(m13_poisson)
plot(predicted, df_analysis$se_sites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m13p <- apply(posterior_samples_m13p, 2, function(x) mean(x > 0))

prob_less_than_0_m13p <- apply(posterior_samples_m13p, 2, function(x) mean(x < 0))

probabilities_m13p <- data.frame(
  Coefficient = colnames(posterior_samples_m13p),
  Prob_Greater_Than_0 = prob_greater_than_0_m13p,
  Prob_Less_Than_0 = prob_less_than_0_m13p
)

print(probabilities_m13p)


# attacks - adjusted controls
m13b_priors_poisson <- list(B = list(mu = c(rep(0, 11), 1),  
                                    V = diag(c(rep(1e6, 11), 1e-6))))  
m13b_poisson <- MCMCglmm(se_sites ~ attacks +
                          polarisation + 
                          resource_sites_count +
                          share_area_covered_coal +
                          petro_county +
                          nightlight_mean_2016 + 
                          number_beds +
                          rev_ex_ratio +
                          detention_count +
                          urumqi_dummy +
                          log(exposure),  
                        data = df_analysis,
                        family = "poisson",
                        prior = m13b_priors_poisson,  
                        nitt = 25000,
                        burnin = 5000,
                        thin = 10)
summary(m13b_poisson)

# diagnostics
posterior_samples_m13bp <- m13b_poisson$Sol

mcmc_samples_m13bp <- as.mcmc(posterior_samples_m13bp)

plot(mcmc_samples_m13bp)

autocorr.plot(m13b_poisson$Sol)

effectiveSize(m13b_poisson$Sol)

heidel.diag(m13b_poisson$Sol)

predicted <- predict(m13b_poisson)
plot(predicted, df_analysis$se_sites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m13bp <- apply(posterior_samples_m13bp, 2, function(x) mean(x > 0))

prob_less_than_0_m13bp <- apply(posterior_samples_m13bp, 2, function(x) mean(x < 0))

probabilities_m13bp <- data.frame(
  Coefficient = colnames(posterior_samples_m13bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m13bp,
  Prob_Less_Than_0 = prob_less_than_0_m13bp
)

print(probabilities_m13bp)


# Region - full controls
m14_priors_poisson <- list(B = list(mu = c(rep(0, 13), 1),  
                                   V = diag(c(rep(1e6, 13), 1e-6))))  
m14_poisson <- MCMCglmm(se_sites ~ xj_region_dummy +
                         polarisation + 
                         resource_sites_count +
                         share_area_covered_coal +
                         petro_county +
                         nightlight_mean_2016 + 
                         bingtuan_dummy +
                         cotton_producing_county +
                         number_beds +
                         rev_ex_ratio +
                         detention_count +
                         urumqi_dummy +
                         log(exposure),  
                       data = df_analysis,
                       family = "poisson",
                       prior = m14_priors_poisson,  
                       nitt = 25000,
                       burnin = 5000,
                       thin = 10)
summary(m14_poisson)

# diagnostics
posterior_samples_m14p <- m14_poisson$Sol

mcmc_samples_m14p <- as.mcmc(posterior_samples_m14p)

plot(mcmc_samples_m14p)

autocorr.plot(m14_poisson$Sol)

effectiveSize(m14_poisson$Sol)

heidel.diag(m14_poisson$Sol)

predicted <- predict(m14_poisson)
plot(predicted, df_analysis$se_sites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m14p <- apply(posterior_samples_m14p, 2, function(x) mean(x > 0))

prob_less_than_0_m14p <- apply(posterior_samples_m14p, 2, function(x) mean(x < 0))

probabilities_m14p <- data.frame(
  Coefficient = colnames(posterior_samples_m14p),
  Prob_Greater_Than_0 = prob_greater_than_0_m14p,
  Prob_Less_Than_0 = prob_less_than_0_m14p
)

print(probabilities_m14p)


# Region - adjusted controls
m14b_priors_poisson <- list(B = list(mu = c(rep(0, 11), 1),  
                                    V = diag(c(rep(1e6, 11), 1e-6))))  
m14b_poisson <- MCMCglmm(se_sites ~ xj_region_dummy +
                          polarisation + 
                          resource_sites_count +
                          share_area_covered_coal +
                          petro_county +
                          nightlight_mean_2016 + 
                          number_beds +
                          rev_ex_ratio +
                          detention_count +
                          urumqi_dummy +
                          log(exposure),  
                        data = df_analysis,
                        family = "poisson",
                        prior = m14b_priors_poisson,  
                        nitt = 25000,
                        burnin = 5000,
                        thin = 10)
summary(m14b_poisson)

# diagnostics
posterior_samples_m14bp <- m14b_poisson$Sol

mcmc_samples_m14bp <- as.mcmc(posterior_samples_m14bp)

plot(mcmc_samples_m14bp)

autocorr.plot(m14b_poisson$Sol)

effectiveSize(m14b_poisson$Sol)

heidel.diag(m14b_poisson$Sol)

predicted <- predict(m14b_poisson)
plot(predicted, df_analysis$se_sites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m14bp <- apply(posterior_samples_m14bp, 2, function(x) mean(x > 0))

prob_less_than_0_m14bp <- apply(posterior_samples_m14bp, 2, function(x) mean(x < 0))

probabilities_m14bp <- data.frame(
  Coefficient = colnames(posterior_samples_m14bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m14bp,
  Prob_Less_Than_0 = prob_less_than_0_m14bp
)

print(probabilities_m14bp)


## DEMOGRAPHIC ENGINEERING AND RESISTANCE 

# both - full controls

m15_priors_poisson <- list(B = list(mu = c(rep(0, 14), 1),  
                                    V = diag(c(rep(1e6, 14), 1e-6))))  
m15_poisson <- MCMCglmm(se_sites ~ relative_change_han_pp +
                          attacks +
                          polarisation + 
                          resource_sites_count +
                          share_area_covered_coal +
                          petro_county +
                          nightlight_mean_2016 + 
                          bingtuan_dummy +
                          cotton_producing_county +
                          number_beds +
                          rev_ex_ratio +
                          detention_count +
                          urumqi_dummy +
                          log(exposure),  
                        data = df_analysis,
                        family = "poisson",
                        prior = m15_priors_poisson,  
                        nitt = 25000,
                        burnin = 5000,
                        thin = 10)
summary(m15_poisson)

# diagnostics
posterior_samples_m15p <- m15_poisson$Sol

mcmc_samples_m15p <- as.mcmc(posterior_samples_m15p)

plot(mcmc_samples_m15p)

autocorr.plot(m15_poisson$Sol)

effectiveSize(m15_poisson$Sol)

heidel.diag(m15_poisson$Sol)

predicted <- predict(m15_poisson)
plot(predicted, df_analysis$se_sites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m15p <- apply(posterior_samples_m15p, 2, function(x) mean(x > 0))

prob_less_than_0_m15p <- apply(posterior_samples_m15p, 2, function(x) mean(x < 0))

probabilities_m15p <- data.frame(
  Coefficient = colnames(posterior_samples_m15p),
  Prob_Greater_Than_0 = prob_greater_than_0_m15p,
  Prob_Less_Than_0 = prob_less_than_0_m15p
)

print(probabilities_m15p)


# both - adjusted controls

m15b_priors_poisson <- list(B = list(mu = c(rep(0, 12), 1),  
                                     V = diag(c(rep(1e6, 12), 1e-6))))  
m15b_poisson <- MCMCglmm(se_sites ~ relative_change_han_pp +
                           attacks +
                           polarisation + 
                           resource_sites_count +
                           share_area_covered_coal +
                           petro_county +
                           nightlight_mean_2016 + 
                           number_beds +
                           rev_ex_ratio +
                           detention_count +
                           urumqi_dummy +
                           log(exposure),  
                         data = df_analysis,
                         family = "poisson",
                         prior = m15b_priors_poisson,  
                         nitt = 25000,
                         burnin = 5000,
                         thin = 10)
summary(m15b_poisson)

# diagnostics
posterior_samples_m15bp <- m15b_poisson$Sol

mcmc_samples_m15bp <- as.mcmc(posterior_samples_m15bp)

plot(mcmc_samples_m15bp)

autocorr.plot(m15b_poisson$Sol)

effectiveSize(m15b_poisson$Sol)

heidel.diag(m15b_poisson$Sol)

predicted <- predict(m15b_poisson)
plot(predicted, df_analysis$se_sites)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m15bp <- apply(posterior_samples_m15bp, 2, function(x) mean(x > 0))

prob_less_than_0_m15bp <- apply(posterior_samples_m15bp, 2, function(x) mean(x < 0))

probabilities_m15bp <- data.frame(
  Coefficient = colnames(posterior_samples_m15bp),
  Prob_Greater_Than_0 = prob_greater_than_0_m15bp,
  Prob_Less_Than_0 = prob_less_than_0_m15bp
)

print(probabilities_m15bp)


















































































##---- ROBUSTNESS / ALTERNATIVE MODELS -----------------------------------------

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
  mutate(destroyedrelative_weighted = destroyedrelative*totalsites) #### das ist quatsch!

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


# OFFSET 
m11_se_full_co <- MCMCglmm(any_se ~ relative_change_han_pp +
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
                             offset(log(totalsites)),
                           data = df_analysis,
                           family = "gaussian",
                           prior = prior,
                           nitt = 20000,
                           burnin = 4000,
                           thin = 10)

summary(m11_se_full_co)

#diagnostics
posterior_samples_m11 <- m11_se_full_co$Sol

mcmc_samples_m11 <- as.mcmc(posterior_samples_m11)

plot(mcmc_samples_m11)

autocorr.plot(m11_se_full_co$Sol)

effectiveSize(m11_se_full_co$Sol)

heidel.diag(m11_se_full_co$Sol)

predicted <- predict(m11_se_full_co)
plot(predicted, df_analysis$destroyedrelative)

#probability of coefficients being greater/ smaller than 1
prob_greater_than_0_m11 <- apply(posterior_samples_m11, 2, function(x) mean(x > 0))

prob_less_than_0_m11 <- apply(posterior_samples_m11, 2, function(x) mean(x < 0))

probabilities_m11 <- data.frame(
  Coefficient = colnames(posterior_samples_m11),
  Prob_Greater_Than_0 = prob_greater_than_0_m11,
  Prob_Less_Than_0 = prob_less_than_0_m11
)

print(probabilities_m11)


