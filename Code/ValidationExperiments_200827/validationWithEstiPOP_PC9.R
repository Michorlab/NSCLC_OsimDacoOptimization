#####
# Author: Kamrine Poels
# Description: use estiPop to estimate cell count at end of experiment
#####

# setwd("Code/ValidationExperiments/")

# Load essential packages and model fits for growth of PC9 cells
library(tidyverse)
# library(pracma)
library(scales)
library(estipop)
load("../../newFitsAndRates200229.RData")

### Create functions for simulations
# d_nras = .005
# d_t790m = .005
# dpc9 = .005
# dc797s_std = .005

# Functions to return estimated growth rates given drug concentrations
# PC9 cell line (sensitive to both)
birth_l858r = Vectorize(function(daco, osi){
  # Create covariate vector according to model
  xvec = c(1, log(daco + 1)^(1/3), log(osi + 1)^(1/1.5), log(daco + 1)^(1/3)*log(osi + 1)^(1/1.5))
  # Predict growth rate
  fit = sum(pc9fit$coefficients*xvec)
  # Transform growth rate
  ret = (ymax*exp(fit)+ymin)/(1+exp(fit))
  if (ret+dpc9 > 0){
    return(ret+dpc9)
  }else{
    return(0)
  }
})

# PC9-DRH (T790m), sensitive to osimertinib
birth_t790m = Vectorize(function(daco, osi){
  xvec = c(1, log(daco+1)^(1/3), log(osi+1)^2)
  fit = sum(pc9DRHfit$coefficients*xvec)
  # Transform
  ret = (ymaxT790M*exp(fit)+yminT790M)/(1+exp(fit))
  return(ret+d_t790m)
})

# PC9 NRAS, resistant to both (ignorel)
birth_NRAS = Vectorize(function(daco, osi){
  xvec = c(1, log(osi+1), log(daco+1))
  # Fit is linear
  fit = sum(pc9Rfit$coefficients*xvec)
  ret = (ymaxNRAS*exp(fit)+yminNRAS)/(1+exp(fit))
  return(ret+d_nras)
})

birth_c797s = Vectorize(function(daco, osi){
  xvec = c(1, log(daco+1), log(osi+1))
  # Fit is linear
  fit = sum(pc9c797s_std$coefficients*xvec)
  ret = (ymax_c797s_std*exp(fit)+ymin_c797s_std)/(1+exp(fit))
  return(ret+dc797s_std)
})

####################################################################################################
# Run simulations
####################################################################################################

# Create vector with drug concentrations used in the validation experiments
drugCombo = tibble(daco = c(0, 4.40, 0, 1.47, 2.94, 1.47, 4.40, 2.94),
                   osi = c(0, 0, 10.38, 5.19, 5.19, 10.38, 10.38, 20.75))

# Set parameters
seed = 9648301 # why a seed? What is random here?
nsims = 50
mu = 10^-7
n0 = 10^6 # initial 
n1 = 0
n2 = 0

### 8 combos analyzed,
## No daco no osi
t = 9
bPC9 = birth_l858r(daco = drugCombo$daco[1], osi = drugCombo$osi[1])
bT790M = birth_t790m(daco = drugCombo$daco[1], osi = drugCombo$osi[1])
bC797S = birth_c797s(daco = drugCombo$daco[1], osi = drugCombo$osi[1])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu,
                                              birth2 = 24*bT790M, death2 = 24*d_t790m, init = c(n0,n1),
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo1 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median),
                     q25 = apply(sumPreds, 1, function(x){sort(x)[round(.25*length(x))]}),
                     q75 = apply(sumPreds, 1, function(x){sort(x)[round(.75*length(x))]}),
                     condition = "Vehicle(DMSO)")

# 4.4 nM daco no osi
t = 37
bPC9 = birth_l858r(daco = drugCombo$daco[2], osi = drugCombo$osi[2])
bT790M = birth_t790m(daco = drugCombo$daco[2], osi = drugCombo$osi[2])
bC797S = birth_c797s(daco = drugCombo$daco[2], osi = drugCombo$osi[2])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu,
                                              birth2 = 24*bT790M, death2 = 24*d_t790m, init = c(n0,n1),
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo2 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median),
                     q25 = apply(sumPreds, 1, function(x){sort(x)[round(.25*length(x))]}),
                     q75 = apply(sumPreds, 1, function(x){sort(x)[round(.75*length(x))]}),
                     condition = "Daco 4.40 nM")

# No daco 10.38 osi
t = 28
bPC9 = birth_l858r(daco = drugCombo$daco[3], osi = drugCombo$osi[3])
bT790M = birth_t790m(daco = drugCombo$daco[3], osi = drugCombo$osi[3])
bC797S = birth_c797s(daco = drugCombo$daco[3], osi = drugCombo$osi[3])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu,
                                              birth2 = 24*bT790M, death2 = 24*d_t790m, init = c(n0,n1),
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo3 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median),
                     q25 = apply(sumPreds, 1, function(x){sort(x)[round(.25*length(x))]}),
                     q75 = apply(sumPreds, 1, function(x){sort(x)[round(.75*length(x))]}),
                     condition = "Osimertinib 10.38 nM")

# 1.47 daco 5.19 osi
t = 95
bPC9 = birth_l858r(daco = drugCombo$daco[4], osi = drugCombo$osi[4])
bT790M = birth_t790m(daco = drugCombo$daco[4], osi = drugCombo$osi[4])
bC797S = birth_c797s(daco = drugCombo$daco[4], osi = drugCombo$osi[4])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu,
                                              birth2 = 24*bT790M, death2 = 24*d_t790m, init = c(n0,n1),
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo4 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median),
                     q25 = apply(sumPreds, 1, function(x){sort(x)[round(.25*length(x))]}),
                     q75 = apply(sumPreds, 1, function(x){sort(x)[round(.75*length(x))]}),
                     condition = "1.47 nM Daco+5.19 nM Osim")

# 2.94 daco 5.19 osi
t = 82
bPC9 = birth_l858r(daco = drugCombo$daco[5], osi = drugCombo$osi[5])
bT790M = birth_t790m(daco = drugCombo$daco[5], osi = drugCombo$osi[5])
bC797S = birth_c797s(daco = drugCombo$daco[5], osi = drugCombo$osi[5])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu,
                                              birth2 = 24*bT790M, death2 = 24*d_t790m, init = c(n0,n1),
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo5 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median),
                     q25 = apply(sumPreds, 1, function(x){sort(x)[round(.25*length(x))]}),
                     q75 = apply(sumPreds, 1, function(x){sort(x)[round(.75*length(x))]}),
                     condition = "2.94 nM Daco+5.19 nM Osim")

# 1.47 daco 10.38 osi
t = 99
bPC9 = birth_l858r(daco = drugCombo$daco[6], osi = drugCombo$osi[6])
bT790M = birth_t790m(daco = drugCombo$daco[6], osi = drugCombo$osi[6])
bC797S = birth_c797s(daco = drugCombo$daco[6], osi = drugCombo$osi[6])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu,
                                              birth2 = 24*bT790M, death2 = 24*d_t790m, init = c(n0,n1),
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo6 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median),
                     q25 = apply(sumPreds, 1, function(x){sort(x)[round(.25*length(x))]}),
                     q75 = apply(sumPreds, 1, function(x){sort(x)[round(.75*length(x))]}),
                     condition = "1.47 nM Daco+10.38 nM Osim")

# 4.40 daco 10.38 osi
t = 102
bPC9 = birth_l858r(daco = drugCombo$daco[7], osi = drugCombo$osi[8])
bT790M = birth_t790m(daco = drugCombo$daco[7], osi = drugCombo$osi[8])
bC797S = birth_c797s(daco = drugCombo$daco[7], osi = drugCombo$osi[8])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu,
                                              birth2 = 24*bT790M, death2 = 24*d_t790m, init = c(n0,n1),
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo7 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median),
                     q25 = apply(sumPreds, 1, function(x){sort(x)[round(.25*length(x))]}),
                     q75 = apply(sumPreds, 1, function(x){sort(x)[round(.75*length(x))]}),
                     condition = "4.40 nM Daco+10.38 nM Osim")

# 2.94 daco 20.75 osi
t = 86
bPC9 = birth_l858r(daco = drugCombo$daco[8], osi = drugCombo$osi[8])
bT790M = birth_t790m(daco = drugCombo$daco[8], osi = drugCombo$osi[8])
bC797S = birth_c797s(daco = drugCombo$daco[8], osi = drugCombo$osi[8])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu,
                                              birth2 = 24*bT790M, death2 = 24*d_t790m, init = c(n0,n1),
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo8 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     q25 = apply(sumPreds, 1, function(x){sort(x)[round(.25*length(x))]}),
                     q75 = apply(sumPreds, 1, function(x){sort(x)[round(.75*length(x))]}),
                     condition = "2.94 nM Daco+20.75 nM Osim")

# Combine predicitons for all combinations
comboPredPC9 = as_tibble(rbind(pred_combo1, pred_combo2, pred_combo3, pred_combo4,
                               pred_combo5, pred_combo6, pred_combo7, pred_combo8))
comboPredPC9 = read_csv("PC9_comparison.csv")
### Load observations
obs_combo = read_csv("../../Validation data/pc9DOR.csv")
# Clean up data set and estimate median, sd ouf cell counts by drug combo
obs_combo = obs_combo %>% 
  separate(PC9, into = c("PC9", "day"), sep = "y", convert = T) %>% 
  gather(key = "condition", value = "cellCount", -day, -PC9) %>% 
  separate(condition, into = c("condition", "replicate"), sep = " -") %>%
  dplyr::select(-PC9, -replicate) %>% 
  na.omit() %>% 
  group_by(condition, day) %>% 
  summarise(medianCount = median(cellCount), sdCount = sd(cellCount))

### Compare predicted to observed
# Join columns of data frames
pc9_comparison = left_join(obs_combo, comboPredPC9, by = c("condition", "day"))
# write_csv(pc9_comparison, "../../Validation data/Simulations_200722/pc9.csv")
pc9_comparison = read_csv("PC9_comparison.csv")
# Plot data: predicted is shown in lines, observed are dots with error bars
# write_csv(pc9_comparison, "PC9_comparison.csv")
pc9_comparison %>% 
  ungroup() %>% 
  mutate(condition = factor(condition, levels = c("Vehicle(DMSO)",
                                                  "Daco 4.40 nM",
                                                  "Osimertinib 10.38 nM",
                                                  "1.47 nM Daco+5.19 nM Osim",
                                                  "2.94 nM Daco+5.19 nM Osim",
                                                  "1.47 nM Daco+10.38 nM Osim",
                                                  "4.40 nM Daco+10.38 nM Osim",
                                                  "2.94 nM Daco+20.75 nM Osim"))) %>%
  ggplot(aes(x = day, y = log10(medianCount), group = condition)) +
  geom_point(show.legend = F) +
  geom_errorbar(aes(ymin = log10(medianCount-2*sdCount), ymax = log10(medianCount+2*sdCount)), 
                width = 5, show.legend = F) +
  geom_line(aes(y = log10(medianPred)), lty = 2, show.legend = F) +
  geom_ribbon(aes(ymin = log10(q25)/1.2, ymax = 1.2*log10(q75)),
              alpha = .2, show.legend = F) +
  scale_y_continuous(name = "Predicated Total Cell Count", labels = scales::math_format(expr = 10^.x))+
  labs(x = "Day on Treatment") +
  facet_wrap(~condition) +
  theme_bw()
ggsave("../../Figures/validation_PfizerResults_PC9_top.pdf", width = 8, height = 5)
