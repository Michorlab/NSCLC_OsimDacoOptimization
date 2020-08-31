#####
# Author: Kamrine Poels
# Description: use estiPop to estimate cell count at end of experiment
# WARNING 10/23/19: Ollie is debugging code
#####

# setwd("Code/ValidationExperiments/")

# Load essential packages and model fits for growth of PC9 cells
library(tidyverse)
library(pracma)
library(scales)
library(estipop)
load("../../fitsAndRates.RData")

### Create functions for simulations

# Functions to return estimated growth rates given drug concentrations
# PC9 cell line (sensitive to both)
birth_l858r = Vectorize(function(daco, osi){
  # Create covariate vector according to model
  xvec = c(1, log(daco+1)^(1/2.5), log(osi+1)^(1/1.5))
  # Predict growth rate
  fit = sum(pc9fit$coefficients*xvec)
  # Transform growth rate
  ret = (ymax*exp(fit)+ymin)/(1+exp(fit))
  return(ret+dpc9)
})

# PC9-DRH (T790m), sensitive to osimertinib
birth_t790m = Vectorize(function(daco, osi){
  xvec = c(1, log(daco+1), log(osi+1))
  fit = sum(pc9DRHfit$coefficients*xvec)
  # Transform
  ret = (ymaxT790M*exp(fit)+yminT790M)/(1+exp(fit))
  return(ret+d_t790m)
})

# PC9 NRAS, resistant to both (ignorel)
birth_NRAS = Vectorize(function(daco, osi){
  xvec = c(1, 0*log(daco+1), log(osi+1))
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
drugCombo = tibble(daco = c(0, 0, 4.40, 0, 1.47, 2.94, 1.47, 4.40, 2.94),
                   osi = c(0, 0, 0, 10.38, 5.19, 5.19, 10.38, 10.38, 20.75))

# Set parameters
seed = 9648301 # why a seed? What is random here?
nsims = 50
mu = 10^-10
n0 = 10^6*0 # initial 
n1 = 10^6*1

# t0 = 1
# pred0 = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu, 
#                                                birth2 = 24*bT790M, death2 = 24*d_t790m, init = 10^6*c(.9, .1), 
#                                                time = t0, approx = T))
# day1init = apply(t(pred0[2,2:3,]), 2, function(x){round(median(x))})

### 8 combos analyzed,
## No daco no osi
t = 6
bPC9 = birth_NRAS(daco = drugCombo$daco[1], osi = drugCombo$osi[1])
bNRAS = birth_NRAS(daco = drugCombo$daco[1], osi = drugCombo$osi[1])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu, 
                                              birth2 = 24*bNRAS, death2 = 24*d_nras, init = c(n0*.1,n1*.9)/.5, 
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo1 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}),
                     condition = "Vehicle(DMSO)")

# 2 nM daco no osi
t = 8
bPC9 = birth_l858r(daco = drugCombo$daco[2], osi = drugCombo$osi[2])
bNRAS = birth_NRAS(daco = drugCombo$daco[2], osi = drugCombo$osi[2])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu, 
                                              birth2 = 24*bNRAS, death2 = 24*d_nras, init = c(n0,n1), 
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo2 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}),  
                     condition = "PF7775 1 uM")

# 4.4 nM daco no osi
t = 8
bPC9 = birth_l858r(daco = drugCombo$daco[3], osi = drugCombo$osi[3])
bNRAS = birth_NRAS(daco = drugCombo$daco[3], osi = drugCombo$osi[3])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9*(1-mu), death1 = 24*dpc9, mutation = 24*mu, 
                                              birth2 = 24*bNRAS, death2 = 24*d_nras, init = c(n0,n1), 
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo3 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}), 
                     condition = "Daco 4.40 nM")

# No daco 10.38 osi
t = 9
bPC9 = birth_l858r(daco = drugCombo$daco[4], osi = drugCombo$osi[4])
bNRAS = birth_NRAS(daco = drugCombo$daco[4], osi = drugCombo$osi[4])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu, 
                                              birth2 = 24*bNRAS, death2 = 24*d_nras, init = c(n0,n1), 
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo4 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}), 
                     condition = "Osimertinib 10.38 nM")

# 1.47 daco 5.19 osi
t = 9
bPC9 = birth_l858r(daco = drugCombo$daco[5], osi = drugCombo$osi[5])
bNRAS = birth_NRAS(daco = drugCombo$daco[5], osi = drugCombo$osi[5])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu, 
                                              birth2 = 24*bNRAS, death2 = 24*d_nras, init = c(n0,n1), 
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo5 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}),
                     condition = "1.47 nM Daco+5.19 nM Osim")

# 2.94 daco 5.19 osi
t = 9
bPC9 = birth_l858r(daco = drugCombo$daco[6], osi = drugCombo$osi[6])
bNRAS = birth_NRAS(daco = drugCombo$daco[6], osi = drugCombo$osi[6])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu, 
                                              birth2 = 24*bNRAS, death2 = 24*d_nras, init = c(n0,n1), 
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo6 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}),
                     condition = "2.94 nM Daco+5.19 nM Osim")

# 1.47 daco 10.38 osi
t = 10
bPC9 = birth_l858r(daco = drugCombo$daco[7], osi = drugCombo$osi[7])
bNRAS = birth_NRAS(daco = drugCombo$daco[7], osi = drugCombo$osi[7])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu, 
                                              birth2 = 24*bNRAS, death2 = 24*d_nras, init = c(n0,n1), 
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo7 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}),
                     condition = "1.47 nM Daco+10.38 nM Osim")

# 4.40 daco 10.38 osi
t = 10
bPC9 = birth_l858r(daco = drugCombo$daco[8], osi = drugCombo$osi[8])
bNRAS = birth_NRAS(daco = drugCombo$daco[8], osi = drugCombo$osi[8])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu, 
                                              birth2 = 24*bNRAS, death2 = 24*d_nras, init = c(n0,n1), 
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo8 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}),
                     condition = "4.40 nM Daco+10.38 nM Osim")

# 2.94 daco 20.75 osi
t = 10
bPC9 = birth_l858r(daco = drugCombo$daco[9], osi = drugCombo$osi[9])
bNRAS = birth_NRAS(daco = drugCombo$daco[9], osi = drugCombo$osi[9])
pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu, 
                                              birth2 = 24*bNRAS, death2 = 24*d_nras, init = c(n0,n1), 
                                              time = t, approx = T))
sumPreds = pred[,2,]+pred[,3,]
pred_combo9 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}),
                     condition = "2.94 nM Daco+20.75 nM Osim")

# Combine predicitons for all combinations
comboPredRPC9nras = as_tibble(rbind(pred_combo1, pred_combo2, pred_combo3, pred_combo4,
                                pred_combo5, pred_combo6, pred_combo7, pred_combo8, pred_combo9))

### Load observations
obs_nras = read_csv("../../Validation data/pc9r_NRASDOR.csv")
# Clean up data set and estimate median, sd ouf cell counts by drug combo
obs_nras = obs_nras %>% 
  separate(`PC9:RPC9-CL6(10:1)`, into = c("PC9_nras", "day"), sep = "y", convert = T) %>% 
  gather(key = "condition", value = "cellCount", -day, -PC9_nras) %>% 
  separate(condition, into = c("condition", "replicate"), sep = " -") %>%
  dplyr::select(-PC9_nras, -replicate) %>% 
  na.omit() %>% 
  group_by(condition, day) %>% 
  summarise(medianCount = median(cellCount), sdCount = sd(cellCount))

### Compare predicted to observed
# Join columns of data frames
rpc9nras_comparison = full_join(obs_nras, comboPredRPC9nras, by = c("condition", "day"))
# write_csv(rpc9nras_comparison, "../../Validation data/Simulations_200722/pc9NRAS.csv")
# Plot data: predicted is shown in lines, observed are dots with error bars
rpc9nras_comparison %>% 
  ungroup() %>%
  mutate(condition = factor(condition, levels = c("Vehicle(DMSO)",
                                                  "PF7775 1 uM",
                                                  "Daco 4.40 nM",
                                                  "Osimertinib 10.38 nM",
                                                  "1.47 nM Daco+5.19 nM Osim",
                                                  "1.47 nM Daco+10.38 nM Osim",
                                                  "2.94 nM Daco+5.19 nM Osim" ,
                                                  "4.40 nM Daco+10.38 nM Osim",
                                                  "2.94 nM Daco+20.75 nM Osim"))) %>% 
  ggplot(aes(x = day, y = medianCount, group = condition, color = condition)) +
  geom_point(show.legend = F) +
  geom_errorbar(aes(ymin = medianCount - sdCount, ymax = medianCount + sdCount), width = 1, show.legend = F) +
  geom_line(aes(y = medianPred), lty = 2, show.legend = F) +
  geom_ribbon(aes(ymin = .75*iq25, ymax = 1.25*iq75, fill = condition), alpha = .2, show.legend = F) +
  scale_y_log10(name = "Predicated Total Cell Count", breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  labs(title = "PC9R NRAS") +
  scale_x_continuous(name = "Day on Treatment", breaks = seq(0,10,2)) +
  facet_wrap(~condition) +
  theme_bw()
# ggsave("../../Figures/validation_PfizerResults_RPC9NRAS.pdf", width = 8, height = 6)
