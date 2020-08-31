#####
# Author: Kamrine Poels
# Description: use estiPop to estimate cell count at end of experiment
#####

# setwd("Code/ValidationExperiments/")

# Load essential packages and model fits for growth of PC9 cells
library(tidyverse)
library(pracma)
library(scales)
library(estipop)
# load("../../fitsAndRates.RData")
load("../../newFitsAndRates200229.RData")
### Create functions for simulations
# d_nras = .005
# d_t790m = .005
# dpc9 = .005
# dc797s_std = .005

# Functions to return estimated growth rates given drug concentrations
# PC9 cell line (sensitive to both)
birth_ex19d = Vectorize(function(daco, osi){
  # Create covariate vector according to model
  xvec = c(1, log(daco+1)^(1/3), log(osi+1)^(1/1.5), log(daco+1)^(1/3)*log(osi+1)^(1/1.5))
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
  xvec = c(1, log(daco+1), log(osi+1))
  fit = sum(pc9Rfit$coefficients*xvec)
  ret = (ymaxNRAS*exp(fit)+yminNRAS)/(1+exp(fit))
  return(ret+d_nras)
})

birth_c797s = Vectorize(function(daco, osi){
  xvec = c(1, log(daco+1), log(osi+1))
  fit = sum(pc9c797s_std$coefficients*xvec)
  ret = (ymax_c797s_std*exp(fit)+ymin_c797s_std)/(1+exp(fit))
  return(ret+dc797s_std)
})

####################################################################################################
# Run simulations
####################################################################################################

# Create vector with drug concentrations used in the validation experiments
drugCombo = tibble(daco = c(0, 4, 0, 1.47, 2.94, 1.47, 4.40, 2.94),
                   osi = c(0, 0, 10.38, 5.19, 5.19, 10.38, 10.38, 20.75))


# Set parameters
seed = 9648301 # why a seed? What is random here?
nsims = 50
mu = 10^-7
n0 = round(10^6*100/101) # initial 
n1 = round(10^6*.1/101)
n2 = 0

stopList = StopList(StopCriterion(indices = c(0), inequality = ">=", value = 10^10))

### 8 combos analyzed,
## No daco no osi
t = 9
bPC9 = birth_ex19d(daco = drugCombo$daco[1], osi = drugCombo$osi[1])
bT790M = birth_t790m(daco = drugCombo$daco[1], osi = drugCombo$osi[1])
bC797S = birth_c797s(daco = drugCombo$daco[1], osi = drugCombo$osi[1])
# pred = replicate(nsims, simBirthDeathMutation(birth1 = 24*bPC9, death1 = 24*dpc9, mutation = 24*mu, 
#                                               birth2 = 24*bT790M, death2 = 24*d_t790m, init = c(n0,n1), 
#                                               time = t, approx = T))
transList = TransitionList(FixedTransition(population = 0, rate = 24*bPC9*(1-2*24*mu), fixed = c(2,0,0)),
                           FixedTransition(population = 0, rate = 24*dpc9, fixed = c(0,0,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,1,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,0,1)),
                           FixedTransition(population = 1, rate = 24*bT790M, fixed = c(0,2,0)),
                           FixedTransition(population = 1, rate = 24*d_t790m, fixed = c(0,0,0)),
                           FixedTransition(population = 2, rate = 24*bC797S, fixed = c(0,0,2)),
                           FixedTransition(population = 2, rate = 24*dc797s_std, fixed = c(0,0,0)))
pred = replicate(nsims, branch(time = t, transitionList = transList, stopList = stopList, 
                               initial = c(n0, n1, n2), approx = T))
sumPreds = pred[,2,]+pred[,3,]+pred[,4,]
pred_combo1 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}),
                     condition = "Vehicle(DMSO)")

# 4.4 nM daco no osi
t = 25
bPC9 = birth_ex19d(daco = drugCombo$daco[2], osi = drugCombo$osi[2])
bT790M = birth_t790m(daco = drugCombo$daco[2], osi = drugCombo$osi[2])
bC797S = birth_c797s(daco = drugCombo$daco[2], osi = drugCombo$osi[2])
transList = TransitionList(FixedTransition(population = 0, rate = 24*bPC9*(1-2*24*mu), fixed = c(2,0,0)),
                           FixedTransition(population = 0, rate = 24*dpc9, fixed = c(0,0,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,1,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,0,1)),
                           FixedTransition(population = 1, rate = 24*bT790M, fixed = c(0,2,0)),
                           FixedTransition(population = 1, rate = 24*d_t790m, fixed = c(0,0,0)),
                           FixedTransition(population = 2, rate = 24*bC797S, fixed = c(0,0,2)),
                           FixedTransition(population = 2, rate = 24*dc797s_std, fixed = c(0,0,0)))
pred = replicate(nsims, branch(time = t, transitionList = transList, stopList = stopList, 
                               initial = c(n0, n1, n2), approx = T))
sumPreds = pred[,2,]+pred[,3,]+pred[,4,]
pred_combo2 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}), 
                     condition = "Daco 4.40 nM")

# No daco 10.38 osi
t = 27
bPC9 = birth_ex19d(daco = drugCombo$daco[3], osi = drugCombo$osi[3])
bT790M = birth_t790m(daco = drugCombo$daco[3], osi = drugCombo$osi[3])
bC797S = birth_c797s(daco = drugCombo$daco[3], osi = drugCombo$osi[3])
transList = TransitionList(FixedTransition(population = 0, rate = 24*bPC9*(1-2*24*mu), fixed = c(2,0,0)),
                           FixedTransition(population = 0, rate = 24*dpc9, fixed = c(0,0,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,1,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,0,1)),
                           FixedTransition(population = 1, rate = 24*bT790M, fixed = c(0,2,0)),
                           FixedTransition(population = 1, rate = 24*d_t790m, fixed = c(0,0,0)),
                           FixedTransition(population = 2, rate = 24*bC797S, fixed = c(0,0,2)),
                           FixedTransition(population = 2, rate = 24*dc797s_std, fixed = c(0,0,0)))
pred = replicate(nsims, branch(time = t, transitionList = transList, stopList = stopList, 
                               initial = c(n0, n1, n2), approx = T))
sumPreds = pred[,2,]+pred[,3,]+pred[,4,]
pred_combo3 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}), 
                     condition = "Osimertinib 10.38 nM")

# 1.47 daco 5.19 osi
t = 58
bPC9 = birth_ex19d(daco = drugCombo$daco[4], osi = drugCombo$osi[4])
bT790M = birth_t790m(daco = drugCombo$daco[4], osi = drugCombo$osi[4]) - d_t790m + 0.01
bC797S = birth_c797s(daco = drugCombo$daco[4], osi = drugCombo$osi[4])
transList = TransitionList(FixedTransition(population = 0, rate = 24*bPC9*(1-2*24*mu), fixed = c(2,0,0)),
                           FixedTransition(population = 0, rate = 24*dpc9, fixed = c(0,0,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,1,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,0,1)),
                           FixedTransition(population = 1, rate = 24*bT790M, fixed = c(0,2,0)),
                           FixedTransition(population = 1, rate = 24*d_t790m, fixed = c(0,0,0)),
                           FixedTransition(population = 2, rate = 24*bC797S, fixed = c(0,0,2)),
                           FixedTransition(population = 2, rate = 24*dc797s_std, fixed = c(0,0,0)))
pred = replicate(nsims, branch(time = t, transitionList = transList, stopList = stopList, 
                               initial = c(n0, n1, n2), approx = T))
sumPreds = pred[,2,]+pred[,3,]+pred[,4,]
pred_combo4 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}),
                     condition = "1.47 nM Daco+5.19 nM Osim")

# 2.94 daco 5.19 osi
t = 80
bPC9 = birth_ex19d(daco = drugCombo$daco[5], osi = drugCombo$osi[5])
bT790M = birth_t790m(daco = drugCombo$daco[5], osi = drugCombo$osi[5]) - d_t790m + 0.01
bC797S = birth_c797s(daco = drugCombo$daco[5], osi = drugCombo$osi[5])
transList = TransitionList(FixedTransition(population = 0, rate = 24*bPC9*(1-2*24*mu), fixed = c(2,0,0)),
                           FixedTransition(population = 0, rate = 24*dpc9, fixed = c(0,0,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,1,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,0,1)),
                           FixedTransition(population = 1, rate = 24*bT790M, fixed = c(0,2,0)),
                           FixedTransition(population = 1, rate = 24*d_t790m, fixed = c(0,0,0)),
                           FixedTransition(population = 2, rate = 24*bC797S, fixed = c(0,0,2)),
                           FixedTransition(population = 2, rate = 24*dc797s_std, fixed = c(0,0,0)))
pred = replicate(nsims, branch(time = t, transitionList = transList, stopList = stopList, 
                               initial = c(n0, n1, n2), approx = T))
sumPreds = pred[,2,]+pred[,3,]+pred[,4,]
pred_combo5 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}),
                     condition = "2.94 nM Daco+5.19 nM Osim")

# 1.47 daco 10.38 osi
t = 104
bPC9 = birth_ex19d(daco = drugCombo$daco[6], osi = drugCombo$osi[6])
bT790M = birth_t790m(daco = drugCombo$daco[7], osi = drugCombo$osi[7]) -d_t790m +0.01
bC797S = birth_c797s(daco = drugCombo$daco[6], osi = drugCombo$osi[6])
transList = TransitionList(FixedTransition(population = 0, rate = 24*bPC9*(1-2*24*mu), fixed = c(2,0,0)),
                           FixedTransition(population = 0, rate = 24*dpc9, fixed = c(0,0,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,1,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,0,1)),
                           FixedTransition(population = 1, rate = 24*bT790M, fixed = c(0,2,0)),
                           FixedTransition(population = 1, rate = 24*d_t790m, fixed = c(0,0,0)),
                           FixedTransition(population = 2, rate = 24*bC797S, fixed = c(0,0,2)),
                           FixedTransition(population = 2, rate = 24*dc797s_std, fixed = c(0,0,0)))
pred = replicate(nsims, branch(time = t, transitionList = transList, stopList = stopList, 
                               initial = c(n0, n1, n2), approx = T))
sumPreds = pred[,2,]+pred[,3,]+pred[,4,]
pred_combo6 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}),
                     condition = "1.47 nM Daco+10.38 nM Osim")

# 4.40 daco 10.38 osi
t = 106
bPC9 = birth_ex19d(daco = drugCombo$daco[7], osi = drugCombo$osi[7])
bT790M = birth_t790m(daco = drugCombo$daco[7], osi = drugCombo$osi[7]) - d_t790m + 0.01
bC797S = birth_c797s(daco = drugCombo$daco[7], osi = drugCombo$osi[7])
transList = TransitionList(FixedTransition(population = 0, rate = 24*bPC9*(1-2*24*mu), fixed = c(2,0,0)),
                           FixedTransition(population = 0, rate = 24*dpc9, fixed = c(0,0,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,1,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,0,1)),
                           FixedTransition(population = 1, rate = 24*bT790M, fixed = c(0,2,0)),
                           FixedTransition(population = 1, rate = 24*d_t790m, fixed = c(0,0,0)),
                           FixedTransition(population = 2, rate = 24*bC797S, fixed = c(0,0,2)),
                           FixedTransition(population = 2, rate = 24*dc797s_std, fixed = c(0,0,0)))
pred = replicate(nsims, branch(time = t, transitionList = transList, stopList = stopList, 
                               initial = c(n0, n1, n2), approx = T))
sumPreds = pred[,2,]+pred[,3,]+pred[,4,]
pred_combo7 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}),
                     condition = "4.40 nM Daco+10.38 nM Osim")

# 2.94 daco + 20.75 osi
t = 120
bPC9 = birth_ex19d(daco = drugCombo$daco[8], osi = drugCombo$osi[8])
bT790M = 0.018# birth_t790m(daco = drugCombo$daco[8], osi = drugCombo$osi[8])
bC797S = birth_c797s(daco = drugCombo$daco[8], osi = drugCombo$osi[8])
transList = TransitionList(FixedTransition(population = 0, rate = 24*bPC9*(1-2*24*mu), fixed = c(2,0,0)),
                           FixedTransition(population = 0, rate = 24*dpc9, fixed = c(0,0,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,1,0)),
                           FixedTransition(population = 0, rate = 24*bPC9*mu, fixed = c(1,0,1)),
                           FixedTransition(population = 1, rate = 24*bT790M, fixed = c(0,2,0)),
                           FixedTransition(population = 1, rate = 24*d_t790m, fixed = c(0,0,0)),
                           FixedTransition(population = 2, rate = 24*bC797S, fixed = c(0,0,2)),
                           FixedTransition(population = 2, rate = 24*dc797s_std, fixed = c(0,0,0)))
pred = replicate(nsims, branch(time = t, transitionList = transList, stopList = stopList, 
                               initial = c(n0, n1, n2), approx = T))
sumPreds = pred[,2,]+pred[,3,]+pred[,4,]
# sumPreds = pred[,2]+pred[,3]+pred[,4]
pred_combo8 = tibble(day = pred[,1,1], medianPred = apply(sumPreds, 1, median), 
                     iq25 = apply(sumPreds, 1, function(x){quantile(x)[2]}), 
                     iq75 = apply(sumPreds, 1, function(x){quantile(x)[4]}),
                     condition = "2.94 nM Daco+20.75 nM Osim")

# Combine predicitons for all combinations
comboPredPool = as_tibble(rbind(pred_combo1, pred_combo2, pred_combo3, pred_combo4,
                                pred_combo5, pred_combo6, pred_combo7, pred_combo8))

### Load observations
obs_pool = read_csv("../../Validation data/pc9_rpc9_100_1.csv")
# Clean up data set and estimate median, sd ouf cell counts by drug combo
obs_pool = obs_pool %>%
  separate(`RPC9-CL6`, into = c("PC9_pool", "day"), sep = "y", convert = T) %>% 
  gather(key = "condition", value = "cellCount", -day, -PC9_pool) %>% 
  separate(condition, into = c("condition", "replicate"), sep = " -") %>%
  dplyr::select(-PC9_pool, -replicate)  %>% 
  na.omit() %>% 
  group_by(condition, day) %>% 
  summarise(medianCount = median(cellCount), sdCount = sd(cellCount))

### Compare predicted to observed
# Join columns of data frames
pc9_100to1pool_comparison = full_join(obs_pool, comboPredPool, by = c("condition", "day"))
# write_csv(pc9_100to1pool_comparison, "../../Validation data/Simulations_200827/pc9Rpc9_100to1.csv")
# Plot data: predicted is shown in lines, observed are dots with error bars
pc9_100to1pool_comparison %>% 
  ungroup() %>% 
  mutate(condition = factor(condition, levels = c("Vehicle(DMSO)",
                                                  "Daco 4.40 nM",
                                                  "Osimertinib 10.38 nM",
                                                  "1.47 nM Daco+5.19 nM Osim",
                                                  "2.94 nM Daco+5.19 nM Osim",
                                                  "1.47 nM Daco+10.38 nM Osim",
                                                  "4.40 nM Daco+10.38 nM Osim",
                                                  "2.94 nM Daco+20.75 nM Osim"))) %>%
  ggplot(aes(x = day, y = medianCount, group = condition, color = condition)) +
  geom_point(show.legend = F) +
  geom_errorbar(aes(ymin = medianCount - sdCount, ymax = medianCount + sdCount), width = 2, show.legend = F) +
  geom_line(aes(y = medianPred), lty = 2, show.legend = F) +
  geom_ribbon(aes(ymin = .5*iq25,
                  ymax = 1.5*iq75, fill = condition), alpha = .2, show.legend = F) +
  scale_y_log10(name = "Predicated Total Cell Count", breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Day on Treatment", color = "Condition", title = "PC9-RPC9 100:1 Pool") +
  facet_wrap(~condition) +
  theme_bw()
# ggsave("../../Figures/validation_PfizerResults_PC9toRPC9_100to1pool_200827.pdf", width = 8, height = 6)
