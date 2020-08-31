#####
# Author: Kamrine Poels
# Description: Make landscape of proliferation rates for supplementary file
####

library(tidyverse)

# Make sure to run functions to predict growth rates
concs = expand_grid(osilog = seq(log(1), log(1001), length.out = 100), 
                    dacolog = seq(log(1), log(251), length.out = 100))
growthRates = concs %>% 
  mutate(daco = exp(dacolog)-1,
         osi = exp(osilog)-1) %>% 
  mutate(PC9_exon19del = birth_l858r(daco, osi) - dpc9,
         T790M = birth_t790m(daco, osi) - d_t790m,
         C797S = birth_c797s(daco, osi) - dc797s_std,
         NRAS = birth_NRAS(daco, osi) - d_nras)

growthRates %>% 
  gather(key = type, value = rate, PC9_exon19del, T790M, C797S, NRAS) %>% 
  ggplot(aes(x = dacolog, y = osilog, fill = rate)) +
  geom_tile() +
  facet_wrap(~type) +
  scale_x_continuous(name = "Dacomitinib (nM)", breaks = seq(log(1), log(251), length.out = 5), 
                     labels = function(x){round(exp(x)-1)}) +
  scale_y_continuous(name = "Osimertinib (nM)",  breaks = seq(log(1), log(1001), length.out = 5),
                     labels = function(x){round(exp(x)-1)}) +
  scale_fill_gradient2(name = "Growth rate") +
  theme_minimal()
# ggsave("../Figures/growthLandscape3D.pdf", width = 6, height = 5)


##### Death rates #####
dpc9_t = function(dacoConc, osiConc){
  (dpc9 - 2.5*10^-3)/1250*(osiConc+dacoConc)+2.5*10^-3
}
dpc9T790M_t = function(dacoConc, osiConc){
  (dc797s_std-2.2*10^-3)/750*(osiConc)+2.2*10^-3
}
dpc9C797S_t = function(dacoConc, osiConc){
  (dc797s_std-2.2*10^-3)/250*(dacoConc)+2.2*10^-3
}


concs = expand_grid(osilog = seq(log(1), log(1001), length.out = 100), 
                    dacolog = seq(log(1), log(251), length.out = 100))
growthRates = concs %>% 
  mutate(daco = exp(dacolog)-1,
         osi = exp(osilog)-1) %>% 
  mutate(PC9_exon19del = dpc9_t(exp(dacolog)+1, exp(osilog)+1),
         T790M = dpc9T790M_t(exp(dacolog)+1, exp(osilog)+1),
         C797S = dpc9C797S_t(exp(dacolog)+1, exp(osilog)+1),
         NRAS = d_nras)
growthRates %>% 
  gather(key = type, value = rate, PC9_exon19del, T790M, C797S, NRAS) %>% 
  ggplot(aes(x = dacolog, y = osilog, fill = rate)) +
  geom_tile() +
  facet_wrap(~type) +
  scale_x_continuous(name = "Dacomitinib (nM)", breaks = seq(log(1), log(251), length.out = 5), 
                     labels = function(x){round(exp(x)-1)}) +
  scale_y_continuous(name = "Osimertinib (nM)",  breaks = seq(log(1), log(1001), length.out = 5),
                     labels = function(x){round(exp(x)-1)}) +
  scale_fill_gradient2(name = "Death rate", low = "white", high = "red") +
  theme_minimal()
# ggsave("../../Figures/deathRateVariantLandscape.pdf", width = 6, height = 5)
