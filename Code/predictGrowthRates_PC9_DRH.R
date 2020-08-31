#####
# Author: Kamrine Poels
# Description: Obtain growth rates in PC9 DRH cell, expected to be resistant to daco
#####

library(reshape2)
# library(ggplot2)
# library(gridExtra)
library(tidyverse)

#### Day 1 of PC9-DRH
# Load data
ccOsi = read.csv("../Data from Scott/PC9-DRH_Osi/Day 1-PC9-DRH_Osi.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("../Data from Scott/PC9-DRH_Daco/Day 1-PC9-DRH_Daco.csv", header = F,
                  stringsAsFactors = F)
# Modify data for easier analysis
ccOsi[1,dim(ccOsi)[2]] = 0
ccOsi$V1 = as.numeric(ccOsi$V1)
ccOsi$V13 = as.numeric(ccOsi$V13)
ccDaco[1,dim(ccDaco)[2]] = 0
ccDaco$V1 = as.numeric(ccDaco$V1)
ccDaco$V13 = as.numeric(ccDaco$V13)
# Melt data frames and bind them
colnames(ccOsi) = ccOsi[1,]
ccOsi = ccOsi[-1,]
ccOsi = melt(ccOsi, id.vars = "NA")
colnames(ccOsi) = c("daco", "osi", "cellCount")
ccOsi$osi = as.numeric(as.character(ccOsi$osi))
colnames(ccDaco) = ccDaco[1,]
ccDaco = ccDaco[-1,]
ccDaco = melt(ccDaco, id.vars = "NA")
colnames(ccDaco) = c("osi", "daco", "cellCount")
ccDaco$daco = as.numeric(as.character(ccDaco$daco))
cc1day = rbind(ccOsi, ccDaco)
ccFrame = cbind(cc1day, hr = rep(24, dim(cc1day)[1]))

#### Day 2 of PC9-DRH
ccOsi = read.csv("../Data from Scott/PC9-DRH_Osi/Day 2-PC9-DRH_Osi.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("../Data from Scott/PC9-DRH_Daco/Day 2-PC9-DRH_Daco.csv", header = F,
                  stringsAsFactors = F)
# Modify data for easier analysis
ccOsi[1,dim(ccOsi)[2]] = 0
ccOsi$V1 = as.numeric(ccOsi$V1)
ccOsi$V13 = as.numeric(ccOsi$V13)
ccDaco[1,dim(ccDaco)[2]] = 0
ccDaco$V1 = as.numeric(ccDaco$V1)
ccDaco$V13 = as.numeric(ccDaco$V13)
# Melt data frames and bind them
colnames(ccOsi) = ccOsi[1,]
ccOsi = ccOsi[-1,]
ccOsi = melt(ccOsi, id.vars = "NA")
colnames(ccOsi) = c("daco", "osi", "cellCount")
ccOsi$osi = as.numeric(as.character(ccOsi$osi))
colnames(ccDaco) = ccDaco[1,]
ccDaco = ccDaco[-1,]
ccDaco = melt(ccDaco, id.vars = "NA")
colnames(ccDaco) = c("osi", "daco", "cellCount")
ccDaco$daco = as.numeric(as.character(ccDaco$daco))
cc2day = rbind(ccOsi, ccDaco)
cc2day = cbind(cc2day, hr = rep(48, dim(cc2day)[1]))
ccFrame = rbind(ccFrame, cc2day)

#### Day 3 of PC9-DRH
ccOsi = read.csv("../Data from Scott/PC9-DRH_Osi/Day 3-PC9-DRH_Osi.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("../Data from Scott/PC9-DRH_Daco/Day 3-PC9-DRH_Daco.csv", header = F,
                  stringsAsFactors = F)
# Modify data for easier analysis
ccOsi[1,dim(ccOsi)[2]] = 0
ccOsi$V1 = as.numeric(ccOsi$V1)
ccOsi$V13 = as.numeric(ccOsi$V13)
ccDaco[1,dim(ccDaco)[2]] = 0
ccDaco$V1 = as.numeric(ccDaco$V1)
ccDaco$V13 = as.numeric(ccDaco$V13)
# Melt data frames and bind them
colnames(ccOsi) = ccOsi[1,]
ccOsi = ccOsi[-1,]
ccOsi = melt(ccOsi, id.vars = "NA")
colnames(ccOsi) = c("daco", "osi", "cellCount")
ccOsi$osi = as.numeric(as.character(ccOsi$osi))
colnames(ccDaco) = ccDaco[1,]
ccDaco = ccDaco[-1,]
ccDaco = melt(ccDaco, id.vars = "NA")
colnames(ccDaco) = c("osi", "daco", "cellCount")
ccDaco$daco = as.numeric(as.character(ccDaco$daco))
cc3day = rbind(ccOsi, ccDaco)
cc3day = cbind(cc3day, hr = rep(72, dim(cc3day)[1]))
ccFrame = as.tibble(rbind(ccFrame, cc3day))


# ccFrame %>%
#   filter(daco == 0) %>%
#   filter(osi %in% c(0, 6, 37.04, 160, 1000)) %>%
#   ggplot(aes(x = hr, y = log10(cellCount), color = as.factor(osi)))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_y_continuous(name = "", labels = scales::math_format(expr = 10^.x))+
#   scale_color_grey(name = "Osimertinib (nM)") +
#   labs(x = "Time (hr)")+
#   theme_bw(base_size = 20)
# ggsave("Figures/cellCount_PC9DRHosi_greyscale.pdf", width = 7, height = 4)

# ccFrame %>%
#   filter(osi == 0) %>%
#   filter(daco %in% c(0, 4, 10, 250)) %>%
#   ggplot(aes(x = hr, y = log10(cellCount), color = as.factor(daco)))+
#   geom_point()+
#   geom_smooth(method = "lm", alpha = .2)+
#   scale_y_continuous(name = "Cell Count for PC9-DRH", labels = scales::math_format(expr = 10^.x))+
#   scale_colour_brewer(name = "Dacomitinib (nM)", palette = "Oranges") +
#   labs(x = "Time (hr)")+
#   theme_bw(base_size = 20)
# ggsave("Figures/cellCount_PC9DRHdaco_greyscale.pdf", width = 7, height = 4)

pc9DRHgrowth = ccFrame %>% 
  # Remove unexpected observations
  filter(!(daco==10 & osi==1000) & !(daco==250 & osi==160) & !(daco==250 & osi==1000)) %>%
  # Estimate growth rates for each drug combo
  group_by(daco, osi) %>% 
  do(model = lm(log(cellCount+1) ~ hr, data = .)) %>% 
  summarise(daco = daco, osi = osi, growth = model$coefficients[2], sd = summary(model)$coefficients[2,2])

# Specify death rate
if (min(pc9DRHgrowth$growth)<0){
  d_t790m = abs(min(pc9DRHgrowth$growth))
}else{
  d_t790m = 0.1
}

# Fit is very simple
yminT790M = min(pc9DRHgrowth$growth)-10^-5
ymaxT790M = max(pc9DRHgrowth$growth)+10^-5

# # Transform to response
# convertGrowth = function(growth, trans = F){
#   if (trans){log((growth-yminT790M)/(ymaxT790M - growth))}
#   else{(ymaxT790M*exp(growth)+yminT790M)/(1+exp(growth))}
# }

pc9DRHfit = lm(log((growth-yminT790M)/(ymaxT790M-growth)) ~ I(log(daco+1)^(1/3))+
                 I(log(osi+1)^2), pc9DRHgrowth)

figA = pc9DRHgrowth %>% 
  mutate(fit = predict(pc9DRHfit, newdata = .)) %>% 
  mutate(fit = (ymaxT790M*exp(fit)+yminT790M)/(1+exp(fit))) %>% 
  ggplot(aes(x = log(daco+1), y = growth))+
  geom_point() +
  geom_line(aes(y = fit), color = "red") +
  labs(title = "Growth by osimertinib concentrations") +
  facet_grid(~as.factor(osi))

figB = pc9DRHgrowth %>% 
  mutate(fit = predict(pc9DRHfit, newdata = .)) %>% 
  mutate(fit = (ymaxT790M*exp(fit)+yminT790M)/(1+exp(fit))) %>% 
  ggplot(aes(x = log(osi+1), y = growth))+
  geom_point() +
  geom_line(aes(y = fit), color = "red") +
  labs(title = "Growth by dacomitinib concentrations") +
  facet_grid(~as.factor(daco))

ggarrange(figA, figB, ncol = 1, labels = c("A", "B"))
# ggsave("../Figures/growthPredictios_PC9DRH.pdf", width = 15, height = 5)
