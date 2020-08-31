#####
# Author: Kamrine Poels
# Description: Obtain growth rates of PC9 cell lines in combined treatment levels of 
#   dacomitinib and osimertinib
#####

library(tidyverse)
library(reshape2)
library(ggpubr)

#### Day 1 of PC9
# Read data
ccOsi = read.csv("../Data from Scott/PC9_Osi/Day 1-PC9_Osi.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("../Data from Scott/PC9_Daco/Day 1-PC9_Daco.csv", header = F,
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

#### Day 2 of PC9
ccOsi = read.csv("../Data from Scott/PC9_Osi/Day 2-PC9_Osi.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("../Data from Scott/PC9_Daco/Day 2-PC9_Daco.csv", header = F,
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

#### Day 3 of PC9
ccOsi = read.csv("../Data from Scott/PC9_Osi/Day 3-PC9_Osi.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("../Data from Scott/PC9_Daco/Day 3-PC9_Daco.csv", header = F,
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
#   filter(osi %in% c(0,2.5, 5, 10, 1000)) %>%
#   ggplot(aes(x = hr, y = log10(cellCount), color = as.factor(osi)))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_y_continuous(name = "", labels = scales::math_format(expr = 10^.x))+
#   scale_color_grey(name = "Osimertinib (nM)") +
#   labs(x = "Time (hr)")+
#   theme_bw(base_size = 20)
# ggsave("Figures/cellCount_PC9osi_greyscale.pdf", width = 7, height = 4)
# 
# ccFrame %>%
#   filter(osi == 0) %>%
#   filter(daco %in% c(0,.25, .5, 1, 250)) %>%
#   ggplot(aes(x = hr, y = log10(cellCount), color = as.factor(daco)))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   scale_y_continuous(name = "Cell Count for PC9", labels = scales::math_format(expr = 10^.x))+
#   scale_colour_brewer(name = "Dacomitinib (nM)", palette = "Oranges", ) +
#   labs(x = "Time (hr)")+
#   theme_bw(base_size = 20)
# ggsave("Figures/cellCount_PC9daco_greyscale.pdf", width = 7, height = 4)

# ccFrame %>% 
#   filter(!(osi == 5 & daco == 250) & !(osi == 10 & daco == 250) &
#            !(osi == 1000 & daco == .5) & !(osi == 1000 & daco == 1)) %>% 
#   ggplot(aes(x = hr, y = log(cellCount)))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   facet_grid(as.factor(osi)~as.factor(daco))

# Estimate growth proliferation rates
# Plan: regress for each concentration level and obtain growth rates.
# Group by combination
pc9growth = ccFrame %>%
  filter(!(osi == 5 & daco == 250) & !(osi == 10 & daco == 250) &
           !(osi == 1000 & daco == .5) & !(osi == 1000 & daco == 1)) %>%
  group_by(daco, osi) %>% 
  do(model = lm(log(cellCount+1) ~ hr, data = .)) %>% 
  summarise(daco = daco, osi = osi, growth = model$coefficients[2], sd = summary(model)$coefficients[2,2])

if (min(pc9growth$growth)<0){
  dpc9 = abs(min(pc9growth$growth))
}else{
  dpc9 = 0.1
}

# Fitting using linear regression
ymin = min(pc9growth$growth)-10^-5
ymax = max(pc9growth$growth)+10^-5

convertGrowth = function(growth, trans = F){
  if (trans){log((growth-ymin)/(ymax - growth))}
  else{(ymax*exp(growth)+ymin)/(1+exp(growth))}
}

pc9fit = lm(log((growth-ymin)/(ymax - growth))~I(log(daco+1)^(1/3))*I(log(osi+1)^(1/1.5)), pc9growth)

figA = pc9growth %>% 
  mutate(fit = predict(pc9fit, newdata = .)) %>% 
  mutate(fit = (ymax*exp(fit)+ymin)/(1+exp(fit))) %>% 
  ggplot(aes(x = log(daco+1), y = growth))+
  geom_point() +
  geom_line(aes(y = fit), color = "red") +
  labs(title = "Growth by osimertinib concentrations") +
  facet_grid(~as.factor(osi))

figB = pc9growth %>% 
  mutate(fit = predict(pc9fit, newdata = .)) %>% 
  mutate(fit = (ymax*exp(fit)+ymin)/(1+exp(fit))) %>% 
  ggplot(aes(x = log(osi+1), y = growth))+
  geom_point() +
  geom_line(aes(y = fit), color = "red") +
  labs(title = "Growth by dacomitinib concentrations") +
  facet_grid(~as.factor(daco))

ggarrange(figA, figB, ncol = 1, labels = c("A", "B"))
# ggsave("../Figures/growthPredictios_PC9.pdf", width = 15, height = 5)
