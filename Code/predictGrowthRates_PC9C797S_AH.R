#####
# Author: Kamrine Poels
# Description: Obtain growth rates of PC9 T790M cell lines in combined treatment levels of 
#   dacomitinib and osimertinib. This cell line is from Aaron Hata's lab.
#####

library(tidyverse)
library(reshape2)

#### Day 1 of PC9
# Read data
ccOsi = read.csv("../Data from Aaron/PC9_C797S_Osi/PC9_C797S_Osi-Day 1.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("../Data from Aaron/PC9_C797S_Daco/PC9_C797S_Daco-Day 1.csv", header = F,
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
ccOsi = read.csv("../Data from Aaron/PC9_C797S_Osi/PC9_C797S_Osi-Day 2.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("../Data from Aaron/PC9_C797S_Daco/PC9_C797S_Daco-Day 2.csv", header = F,
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
ccOsi = read.csv("../Data from Aaron/PC9_C797S_Osi/PC9_C797S_Osi-Day 3.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("../Data from Aaron/PC9_C797S_Daco/PC9_C797S_Daco-Day 3.csv", header = F,
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
#   filter(!(daco==10 & osi==1000) & !(daco==250 & osi==160) & !(daco==250 & osi==1000)) %>%
#   ggplot(aes(x = hr, y = cellCount)) + 
#   geom_point() +
#   geom_smooth(method = "lm") +
#   facet_grid(as.factor(daco)~as.factor(osi))

C797Sgrowth = ccFrame %>% 
  filter(!(daco==10 & osi==1000) & !(daco==250 & osi==160) & !(daco==250 & osi==1000)) %>%
  group_by(daco, osi) %>% 
  group_by(daco, osi) %>% 
  do(model = lm(log(cellCount+1) ~ hr, data = .)) %>% 
  summarise(daco = daco, osi = osi, growth = model$coefficients[2], sd = summary(model)$coefficients[2,2])

# Assume death rate is absolute value of minimum observed proliferation rate
dc797s_AH = abs(min(C797Sgrowth$growth))

pc9C797Sfit = lm(growth~I(log(daco+1))+I(log(osi+1)), C797Sgrowth)

# # Plot proliferation rates and predicted rates
figA = C797Sgrowth %>%
  mutate(fit = predict(pc9C797Sfit, newdata = .)) %>%
  # mutate(fit = (ymaxC797S_ah*exp(fit)+yminC797S_ah)/(1+exp(fit))) %>%
  ggplot(aes(x = log(daco+1), y = growth)) +
  geom_point()+
  geom_line(aes(y = fit), color = "red") +
  labs(title = "Growth by osimertinib concentrations") +
  facet_grid(~as.factor(osi))

figB = C797Sgrowth %>%
  mutate(fit = predict(pc9C797Sfit, newdata = .)) %>%
  # mutate(fit = (ymaxC797S_ah*exp(fit)+yminC797S_ah)/(1+exp(fit))) %>%
  ggplot(aes(x = log(osi+1), y = growth)) +
  geom_point()+
  geom_line(aes(y = fit), color = "red") +
  labs(title = "Growth by dacomitinib concentrations") +
  facet_grid(~as.factor(daco))

ggarrange(figA, figB, ncol = 1, labels = c("A", "B"))

### Standardize to pfizer's PC9 cells
# Load PC9 data from Pfizer's lab and Aaron Hata's lab
source("predictGrowthRates_PC9.R")
source("predictGrowthRates_PC9AH.R")

# Combine data sets and build projection from Hata's data onto Pfizer's
hata2pfizer = full_join(pc9growth, pc9_AHgrowth, by = c("daco", "osi")) %>% 
  rename(pfizer = growth.x, hata = growth.y)
hata2pfizer = lm(pfizer~hata, hata2pfizer)

# Build new data set of C797S growths that are standardized
C797Sgrowth_std = C797Sgrowth %>% 
  rename(hata = growth) %>% 
  mutate(fitGrowth = predict(hata2pfizer, newdata = .))

# Estimate death rate from standardized estimates
dc797s_std = abs(min(C797Sgrowth_std$fitGrowth))

ymin_c797s_std = min(C797Sgrowth_std$fitGrowth)-10^-5
ymax_c797s_std = max(C797Sgrowth_std$fitGrowth)+10^-5

pc9c797s_std = lm(log((fitGrowth-ymin_c797s_std)/(ymax_c797s_std-fitGrowth)) ~ 
                    I(log(daco+1))+I(log(osi+1)), C797Sgrowth_std)

figA = C797Sgrowth_std %>% 
  mutate(fit = predict(pc9c797s_std, newdata = .)) %>% 
  mutate(fit = (ymax_c797s_std*exp(fit)+ymin_c797s_std)/(1+exp(fit))) %>%
  ggplot(aes(x = log(daco+1), y = fitGrowth)) +
  geom_point() +
  geom_line(aes(y = fit), color = "red") +
  labs(title = "Growth by osimertinib concentrations") +
  facet_grid(~as.factor(osi))

figB = C797Sgrowth_std %>% 
  mutate(fit = predict(pc9c797s_std, newdata = .)) %>% 
  mutate(fit = (ymax_c797s_std*exp(fit)+ymin_c797s_std)/(1+exp(fit))) %>% 
  ggplot(aes(x = log(osi+1), y = fitGrowth))+
  geom_point() +
  geom_line(aes(y = fit), color = "red") +
  labs(title = "Growth by dacomitinib concentrations") +
  facet_grid(~as.factor(daco))

ggarrange(figA, figB, ncol = 1, labels = c("A", "B"))
# ggsave("../Figures/growthPredictios_PC9C797S_std.pdf", width = 15, height = 5)
