#####
# Author: Kamrine Poels
# Description: Obtain growth rates of PC9 T790M cell lines in combined treatment levels of 
#   dacomitinib and osimertinib. This cell line is from Aaron Hata's lab.
#####

library(reshape2)
library(tidyverse)
library(scatterplot3d)

#### Day 1 of PC9
# Read data
ccOsi = read.csv("Data from Aaron/PC9_T790M_Osi/PC9_T790M_Osi-Day 1.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("Data from Aaron/PC9_T790M_Daco/PC9_T790M_Daco-Day 1.csv", header = F,
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
ccOsi = read.csv("Data from Aaron/PC9_T790M_Osi/PC9_T790M_Osi-Day 2.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("Data from Aaron/PC9_T790M_Daco/PC9_T790M_Daco-Day 2.csv", header = F,
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
ccOsi = read.csv("Data from Aaron/PC9_T790M_Osi/PC9_T790M_Osi-Day 3.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("Data from Aaron/PC9_T790M_Daco/PC9_T790M_Daco-Day 3.csv", header = F,
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

### Growth rate estimation
t790mGrowth = ccFrame %>% 
  filter(!(daco==10 & osi==1000) & !(daco==250 & osi==160) & !(daco==250 & osi==1000)) %>% 
  # Estimate growth rates for each drug combo
  group_by(daco, osi) %>% 
  do(model = lm(log(cellCount+1) ~ hr, data = .)) %>% 
  summarise(daco = daco, osi = osi, growth = model$coefficients[2])

# Assume death rate is absolute value of minimum proliferation rate
dt790m_AH = abs(min(t790mGrowth$growth))

# # Plot proliferation rates and predicted rates
# t790mGrowth %>% 
#   mutate(fit = predict(pc9T790mfit, newdata = .)) %>% 
#   mutate(fit = (ymaxT790M_ah*exp(fit)+yminT790M_ah)/(1+exp(fit))) %>% 
#   ggplot(aes(x = log(daco+1), y = growth)) +
#   geom_point() +
#   geom_line(aes(y = fit), color = "red")+
#   facet_grid(~as.factor(osi))

### Landscape of growth proliferation rate
yminT790M_ah = min(t790mGrowth$growth)-10^-5
ymaxT790M_ah = max(t790mGrowth$growth)+10^-5
# Is there a better fit?
pc9T790Mfit = lm(log((growth-yminT790M_ah)/(ymaxT790M_ah-growth))~I(log(daco+1))+I(log(osi+1)), t790mGrowth)

