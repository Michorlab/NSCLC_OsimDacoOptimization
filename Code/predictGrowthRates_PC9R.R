#####
# Author: Kamrine Poels
# Description: Obtain growth rates in PC9R cell line, expected to be resistant to daco.
#   This cell line displays the NRAS point mutation
#####

library(tidyverse)
library(reshape2)

#### Day 1 of PC9-DRH
# Load data
ccOsi = read.csv("../Data from Scott/PC9R_Osi/Day 1-PC9R_Osi.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("../Data from Scott/PC9R_Daco/Day 1-PC9R_Daco.csv", header = F,
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
ccOsi = read.csv("../Data from Scott/PC9R_Osi/Day 2-PC9R_Osi.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("../Data from Scott/PC9R_Daco/Day 2-PC9R_Daco.csv", header = F,
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
ccOsi = read.csv("../Data from Scott/PC9R_Osi/Day 3-PC9R_Osi.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("../Data from Scott/PC9R_Daco/Day 3-PC9R_Daco.csv", header = F,
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
ccFrame = rbind(ccFrame, cc3day)

# The following plots show that log(y) ~ log(hour) is a better fit. However, this goes against the assumption of tumor growth. We think this may be due to confluency..
# ccFrame %>%
#   filter(daco == 0 ) %>%
#   ggplot(aes(x = hr, y = log10(cellCount), color = as.factor(osi)))+
#   geom_point()+
#   geom_smooth(method = "lm", alpha = .1, formula = y ~ x)+
#   scale_y_continuous(name = "Cell Count for PC9 (NRAS)", labels = scales::math_format(expr = 10^.x))+
#   labs(x = "Time (hr)", color = "Osimertinib\nlevel")+
#   theme_bw(base_size = 20)
# ggsave("Figures/cellCount_PC9Rosi.pdf", width = 7, height = 5)

# ccFrame %>%
#   filter(osi == 0) %>%
#   ggplot(aes(x = hr, y = log10(cellCount), color = as.factor(daco)))+
#   geom_point()+
#   geom_smooth(method = "lm", formula = y~x, alpha = .1)+
#   scale_y_continuous(name = "", labels = scales::math_format(expr = 10^.x))+
#   labs(x = "Time (hr)", color = "Dacomitinib\nlevel")+
#   theme_bw(base_size = 20) # +
#   # facet_grid(~as.factor(daco))
# ggsave("Figures/cellCount_PC9Rdaco.pdf", width = 7, height = 5)

PC9Rgrowth = ccFrame %>% 
  # Remove unexpected observations
  filter(!(daco==10 & osi==1000) & !(daco==250 & osi==160) & !(daco==250 & osi==1000)) %>% 
  group_by(daco, osi) %>% 
  do(model = lm(log(cellCount+1) ~ hr, data = .)) %>% 
  summarise(daco = daco, osi = osi, growth = model$coefficients[2], sd = summary(model)$coefficients[2,2])

# Specify death rate
if (min(PC9Rgrowth$growth)<0){
  d_nras = abs(min(PC9Rgrowth$growth))
}else{
  d_nras = 0.01
}

# Transform growth to the sigmoidal scale
yminNRAS = min(PC9Rgrowth$growth) - 10^-5
ymaxNRAS = max(PC9Rgrowth$growth) + 10^-5
# Fit function (may revise later)
# convertGrowth = function(growth, trans = F){
#   if (trans){log((growth-yminNRAS)/(ymaxNRAS - growth))}
#   else{(ymaxNRAS*exp(growth)+yminNRAS)/(1+exp(growth))}
# }

pc9Rfit = lm(log((growth - yminNRAS)/(ymaxNRAS - growth))~I(log(osi+1))+I(log(daco+1)), PC9Rgrowth)

figA = PC9Rgrowth %>% 
  mutate(fit = predict(pc9Rfit, newdata = .)) %>% 
  mutate(fit = (ymaxNRAS*exp(fit)+yminNRAS)/(1+exp(fit))) %>% 
  ggplot(aes(x = log(daco+1), y = growth))+
  geom_point() +
  geom_line(aes(y = fit), color = "red") +
  labs(title = "Growth by osimertinib concentrations") +
  facet_grid(~as.factor(osi))

figB = PC9Rgrowth %>% 
  mutate(fit = predict(pc9Rfit, newdata = .)) %>% 
  mutate(fit = (ymaxNRAS*exp(fit)+yminNRAS)/(1+exp(fit))) %>% 
  ggplot(aes(x = log(osi+1), y = growth))+
  geom_point() +
  geom_line(aes(y = fit), color = "red") +
  labs(title = "Growth by dacomitinib concentrations") +
  facet_grid(~as.factor(daco))

ggarrange(figA, figB, ncol = 1, labels = c("A", "B"))
# ggsave("../Figures/growthPredictios_PC9NRAS.pdf", width = 15, height = 5)


              