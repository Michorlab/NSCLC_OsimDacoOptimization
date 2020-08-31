#####
# Author: Kamrine Poels
# Description: Obtain growth rates in HCC827R cell line, expected to be resistant to daco
#####

library(reshape2)

#### Day 1 of PC9-DRH
# Load data
ccOsi = read.csv("Data from Scott/HCC827R_Osi/Day 1-HCC827R_Osi.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("Data from Scott/HCC827R_Daco/Day 1-HCC827R_Daco.csv", header = F,
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
ccOsi = read.csv("Data from Scott/HCC827R_Osi/Day 2-HCC827R_Osi.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("Data from Scott/HCC827R_Daco/Day 2-HCC827R_Daco.csv", header = F,
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
ccOsi = read.csv("Data from Scott/HCC827R_Osi/Day 3-HCC827R_Osi.csv", header = F,
                 stringsAsFactors = F)
ccDaco = read.csv("Data from Scott/HCC827R_Daco/Day 3-HCC827R_Daco.csv", header = F,
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
# ccFrame = rbind(data.frame(osi = c(0,0), daco = c(0,0), cellCount = c(3000, 3000), 
#                            hr = c(0,0)), ccFrame)

ccFrame %>%
  filter(daco == 0) %>%
  ggplot(aes(x = hr, y = log10(cellCount), color = as.factor(osi)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = .2)+
  scale_y_continuous(name = "Cell Count for HCC827 (MET amp)", labels = scales::math_format(expr = 10^.x))+
  labs(x = "Time (hr)", color = "Osimertinib\nlevel")+
  theme_bw(base_size = 20)
# ggsave("Figures/cellCount_HCC827Rosi.pdf", width = 7, height = 5)

ccFrame %>%
  filter(osi == 0) %>%
  ggplot(aes(x = hr, y = log10(cellCount), color = as.factor(daco)))+
  geom_point()+
  geom_smooth(method = "lm", alpha = .1)+
  scale_y_continuous(name = "", labels = scales::math_format(expr = 10^.x))+
  labs(x = "Time (hr)", color = "Dacomitinib\nlevel")+
  theme_bw(base_size = 20)
# ggsave("Figures/cellCount_HCC827Rdaco.pdf", width = 7, height = 5)

HCC827Rgrowth = ccFrame %>%
  filter(!(osi == 160 & daco == 250) & !(osi == 1000 & daco == 250) &
           !(osi == 1000 & daco == 10)) %>%
  group_by(daco, osi) %>%
  do(model = lm(log(cellCount+1) ~ hr, data = .)) %>%
  summarise(daco = daco, osi = osi, growth = model$coefficients[2])

# Specify death rate
if (min(HCC827Rgrowth$growth)<0){
  d0_HCCR = abs(min(HCC827Rgrowth$growth))
}else{
  d0_HCCR = 0.001
}

# Fit is a bit strange for daco
ymin_HCCR = min(HCC827Rgrowth$growth)-10^-5
ymax_HCCR = max(HCC827Rgrowth$growth)+10^-5
HCC827Rfit = lm(log((growth-ymin_HCCR)/(ymax_HCCR-growth))~ I(log(daco+1)^(1/2))+
                 I(log(osi+1)^(1/2)), HCC827Rgrowth)

convertGrowth = function(growth, trans = F){
  if (trans){log((growth- ymin_HCCR)/(ymax_HCCR - growth))}
  else{(ymax_HCCR*exp(growth)+ymin_HCCR)/(1+exp(growth))}
}

HCC827Rgrowth %>% 
  mutate(fit = predict(HCC827Rfit, newdata = .)) %>% 
  mutate(fit = (ymax_HCCR*exp(fit)+ymin_HCCR)/(1+exp(fit))) %>% 
  ggplot(aes(x = log(daco+1), y = growth))+
  geom_point() +
  geom_line(aes(y = fit), color = "red") +
  facet_grid(~as.factor(osi))

HCC827Rgrowth %>% 
  mutate(fit = predict(HCC827Rfit, newdata = .)) %>% 
  mutate(fit = (ymax_HCCR*exp(fit)+ymin_HCCR)/(1+exp(fit))) %>% 
  ggplot(aes(x = log(osi+1), y = growth))+
  geom_point() +
  geom_line(aes(y = fit), color = "red") +
  facet_grid(~as.factor(daco))
