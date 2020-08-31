#####
# Author: Kamrine Poels
# Description: Convert HCC827 growth rates to PC9 by projecting growth proliferation 
#   rate of HCC827 onto PC9. Please note that this cell line did not end up being used in 
#   manuscript
#####

library(ggplot2)
library(scatterplot3d)

# Call scripts to estimate growth rates
source("predictGrowthRates_PC9.R")
# Run following before omitting zeroes
# source("predictGrowthRates_HCC827.R")
source("predictGrowthRates_HCC827R.R")

## Method: Projecting growth proliferation rate of HCC827 onto PC9
# Remove zero values from both sensitive cell lines
pc9growth = pc9growth[HCC827growth$growth != 0,]
HCC827growth = HCC827growth[HCC827growth$growth != 0,]

# Regressing HCC827 on PC9
hcc2pc9 = lm(pc9growth$growth~HCC827growth$growth)
# Plot fit of conversion
ggplot(data.frame(pc9 = pc9growth$growth, hcc827 = HCC827growth$growth, 
                  daco = pc9growth$daco, osi = pc9growth$osi), 
       aes(x = hcc827, y = pc9, color = log(daco+1), size = log(osi+1)))+
  geom_point()+
  geom_smooth(method = "lm", show.legend = F)+
  scale_colour_gradient(low = "black", high = "red",
                        space = "Lab", na.value = "grey50", guide = "colourbar",
                        aesthetics = "colour")+
  labs(x="HCC827", y = "PC9", title = "Proliferation Growth Rates",
       color = "Dacomitinib\n(log(nM+1))", size = "Osimertinib\n(log(nM+1))")+
  theme_bw()
# Save last plot
# ggsave("../Figures/hcc2pc9_conversionOLS.png", width = 6, height = 5)

# Predict growth rates of MET amp onto PC9
metPC9 = cbind(rep(1, dim(HCC827Rgrowth)[1]), 
               HCC827Rgrowth$growth)%*%hcc2pc9$coefficients
# Build data frame with data
PC9METgrowth = cbind(HCC827Rgrowth[,1:2], metPC9)
colnames(PC9METgrowth) = colnames(pc9growth)

# Construct growth rate landscape
PC9metwOsi = lm(growth~log(daco+1)+log(osi+1), PC9METgrowth)
PC9met = lm(growth~log(daco+1), PC9METgrowth)
d_met = min(PC9METgrowth$growth)
save(PC9met, d_met, file = "fitsAndRates_PC9MET.RData")
# Plot landscape and fit
s3d = scatterplot3d(x = log(daco+1), y = log(osi+1), z = growth, box = F, 
                    highlight.3d = T, pch = 20, )
s3d$contour3d(Vectorize(function(x, y){
  c(1,log(x+1), log(y+1))%*%PC9metwOsi$coefficients}))
s3d$contour3d(Vectorize(function(x, y){
  c(1,log(x+1))%*%PC9met$coefficients}), col = "green")
legend("topright", legend = c("Yes", "No"), title = "Osimertinib as predictor",
       col = c("black", "green"), lty = 2, bty = "n")
