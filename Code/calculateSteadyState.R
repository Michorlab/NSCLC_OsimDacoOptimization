#####
# Author: Kamrine Poels
# Description: Obtain average concentrations of drugs at steady state for 100-1000 simulated subjects
#               Before running, load drug concentration files of interest from Concentrations/ directory

##### Load datasets
library(pracma)
library(xtable)

##### Functions
# ssMeanConc takes two interval parameters and a list which is output from ubiquity
# Returns a data frame of two columnts with an average oncentration for each subject
# Steady state is: \bar{C}_{ss} = \frac{\int_{s}^{s+\tau} C(t) dt}{\tau}
ssMeanConc = function(tauD = 24, tauO = 24, concList){
  # Select the begining of a day at steady state (3 weeks after start of treatment)
  startSS = 3*7*24
  # Specify end of interval at steady state
  endSSD = startSS + tauD
  endSSO = startSS + tauO
  # Use time indices from Ubiquity output and subset to day (or half day) at steady state
  timeIX = concList$times$time
  ixD = which(timeIX >= startSS & timeIX <= endSSD)
  ixO = which(timeIX >= startSS & timeIX <= endSSO)
  timeIXD = timeIX[ixD]
  timeIXO = timeIX[ixO]
  # Subset drug concentrations to day (or half day) at steady state
  dacoCs = concList$outputs$C_daco_c[,ixD]
  osiCs = concList$outputs$C_osi[,ixO]
  # Integrate over one continuous curve
  dacoSS = apply(dacoCs, MARGIN = 1, function(x){trapz(timeIXD, x)/tauD})
  osiSS = apply(osiCs, MARGIN = 1, function(x){trapz(timeIXO, x)/tauO})
  return(data.frame(dacoSS = dacoSS, osiSS = osiSS))
}

# Estimate quantiles from average concentrations at steady state
group1lev1conc = apply(ssMeanConc(concList = group1.1), MARGIN = 2, quantile)
group1lev2conc = apply(ssMeanConc(concList = group1.2), MARGIN = 2, quantile)
group1lev3conc = apply(ssMeanConc(concList = group1.3), MARGIN = 2, quantile)

group2lev1conc = apply(ssMeanConc(concList = group2.1, tauO = 12), MARGIN = 2, quantile)
group2lev2conc = apply(ssMeanConc(concList = group2.2, tauO = 12), MARGIN = 2, quantile)
group2lev3conc = apply(ssMeanConc(concList = group2.3, tauO = 12), MARGIN = 2, quantile)

# Print tables of quantiles
print(xtable(group1lev3conc, digits = 2), comment = F)
print(xtable(group2lev3conc, digits = 2), comment = F)