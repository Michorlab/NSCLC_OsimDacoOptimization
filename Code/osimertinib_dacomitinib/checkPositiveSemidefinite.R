#####
# Author: Kamrine Poels
# Description: Finds the correlations between drugs that will yield a positive semi definite covariance matrix
#####

# Load necessary libraries
library(matrixcalc)
library(tidyverse)

# Build covariance matrix of random effects. These correlations should not change!
eta = c("kao", "CL1o", "V1o", "CL2o", "V2o", "CLd", "V2d", "kad", "Qd", "V3d")
sigma2diag = c(.7921, .2116, .2704, .2704, .3844, .103, .092, 1.466, .025, .178)
sigma = diag(sigma2diag)
colnames(sigma) = rownames(sigma) = eta

sigma["CL1o","CL2o"] = sigma["CL2o","CL1o"] = .9*sqrt(sigma["CL1o","CL1o"]*sigma["CL2o","CL2o"])
sigma["CLd","Qd"] = sigma["Qd","CLd"] = .7*sqrt(sigma["CLd","CLd"]*sigma["Qd","Qd"])

# Check if covariance matrix is positive semidefinite (it should be since it's given matrix)
is.positive.semi.definite(sigma)

# Function checks is covariance matrix with given correlations is still positive semidefinite
corrPosiSemiDefi = function(vec){
  vCcorr = vec[1] 
  vPcorr = vec[2]
  cCcorr = vec[3]
  cPcorr = vec[4]
  tempSigma = sigma
  tempSigma["V1o","V2d"] = tempSigma["V2d","V1o"] = vCcorr*sqrt(tempSigma["V1o","V1o"]*tempSigma["V2d","V2d"])
  tempSigma["V2o","V3d"] = tempSigma["V3d","V2o"] = vPcorr*sqrt(tempSigma["V2o","V2o"]*tempSigma["V3d","V3d"])
  tempSigma["CL1o","CLd"] = tempSigma["CLd","CL1o"] = cCcorr*sqrt(tempSigma["CL1o","CL1o"]*tempSigma["CLd","CLd"])
  tempSigma["CL2o","Qd"] = tempSigma["Qd","CL2o"] = cPcorr*sqrt(tempSigma["CL2o","CL2o"]*tempSigma["Qd","Qd"])
  return(is.positive.semi.definite(tempSigma))
}

# Create different combinations of correlations to test covariance matrix
posiDefi = tibble(vCcorr =  seq(0,.95,.05), vPcorr =  seq(0,.95,.05), cCcorr = seq(0,.95,.05), cPcorr = seq(0,.95,.05))
posiDefi = expand(posiDefi, vCcorr, vPcorr, cCcorr, cPcorr)

# Apply function and subset correlations that will yield a positive correlation matrix
psd = apply(posiDefi, 1, corrPosiSemiDefi)
posiDefi = posiDefi %>% 
  mutate(psd = psd) %>% 
  filter(psd == T) %>% 
  dplyr::select(-psd)
  
# Insert desired correlations and show covariance matrix.
sigma["V1o","V2d"] = sigma["V2d","V1o"] = .85*sqrt(sigma["V1o","V1o"]*sigma["V2d","V2d"])
sigma["V2o","V3d"] = sigma["V3d","V2o"] = .85*sqrt(sigma["V2o","V2o"]*sigma["V3d","V3d"])

sigma["CL1o","CLd"] = sigma["CLd","CL1o"] = .1*sqrt(sigma["CL1o","CL1o"]*sigma["CLd","CLd"])
sigma["CL2o","Qd"] = sigma["Qd","CL2o"] = .1*sqrt(sigma["CL2o","CL2o"]*sigma["Qd","Qd"])

sigma
