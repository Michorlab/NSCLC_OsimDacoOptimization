#####
# Author: Kamrine Poels
# Description: predicts number of cells by resistance type. Input of drug concentrations 
#   is from UBIQUITY
# Notes: death rates are kept constant
# WARNING: These functions use trapezoidal integration
#####

library(pracma)
# source("gettingVarianceOfOutcome.R")

# Functions to return estimated growth rates given drug concentrations
# PC9 cell line (sensitive to both)
birth_sens = Vectorize(function(daco, osi){
  # Create covariate vector according to model
  xvec = c(1, log(daco+1)^(1/3), log(osi+1)^(1/1.5),  log(daco+1)^(1/3)*log(osi+1)^(1/1.5))
  # Predict growth rate
  fit = sum(pc9fit$coefficients*xvec)
  # Transform growth rate
  ret = (ymax*exp(fit)+ymin)/(1+exp(fit))
  return(ret+dpc9)
})

# PC9-DRH (T790m), sensitive to osimertinib
birth_res1 = Vectorize(function(daco, osi){
  xvec = c(1, log(daco+1)^(1/3), log(osi+1)^2)
  fit = sum(pc9DRHfit$coefficients*xvec)
  # Transform
  ret = (ymaxT790M*exp(fit)+yminT790M)/(1+exp(fit))
  return(ret+d_t790m)
})

# PC9R (NRAS point mutation), sensitive to osimertinib
birth_res2 = Vectorize(function(daco, osi){
  xvec = c(1, 0*log(osi+1), log(daco+1))
  # Regression is linear, so no need to transform
  fit = sum(pc9Rfit$coefficients*xvec)
  # Transform
  ret = (ymaxNRAS*exp(fit)+yminNRAS)/(1+exp(fit))
  return(ret+d_nras)
})

# PC9 C797s
birth_res3 = Vectorize(function(daco, osi){
  xvec = c(1, log(daco+1), log(osi+1))
  # Fit is linear
  fit = sum(pc9c797s_std$coefficients*xvec)
  ret = (ymax_c797s_std*exp(fit)+ymin_c797s_std)/(1+exp(fit))
  return(ret+dc797s_std)
})

## MET amplifications
# birth_res4 = Vectorize(function(daco, osi){
#   xvec = c(1, log(daco+1))
#   ret = sum(xvec*PC9met$coefficients)
#   # varOut = var(PC9met$model$growth)
#   # ret = rnorm(1, ret, sqrt(varOut))
#   return(ret+d_met)
# })


# Without MET amplification
pred_all = function(t, n0, n1, n2, n3, mus,
                    conc.osi, conc.daco, ubiquityTimes, seed){
  ix = max(which(ubiquityTimes <= t))
  # Estimate birth rate of sensitive cell line
  set.seed(seed)
  b0 = birth_sens(conc.daco[1:ix], conc.osi[1:ix])
  # Integrate by using trapezoid sums
  areaInt0 = cumtrapz(ubiquityTimes[1:ix], b0*(1-sum(mus)) - dpc9)
  # Estimate sensitive cell line population at different times
  n0t = n0*exp(areaInt0) # this should be a vector
  ret0 = n0t[ix] # latest sensitive cell line population
  # T790m
  b1 = birth_res1(conc.daco, conc.osi)[1:ix]
  innerInt1 = cumtrapz(ubiquityTimes[1:ix], b1 - d_t790m)
  outerInt1 = trapz(ubiquityTimes[1:ix], n0t*b0*exp(-innerInt1))
  ret1 = (n1+mus[1]*outerInt1)/exp(-innerInt1[ix])
  if (ret1 < 0 | is.na(ret1)){ret1 = 0}
  # NRAS
  b2 = birth_res2(conc.daco, conc.osi)[1:ix]
  innerInt2 = cumtrapz(ubiquityTimes[1:ix], b2 - d_nras)
  outerInt2 = trapz(ubiquityTimes[1:ix], n0t*b0*exp(-innerInt2))
  ret2 = (n2+mus[2]*outerInt2)/exp(-innerInt2[ix])
  if (ret2 < 0 | is.na(ret2)){ret2 = 0}
  # C797s
  b3 = birth_res3(conc.daco, conc.osi)[1:ix]
  innerInt3 = cumtrapz(ubiquityTimes[1:ix], b3 - dc797s_std)
  outerInt3 = trapz(ubiquityTimes[1:ix], n0t*b0*exp(-innerInt3))
  ret3 = (n3+mus[3]*outerInt3)/exp(-innerInt3[ix])
  if (ret3 < 0 | is.na(ret3)){ret3 = 0}
  # Return all counts
  return(data.frame("n0_ex19del" = ret0, "n1_T790m" = ret1, "n2_NRAS" = ret2, "n3_C797S"= ret3))
}

# # With MET amplification
# pred_all = function(t, n01, n02, n03, n04, n1, n2, n3, n4, mus, 
#                     conc.osi, conc.daco, ubiquityTimes, seed){
#   ix = max(which(ubiquityTimes <= t))
#   # Estimate birth rate of sensitive cell line
#   set.seed(seed)
#   b0 = birth_sens(conc.daco[1:ix], conc.osi[1:ix])
#   # Integrate by using trapezoid sums
#   areaInt01 = cumtrapz(ubiquityTimes[1:ix], b0*(1-sum(mus))-d0)
#   # Estimate sensitive cell line population at different times
#   n01t = n01*exp(areaInt01) # this should be a vector
#   ret01 = n01t[ix] # latest sensitive cell line population
#   # MET CN 2
#   outerInt02 = cumtrapz(ubiquityTimes[1:ix], n01t*b0*exp(-areaInt01))
#   n02t = (n02+mus[4]*outerInt02)/exp(-areaInt01[ix])
#   ret02 = n02t[ix]
#   if (ret02 < 0){ret02 = 0}
#   # MET CN 3
#   outerInt03 = cumtrapz(ubiquityTimes[1:ix], n02t*b0*exp(-areaInt01))
#   n03t = (n03+mus[4]*outerInt03)/exp(-areaInt01[ix])
#   ret03 = n03t[ix]
#   if (ret03 < 0){ret03 = 0}
#   # MET CN 4
#   outerInt04 = cumtrapz(ubiquityTimes[1:ix], n03t*b0*exp(-areaInt01))
#   n04t = (n04+mus[4]*outerInt04)/exp(-areaInt01[ix])
#   ret04 = n04t[ix]
#   if (ret04 < 0){ret04 = 0}
#   # Add all senstive cell types
#   n0t = n01t+n02t+n03t+n04t
#   ret0 = n0t[ix]
#   # T790m 
#   b1 = birth_res1(conc.daco, conc.osi)[1:ix]
#   innerInt1 = cumtrapz(ubiquityTimes[1:ix], b1-d1)
#   outerInt1 = trapz(ubiquityTimes[1:ix], n0t*b0*exp(-innerInt1))
#   ret1 = (n1+mus[1]*outerInt1)/exp(-innerInt1[ix])
#   if (ret1 < 0){ret1 = 0}
#   # NRAS
#   b2 = birth_res2(conc.daco, conc.osi)[1:ix]
#   innerInt2 = cumtrapz(ubiquityTimes[1:ix], b2-d2)
#   outerInt2 = trapz(ubiquityTimes[1:ix], n0t*b0*exp(-innerInt2))
#   ret2 = (n2+mus[2]*outerInt2)/exp(-innerInt2[ix])
#   if (ret2 < 0){ret2 = 0}
#   # C797s
#   b3 = birth_res3(conc.daco, conc.osi)[1:ix]
#   innerInt3 = cumtrapz(ubiquityTimes[1:ix], b3-dc797_std)
#   outerInt3 = trapz(ubiquityTimes[1:ix], n0t*b0*exp(-innerInt3))
#   ret3 = (n3+mus[3]*outerInt3)/exp(-innerInt3[ix])
#   if (ret3 < 0){ret3 = 0}
#   # MET amplification
#   b4 = birth_res4(conc.daco, conc.osi)[1:ix]
#   innerInt4 = cumtrapz(ubiquityTimes[1:ix], b4-d_met)
#   outerInt4 = trapz(ubiquityTimes[1:ix], n04t*b4*exp(-innerInt4))
#   ret4 = (n4+mus[4]*outerInt4)/exp(-innerInt4[ix])
#   if (ret4 < 0){ret4 = 0}
#   # Return all counts
#   return(data.frame(n0 = ret0, n1 = ret1, n2 = ret2, n3 = ret3, n4 = ret4))
# }
