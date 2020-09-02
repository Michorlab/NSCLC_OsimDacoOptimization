#####
# Author: Kamrine Poels
# Description: predicts number of cells by MET CN in HCC827 cells
# Notes: death rates are kept constant
# WARNING: These functions use trapezoidal integration
#####

library(pracma)

# Only HCC827
birth_sens = Vectorize(function(daco, osi){
  xvec = c(1, log(log(daco+1)+1), log(osi + 1)^(1/2))
  fit = sum(xvec*HCC827fit$coefficients)
  fit = (ymax_HCC*exp(fit)+ymin_HCC)/(1+exp(fit))
  varOut = getVar(xvec, extractInfo(HCC827fit), ymax = ymax_HCC, ymin = ymin_HCC)
  ret = rnorm(1, fit, sqrt(varOut))
  return(ret+d0_HCC)
})

birth_res = Vectorize(function(daco, osi){
  xvec = c(1, log(daco+1))
  fit = sum(xvec*HCC827Rfit$coefficients)
  varOut = var(HCC827Rfit$model$growth)
  ret = rnorm(1, fit, sqrt(varOut))
  return(ret+d1_HCC)
})

pred_all = function(t, n01, n02, n03, n04, n1, musMet, conc.osi, conc.daco, ubiquityTimes, seed){
  ix = max(which(ubiquityTimes <= t))
  # Estimate birth rate of sensitive cell line
  set.seed(seed)
  b0 = birth_sens(conc.daco[1:ix], conc.osi[1:ix])
  # Integrate by using trapezoid sums
  areaInt01 = cumtrapz(ubiquityTimes[1:ix], b0*(1-musMet)-d0_HCC)
  # Estimate sensitive cell line population at different times
  n01t = n01*exp(areaInt01) # this should be a vector
  ret01 = n01t[ix] # latest sensitive cell line population
  # MET CN 2
  outerInt02 = cumtrapz(ubiquityTimes[1:ix], n01t*b0*exp(-areaInt01))
  n02t = (n02+musMet*outerInt02)/exp(-areaInt01[ix])
  ret02 = n02t[ix]
  if (ret02 < 0){ret02 = 0}
  # MET CN 3
  outerInt03 = cumtrapz(ubiquityTimes[1:ix], n02t*b0*exp(-areaInt01))
  n03t = (n03+musMet*outerInt03)/exp(-areaInt01[ix])
  ret03 = n03t[ix]
  if (ret03 < 0){ret03 = 0}
  # MET CN 4
  outerInt04 = cumtrapz(ubiquityTimes[1:ix], n03t*b0*exp(-areaInt01))
  n04t = (n04+musMet*outerInt04)/exp(-areaInt01[ix])
  ret04 = n04t[ix]
  if (ret04 < 0){ret04 = 0}
  # Resistant cell type
  b1 = birth_res(conc.daco, conc.osi)[1:ix]
  innerInt1 = cumtrapz(ubiquityTimes[1:ix], b1-d1_HCC)
  outerInt1 = trapz(ubiquityTimes[1:ix], n04t*b1*exp(-innerInt1))
  ret1 = (n1+musMet*outerInt1)/exp(-innerInt1[ix])
  if (ret1 < 0){ret1 = 0}
  # Return all counts
  return(data.frame(n01 = ret01, n02 = ret02, n03 = ret03, n04 = ret04, n1 = ret1))
}
