load("../../newFitsAndRates200229.RData")
source("predictCount_ubiquity.R")

library(tidyverse)

N = 10
set.seed(548331)
bsld = rnorm(N, 7.5, (11.7-7.5)/qnorm(.75))
bsld[bsld<1] = runif(sum(bsld<1), 4.5, 7.5)
n0_init = 4/3*pi*(bsld/2)^3*10^7
p01 = (rnorm(N, 0.0495, 0.011)/100)*(runif(N)<.8) # 80 % of patients had .001-01% frequency T790m
p02 = rep(0,N) #
p03 = rep(0,N) #(rnorm(N, 0.0495, 0.011)/1000)*(runif(N)<.1 | (runif(N)<.4 & p01>0)) # C797s dependent on p01
# Set cell type from initial population and sampled proportion
n1_init = round(n0_init*p01)
n2_init = round(n0_init*p02)
n3_init = round(n0_init*p03)
n0_init = n0_init-n1_init-n2_init-n3_init

# Create data frame with all rates for point mutations
mus = cbind(10^rnorm(N, -7, 1/qnorm(.95)),
            (10/6^(1/7))^rnorm(N, -11, 1/qnorm(.995)),
            10^rnorm(N, -9, 1/qnorm(.95)))

seed = sample(N*50, N)

## Group 1 Level 1
load("../Concentrations_demo/exampleConcentrations.RData")
appFunc = function(it, drugConc){
  nt = pred_all(24*7, n0 = n0_init[it], n1 = n1_init[it], n2 = n2_init[it], 
                n3 = n3_init[it], mus = mus[it,],
                conc.osi = drugConc$outputs$C_osi[it,],
                conc.daco = drugConc$outputs$C_daco_c[it,], 
                ubiquityTimes = drugConc$times$time,
                seed = seed[it])
  return(nt)
}
# Run first schedule
start = Sys.time()
demoCount = lapply(1:N, appFunc, drugConc = som)
demoCount = bind_rows(demoCount)
Sys.time()-start

# Run second schedule
start = Sys.time()
demo2Count = lapply(1:N, appFunc, drugConc = som2)
demo2Count = bind_rows(demo2Count)
Sys.time()-start

save(demoCount, demo2Count, file = "../Results_demo/cellCounts.RData")
