#####
# Author: Kamrine Poels
# Description: Runs scripts to fit growth rates and saves them in one file.


source("predictGrowthRates_PC9.R")
source("predictGrowthRates_PC9_DRH.R")
source("predictGrowthRates_PC9R.R")
source("predictGrowthRates_PC9C797S_AH.R")

save(ymin,ymax,dpc9,pc9fit,yminT790M,ymaxT790M,d_t790m,pc9DRHfit,yminNRAS,ymaxNRAS,d_nras,
     pc9Rfit,ymin_c797s_std,ymax_c797s_std,dc797s_std,pc9c797s_std, 
     file = "../newFitsAndRates200229.RData")
