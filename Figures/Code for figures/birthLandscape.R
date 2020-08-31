#####
# Author : Kamrine Poels
# Description: Build 3D figure of birth rate landscape
####

library(plot3D)

### PC9 cells
# Run program to get estimated growths
source("predictGrowthRates_PC9.R")
source("birthFunctions.R")

# Specify axes
x = pc9growth$daco
y = pc9growth$osi
z = pc9growth$growth + dpc9 # Add death rate since we want birth rate

# For prediction, select grid points
grid.lines = 15
# transform and take inverse to space out according to scale used in figure
x.pred = exp(seq(log(min(x)+1), log(max(x)+1), length.out = grid.lines))-1 
y.pred = exp(seq(log(min(y)+1), log(max(y)+1), length.out = grid.lines))-1
# Expand dataset
xy = expand.grid(daco = x.pred, osi = y.pred)
# Predict and transform to birth rate
z.pred = matrix(birth_ex19del(daco = xy$daco, osi = xy$osi), nrow = grid.lines, 
                ncol = grid.lines)

# Save plot
# pdf("../Figures/growthLandscape_Fig2D.pdf", width = 5, height = 4)
par(mar = c(3,0,3,5))
scatter3D(x = log(x+1), y = log(y+1), z = z, 
          bty = "g", pch = 20, phi = 0, theta = 60,
          xlab = "Dacomitinib", ylab = "Osimertinib", zlab = "Birth rate",
          clab = "Birth rate\n(per hour)",
          surf = list(x = log(x.pred+1), y = log(y.pred+1), z = z.pred, facets = NA))
# dev.off()
rm(list = ls())
### 
### PC9-DRH cells
# Run program to get estimated growths
source("predictGrowthRates_PC9_DRH.R")
source("birthFunctions.R")

# Specify axes
x = pc9DRHgrowth$daco
y = pc9DRHgrowth$osi
z = pc9DRHgrowth$growth + d_t790m # Add death rate since we want birth rate

# For prediction, select grid points
grid.lines = 15
# transform and take inverse to space out according to scale used in figure
x.pred = exp(seq(log(min(x)+1), log(max(x)+1), length.out = grid.lines))-1 
y.pred = exp(seq(log(min(y)+1), log(max(y)+1), length.out = grid.lines))-1
# Expand dataset
xy = expand.grid(daco = x.pred, osi = y.pred)
# Predict and transform to birth rate
z.pred = matrix(birth_t790m(daco = xy$daco, osi = xy$osi), nrow = grid.lines, 
                ncol = grid.lines)

# Save plot
# pdf("../Figures/growthLandscape_PC9DRH.pdf", width = 5, height = 4)
par(mar = c(3,0,3,5))
scatter3D(x = log(x+1), y = log(y+1), z = z, 
          bty = "g", pch = 20, phi = 0, theta = 60,
          xlab = "Dacomitinib", ylab = "Osimertinib", zlab = "Birth rate",
          clab = "Birth rate\n(per hour)",
          surf = list(x = log(x.pred+1), y = log(y.pred+1), z = z.pred, facets = NA))
# dev.off()
rm(list = ls())
### 

### 
### PC9-C797S cells
# Run program to get estimated growths
source("predictGrowthRates_PC9C797S_AH.R")
source("birthFunctions.R")
# Specify axes
x = C797Sgrowth_std$daco
y = C797Sgrowth_std$osi
z = C797Sgrowth_std$fitGrowth + dc797s_std # Add death rate since we want birth rate

# For prediction, select grid points
grid.lines = 15
# transform and take inverse to space out according to scale used in figure
x.pred = exp(seq(log(min(x)+1), log(max(x)+1), length.out = grid.lines))-1 
y.pred = exp(seq(log(min(y)+1), log(max(y)+1), length.out = grid.lines))-1
# Expand dataset
xy = expand.grid(daco = x.pred, osi = y.pred)
# Predict and transform to birth rate
z.pred = matrix(birth_c797s(daco = xy$daco, osi = xy$osi), nrow = grid.lines, 
                ncol = grid.lines)

# Save plot
pdf("../Figures/growthLandscape_PC9C797S.pdf", width = 5, height = 4)
par(mar = c(3,0,3,5))
scatter3D(x = log(x+1), y = log(y+1), z = z, 
          bty = "g", pch = 20, phi = 0, theta = 60,
          xlab = "Dacomitinib", ylab = "Osimertinib", zlab = "Birth rate",
          clab = "Birth rate\n(per hour)",
          surf = list(x = log(x.pred+1), y = log(y.pred+1), z = z.pred, facets = NA))
dev.off()
rm(list = ls())
### 
### NRAS cells
# Run program to get estimated growths
# THIS PLOT DOES NOT MATCH THE LEGEND FROM THE PLOTS FROM ABOVE SO IT WILL NOT BE INCLUDED
source("predictGrowthRates_PC9R.R")
source("birthFunctions.R")
# Specify axes
x = PC9Rgrowth$daco
y = PC9Rgrowth$osi
z = PC9Rgrowth$growth + d_nras # Add death rate since we want birth rate

# For prediction, select grid points
grid.lines = 15
# transform and take inverse to space out according to scale used in figure
x.pred = exp(seq(log(min(x)+1), log(max(x)+1), length.out = grid.lines))-1 
y.pred = exp(seq(log(min(y)+1), log(max(y)+1), length.out = grid.lines))-1
# Expand dataset
xy = expand.grid(daco = x.pred, osi = y.pred)
# Predict and transform to birth rate
z.pred = matrix(birth_NRAS(daco = xy$daco, osi = xy$osi), nrow = grid.lines, 
                ncol = grid.lines)

# Save plot
pdf("Figures/growthLandscape_PC9R.pdf", width = 5, height = 4)
# par(mar = c(3,0,3,5))
scatter3D(x = log(x+1), y = log(y+1), z = z, 
          bty = "g", pch = 20, phi = 0, theta = 60,
          xlab = "Dacomitinib", ylab = "Osimertinib", zlab = "Birth rate",
          clab = "Birth rate\n(per hour)",
          surf = list(x = log(x.pred+1), y = log(y.pred+1), z = z.pred, facets = NA), zlim = c(0,.035))
# dev.off()

