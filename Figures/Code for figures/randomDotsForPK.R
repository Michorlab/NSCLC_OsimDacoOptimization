#####
# Author: Kam Poels
# Description: PK parameters for Figure 4


library(mvtnorm)

sigma = matrix(c(1,.5,.5,1.2), nrow = 2)
pks = rmvnorm(n = 500, mean = c(0,0), sigma = sigma)

pdf(file = "pkPlot_figure4.pdf", width = 5, height = 4)
plot(pks, pch = 20, col = rgb(0,0,0,1/2), axes = "n")
dev.off()
