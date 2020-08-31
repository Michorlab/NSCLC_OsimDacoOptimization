#####
# Author: Kamrine Poels
# Description: Plot isoboles of dacomitinib and osimertinib in PC9 and HCC827 cell lines
####

# pdf("../isoboles.pdf", width = 10, height = 4)
par(mfrow = c(1,2), mar = c(5,4,2,.5)+.1)
# PC9
daco = c(0.3215, 0, 0.2345, 0.2293)
osi = c(0, 9.639, 2.5, 5)

plot(daco, osi, frame.plot = F, pch = 19, xlab = "Dacomitinib (nM)", ylab = "Osimertinib (nM)",
     main = "IC50 in PC9 Cells")
abline(a = osi[2], b = -osi[2]/daco[1], lty = 2, lwd = 3, col = "grey")

# Hcc827

dacoH = c(0, 1.207, .6412, .25, .5, 1)
osiH = c(3.5, 0, 2.5, 2.589, 2.938, 1.075)

plot(dacoH, osiH, frame.plot = F, pch = 19, xlab = "Dacomitinib (nM)", ylab = "Osimertinib (nM)",
     main = "IC50 in HCC827 Cells")
abline(a = osiH[1], -osiH[1]/dacoH[2], lty = 2, lwd = 3, col = "grey")
# dev.off()
# PC9R

daco = c(0, 38.33, 20.25)
osi = c(56.25, 0, 6) 
plot(daco, osi)
abline(a = osi[1], -osi[1]/daco[2])

   