#####
# Author: Kamrine Poels
# Description: Builds figures of birth rates under a specific regimen for one individual
####

# Loa necessary packages
library(tidyverse)

# Load concentrations of one regimen
load("../../Concentrations/Corr.8/30dacoQD_40osiQD.RData")

# Load fitness data from PC9 experiments
load("../../Results_10092019/fitsAndRates.RData")

# Let numIX be number of observations to include in plot (right before end of day 4)
numIX = 72
# Get drug concentrations
dacoConc = group1.1$tcsummary$o.C_daco_c.median[1:numIX]
osiConc = group1.1$tcsummary$o.C_osi.median[1:numIX]

### Predict birth rates
# exon 19 del
birth0 = predict(pc9fit, newdata = data.frame(daco = dacoConc, osi = osiConc))
birth0 = (ymax*exp(birth0)+ymin)/(1+exp(birth0)) + dpc9
# T790M
birth1 = predict(pc9DRHfit, newdata = data.frame(daco = dacoConc, osi = osiConc))
birth1 = (ymaxT790M *exp(birth1)+yminT790M)/(1+exp(birth1)) + d_t790m
# NRAS
birth2 = predict(pc9Rfit, newdata = data.frame(daco = 0, osi = osiConc)) 
birth2 = (ymaxNRAS*exp(birth2)+yminNRAS)/(1+exp(birth2)) + d_nras
# C797S
birth3 = predict(pc9c797s_std, newdata = data.frame(daco = dacoConc, osi = osiConc))
birth3 = (ymax_c797s_std*exp(birth3)+ymin_c797s_std)/(1+exp(birth3)) + dc797s_std

## Build tibble and plot
birthRates = tibble(hour = group1.1$times$ts.hour[1:numIX],
                    exon19del = birth0,
                    T790M = birth1,
                    C797S = birth3,
                    NRAS = birth2)
birthRates %>% 
  mutate(T790M = case_when(T790M < 0 ~ 0,
                           T ~ T790M)) %>%
  gather(key = type, value = birth, -hour) %>% 
  ggplot(aes(x = hour, y = birth, color = type)) +
  geom_line(lwd = 1) +
  scale_x_continuous(name = "Hours since first dose", breaks = c(0,24,48,72),
                     sec.axis = dup_axis(name = "", labels = rep("30 mg daco+\n40 mg osi",4))) +
  scale_y_log10(name = expression(Birth~rate~(h^-1)), breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_color_manual(name = "Cell type",
                    values = c("darkgoldenrod2", "royalblue4", "green4", "firebrick")) +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.position = "bottom", #c(.85,.8))
        # legend.box = "vertical",
        legend.box.background = element_rect(colour = "black"))
# ggsave("../Figures/birthRate_30dacoQD40osiQD.pdf", width = 5.5, height = 4)

