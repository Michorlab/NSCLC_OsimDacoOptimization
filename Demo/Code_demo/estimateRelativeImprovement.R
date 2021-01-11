####
# Author: Kamrine Poels
# Description: Estimate relative improvement percentages and construct a waterplot of schedule comparisons
#####

# Load packages
library(tidyverse)

# Create character vector to read all files in a loop
files = list.files("../Results_demo/")
for (file in files){
  load(paste0("../Results_demo/", file))
}
rm(file, files)

# Function estimates relative improvement: (N1(t)-N2(t))/N1(t)
compSched = function(sched1, sched2){
  tot1 = apply(sched1, 1, sum)
  tot2 = apply(sched2, 1, sum)
  ret = (tot1 - tot2)/tot1
  return(ret)
}

# Compare drug schedules. If there is one more comparison, add it as another vector to the tibble below
comps = tibble(lev1 = compSched(demo2Count, demoCount))

# Make waterplot
comps %>%
  arrange(lev1) %>%
  # Add ID variable, change according to N
  mutate(id = 1:10) %>%
  ### Uncomment following line when comps has more than 1 comparison
  # gather(key = sched, value = improvement, -id) %>%
  rename(improvement = lev1) %>% 
  ggplot(aes(x = id, y = improvement)) +
  geom_bar(position = position_dodge(width = .5), stat = "identity", show.legend = F,
           fill = "grey45") +
  scale_y_continuous(name = "Relative Improvement After 1 year of Treatment", labels = scales::percent_format(accuracy = 1)) +
  ### Uncomment following line when comps has more than 1 comparison
  # facet_grid(sched~.)+
  theme_bw(base_size = 15) +
  theme(legend.position = "bottom", axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
ggsave("../demoWaterplot.pdf", width = 5, height = 5)


##################################################
########## FIGURE 3C (DO NOT RUN FOR DEMO)
##################################################
# 
# library(latticeExtra)
# 
# # Estimate improvement of 30 mg daco QD + 40 mg osi BID
# comps = tibble(compSched(d15QDo40QD_count, d30QDo40BID_count),
#                compSched(d15QDo40BID_count, d30QDo40BID_count),
#                compSched(d15QDo80QD_count, d30QDo40BID_count),
#                compSched(d15BIDo40QD_count, d30QDo40BID_count),
#                compSched(d15BIDo40BID_count, d30QDo40BID_count),
#                compSched(d15BIDo80QD_count, d30QDo40BID_count),
#                compSched(d30QDo40QD_count, d30QDo40BID_count),
#                compSched(d30QDo40BID_count, d30QDo40BID_count),
#                compSched(d30QDo80QD_count, d30QDo40BID_count),
#                compSched(d15TIDo40QD_count, d30QDo40BID_count),
#                compSched(d15TIDo40BID_count, d30QDo40BID_count),
#                compSched(d15TIDo80QD_count, d30QDo40BID_count))
# # Build matrix for easier plotting and easier managing
# medimprovs = matrix(apply(comps, 2, median), nrow = 4, byrow = T)
# # Rename doses and edit for easier plotting
# medimprovs = medimprovs %>% 
#   as.tibble() %>% 
#   rename(o40qd = V1, o40bid = V2, o80qd = V3) %>% 
#   mutate(daco = c("15mg QD", "15mg BID", "30mg QD", "15mg TID")) %>% 
#   gather(key = osi, value = improvement, -daco) %>% 
#   mutate(improvement = case_when(is.na(improvement) ~ -0.0001,
#                                  improvement < -1 ~ -.5,
#                                  T ~ improvement),
#          osi = factor(osi, levels = c("o40bid", "o80qd", "o40qd")),
#          daco = factor(daco, levels = c("15mg TID", "30mg QD", "15mg BID", "15mg QD"))) %>% 
#   mutate(osi = recode(osi, o80qd = "80mg QD", o40bid = "40mg BID", o40qd = "40mg QD"))
# 
# # Build 3-D plot of improvements
# # pdf(file = "../Figures/otherCompScheds.pdf")
# cloud(x = improvement ~ osi + daco, data = medimprovs, 
#       panel.3d.cloud = panel.3dbars,
#       col.facet = c(rgb(0,0,1,1/2), rgb(1,0,0,1/2),
#                     rgb(0,0,0,1/2))[1*(medimprovs$improvement < 0) + 1*(medimprovs$improvement == -0.0001) + 1],
#       xbase = 0.4, 
#       ybase = 0.4, 
#       xlab = "Osimertinib", ylab = "Dacomitinib", zlab = "",
#       scales = list(arrows = FALSE, col = 1),
#       par.settings = list(axis.line = list(col = NA), clip = list(panel = "off")))
# # dev.off()
# 
# # Get p-values
# t.test_count = function(x_count, y_count){
#   x = x_count %>% 
#     mutate(total = (n0_ex19del+n1_T790m+n2_NRAS+n3_C797S),
#            logTotal = log(total)) %>% 
#     select(logTotal)
#   y = y_count %>% 
#     mutate(total = (n0_ex19del+n1_T790m+n2_NRAS+n3_C797S),
#            logTotal = log(total)) %>% 
#     select(logTotal)
#   t.test(x,y, var.equal = F, alternative = "less")$p.val
# }
# 
# p.val = c(t.test_count(d15QDo40QD_count, d30QDo40BID_count),
#           t.test_count(d15QDo40BID_count, d30QDo40BID_count),
#           t.test_count(d15QDo80QD_count, d30QDo40BID_count),
#           t.test_count(d15BIDo40QD_count, d30QDo40BID_count),
#           t.test_count(d15BIDo40BID_count, d30QDo40BID_count),
#           t.test_count(d15BIDo80QD_count, d30QDo40BID_count),
#           t.test_count(d30QDo40QD_count, d30QDo40BID_count),
#           t.test_count(d30QDo40BID_count, d30QDo40BID_count),
#           t.test_count(d30QDo80QD_count, d30QDo40BID_count),
#           t.test_count(d15TIDo40QD_count, d30QDo40BID_count),
#           t.test_count(d15TIDo40BID_count, d30QDo40BID_count),
#           t.test_count(d15TIDo80QD_count, d30QDo40BID_count))
# 
# # Build matrix for easier plotting and easier managing
# adjustedpvals = matrix(p.val, nrow = 4, byrow = T)
# # Rename doses and edit for easier plotting
# adjustedpvals = adjustedpvals %>% 
#   as.tibble() %>% 
#   rename(o40qd = V1, o40bid = V2, o80qd = V3) %>% 
#   mutate(daco = c("15mg QD", "15mg BID", "30mg QD", "15mg TID")) %>% 
#   gather(key = osi, value = p.val, -daco) %>% 
#   mutate(osi = factor(osi, levels = c("o40bid", "o80qd", "o40qd")),
#          daco = factor(daco, levels = c("15mg TID", "30mg QD", "15mg BID", "15mg QD"))) %>% 
#   mutate(osi = recode(osi, o80qd = "80mg QD", o40bid = "40mg BID", o40qd = "40mg QD"))
# 
# adjustedpvals = adjustedpvals %>% 
#   mutate(Bonferroni = p.adjust(p.val, method = "bonferroni"),
#          Holm = p.adjust(p.val, method = "holm"),
#          Hochberg = p.adjust(p.val, method = "hochberg"),
#          Hommel = p.adjust(p.val, method = "hommel"),
#          BH = p.adjust(p.val, method = "BH"),
#          BY = p.adjust(p.val, method = "BY"),
#          FDR = p.adjust(p.val, method = "fdr")) %>% 
#   rename(Raw = p.val)
# 
# adjustedpvals  %>% 
#   gather(key = test, value = p.val, -daco, -osi) %>% 
#   mutate(combo = paste(daco, osi, sep = " + "),
#          p.val = case_when(p.val > .2 ~ .2,
#                            T ~ p.val)) %>% 
#   filter(combo != "30mg QD + 40mg BID") %>% 
#   ggplot(aes(x = test, y = combo, fill = p.val)) +
#   geom_tile() +
#   geom_text(aes(label = round(p.val, 3))) +
#   scale_x_discrete(name = "Method of adjustment") +
#   scale_y_discrete(name = "Dacomitinib + osimertinib combination") +
#   scale_fill_gradient(name = "P-value", high = "yellow", low = "red") +
#   theme_minimal()
# # ggsave("../Figures/pvalues_For_otherSchedsFigure.pdf", width = 8, height = 6)
#                       