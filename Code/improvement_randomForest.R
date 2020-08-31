#####
# Author: Kamrine Poels
# Description: Runs a random forest to rank most important predictors that seem to most affect disparities
#               between schedules.
#####

library(tidyverse)
library(randomForest)

# Simulate initial conditions
# Double check that these initial conditions match the ones used for the simulations!
N = 1000
set.seed(548331)
bsld = rnorm(N, 7.5, (11.7-7.5)/qnorm(.75))
bsld[bsld<1] = runif(sum(bsld<1), 4.5, 7.5)
n0_init = 4/3*pi*(bsld/2)^3*10^7
p01 = (rnorm(N, 0.0495, 0.011)/100)*(runif(N)<.8) # 80 % of patients had .001-01% frequency T790m
p02 = (rnorm(N, 0.00495, 0.011)/100)*(runif(N)<.05) # ~1% of patients with NRAS, increased proportion
p03 = rep(0,N) #(rnorm(N, 0.0495, 0.011)/1000)*(runif(N)<.1 | (runif(N)<.4 & p01>0)) # C797s dependent on p01
# Set cell type from initial population and sampled proportion
n1_init = round(n0_init*p01)
n2_init = round(n0_init*p02)
n3_init = round(n0_init*p03)
n0_init = n0_init-n1_init-n2_init-n3_init

# save(n0_init, n1_init, n2_init, n3_init, file = "../initialCondition_n100.RData")
load("../Concentrations/YearConc85/30dacoQD_40osiQD.RData")

popPK = group1.1$subjects[[1]]

tib = as.tibble(bind_cols(popPK, p01 = p01, p02 = p02, n0 = n0_init + n1_init + n2_init, comps)) %>% 
  dplyr::select(-MWo, -MWd, -lev2, -lev3) #%>% 
  mutate(lev1 = as.factor(lev1 > .01))
tib_noPK = as.tibble(bind_cols(p01 = p01, p02 = p02, comps)) %>% 
  dplyr::select(-lev2, -lev3) #%>% 
  mutate(lev1 = as.factor(lev1 > .01))
  
set.seed(1961277)
rf = as.data.frame(replicate(50, randomForest(lev1~ ., data = tib)$importance))
rf_noPK = as.data.frame(replicate(50, randomForest(lev~ ., data = tib_noPK)))

rf %>% 
  t() %>% 
  as.tibble() %>% 
  gather(key = Feature, value = IncNodePurity) %>% 
  group_by(Feature) %>% 
  summarise(IncNodePurity_mean = mean(IncNodePurity), IncNodePurity_sd = sd(IncNodePurity)) %>%
  mutate(Feature = fct_reorder(Feature, IncNodePurity_mean)) %>% 
  ggplot(aes(y = IncNodePurity_mean, x = Feature)) +
  geom_point() +
  coord_flip() +
  scale_x_discrete(name = "Features",
                   labels = rev(c(expression(Cl[c*o]),
                                  expression(k[a*o]),
                                  expression(Cl[p*o]),
                                  expression(V[c*o]),
                                  expression(V[c*d]),
                                  expression(Cl[c*d]), 
                                  expression(p[T790M]), 
                                  expression(Cl[p*d]),
                                  expression(n[0]),
                                  expression(V[p*d]),
                                  expression(V[p*o]),
                                  expression(k[a*d]),
                                  expression(p[NRAS])))) +
  scale_y_continuous(name = "Node Purity") +
  labs(title = "Variable Importance from Random Forests") +
  geom_errorbar(aes(ymin = IncNodePurity_mean - IncNodePurity_sd, 
                    ymax = IncNodePurity_mean + IncNodePurity_sd), width = .25) +
  theme_bw(base_size = 15)
ggsave("../Figures/corr85_1year_nodePurity.pdf", width = 7, height = 5)

rf %>% 
  t() %>% 
  as.tibble() %>% 
  gather(key = feature, value = IncNodePurity) %>% 
  group_by(feature) %>% 
  summarise(IncNodePurity_mean = mean(IncNodePurity), IncNodePurity_sd = sd(IncNodePurity)) %>%
  mutate(feature = fct_reorder(feature, IncNodePurity_mean)) %>% 
  ggplot(aes(y = IncNodePurity_mean, x = feature)) +
  geom_point() +
  coord_flip() +
  scale_x_discrete(name = "Features",
                   labels = rev(c(expression(p[NRAS]),
                                  expression(k[a*o]),
                                  expression(Cl[c*o]),
                                  expression(V[c*o]),
                                  expression(Cl[p*o]),
                                  expression(V[c*d]),
                                  expression(n[0]),
                                  expression(Cl[c*d]),
                                  expression(k[a*d]),
                                  expression(Cl[p*d]),
                                  expression(V[p*d]),
                                  expression(V[p*o]),
                                  expression(p[T790M])))) +
  scale_y_continuous(name = "Node Purity") +
  labs(title = "Variable Importance from Random Forests") +
  geom_errorbar(aes(ymin = IncNodePurity_mean - IncNodePurity_sd, 
                    ymax = IncNodePurity_mean + IncNodePurity_sd), width = .25) +
  theme_bw(base_size = 15)
# ggsave("../Figures/corr.8_nodePurity.pdf", width = 7, height = 5)

# correlations = tib %>% 
#   gather(key = level, value = improvement, lev1, lev2, lev3) %>% 
#   group_by(level) %>% 
#   summarise(kao = cor(kao, improvement, method = "spearman"),
#             CL1o = cor(CL1o, improvement, method = "spearman"),
#             V1o = cor(V1o, improvement, method = "spearman"),
#             kad = cor(kad, improvement, method = "spearman"),
#             CLd = cor(CLd, improvement, method = "spearman"),
#             V2d = cor(V2d, improvement, method = "spearman")) %>% 
#   gather(key = pkParam, value = correlation, -level)
# 
# corpval = tib %>% 
#   gather(key = level, value = improvement, lev1, lev2, lev3) %>% 
#   group_by(level) %>% 
#   summarise(kao = cor.test(kao, improvement, method = "spearman")$p.val,
#             CL1o = cor.test(CL1o, improvement, method = "spearman")$p.val,
#             V1o = cor.test(V1o, improvement, method = "spearman")$p.val,
#             kad = cor.test(kad, improvement, method = "spearman")$p.val,
#             CLd = cor.test(CLd, improvement, method = "spearman")$p.val,
#             V2d = cor.test(V2d, improvement, method = "spearman")$p.val) %>% 
#   gather(key = pkParam, value = pval, -level)
# 
# spearManCors = full_join(correlations, corpval)
# 
# spearManCors %>% 
#   filter(level == "lev1") %>%
#   arrange(abs(correlation)) %>% 
#   mutate(flag = 1*(pval < .005), flag = recode(flag, `1` = "p < 0.0001", `0` = "")) %>% 
#   ggplot(aes(x = reorder(pkParam, correlation), y = correlation)) +
#   geom_col(fill = "dark grey") +
#   geom_text(aes(label = flag, y = correlation +sign(correlation)*.05)) +
#   # scale_x_discrete(name = "PK Parameters", # uncomment for main figure
#   #                  labels = c(expression(V[1*o]), expression(V[1*d]),
#   #                             expression(CL[1*d]), expression(k[a*d]),
#   #                             expression(k[a*o]), expression(CL[1*o]))) +
#   labs(x = "PK Parameters", y = "Spearman Correlation") +
#   theme_bw(base_size = 15)
# # ggsave(filename = "../Results_101919/pkImprovementCorrelation.pdf", width = 6, height = 5)
# 
# levelslab = c("lev1" = "Level 1", "lev2"= "Level 2", "lev3" = "Level 3")
# drugslab = c("V1o" = "Osimertinib", "V2d" = "Dacomitinib")
# 
# tib %>% 
#   gather(key = level, value = improvement, lev1, lev2, lev3) %>% 
#   gather(key = drug, value = V, V1o, V2d) %>% 
#   ggplot(aes(x = V, y = improvement)) +
#   geom_point(alpha = .3, color = "midnightblue") +
#   facet_grid(level~drug, scales = "free_x", 
#              labeller = labeller(level = levelslab, drug = drugslab))+
#   scale_y_continuous(name = "") +
#   labs(x = "Volume of distribution") +
#   theme_bw(base_size = 15)
# # ggsave(filename = "../Results_101919/volumeScatterAndImprovement_Corr8_102019.pdf", height = 8, width = 7)
# 
# drugslab = c("CL1o" = "Osimertinib", "CLd" = "Dacomitinib")
# tib %>% 
#   gather(key = level, value = improvement, lev1, lev2, lev3) %>% 
#   gather(key = drug, value = Cl, CL1o, CLd) %>% 
#   ggplot(aes(x = Cl, y = improvement)) +
#   geom_point(alpha = .3, color = "midnightblue") +
#   facet_grid(level~drug, scales = "free_x", 
#              labeller = labeller(level = levelslab, drug = drugslab))+
#   scale_y_continuous(name = "Relative improvement", labels = scales::percent) +
#   labs(x = "Clearance of volume") +
#   theme_bw(base_size = 15) +
#   theme(strip.text.y = element_blank())
# ggsave(filename = "../Results_10092019/Figures/clearanceScatterAndImprovement.pdf", height = 8, width = 7)
