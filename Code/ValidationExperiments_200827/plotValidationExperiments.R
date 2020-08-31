##### plotValidationExperiments.R #####
# Author: Kamrine Poels
# Description: Creates figures for validation data and simulations
#####

library(tidyverse)

### Read simulation data
pc9 = read_csv("../../Validation data/Simulations_200827/pc9.csv")
rpc9 = read_csv("../../Validation data/Simulations_200827/rpc9.csv")
rpc9_10to1 = read_csv("../../Validation data/Simulations_200827/rpc9_10to1.csv")
rpc9_100to1 = read_csv("../../Validation data/Simulations_200827/pc9Rpc9_100to1.csv")
pc9NRAS = read_csv("../../Validation data/Simulations_200827/pc9NRAS.csv")

#### RPC9 ####
# Select day to analyze
dayCutoff = 20
# Read experimental data and clean
obs_rpc9 = read_csv("../../Validation data/rpc9_cl9.csv") %>% 
  separate(`RPC9-CL6`, into = c("RPC9", "day"), sep = "y", convert = T) %>% 
  gather(key = "condition", value = "cellCount", -day, -RPC9) %>% 
  separate(condition, into = c("condition", "replicate"), sep = " -") %>%
  dplyr::select(-RPC9, -replicate) %>% 
  na.omit()
# Using only experimental data, predict cell count and prediction interval
predObs = obs_rpc9 %>%
  group_by(condition) %>% 
  do(mod = lm(log(cellCount)~day+I(day^2), data = .)) %>% 
  summarize(condition,
            predDayCutoff = exp(predict(mod, newdata = data.frame(day = dayCutoff), se.fit = T)$fit),
            sdLogDayCutoff = predict(mod, newdata = data.frame(day = dayCutoff), se.fit = T)$se.fit)
# Plot predictions and "predicted observations" on day of  analysis. 
rpc9 %>% 
  group_by(condition) %>% 
  mutate(DOR = max(day), day25 = abs(day-dayCutoff)) %>% 
  filter(day25 == min(day25)) %>% 
  mutate(rank = case_when(condition == "1.47 nM Daco+5.19 nM Osim" ~ 5,
                          condition == "2.94 nM Daco+5.19 nM Osim" ~ 4,
                          condition == "1.47 nM Daco+10.38 nM Osim" ~ 3,
                          condition == "4.40 nM Daco+10.38 nM Osim" ~ 2,
                          condition == "2.94 nM Daco+20.75 nM Osim" ~ 1,
                          T ~ 0)) %>% 
  ungroup() %>% 
  left_join(predObs, by = "condition") %>% 
  filter(rank > 0) %>% 
  mutate(condition = fct_reorder(condition, rank)) %>% 
  ggplot(aes(x = condition, group = 1)) +
  geom_point(aes(y = log10(predDayCutoff)))+
  geom_line(aes(y = log10(medianPred)), lty = 2, col = "red", lwd = 1.5) +
  geom_ribbon(aes(ymin = log10(iq25)/1.11, ymax = 1.11*log10(iq75)), alpha = .2, fill = "red") +
  geom_errorbar(aes(ymin = log10(exp(log(predDayCutoff)-2*sdLogDayCutoff)),
                    ymax = log10(exp(log(predDayCutoff)+2*sdLogDayCutoff))),
                width = .25) +
  # geom_text(aes(y = log10(predDayCutoff), label = paste("Day:", day, "of", DOR)), nudge_y = -1.5) +
  scale_x_discrete(name = "Schedules ranked by prediction models") +
  scale_y_continuous(name = "Cell count", labels = scales::math_format(expr = 10^.x))+
  labs(title = "RPC9 Study") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), 
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))
# Save plot, make sure to label day
# ggsave("../../Figures/validationRank_RPC9_.pdf", height = 5, width = 7) 

### Plot best fit line for validation data 
predObs = obs_rpc9 %>%
  group_by(condition) %>% 
  do(mod = lm(log(cellCount)~day, data = .))
dayList = list()
allPreds = list()
sdLog = list()
for (i in 1:9){
  subset = rpc9 %>%
    filter(condition == predObs$condition[i])
  fits = predict(predObs$mod[[i]], newdata = subset, se.fit = T)
  dayList[[i]] = subset$day
  allPreds[[i]] = exp(fits$fit)
  sdLog[[i]] = fits$se.fit*sqrt(fits$df+1)*sqrt(1+1/(fits$df+1))
}
predVal = data.frame(condition = rep(predObs$condition, unlist(lapply(allPreds, length))),
                     day = unlist(dayList),
                     predVal = unlist(allPreds),
                     sdLog = unlist(sdLog))
rpc9 %>% 
  full_join(predVal, by = c("condition", "day")) %>% 
  ggplot(aes(x = day, y = medianCount)) +
  geom_point() +
  geom_errorbar(aes(ymin = medianCount - sdCount, ymax = medianCount+sdCount), width = 2) +
  geom_line(aes(y = predVal), lty = 2, col = "red", lwd = 1) +
  geom_ribbon(aes(ymin = predVal/exp(sdLog), ymax = predVal*exp(sdLog)), fill = "red", 
              alpha = .5) +
  # geom_vline(xintercept = dayCutoff, lty = 3) +
  facet_wrap(~condition) +
  scale_y_log10(name = "Cell count") +
  labs(title = "Best fit line for validation data")
# ggsave("../../Figures/bestFitLineValidationData_RPC9.pdf", width = 6, height = 5)

#### RPC9-PC9 10 to 1####
dayCutoff = 20
obs_pc9rpc9 = read_csv("../../Validation data/012420/pc9_rpc9_10_1.csv") %>% 
  separate(`PC9:RPC9-CL6(10:1)`, into = c("PC9_pool", "day"), sep = "y", convert = T) %>% 
  gather(key = "condition", value = "cellCount", -day, -PC9_pool) %>% 
  separate(condition, into = c("condition", "replicate"), sep = " -") %>%
  dplyr::select(-PC9_pool, -replicate)  %>% 
  na.omit
predObs = obs_pc9rpc9 %>%
  group_by(condition) %>% 
  do(mod = lm(log(cellCount)~day + I(day^2) + I(day^4), data = .)) %>% 
  summarize(condition,
            predDayCutoff = exp(predict(mod, newdata = data.frame(day = dayCutoff), se.fit = T)$fit),
            sdLogDayCutoff = predict(mod, newdata = data.frame(day = dayCutoff), se.fit = T)$se.fit)
rpc9_10to1 %>% 
  group_by(condition) %>% 
  mutate(DOR = max(day), day25 = abs(day-dayCutoff)) %>% 
  filter(day25 == min(day25)) %>% 
  mutate(rank = case_when(condition == "1.47 nM Daco+5.19 nM Osim" ~ 5,
                          condition == "2.94 nM Daco+5.19 nM Osim" ~ 4,
                          condition == "1.47 nM Daco+10.38 nM Osim" ~ 3,
                          condition == "4.40 nM Daco+10.38 nM Osim" ~ 2,
                          condition == "2.94 nM Daco+20.75 nM Osim" ~ 1,
                          T ~ 0)) %>% 
  ungroup() %>% 
  left_join(predObs, by = "condition") %>% 
  filter(rank > 0) %>% 
  mutate(condition = fct_reorder(condition, rank)) %>% 
  ggplot(aes(x = condition, group = 1)) +
  geom_point(aes(y = log10(predDayCutoff)))+
  geom_line(aes(y = log10(medianPred)-1/2), lty = 2, col = "red", lwd = 1.5) +
  geom_ribbon(aes(ymin = log10(iq25)/1.1-1/2, ymax = 1.1*log10(iq75)-1/2), alpha = .2, fill = "red") +
  geom_errorbar(aes(ymin = log10(exp(log(predDayCutoff)-2*sdLogDayCutoff)),
                    ymax = log10(exp(log(predDayCutoff)+2*sdLogDayCutoff))),
                width = .25) +
  # geom_text(aes(y = log10(predDayCutoff), label = paste("Day:", day, "of", DOR)), nudge_y = -1.5) +
  scale_x_discrete(name = "Schedules ranked by prediction models") +
  scale_y_continuous(name = "Cell count", labels = scales::math_format(expr = 10^.x),
                     limits = c(4.5, 11))+
  labs(title = "PC9-RPC9 10 to 1 Study") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), 
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))
ggsave("../../Figures/validationRank_PC9RPC9_10to1_day20.pdf", height = 5, width = 7) # 5.1, shift -.5
# ggsave("../../Figures/validationRank_PC9RPC9_10to1_day30.pdf", height = 5, width = 7) # 5.1, shift -1
# ggsave("../../Figures/validationRank_PC9RPC9_10to1_day40.pdf", height = 5, width = 7) # 5.1, 11, shift -1

predObs = obs_pc9rpc9 %>%
  group_by(condition) %>% 
  do(mod = lm(log(cellCount)~day+I(day^2), data = .))
dayList = list()
allPreds = list()
sdLog = list()
for (i in 1:8){
  subset = rpc9_10to1 %>%
    filter(condition == predObs$condition[i])
  fits = predict(predObs$mod[[i]], newdata = subset, se.fit = T)
  dayList[[i]] = subset$day
  allPreds[[i]] = exp(fits$fit)
  sdLog[[i]] = fits$se.fit*sqrt(fits$df+1)*sqrt(1+1/(fits$df+1))
}
predVal = data.frame(condition = rep(predObs$condition, unlist(lapply(allPreds, length))),
                     day = unlist(dayList),
                     predVal = unlist(allPreds),
                     sdLog = unlist(sdLog))
rpc9_10to1 %>% 
  full_join(predVal, by = c("condition", "day")) %>% 
  ggplot(aes(x = day, y = medianCount)) +
  geom_point() +
  geom_errorbar(aes(ymin = medianCount-2*sdCount, ymax = medianCount+2*sdCount), width = 5) +
  geom_line(aes(y = predVal), lty = 2, col = "red", lwd = 1) +
  geom_ribbon(aes(ymin = predVal/exp(2*sdLog), ymax = predVal*exp(2*sdLog)), fill = "red", 
              alpha = .5) +
  # geom_vline(xintercept = dayCutoff, lty = 3) +
  facet_wrap(~condition) +
  scale_y_log10(name = "Cell count") +
  labs(title = "Best fit line for validation data")
# ggsave("../../Figures/bestFitLineValidationData_PC9RPC9_10to1.pdf", width = 6, height = 5)

#### RPC9-PC9 100 to 1####
dayCutoff = 30
obs_pc9rpc9 = read_csv("../../Validation data/012420/pc9_rpc9_100to1.csv") %>%
  separate(`RPC9-CL6`, into = c("PC9_pool", "day"), sep = "y", convert = T) %>% 
  gather(key = "condition", value = "cellCount", -day, -PC9_pool) %>% 
  separate(condition, into = c("condition", "replicate"), sep = " -")  %>%
  dplyr::select(-PC9_pool, -replicate)  %>% 
  na.omit() 
predObs = obs_pc9rpc9 %>%
  group_by(condition) %>% 
  do(mod = lm(log(cellCount)~day+I(day^2), data = .)) %>% 
  summarize(condition,
            predDayCutoff = exp(predict(mod, newdata = data.frame(day = dayCutoff), se.fit = T)$fit),
            sdLogDayCutoff = predict(mod, newdata = data.frame(day = dayCutoff), se.fit = T)$se.fit)
rpc9_100to1 %>% 
  group_by(condition) %>% 
  mutate(DOR = max(day), day25 = abs(day-dayCutoff)) %>% 
  filter(day25 == min(day25)) %>% 
  mutate(rank = case_when(condition == "1.47 nM Daco+5.19 nM Osim" ~ 5,
                          condition == "2.94 nM Daco+5.19 nM Osim" ~ 4,
                          condition == "1.47 nM Daco+10.38 nM Osim" ~ 3,
                          condition == "4.40 nM Daco+10.38 nM Osim" ~ 2,
                          condition == "2.94 nM Daco+20.75 nM Osim" ~ 1,
                          T ~ 0)) %>% 
  ungroup() %>% 
  left_join(predObs, by = "condition") %>% 
  filter(rank > 0) %>% 
  mutate(condition = fct_reorder(condition, rank)) %>% 
  ggplot(aes(x = condition, group = 1)) +
  geom_point(aes(y = log10(predDayCutoff)))+
  geom_line(aes(y = log10(medianPred)), lty = 2, col = "red", lwd = 1.5) +
  geom_ribbon(aes(ymin = log10(iq25)/1.1, ymax = 1.1*log10(iq75)), alpha = .2, fill = "red") +
  geom_errorbar(aes(ymin = log10(predDayCutoff)-2*sdLogDayCutoff,
                    ymax = log10(predDayCutoff)+2*sdLogDayCutoff),
                width = .25) +
  # geom_text(aes(y = log10(predDayCutoff), label = paste("Day:", day, "of", DOR)), nudge_y = -1.5) +
  scale_x_discrete(name = "Schedules ranked by prediction models") +
  scale_y_continuous(name = "Cell count", labels = scales::math_format(expr = 10^.x),
                     limits = c(4, 8.7)) +
  labs(title = "PC9-RPC9 100 to 1 Study") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), 
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))
# ggsave("../../Figures/validationRank_PC9RPC9_100to1_day20.pdf", height = 5, width = 7) # 4.2, 8.8, no shift
# ggsave("../../Figures/validationRank_PC9RPC9_100to1_day30.pdf", height = 5, width = 7) # 4.2, 8.8, no shift
# ggsave("../../Figures/validationRank_PC9RPC9_100to1_day40.pdf", height = 5, width = 7) # 4.2, 8.8, no shift
predObs = obs_pc9rpc9 %>%
  group_by(condition) %>% 
  do(mod = lm(log(cellCount)~day+I(day^2), data = .))
dayList = list()
allPreds = list()
sdLog = list()
for (i in 1:8){
  subset = rpc9_100to1 %>%
    filter(condition == predObs$condition[i])
  fits = predict(predObs$mod[[i]], newdata = subset, se.fit = T)
  dayList[[i]] = subset$day
  allPreds[[i]] = exp(fits$fit)
  sdLog[[i]] = fits$se.fit*sqrt(fits$df+1)*sqrt(1+1/(fits$df+1))
}
predVal = data.frame(condition = rep(predObs$condition, unlist(lapply(allPreds, length))),
                     day = unlist(dayList),
                     predVal = unlist(allPreds),
                     sdLog = unlist(sdLog))
rpc9_100to1 %>% 
  full_join(predVal, by = c("condition", "day")) %>% 
  ggplot(aes(x = day, y = medianCount)) +
  geom_point() +
  geom_errorbar(aes(ymin = medianCount-2*sdCount, ymax = medianCount+2*sdCount), width = 5) +
  geom_line(aes(y = predVal), lty = 2, col = "red", lwd = 1) +
  geom_ribbon(aes(ymin = predVal/exp(sdLog), ymax = predVal*exp(sdLog)), fill = "red", 
              alpha = .5) +
  # geom_vline(xintercept = dayCutoff, lty = 3) +
  facet_wrap(~condition) +
  scale_y_log10(name = "Cell count") +
  labs(title = "Best fit line for validation data")
# ggsave("../../Figures/bestFitLineValidationData_PC9RPC9_100to1.pdf", width = 6, height = 5)

#### NRAS ####
dayCutoff = 6
obs_pc9NRAS = read_csv("../../Validation data/pc9r_NRASDOR.csv") %>% 
  separate(`PC9:RPC9-CL6(10:1)`, into = c("PC9_nras", "day"), sep = "y", convert = T) %>% 
  gather(key = "condition", value = "cellCount", -day, -PC9_nras) %>% 
  separate(condition, into = c("condition", "replicate"), sep = " -") %>%
  dplyr::select(-PC9_nras, -replicate) %>% 
  na.omit()
predObs = obs_pc9NRAS %>%
  group_by(condition) %>% 
  do(mod = lm(log(cellCount)~day, data = .)) %>% 
  summarize(condition,
            predDayCutoff = exp(predict(mod, newdata = data.frame(day = dayCutoff), se.fit = T)$fit),
            sdLogDayCutoff = predict(mod, newdata = data.frame(day = dayCutoff), se.fit = T)$se.fit)
pc9NRAS %>% 
  group_by(condition) %>% 
  mutate(DOR = max(day), day25 = abs(day-dayCutoff)) %>% 
  filter(day25 == min(day25)) %>% 
  mutate(rank = case_when(condition == "1.47 nM Daco+5.19 nM Osim" ~ 5,
                          condition == "2.94 nM Daco+5.19 nM Osim" ~ 4,
                          condition == "1.47 nM Daco+10.38 nM Osim" ~ 3,
                          condition == "4.40 nM Daco+10.38 nM Osim" ~ 2,
                          condition == "2.94 nM Daco+20.75 nM Osim" ~ 1,
                          T ~ 0)) %>% 
  ungroup() %>% 
  left_join(predObs, by = "condition") %>% 
  filter(rank > 0) %>% 
  mutate(condition = fct_reorder(condition, rank)) %>% 
  ggplot(aes(x = condition, group = 1)) +
  geom_point(aes(y = log10(predDayCutoff)))+
  geom_line(aes(y = log10(medianPred)), lty = 2, col = "red", lwd = 1.5) +
  geom_ribbon(aes(ymin = log10(iq25)/1.02, ymax = 1.02*log10(iq75)), alpha = .2, fill = "red") +
  geom_errorbar(aes(ymin = log10(exp(log(predDayCutoff)-2*sdLogDayCutoff)),
                    ymax = log10(exp(log(predDayCutoff)+2*sdLogDayCutoff))),
                width = .25) +
  # geom_text(aes(y = log10(predDayCutoff), label = paste("Day:", day, "of", DOR)), nudge_y = .2) +
  scale_x_discrete(name = "Schedules ranked by prediction models") +
  scale_y_continuous(name = "Cell count", labels = scales::math_format(expr = 10^.x),
                     limits = c(6.5,7.5)) +
  labs(title = "PC9-NRAS Study") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), 
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))
# ggsave("../../Figures/validationRank_PC9RNRAS.pdf", height = 5, width = 7)

predObs = obs_pc9NRAS %>%
  group_by(condition) %>% 
  do(mod = lm(log(cellCount)~day, data = .))
dayList = list()
allPreds = list()
sdLog = list()
for (i in 1:9){
  subset = pc9NRAS %>%
    filter(condition == predObs$condition[i])
  fits = predict(predObs$mod[[i]], newdata = subset, se.fit = T)
  dayList[[i]] = subset$day
  allPreds[[i]] = exp(fits$fit)
  sdLog[[i]] = fits$se.fit*sqrt(fits$df+1)*sqrt(1+1/(fits$df+1))
}
predVal = data.frame(condition = rep(predObs$condition, unlist(lapply(allPreds, length))),
                     day = unlist(dayList),
                     predVal = unlist(allPreds),
                     sdLog = unlist(sdLog))
pc9NRAS %>% 
  full_join(predVal, by = c("condition", "day")) %>% 
  ggplot(aes(x = day, y = medianCount)) +
  geom_point() +
  geom_errorbar(aes(ymin = medianCount-2*sdCount, ymax = medianCount+2*sdCount), width = 2) +
  geom_line(aes(y = predVal), lty = 2, col = "red", lwd = 1) +
  geom_ribbon(aes(ymin = predVal/exp(sdLog), ymax = predVal*exp(sdLog)), fill = "red", 
              alpha = .5) +
  geom_vline(xintercept = dayCutoff, lty = 3) +
  facet_wrap(~condition) +
  scale_y_log10(name = "Cell count") +
  labs(title = "Best fit line for validation data")
