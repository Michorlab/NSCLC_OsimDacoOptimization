#clearing the workspace
rm(list=ls())
graphics.off()
# This gets rid of that weird grepl warning message:
# Sys.setlocale(locale="C")
# setwd("Code/osimertinib_dacomitinib/")
library("deSolve")
library("ggplot2")
library("foreach")
library("doParallel")
library("doRNG")
source("library/r_general/ubiquity.r")
options(error=traceback)
options(show.error.locations = T)

# Rebuilding the system (R scripts and compiling C code)
build_system()

# loading the different functions
source("transient/auto_rcomponents.r");

# Loading the system information
cfg = system_fetch_cfg()

# set name   | Description
# -------------------------------------------------------
# default    | mAb in Humans
# Selecting the default paraemter set
cfg = system_select_set(cfg, 'default')

# Fetching the parameter values
parameters = system_fetch_parameters(cfg)

# To overwrite the default dosing uncomment
cfg = system_zero_inputs(cfg)
# Osimertinib dosing times and dosing values
cfg = system_set_bolus(cfg, state = "Ato",
                           times  = seq(0, 24*7*50, 24),
                           values = rep(80, 351))
# Dacomitinib dosing times and dosing values
cfg = system_set_bolus(cfg, state = "Atd",
                       times = seq(0, 24*7*50, 24),
                       values = rep(30, 351))


# The following applies to both individual and stochastic simulations:
# Define the solver to use
cfg=system_set_option(cfg,group   = "simulation",
                          option  = "solver",
                          value   = "lsoda")

# # Specify the output times
cfg=system_set_option(cfg, group  = "simulation",
                           option = "output_times",
                           value  = seq(0, 24*7*50, 1))
# -------------------------------------------------------------------------
# # Stochastic Simulation:
cfg=system_set_option(cfg, group  = "stochastic",
                           option = "nsub",
                           value  = 1000) # number of subjects
                           

# Uncomment the following to parallelize the simulations
#
# cfg=system_set_option(cfg, group  = "simulation",
#                            option = "parallel",    
#                            value  = "multicore")
# 
# cfg=system_set_option(cfg, group  = "simulation",
#                            option = "compute_cores", 
#                            value  = detectCores() - 1)
  
som = simulate_subjects(parameters, cfg)
rm(parameters, cfg)
#####
# Make plots and save things
# graphics.off()
# osiFig = 
ggplot(som$tcsummary, aes(x=ts.weeks, y=o.C_osi.mean)) +
  geom_ribbon(aes(ymin=o.C_osi.lb_ci, ymax=o.C_osi.ub_ci), fill="lightblue",alpha=0.6)+
  geom_line(linetype="solid", size=0.7, color="blue")+
  geom_line(aes(x=ts.weeks, y=o.C_osi.ub_ci), linetype="dashed", size=0.2, color="blue")+
  geom_line(aes(x=ts.weeks, y=o.C_osi.lb_ci), linetype="dashed", size=0.2, color="blue")+
  geom_line(aes(x=ts.weeks, y=o.C_osi_met.mean), linetype="solid", size=.7, color="red")+
  geom_line(aes(x=ts.weeks, y=o.C_osi_met.ub_ci), linetype="dashed", size=.2, color="red")+
  geom_line(aes(x=ts.weeks, y=o.C_osi_met.lb_ci), linetype="dashed", size=.2, color="red")+
  geom_ribbon(aes(ymin=o.C_osi_met.lb_ci, ymax=o.C_osi_met.ub_ci), fill="pink",alpha=.6)+
  xlim(0,1) +
  labs(x = "Time (weeks)", y = "Concentration (nM)",
       title = "Osimertinib and Metabolite Concentration")+theme_classic()

# dacoFig = 
ggplot(som$tcsummary, aes(x=ts.days, y=o.C_daco_c.mean)) +
  geom_ribbon(aes(ymin=o.C_daco_c.lb_ci, ymax=o.C_daco_c.ub_ci), fill="lightblue",alpha=0.6)+
  geom_line(linetype="solid", size=0.7, color="blue")+
  geom_line(aes(y=o.C_daco_c.ub_ci), linetype="dashed", size=0.2, color="blue")+
  geom_line(aes(y=o.C_daco_c.lb_ci), linetype="dashed", size=0.2, color="blue")+
  geom_line(aes(y=o.C_daco_p.mean), linetype="solid", size=.7, color="red")+
  geom_line(aes(y=o.C_daco_p.ub_ci), linetype="dashed", size=.2, color="red")+
  geom_line(aes(y=o.C_daco_p.lb_ci), linetype="dashed", size=.2, color="red")+
  geom_ribbon(aes(ymin=o.C_daco_p.lb_ci, ymax=o.C_daco_p.ub_ci), fill="pink",alpha=.6)+
  xlim(0,7) +
  labs(x = "Time (days)", y = "Concentration (nM)",
       title = "Dacomitinib Central and Peripheral Compartments") + theme_classic()

# # pdf("../../Figures/osimertinib_pkConcentration.pdf", width = 8, height = 4, pointsize = .7)
# osiFig
# # dev.off()
# 
# # pdf("../../Figures/dacomitinib_pkConcentrationWithBreaks.pdf", width = 9, height = 4)
# dacoFig
# # dev.off()
# 

group2.1 = som
save(group2.1, file = "../../Concentrations/LighterPatients/30dacoQD_80osiQD.RData")