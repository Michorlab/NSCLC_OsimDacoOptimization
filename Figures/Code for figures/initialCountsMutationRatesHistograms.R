#####
# Author: Kamrine Poels
# Description: Constructs a histogram of the initial cell counts and mutation rates
#####

library(tidyverse)

# Simulate initial conditions
N = 1000
set.seed(548331)
bsld = rexp(N, rate = log(2)/6)+1
n0_init = bsld*10^8
p01 = 10^rnorm(N, -4, .08)*(runif(N)<.8) # 80 % of patients had .001-01% frequency T790m
p02 = 10^rnorm(N, -6, .1)*(runif(N)<.05) # ~1% of patients with NRAS, increased proportion
p03 = rep(0,N) #(rnorm(N, 0.0495, 0.011)/1000)*(runif(N)<.1 | (runif(N)<.4 & p01>0)) # C797s dependent on p01
# Set cell type from initial population and sampled proportion
n1_init = round(n0_init*p01)
n2_init = round(n0_init*p02)
n3_init = round(n0_init*p03)
n0_init = n0_init-n1_init-n2_init-n3_init

initCount = tibble("exon19del" = n0_init, "t790m" = n1_init, "nras" = n2_init, "c797s" = n3_init)
initCount %>% 
  gather(key = "type", value = "count") %>% 
  ggplot(aes(x = count, fill = type)) +
  geom_histogram(position = "identity", alpha = .4) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "Cell Count",
       y = "Number of Simulated Patients")+
  scale_fill_manual(name = "Cell Type", labels = c("Exon 19 del", "NRAS", "T790M"), 
                    values = c("blue", "green4", "red")) + 
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.position = c(.15,.65),
        legend.box = "vertical",
        legend.box.background = element_rect(colour = "black"))
# ggsave("../initialCount.pdf", width = 6, height = 3)

tibble(ID = 1:N, count = n0_init) %>% 
  ggplot(aes(x =  log(count,10))) +
  geom_histogram(color = "black", fill = "grey") +
  scale_x_continuous(name = "Initial cell count", labels = scales::math_format(expr = 10^.x)) +
  scale_y_continuous("Number of patients") +
  theme_minimal()
ggsave("../Figures/initialCellCount.pdf", width = 6, height = 4)  


#### Mutation rates
# Sample rates for NRAS
munras = (10/6^(1/7))^rnorm(N, -11, 1/qnorm(.995))
# Create data frame with all rates for point mutations
mus = tibble(T790M = 10^rnorm(N, -7, 1/qnorm(.95)),
            NRAS = munras,
            C797S = 10^rnorm(N, -9, 1/qnorm(.95)))
mus %>% 
  gather(key = Mutation, value = Rate) %>% 
  ggplot(aes(x = Rate, fill = Mutation)) +
  geom_density(alpha = .5) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(y = "Density", title = "Mutation rates per resistance type")
# ggsave("../../Figures/mutationRates.pdf", width = 7, height = 5)
