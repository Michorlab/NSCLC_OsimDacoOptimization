#####
# Author: Kamrine Poels
# Description: Plots the initial cell count (with pre-existant resistance) of simulated patients
#####

patPops = tibble(exon19del = n0_init, T790M = n1_init, NRAS = n2_init, C797S = n3_init)
patProps = tibble(exon19del = 1-p01-p02-p03, T790M = p01, NRAS = p02, C797S = p03)



patProps %>% 
  mutate(ID = 1:1000) %>% 
  # filter(ID %% 10 == 0) %>%
  # mutate(propRes = T790M+NRAS+C797S) %>% 
  gather(key = type, value = propRes, T790M, NRAS) %>% 
  ggplot(aes(x = reorder(ID, propRes), y = propRes)) +
  geom_bar(stat = "identity", alpha = .8) +
  scale_y_continuous(name = "Pre-existence of resistance", labels = scales::percent)+
  theme_classic() +
  coord_flip() +
  scale_fill_manual(name = "Cell type",
                    values = c("darkgoldenrod2", "royalblue4", "green4", "firebrick")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(angle = -90, hjust = 1)) +
  facet_wrap(~type, scales = "free_x")
ggsave("../Figures/initialProportionsByPatient.pdf", width = 8, height = 5)
