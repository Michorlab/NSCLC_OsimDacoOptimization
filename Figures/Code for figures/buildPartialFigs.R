library(tidyverse)

setwd("Figures/Code for figures/")

### Toxicity constraint from Fig 1a
drug1 = 0:10; drug2 = 0:10
tibble(drug1, drug2) %>% 
  expand(drug1, drug2) %>% 
  mutate(z = exp(-1/drug1-2/drug2-4/(drug1*drug2))) %>% 
  ggplot(aes(x = drug1, y = drug2, fill = z)) +
  geom_tile(show.legend = F) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_void()
ggsave("toxicityConstraintFig1.pdf", width = 3, height = 2.5)

### Predicted outcome in Fig 1a
tibble(y = rnorm(10)+.2) %>% 
  arrange(y) %>% 
  mutate(x = 1:10, imp = 1*(y > -.2)+ 1*(y > .25))%>%
  ggplot(aes(x, y, fill = as.factor(imp))) +
  geom_bar(stat = "identity", show.legend = F) +
  scale_fill_discrete(h = c(0,180), c = 100, l = 70) + 
  theme_void()
ggsave("waterfallPlotFig1.pdf", width = 3, height = 2.5)
