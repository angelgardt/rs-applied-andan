library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = "bottom")
library(ez)

share <- read_csv("https://raw.githubusercontent.com/angelgardt/mk_ggplot2/master/sharexp_data.csv")
str(share)

unique(share$trialtype)


share %>% 
  filter(trialtype != "both") %>% 
  mutate(setsize = as_factor(setsize)) -> share

share %>% apply(2, is.na) %>% apply(2, sum)

share %>% 
  ggplot(aes(setsize, time1)) +
  stat_summary(geom = "pointrange", 
               fun.data = mean_cl_boot)

ezANOVA(data = share, 
        dv = time1, 
        wid = id,
        within = setsize,
        detailed = TRUE) %>% 
  psychReport::aovEffectSize()


pairwise.t.test(share$time1, share$setsize, p.adjust.method = "bonf")


ezANOVA(data = share, 
        dv = time1, 
        wid = id,
        within = .(setsize, trialtype),
        detailed = TRUE) %>% 
  psychReport::aovEffectSize()

pairwise.t.test(share$time1, 
                interaction(share$setsize, share$trialtype), 
                p.adjust.method = "bonf")


share %>% 
  ggplot(aes(setsize, time1, color = trialtype)) +
  stat_summary(geom = "pointrange", 
               fun.data = mean_cl_boot,
               position = position_dodge(.3))





