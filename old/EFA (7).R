library(tidyverse)
theme_set(theme_bw())

ket <- read_tsv("data.csv")

colnames(ket)[1:163] -> items

ket %>% 
  filter(country == "US") %>% 
  mutate_at(.vars = items, ~ na_if(., 0)) %>% 
  drop_na() %>% 
  select(items) %>% 
  mutate(id = 1:nrow(.)) %>% 
  mutate_at(vars(-id), ~ . -1) -> ket_us

ket_us %>%
  mutate_at(vars(A8, A9, A10),
            ~ recode(
              .,
              "0" = "4",
              "1" = "3",
              "2" = "2",
              "3" = "1",
              "4" = "0"
            ) %>% as.numeric()) -> ket_us

colnames(ket_us)

fan <- factanal(ket_us %>% select(-id), factors = 16)

fan$loadings
fan$uniquenesses %>% sort()

factanal(ket_us %>% select(-id), factors = 16, rotation = "promax")



factanal(ket_us %>% select(-id), factors = 8, rotation = "promax")


fan$scores




pizza <- read_csv('https://raw.githubusercontent.com/angelgardt/hseuxlab-andan/master/Pizza.csv')


str(pizza)

fan <- factanal(pizza %>% select(-id, -brand), factors = 3, scores = 'regression') #укажем scores, чтобы сохранились сами значения факторов
fan

fan2 <- factanal(pizza %>% select(-id, -brand), factors = 2, scores = 'regression') #укажем scores, чтобы сохранились сами значения факторов
fan2$scores

pizza %>% 
  ggplot(aes(fan2$scores[,1], fan2$scores[,2], color = brand)) +
  geom_point()



