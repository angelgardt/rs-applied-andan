library(tidyverse)
theme_set(theme_bw())

### TAIA

taia <- read_csv(
  sprintf(
    "https://docs.google.com/uc?id=%s&export=download",
    "1L-6pG9eaJz09ILrG6sKipnHoOxiU_T2S"
  )
)
str(taia)

EFAtools::BARTLETT(taia)
EFAtools::KMO(taia)

factanal(taia, 
         factors = 6, 
         scores = 'regression')

factanal(taia, 
         factors = 6, 
         scores = 'regression',
         rotation = "promax") 

factanal(taia, 
         factors = 5, 
         scores = 'regression',
         rotation = "varimax") 

factanal(taia, 
         factors = 5, 
         scores = 'regression',
         rotation = "promax") 



### PIZZA

pizza <- read_csv('https://raw.githubusercontent.com/angelgardt/hseuxlab-andan/master/Pizza.csv')
str(pizza)

pizza %>% select(-id, -brand) -> pizza_efa

EFAtools::BARTLETT(pizza_efa)
EFAtools::KMO(pizza_efa)

factanal(pizza_efa, 
         factors = 5, 
         scores = 'regression') 

factanal(pizza_efa, 
         factors = 4, 
         scores = 'regression') 

fan <- factanal(pizza_efa, 
                factors = 3, 
                scores = 'regression') 
# укажем scores, чтобы сохранились сами значения факторов
fan

fan2 <- factanal(pizza_efa, 
                 factors = 2, 
                 scores = 'regression')
fan2
fan2$scores

pizza %>% 
  ggplot(aes(fan2$scores[,1], 
             fan2$scores[,2],
             color = brand)) +
  geom_point()
