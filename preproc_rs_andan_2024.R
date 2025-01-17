# install.packages("tidyverse")
library(tidyverse)

netflix <- read_csv("netflix_titles.csv")
str(netflix)
View(netflix)

nrow(netflix)
ncol(netflix)

netflix$type
netflix[5, ]

unique(netflix$type)
length(unique(netflix$type))
table(netflix$type)

sort(table(netflix$type), decreasing = TRUE)

max(table(netflix$country))

sqrt(cos(sin(4 * 8)))


(4 * 8) %>% 
  sin() %>% 
  cos() %>% 
  sqrt()  

netflix %>% 
  filter(type != "Movie") %>% 
  group_by(country) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


netflix %>% 
  filter(type == "Movie") %>% 
  select(release_year, title) %>% 
  arrange(desc(release_year))

netflix %>% 
  filter(type == "Movie") %>% 
  pull(release_year) %>% # mean()
  sort(decreasing = TRUE) %>% .[1]

netflix %>% 
  filter(type == "Movie") %>% 
  pull(release_year) %>%
  table()



