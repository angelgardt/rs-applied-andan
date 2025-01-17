# install.packages("tidyverse")

library(tidyverse)

getwd()

netflix <- read_csv("netflix_titles.csv")
# rm(neflix)

View(netflix)
str(netflix)

nrow(netflix)
ncol(netflix)

netflix$type
unique(netflix$type)
table(netflix$type)

sort(table(netflix$country))
max(table(netflix$country))

sqrt(cos(sin(4 * 8)))

sin(5)

5 %>% sin()

4 * 8 %>% 
  sin() %>% 
  cos() %>% 
  sqrt()

netflix %>% 
  filter(type != "TV Show") %>% 
  group_by(country) %>% 
  summarise(n = n()) %>% # class()
  # arrange(n)
  arrange(desc(n))

netflix %>% 
  filter(type == "TV Show") %>% 
  group_by(country) %>% 
  summarise(n = n()) %>% # class()
  arrange(n)

netflix %>% 
  filter(type == "Movie") %>% 
  select(release_year) %>% 
  arrange(release_year)

ket <- read_tsv("data.csv")
str(ket)
View(ket)

# read.csv("data.csv", sep = "\t")

ket %>% 
  select_at(vars(starts_with("A"))) %>% 
  mutate(sum = A1 + A2 + A3 + 
           A4 + A5 + A6 + A7 + 
           A8 + A9 + A10) -> ket_sum

max(ket_sum$sum)

ket_sum %>% 
  summarise(mean_ket = mean(sum),
            median_ket = median(sum),
            median_a1 = median(A1))


View(ket)

ket %>% 
  pivot_longer(cols = -id,
               names_to = "item", 
               values_to = "score") %>% 
  group_by(item) %>% 
  summarise(mean = mean(score),
            median = median(score),
            sd = sd(score),
            n = n(),
            max = max(score),
            min = min(score))



