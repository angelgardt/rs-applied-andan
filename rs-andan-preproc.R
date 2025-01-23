# install.packages("tidyverse", dependencies = TRUE)

library(tidyverse)

## working dir
# getwd()
# setwd()

## get data
## https://www.kaggle.com/datasets/mylesoneill/game-of-thrones
deaths <- read_csv("data/character-deaths.csv")
str(deaths)

View(deaths)

nrow(deaths)
ncol(deaths)

deaths$Allegiances

unique(deaths$Allegiances)
unique(deaths$Gender)
class(deaths$Gender)

table(deaths$Gender)
sum(deaths$Gender) / length(deaths$Gender)
mean(deaths$Gender)

colnames(deaths)
# table(deaths$Book of Death)
table(deaths$`Book of Death`)

sort(table(deaths$Allegiances))
max(table(deaths$Allegiances))


## what is pipe and how it works
cos(0)
0 %>% cos()


sqrt(cos(sin(4 * 8)))

(4 * 8) %>% 
  sin() %>% 
  cos() %>% 
  sqrt()  


colnames(deaths)
deaths %>% 
  rename("Death_Year" = "Death Year",
         "Book_of_Death" = "Book of Death",
         "Death_Chapter" = "Death Chapter",
         "Book_Intro_Chapter" = "Book Intro Chapter") %>% 
  filter(Allegiances == "None") -> deaths_no_allegiance

# deaths %>% 
#   rename_all(.funs = str_replace_all, 
#              pattern = " ", 
#              replacement = "_") %>% 
#   rename_all(tolower) %>% 
#   colnames()

# colnames(deaths_no_allegiance)
# colnames(deaths)


mean(deaths_no_allegiance$Death_Year)
mean(deaths_no_allegiance$Death_Year, na.rm = TRUE)

deaths_no_allegiance %>% 
  summarise(n = n(),
            year_death_mean = mean(Death_Year),
            year_death_sd = sd(Death_Year),
            .by = Book_of_Death) %>% 
  #arrange(Book_of_Death)
  arrange(desc(Book_of_Death))


deaths_no_allegiance %>% 
  summarise(n = n(),
            chapter_death_median = median(Death_Chapter, na.rm = TRUE),
            chapter_death_iqr = IQR(Death_Chapter, na.rm = TRUE),
            .by = Book_of_Death) %>% 
  arrange(desc(Book_of_Death))


## NAs
deaths$Allegiances == NA

is.na(deaths$Allegiances)
is.na(deaths$Allegiances) %>% sum()
deaths$Allegiances %>% is.na() %>% sum()


is.na(deaths$`Death Year`) %>% sum()

deaths %>% sapply(is.na) %>% apply(2, sum)
deaths %>% sapply(function(x) sum(is.na(x)))


deaths %>% drop_na() # %>% nrow()
deaths %>% 
  filter(!is.na(`Book of Death`))
deaths %>% 
  filter(!(is.na(`Book of Death`) & is.na(`Death Chapter`) & is.na(`Death Year`)))


deaths %>% 
  mutate(Allegiances_Binary = ifelse(Allegiances == "None", "No", "Yes")) %>% 
  # distinct(Allegiances_Binary, Nobility)
  summarise(n = n(),
            .by = c(Allegiances_Binary, Nobility))

deaths %>% 
  mutate(Allegiances = ifelse(Allegiances == "House Stark", "Stark", Allegiances)) %>% 
  filter(Allegiances == "Stark") %>% 
  summarise(n = n(),
            .by = c(Allegiances, Gender))

