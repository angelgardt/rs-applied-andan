## install required packages
# install.packages("tidyverse", "moments")

## another way to install required packages, more safe
pkgs <- c("tidyverse", "moments")
install.packages(pkgs[!pkgs %in% installed.packages()])

## load required package
library(tidyverse)

## create subfolder for data files
# dir.create("data")

## read csv file, separator is comma --- data of GoT charaster deaths
## source: ## https://www.kaggle.com/datasets/mylesoneill/game-of-thrones
deaths <- read_csv("data/character-deaths.csv")
str(deaths) ## check structure
rm(deaths) ## remove object, we'll not use it in this script

## rm(list = ls()) ## remove all objects from environment -- use carefully!

## read another csv file --- 16PF questionnaire
## source: https://www.kaggle.com/datasets/tunguz/cattells-16-personality-factors
ket <- read_csv("data/ket.csv") ## pay attertion to message in console
str(ket) ## check structure --- see smth strange?

## read ket.csv with correct tab separator
ket <- read_tsv("data/ket.csv") ## rewrite ket object
str(ket) ## check structure again --- now looks good
View(ket) ## another way to check
head(ket) ## look at several (6 by default) first rows
tail(ket, n = 4) ## look at several (4 in this case) last rows

## get some basic info about the data
nrow(ket) ## number of rows
ncol(ket) ## number of columns
colnames(ket) ## names of columns
class(ket) ## wow, that's dataframe/tibble
sapply(ket, class) ## check data type of each column --- apply class function to each column of our data

## get some descriptive stats
## let's start with A1 column (A1 item in questionnaire)
mean(ket$A1) ## [arithmetic] mean
mean(ket$A1, trim = .2) ## trimmed mean

?mean ## what's the 'trim' argument in previous? let's see help about the 'mean' function
help(mean) ## or let's see help this way

var(ket$A1) ## variance (dispersion)
sd(ket$A1) ## standard deviation
median(ket$A1) ## median
quantile(ket$A1) ## by default --- return quartiles
quantile(ket$A1, probs = .20) ## you may tell R, what specific quantile you are interested in
max(ket$A1) ## get max value
min(ket$A1) ## get min value
table(ket$A1) ## get prop table, A1 is discrete var, it will work
table(ket$country) ## or same for countries --- doesn't look so good
sort(table(ket$country)) ## let's sort previous output (ascending by default) --- now looks better
sort(table(ket$country), decreasing = TRUE) ## you may sort it descending as well
ket$country %>% table() %>% sort(decreasing = TRUE) ## same as previous line, but tidyverse-styles with pipe

## or another way using group_by() %>% summarise()
ket %>% ## take data
  group_by(country) %>% ## group data by country
  summarise( ## calculate something
    n = n() ## number of observations
    ) %>% 
  # arrange(n) ## arrange (sort) aggregated data by new column 'n' --- ascending by default 
  arrange(desc(n)) ## arrange (sort) aggregated data by new column 'n' --- now descending

ket %>% 
  filter(country == "US") %>% nrow()
ket %>% 
  filter(country == "US" & country == "FR") %>% nrow()
ket %>% 
  filter(country == "US" | country == "RU") %>% nrow()
ket %>% 
  filter(age < 18) %>% nrow()
ket %>% 
  filter(age > 18 & age < 35) %>% nrow()
ket %>% 
  filter(age < 18 & age > 60) %>% nrow()


summary(ket) ## get basic descriptive stats for all columns


ket$A1 %>% is.na() %>% sum() ## check number of NA in A1 columns --- no NA, cool!
ket %>% sapply(is.na) %>% apply(2, sum) ## check number of NA for all columns
ket %>% sapply(function(x) x %>% is.na() %>% sum()) ## check number of NA for all columns another way --- same result


unique(ket$gender) ## check unique values of gender variable --- hmmm, need codebook…

## let's fix gender var
ket %>% 
  mutate(
    gender = ifelse(gender == 0, NA,
                    ifelse(gender == 1, "male",
                           ifelse(gender == 2, "female", "other")))
  ) %>% 
  pull(gender) %>% ## pull gender column as vector
  # is.na() %>% sum() ## check number of NA
  unique() ## check unique values
## we didn't save these changes!


## or another shorter way
ket %>% ## take data 
  mutate( ## change something
    gender = recode( ## save to gender (rewrite column) as follow [according codebook]
      gender, ## take gender var
      "0" = NA_character_, ## set NA for 0 --- recode function requires explicit type of NA
      "1" = "male", ## set 'male' for 1
      "2" = "female", ## set 'female' for 2
      "3" = "other" ## set 'other' for 3
    )
  ) -> ket ## save changes
ket %>% pull(gender) %>% unique() ## check new gender values

## let's remove NA
ket %>% drop_na() ## remove all rows that contains at least one NA
ket %>% drop_na() %>% nrow() ## check the number of rows after NA removing --- ok, appropriate
ket %>% drop_na() -> cattel ## save the data without NA to new object


## let's count desriptive stats for each item more stylish
## let's do it for the age first
cattel %>% ## take data
  summarise( ## calculate something
    n_age = n(), ## number of observations --- name output column as 'n_age'
    mean_age = mean(age), ## mean age --- name output column as 'mean_age'
    sd_age = sd(age), ## standard deviation of age  --- name output column as 'sd_age'
    max_age = max(age), ## max age --- name output column as 'max_age'
    min_age = min(age) ## min age --- name output column as 'min_age'
    )
## or simplier
cattel %>% ## take data
  summarise( ## calculate something
    n = n(), ## number of observations --- name output column as 'n'
    mean = mean(age), ## mean age --- name output column as 'mean'
    sd = sd(age), ## standard deviation of age  --- name output column as 'sd'
    max = max(age), ## max age --- name output column as 'max'
    min = min(age) ## min age --- name output column as 'min'
  )
## hmmm, look at max age

## breefly check basic graphs
hist(cattel$age) ## histogram
boxplot(cattel$age) ## boxplot

## what happens, if we set some threshold for the age?
cattel %>% # nrow()
  filter(age <= 100) %>% # nrow()
  pull(age) %>% boxplot()
## ok, more or less

cattel %>% filter(age <= 100) -> cattel ## remove inappropriate observations (based on age), rewrite data

## let's get descripstipe stats for all items at once
cattel %>% # colnames()
  select(cols = -c(age, gender, accuracy, country, source, elapsed)) %>% # colnames() ## [de]select cols
  pivot_longer( ## pivot data to long format
    cols = everything(), ## with all columns
    names_to = "item", ## name column with titles as 'item'
    values_to = "score" ## name column with values as 'score'
    ) %>% 
  summarise( ## calculate something
    n = n(), ## number of observations
    mean = mean(score), ## mean score
    median = median(score), ## median score
    sd = sd(score), ## sd of score
    min = min(score), ## min score
    max = max(score), ## max score
    skew = moments::skewness(score), ## skewness of score --- call function skewness() from moments package
    kurt = moments::kurtosis(score), ## kurtosis of score --- call function kurtosis() from moments package
    .by = "item" ## group rows by 'item' variable
  ) # %>% filter(str_detect(item, "^A")) ## filter rows where item name starts with A

## or we can do it by scale using select(starts_with(…))
cattel %>% 
  select(cols = -c(age, gender, accuracy, country, source, elapsed)) %>% # colnames() ## [de]select cols
  select(starts_with("A")) %>% ## select all columns where name starts with 'A'
  pivot_longer( ## pivot data to long format
    cols = everything(), ## with all columns
    names_to = "item", ## name column with titles as 'item'
    values_to = "score" ## name column with values as 'score'
  ) %>% 
  summarise( ## calculate something
    n = n(), ## number of observations
    mean = mean(score), ## mean score
    median = median(score), ## median score
    sd = sd(score), ## sd of score
    min = min(score), ## min score
    max = max(score), ## max score
    skew = moments::skewness(score), ## skewness of score --- call function skewness() from moments package
    kurt = moments::kurtosis(score), ## kurtosis of score --- call function kurtosis() from moments package
    .by = "item" ## group rows by 'item' variable
  )


## let's draw something beautiful
ggplot() ## we need canvas

cattel %>% ## and also we need data
  ggplot()
## not really impressive for now

## let's draw barplot for gender
cattel %>% 
  ggplot(aes(gender)) ## tell ggplot _what_ variable we would like to draw
## hmmm, ok

cattel %>% 
  ggplot(aes(gender)) +
  geom_bar() ## tell ggplot _how_ we would like to draw it
## well, kinda

## let's add some shine
cattel %>% 
  ggplot(aes(gender)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Пол",
       y = "Частота") +
  scale_x_discrete(labels = c("female" = "Женский",
                              "male" = "Мужской",
                              "other" = "Другое"))

## let's draw distributions of items scores
theme_set(theme_minimal()) ## set minimalistic theme for all future graphs
cattel %>% 
  select(starts_with("A", ignore.case = FALSE)) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(value)) +
  geom_bar() +
  facet_wrap(~ name) ## make subgraph for each item
## well, not bad

## again we can do something to make it better
cattel %>% 
  select(starts_with("A", ignore.case = FALSE)) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(value)) +
  geom_bar() +
  facet_wrap(~ factor(name, ## make subgraph for each item
                      ordered = TRUE, levels = paste0("A", 1:10) ## order them correctly
                      ),
             scales = "free_x" ## save x-labels for each subgraph
             ) + 
  scale_x_continuous(breaks = 0:5) + ## make all point on x axis visible
  labs(x = "Балл", ## set label to x axis
       y = "Частота" ## set label to y axis
       )


## let's do a scatterplot --- relation between age and total score of scale B for participants from US
cattel %>% 
  mutate(id = 1:nrow(cattel)) %>% ## create id column --- just a number of the row in dataset
  filter(country == "US") %>% ## filter US participants
  select(starts_with("B"), age, id) %>% ## select columns we need
  pivot_longer(
    cols = -c(age, id), ## pivot to long format, ignore columns age and id
    names_to = "item", ## put name to 'item' column
    values_to = "score" ## put values to 'score' column
               ) %>% ##
  summarise( ## calculate something
    total_score = sum(score), ## total score as sum of item scores
    .by = c(age, id) ## for each participant (id) and preserve age column for future visualization
    ) %>% 
  ggplot(aes(age, total_score)) + ## use age as x and total as y
  geom_point() ## draw points

## and again add some shine
cattel %>% 
  mutate(id = 1:nrow(cattel)) %>% 
  filter(country == "US") %>% 
  select(starts_with("B"), age, id) %>% 
  pivot_longer(cols = -c(age, id),
               names_to = "item",
               values_to = "score") %>% 
  summarise(total_score = sum(score),
            .by = c(age, id)) %>% 
  ggplot(aes(age, total_score)) +
  geom_point(alpha = .5) + ## set transparency for points (alpha channel)
  labs(
    x = "Возраст", ## set x label
    y = "Суммарный балл", ## set y label
    title = "Связь выраженности личностных черт респондентов с их возрастом", ## set graph title
    subtitle = "Шкала B опросника 16PF", ## set graph subtitle
    caption = "данные по респондентам из США" ## add graph caption
    )


## oh, maybe we need gender on the plot?
cattel %>% 
  mutate(id = 1:nrow(cattel)) %>% 
  filter(country == "US") %>% 
  select(starts_with("B"), age, id, gender) %>% ## now select all B scale items, age, id, and gender
  pivot_longer(cols = -c(age, id, gender), ## pivot to long format, ignore columns age, id, and gender
               names_to = "item",
               values_to = "score") %>% 
  summarise( ## calculate something
    total_score = sum(score), ## total score as sum of item scores
    .by = c(age, id) ## for each participant (id) and preserve age and gender columns for future visualization
    ) %>% 
  ggplot(aes(age, ## use age as x
             total_score, ## use total_score as y
             color = gender) ## and display gender with color
         ) +
  geom_point(alpha = .5) + ## set transparency for points
  theme(legend.position = "bottom") + ## place color legend below the plot
  scale_color_discrete(labels = c("female" = "Женский", ## set labels for color
                                  "male" = "Мужской", 
                                  "other" = "Другое")) +
  labs(x = "Возраст",
       y = "Суммарный балл",
       color = "Пол", ## set label for color legend
       title = "Связь выраженности личностных черт респондентов с их возрастом",
       subtitle = "Шкала B опросника 16PF",
       caption = "данные по респондентам из США")
