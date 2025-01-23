# install.packages("tidyverse", "moments")

pkgs <- c("tidyverse", "moments")
install.packages(pkgs[!pkgs %in% installed.packages()])

library(tidyverse)

# dir.create("data")

## source: ## https://www.kaggle.com/datasets/mylesoneill/game-of-thrones
deaths <- read_csv("data/character-deaths.csv")
str(deaths)
rm(deaths)

## rm(list = ls())

## source: https://www.kaggle.com/datasets/tunguz/cattells-16-personality-factors
ket <- read_csv("data/ket.csv")
str(ket)

ket <- read_tsv("data/ket.csv")
str(ket)
View(ket)
head(ket)
tail(ket, n = 4)

nrow(ket)
ncol(ket)
colnames(ket)
class(ket)
sapply(ket, class)


mean(ket$A1)
mean(ket$A1, trim = .2)

?mean
help(mean)

var(ket$A1)
sd(ket$A1)
median(ket$A1)
quantile(ket$A1)
quantile(ket$A1, probs = .20)
max(ket$A1)
min(ket$A1)
table(ket$A1)
table(ket$country)
sort(table(ket$country))
sort(table(ket$country), decreasing = TRUE)
ket$country %>% table() %>% sort(decreasing = TRUE)

ket %>%
  group_by(country) %>%
  summarise(
    n = n()
    ) %>% 
  # arrange(n)
  arrange(desc(n))

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


summary(ket)


ket$A1 %>% is.na() %>% sum()
ket %>% sapply(is.na) %>% apply(2, sum)
ket %>% sapply(function(x) x %>% is.na() %>% sum())


unique(ket$gender)

ket %>% 
  mutate(
    gender = ifelse(gender == 0, NA,
                    ifelse(gender == 1, "male",
                           ifelse(gender == 2, "female", "other")))
  ) %>% 
  pull(gender) %>%
  # is.na() %>% sum()
  unique()


ket %>%
  mutate(
    gender = recode(
      gender,
      "0" = NA_character_,
      "1" = "male",
      "2" = "female",
      "3" = "other"
    )
  ) -> ket
ket %>% pull(gender) %>% unique()


ket %>% drop_na()
ket %>% drop_na() %>% nrow()
ket %>% drop_na() -> cattel



cattel %>%
  summarise(
    n_age = n(),
    mean_age = mean(age),
    sd_age = sd(age),
    max_age = max(age),
    min_age = min(age)
    )

cattel %>% 
  summarise(
    n = n(),
    mean = mean(age),
    sd = sd(age),
    max = max(age),
    min = min(age)
  )


hist(cattel$age)
boxplot(cattel$age)


cattel %>% # nrow()
  filter(age <= 100) %>% # nrow()
  pull(age) %>% boxplot()


cattel %>% filter(age <= 100) -> cattel


cattel %>% # colnames()
  select(cols = -c(age, gender, accuracy, country, source, elapsed)) %>% # colnames()
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "score"
    ) %>% 
  summarise(
    n = n(),
    mean = mean(score),
    median = median(score),
    sd = sd(score),
    min = min(score),
    max = max(score),
    skew = moments::skewness(score),
    kurt = moments::kurtosis(score),
    .by = "item"
  ) # %>% filter(str_detect(item, "^A"))


cattel %>% 
  select(cols = -c(age, gender, accuracy, country, source, elapsed)) %>% # colnames()
  select(starts_with("A")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "score"
  ) %>% 
  summarise(
    n = n(),
    mean = mean(score),
    median = median(score),
    sd = sd(score),
    min = min(score),
    max = max(score),
    skew = moments::skewness(score),
    kurt = moments::kurtosis(score),
    .by = "item"
  )



ggplot()

cattel %>%
  ggplot()


cattel %>% 
  ggplot(aes(gender))


cattel %>% 
  ggplot(aes(gender)) +
  geom_bar()



cattel %>% 
  ggplot(aes(gender)) +
  geom_bar() +
  theme_bw() +
  labs(x = "Пол",
       y = "Частота") +
  scale_x_discrete(labels = c("female" = "Женский",
                              "male" = "Мужской",
                              "other" = "Другое"))


theme_set(theme_minimal())
cattel %>% 
  select(starts_with("A", ignore.case = FALSE)) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(value)) +
  geom_bar() +
  facet_wrap(~ name)


cattel %>% 
  select(starts_with("A", ignore.case = FALSE)) %>% 
  pivot_longer(cols = everything()) %>% 
  ggplot(aes(value)) +
  geom_bar() +
  facet_wrap(~ factor(name,
                      ordered = TRUE, levels = paste0("A", 1:10)
                      ),
             scales = "free_x"
             ) + 
  scale_x_continuous(breaks = 0:5) +
  labs(x = "Балл",
       y = "Частота")



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
  geom_point()


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
  geom_point(alpha = .5) +
  labs(x = "Возраст",
       y = "Суммарный балл",
       title = "Связь выраженности личностных черт респондентов с их возрастом",
       subtitle = "Шкала B опросника 16PF",
       caption = "данные по респондентам из США")


cattel %>% 
  mutate(id = 1:nrow(cattel)) %>% 
  filter(country == "US") %>% 
  select(starts_with("B"), age, id, gender) %>% 
  pivot_longer(cols = -c(age, id, gender),
               names_to = "item",
               values_to = "score") %>% 
  summarise(total_score = sum(score),
            .by = c(age, id, gender)) %>% 
  ggplot(aes(age, total_score, color = gender)) +
  geom_point(alpha = .5) +
  theme(legend.position = "bottom") +
  scale_color_discrete(labels = c("female" = "Женский",
                                  "male" = "Мужской", 
                                  "other" = "Другое")) +
  labs(x = "Возраст",
       y = "Суммарный балл",
       color = "Пол",
       title = "Связь выраженности личностных черт респондентов с их возрастом",
       subtitle = "Шкала B опросника 16PF",
       caption = "данные по респондентам из США")
