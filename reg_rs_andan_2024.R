library(tidyverse)
theme_set(theme_bw())

managers <- read_csv("managers_reg.csv")
str(managers)

table(managers$region)

cor(managers$grade_score, managers$fot)
cor.test(managers$grade_score, managers$fot)

cor.test(managers$experience, managers$fot)

plot(managers$experience, managers$fot)

managers %>% 
  ggplot(aes(grade_score, fot)) +
  geom_point() +
  geom_smooth(method = "lm")

managers %>% 
  select(fot, grade_score, experience) %>% 
  cor() %>% # round(2)
  corrplot::corrplot()


cor.test(managers$grade_score, managers$fot, method = "sp")



model1 <- lm(fot ~ grade_score, managers)
summary(model1)

par(mfrow=c(2,2))
plot(model1)





