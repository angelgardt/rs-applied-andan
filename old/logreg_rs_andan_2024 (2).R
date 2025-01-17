library(tidyverse)

managers <- read_csv("managers_log.csv")


str(managers)
unique(managers$lvl)

managers %>% 
  mutate(lvl = ifelse(lvl == "Менеджер", 1, 0)) %>% 
  select(-...1, -id) -> managers

model1 <- glm(lvl ~ ., data = managers, family = binomial)
summary(model1)

model0 <- glm(lvl ~ 1, data = managers, family = binomial)
# summary(model0)


anova(model1, model0, test = "Chi")
anova(model0, model1, test = "Chi")

AIC(model1, model0)
BIC(model0, model1)

model2 <- update(model1, .~. -subdiv_regulations -autonomy -company_regulations -error_cost)
summary(model2)

exp(1.605)





