#install.packages("tidyverse", dependencies = TRUE)

#library(tidyverse)


managers <- read.csv("managers.csv")

str(managers)
View(managers)

unique(managers$lvl)
# managers[colnames(managers)[-c(1:2)]] -> managers

managers %>% select(-X, -id) -> managers
View(managers)

managers$lvl <- ifelse(managers$lvl == "РЎРѕС‚СЂСѓРґРЅРёРє", 0, 1)



fit1 <- glm(lvl ~ ., family = binomial, data = managers)
summary(fit1)

fit_null <- glm(lvl ~ 1, family = binomial, data = managers)

anova(fit_null, fit1, test = 'Chi')

AIC(fit_null, fit1)
BIC(fit_null, fit1)


