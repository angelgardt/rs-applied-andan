library(tidyverse)

getwd()

managers <- read_csv("managers.csv")
str(managers)

cor.test(managers$fot, managers$grade_score)
cor.test(managers$fot, managers$grade_score, method = "sp")


fit1 <- lm(fot ~ grade_score, managers)
summary(fit1)


mean(fit1$residuals)
hist(fit1$residuals)
plot(fit1)


fit2 <- lm(fot ~ grade_score * experience, managers)
summary(fit2)
mean(fit2$residuals)
hist(fit2$residuals)
plot(fit2)

fit3 <- lm(fot ~ grade_score + experience, managers)
summary(fit2)


car::vif(fit2)
car::vif(fit3)


fit4 <- lm(fot ~ grade_score * experience * region, managers)
summary(fit4)

fit4.1 <- update(fit4, .~. -grade_score:experience:regionOther)
summary(fit4.1)

# drop1(fit4)


anova(fit4)



