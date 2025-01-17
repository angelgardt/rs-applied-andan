library(tidyverse)
library(lavaan)
library(semPlot)

# reading data
tolunc <- read_csv2('https://raw.githubusercontent.com/angelgardt/hseuxlab-wlm2021/master/data/tolerance_uncertainty.csv')

# checking data
str(tolunc)
summary(tolunc)

# CFA model
mdl1 <- "F1 =~ tu1 + tu2 + tu3 + tu4 + tu5 + tu6 + tu7 + tu8 + tu9 + tu10 + tu11 + tu12 + tu13 + tu14 + tu15 + tu16 + tu17 + tu18 + tu19 + tu20 + tu21 + tu22"
model1 <- cfa(mdl1, data = tolunc)
summary(model1)

# standardized solution
sfit <- standardizedsolution(model1)

# factor loadings
sfit %>% 
  filter(op == "=~")

# correlations
# in this case, no correlations coz there is none in our model
sfit %>% 
  filter(op == "~~" & sfit$lhs != sfit$rhs)

# residuals
sfit %>% 
  filter(op == "~~" & sfit$lhs == sfit$rhs)

# fit measures
fitmeasures(model1, c("chisq", "gfi", "agfi","cfi", "tli", "srmr", "rmsea"))

# residual correlations
resid(model1, type = 'cor')

# visualization
semPaths(model1, 'std')

# modification indices
modificationindices(model1)


# model comparison
mdl2 <- "F1 =~ tu1 + tu2 + tu3 + tu4 + tu5 + tu6 + tu7 + tu8 + tu9 + tu10 + tu11 + tu12 + tu13 + tu14 + tu15 + tu16 + tu17 + tu18 + tu19 + tu20 + tu21 + tu22
tu1 ~~ tu2"

model2 <- cfa(mdl2, data = tolunc)
summary(model2)

anova(model1, model2)


