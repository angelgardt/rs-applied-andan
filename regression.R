am <- read_csv("http://d396qusza40orc.cloudfront.net/statistics/lec_resources/states.csv")

str(am)

am$state
read.table(am)

#создать из столбца список (функция "с"):
a <- c(am$hs_grad)
b <- c(am$poverty)

#посчитать корреляцию:
cor(a, b)

#построить регрессионную прямую:
scatter.smooth(a,b)

summary(am)

#Не знаю, как создала data для переменных a, b
lm(data)
model <- lm(data)

#полностью результаты регрессионного анализа
summary(model)

#РАССЧИТАТЬ ЛИНЕЙНУЮ РЕГРЕССИЮ

#1. создать переменные (списки) из столбцов
c <- c(am$white)
d <- c(am$metro_res)
e <-  c(am$female_house)

#2. Создать dataframe (tibble) из этих столбцов
tibble(c, d)

#3. Назвать этот dataframe (у меня название "ne")
ne <- tibble(c, d)

#4. Рассчитать показатели линейной регрессии (не знаю, почему так, вычитала в интернете)
lm(ne)
model <- lm(ne)

#5. Показать полностью результаты регрессионного анализа
summary(model)

#РАССЧИТАТЬ МНОЖЕСТВЕННУЮ РЕГРЕССИЮ (на первом месте -  зависимая переменная, на остальных в любом порядке предикторы)
mno <- tibble(b, a, c, d, e)
mno <- tibble(am$poverty, am$metro_res, am$white, am$hs_grad)
lm(mno)
summary(lm(mno))



