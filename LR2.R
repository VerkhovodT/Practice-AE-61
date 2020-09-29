setwd('C:/Users/VerkhovodTS/Desktop/R')
f <- read.csv2('countries.csv', header = TRUE, encoding = 'UNICOD')
library(dplyr)
glimpse(f)
head(f)

library(ggplot2)
par(mfrow = c(1,5))
hist(f$GDP, col='green', main = 'GDP', xlab = 'USD')
hist(f$Population,col = 'red', main = 'Population', xlab = 'Value')
hist(f$Unemployment,col = 'purple', main = 'Unemployment', xlab = '%')
hist(f$Area..km.2,col = 'blue', main = 'Area..km.2', xlab = 'km.2')
hist(f$Average.life.expectancy..years,col = 'yellow', main = 'Average.life.expectancy..years', xlab = 'Years')

par(mfrow = c(1,5))
boxplot(f$GDP)
boxplot(f$Population)
boxplot(f$Unemployment)
boxplot(f$Area..km.2)
boxplot(f$Average.life.expectancy..years)
qplot(Constitutional.form, GDP, data = f, geom = "boxplot")
qplot(Constitutional.form, GDP, data = f, geom = "violin")

library(psych)
describe(f)

pairs.panels(f, lm=TRUE, method = "pearson", hist.col = "#ed5d09")

library(tidyr)

f_fill1 <- tidyr::fill(f,Unemployment, .direction ='down')
f_fill1 <- tidyr::fill(f_fill1,Average.life.expectancy..years, .direction = 'up')
head(f_fill1)

f_fill2 <- f
f_fill2$Unemployment <-ifelse(is.na(f$Unemployment), round(mean(f$Unemployment, na.rm = TRUE)),f$Unemployment)
f_fill2$Average.life.expectancy..years <-ifelse(is.na(f$Average.life.expectancy..years), round(mean(f$Average.life.expectancy..years, na.rm = TRUE)),f$Average.life.expectancy..years)
head(f_fill2)

f<- f_fill2

f_ej2 <- f
f_ej2$GDP<- ifelse(f$GDP < mean(f$GDP)+sd(f$GDP)*3, f$GDP, mean(f$GDP)+sd(f$GDP)*3)
describe(f_ej2$GDP)
f_ej2$Population<- ifelse(f$Population < mean(f$Population)+sd(f$Population)*3, f$Population, mean(f$Population)+sd(f$Population)*3)
describe(f_ej2$Population)
f_ej2$Area..km.2<- ifelse(f$Area..km.2 < mean(f$Area..km.2)+sd(f$Area..km.2)*3, f$Area..km.2, mean(f$Area..km.2)+sd(f$Area..km.2)*3)
describe(f_ej2$Area..km.2)
f_ej2$Unemployment<- ifelse(f$Unemployment < mean(f$Unemployment)+sd(f$Unemployment)*3, f$Unemployment, mean(f$Unemployment)+sd(f$Unemployment)*3)
describe(f_ej2$Unemployment)
f <- f_ej2

set.seed(123)
library(caTools)
split = sample.split(f$GDP, SplitRatio = 0.8)
f_train = subset(f, split == TRUE)
f_test = subset(f, split == FALSE)
write.csv2(f_train, file = "gdp_train2.csv")
write.csv2(f_test, file = "gdp_test2.csv")