#Download the data

setwd('C:/Users/VerkhovodTS/Desktop/R')
f_train <- read.csv2('gdp_train2.csv', header = TRUE, encoding = 'UNICOD')
f_train <- f_train[,-1] 
f_train <- f_train[,-1]
f_test <- read.csv2('gdp_test2.csv', header = TRUE, encoding = 'UNICOD')
f_test <- f_test[,-1] 
f_test <- f_test[,-1] 
#Висновок: Ми задали навчальну і тестову вибірки. Видалили перші два стовпчики в кожній з вибірок.

#Simple Linear Regression (one factor – Population) 
#Fitting Simple Linear Regression to the Training set
model_sr <- lm(GDP~Population, f_train)
summary(model_sr)
#Висновок: Обрана змінна значуща, коефіцієнт детермінації =0,53

#Predicting 
p_sr <- predict(model_sr, f_test)
r2_sr <- 1-sum((f_train$GDP - predict(model_sr, f_train))^2)/sum((f_train$GDP - mean(f_train$GDP))^2) 
R2_sr <- cor(f_train$GDP, fitted(model_sr))^2

train_mse_sr <- sum((f_train$GDP-predict(model_sr, f_train))^2)/length(f_train$GDP) 
test_mse_sr <- sum((f_test$GDP-p_sr)^2)/length(p_sr) 
#Висновок: вручну розраховані коефіцієнти детермінації. 
#Значення середньоквадратичної похибки на навчальній вибірці – 4.30653465413521e+23, 
#на тестовій вибірці – 2.26201016712423e+23, тобто перенавчання немає.

#Visualising 
library(ggplot2) 
ggplot()+
  geom_point(aes(f_train$Population, f_train$GDP), color='red')+
  geom_point(aes(f_test$Population, f_test$GDP), color='dark blue')+
  geom_line(aes(f_test$Population, p_sr), color='black')+
  ggtitle('GDP vs Population')+
  xlab('Population')+
  ylab('GDP')
#Висновок: на графіку червоним позначені точки навчальної вибірки, синім – точки тестової 
#вибірки, чорна лінія – модельні значення. 

#Multiple Linear Regression (many factors) 
#All factors
model_mr <- lm(data = f_train, GDP ~ .)
summary(model_mr)
#Висновок: змінні Unemployment та Constitutional.form найменш значущі, коефіцієнт детермінації дорівнює 0,63. 

#Optimized model 
#Оптимізуємо модель за допомогою виключення з неї змінних  Unemployment та Constitutional.form.
model_opt <- lm(data=f_train, GDP~Population+Area..km.2 +Average.life.expectancy..years)
summary(model_opt)
#Висновок: усі змінні значущі, коефіцієнт детермінації = 0,61. 

#Prediction
p_mr <- predict(model_opt,f_test)
train_mse_opt <- sum((f_train$GDP-predict(model_opt,f_train))^2)/length(f_train$GDP)
tast_mse_opt <- sum((f_test$GDP-p_mr)^2)/length(p_mr)
#Висновок: значення середньоквадратичної помилки покращилися – на навчальній вибірці – 3.51021576407741e+23, 
#на тестовій вибірці – 1.8074039522629e+23, перенавчання немає. 

#Visualising
ggplot()+
  geom_point(aes(f_train$Population, f_train$GDP), color='red')+
  geom_point(aes(f_test$Population, f_test$GDP), color='dark blue')+
  geom_line(aes(f_test$Population, p_mr), color='black')+
  ggtitle('GDP vs Population')+
  xlab('Population')+
  ylab('GDP')
#Висновок: на графіку червоним позначені точки навчальної вибірки, синім – точки тестової 
#вибірки, чорна лінія – модельні значення.

#Polynomial Linear Regression (one factor - Population) 

#Features extending
f_train_poly <-f_train[,c('GDP','Population')]
f_test_poly <-f_test[,c('GDP','Population')]
f_train_poly$Population2 <-f_train_poly$Population^2
f_train_poly$Population3 <-f_train_poly$Population^3
f_test_poly$Population2 <-f_test_poly$Population^2
f_test_poly$Population3 <-f_test_poly$Population^3
#Висновок: Ми додали значення Population^2 та Population^3

#3 powers
model_pr <-lm(data = f_train_poly, GDP~Population2+Population3)
summary(model_pr)
#Висновок: змінні Population^2 та Population^3 значущі, коефіцієнт детермінації = 0.51.

#Predicting 
p_pr<- predict(model_pr, f_test_poly)

train_mse_poly <-sum((f_train_poly$GDP-predict(model_pr,f_train_poly))^2)/
  length(f_train_poly$GDP)
test_mse_poly <-sum((f_test_poly$GDP-p_pr)^2)/length(p_pr)
#Висновок: значення середньоквадратичної помилки трохи зросли на навчальній вибірці – 4.45982231155079e+23, 
#на тестовій вибірці – 2.8696890816788e+23, тобто перенавчання немає.

#Visualising 
ggplot()+
  geom_point(aes(f_train_poly$Population, f_train_poly$GDP), color='red')+
  geom_point(aes(f_test_poly$Population, f_test_poly$GDP), color='dark blue')+
  geom_line(aes(f_test_poly$Population, p_pr), color='black')+
  ggtitle('GDP vs Population')+
  xlab('Population')+
  ylab('GDP')
#Висновок: на графіку червоним позначені точки навчальної вибірки, синім – точки тестової 
#вибірки, чорна лінія – модельні значення.

#Saving results
fit <-data.frame(p_sr,p_mr,p_pr)
write.csv2(fit, file = "GDP_fit.csv")
#Висновок: результати моделювання збережені у файлі. 






