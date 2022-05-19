# dane_9.R
# lab
przychody = c(210, 270, 290, 310, 370, 400, 450, 480, 510, 520)
wydatki = c(140, 190, 250, 270, 290, 310, 340, 360, 420, 390)

data_set = data.frame(przychody=przychody, wydatki=wydatki)
data_set

plot(data_set)

model = lm(
  wydatki ~ przychody,
  data = data_set
)
model

abline(model, col="blue")

coef(model)
confint(model)
fitted(model)
?fitted
residuals(model)

wydatki - fitted(model)


temp_przychody = data.frame(
  przychody = seq(min(przychody), max(przychody), length=100)
)
temp_przychody

pred = predict(model, temp_przychody, interval = "confidence")
#plot(pred)

lines(temp_przychody$przychody, pred[, 2], lty=2, col="green")
lines(temp_przychody$przychody, pred[, 3], lty=2, col="red")


--- ---
  
mtcars
dane = mtcars[, c(1, 3:7)]
dane
plot(dane)

?prcomp
model_pca = prcomp(dane, scale. = TRUE, center = TRUE)
model_pca
plot(model_pca)

model_pca$x
plot(model_pca$x)

podsumowanie = summary(model_pca)
podsumowanie

barplot(podsumowanie$importance[1, ]) # standard_deviation
barplot(podsumowanie$importance[2, ]) # proportion of variance
barplot(podsumowanie$importance[3, ]) # cumulative proportion

