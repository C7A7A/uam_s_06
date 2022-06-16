###
# 1. ilość klas zbioru danych - 1
# 2. 150 (50 * 3)
# 3. nie

###
# 4. należy zmienic wagi w zbiorze danych, a następnie wytenować klasyfikator ponownie 
# 5. LDA wykorzystuje liniową funkcję klasyfikującą lub liniową funkcję dyskryminacyjną, QDA wykorzystuje kwadratową funkcję klasyfikacyjną
# 6. prosta
# 7. wiele prostych, które próbują jak najlepiej odzielić od siebie poszczególne klasy
# 8. parabola, parabole

###
# 9.
library(MASS)
library(class)

plot(Cushings$Tetrahydrocortisone, Cushings$Pregnanetriol, col=Cushings$Type, pch=20)
model = qda(Cushings[1:2], Cushings$Type)
model

klasyfikacja = predict(model)
klasyfikacja
etykiety_klasyfikacja = klasyfikacja$class
etykiety_klasyfikacja

tabela_przynaleznosci = table(Cushings$Type, etykiety_klasyfikacja)
tabela_przynaleznosci

blad = mean(Cushings$Type != etykiety_klasyfikacja)
blad

n = 250
x = seq(0, 60, length=n)
y = seq(0, 12, length=n)
siatka = expand.grid(Tetrahydrocortisone = x, Pregnanetriol = y)
z = predict(model, siatka)
plot(siatka, col=z$class, pch=20, cex=1)

filled.contour(
  x, 
  y, 
  matrix(as.numeric(z$class), nrow=n), 
  levels=0.5:4.5, 
  col=c("gray", "lightcoral", "lightgreen", "lightblue"),
  plot.axes={
    axis(1);
    axis(2);
    points(Cushings$Tetrahydrocortisone, Cushings$Pregnanetriol, col=Cushings$Type, pch=20)
  }
)
