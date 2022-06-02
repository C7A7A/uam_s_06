library(MASS)
library(class)

Cushings
plot(Cushings$Tetrahydrocortisone, Cushings$Pregnanetriol, col=Cushings$Type, pch=20)
model = lda(Cushings[1:2], Cushings$Type)
model

klasyfikacja = predict(model)
klasyfikacja
etykiety_klasyfikacja = klasyfikacja$class

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
#contour(x, y, matrix(as.numeric(z$class), nrow=n), levels=0.5:4.5, add=TRUE)
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


###
z = knn(Cushings[1:2], siatka, cl=Cushings$Type, k=1)
filled.contour(
  x, 
  y, 
  matrix(as.numeric(z), nrow=n), 
  levels=0.5:4.5, 
  col=c("gray", "lightcoral", "lightgreen", "lightblue"),
  plot.axes={
    axis(1);
    axis(2);
    points(Cushings$Tetrahydrocortisone, Cushings$Pregnanetriol, col=Cushings$Type, pch=20)
  }
)



