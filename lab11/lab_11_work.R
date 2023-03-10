# 1. Supervised (z nadzorem), Unsupervised (bez nadzoru), Reinforcenment (ze wzmocnieniem)
# 2. tak
# 3. tak
# 4. tak
# 5. n-krotnie losujemy pojedyczną obserwacje ze zwraceniem z n-elementowej próby uczącej 

###

# 6. Uczenie się pod nadzorem lub uczenie się z przykładów jest procesem budowy, na bazie dostępnych 
#    danych wejściowych Xi oraz wejściowych Yi, i = 1, 2 ..., n, reguły klasyfikacyjnen zwanej inaczej klasyfikatorem,
#    służącej do predykcji etykiety Y grupy, do której należy obserwacja X

# 7. Chcemy na podstawie wektora cech X znaleźć odpowiednią etykiete Y. Naszym celem jest znalezienie takiego
#    klasyfikatora d, który daje dokładną predykcję. Miarą jakości takiego klasyfikatora jest jego rzeczywisty poziom błędy
#         e(d) = P(d(X) != Y)

# 8. Klasyfikator bayesowski jest OPTYMALNY, tj. jeżeli d jest jakimkolwiek innym klasyfikatorem, to e(dB) <= e(d)
#    gdzue e(d) jest rzeczywistym poziomem błędy klasyfikatora d.
#
#    Niestety, klasyfikator bayesowski zależy od rozkładu prawdopodobieństwa pary (X, Y). Najczęściec rozkład ten nie jest znany
#    i stąd również nie jest znany klasyfikator bayesowski dB.

# 9. Błąd ponownego podstawienia polega na użyciu próby uczącej jakko próby testowej. Należy podzielić probę na 2 podzbiory: próbę 
#    uczącą oraz próbę testową, aby błąd ponownego podstawienia nie pojawiał się.

# 10 Overfitting występuje, kiedy dostosowujemy model dokładnie do danych treningowych. W takiej sytuacji model nie będzie w stanie
#    prawidłowo przewidzieć etykiet dla danych testowych. Przeuczenie dostosowuje się dokładnie do danych treningowych, kiedy model
#    uczony jest zbyt długo na danym datasecie lub kiedy model jest zbyt złożony.

###

# 11.

library(MASS)
library(ggplot2)

blad = function(E1, E2)
{
  sum(E1 != E2) / length(E1)
}

generujZbior = function(srednia1, srednia2, n)
{
  dane1 = mvrnorm(n, srednia1, Sigma = diag(c(1, 1)))
  ramka1 = data.frame(x = dane1[, 1], y = dane1[, 2], etykieta = rep(1, nrow(dane1)))
  
  dane2 = mvrnorm(n, srednia2, Sigma = diag(c(1, 1)))
  ramka2 = data.frame(x = dane2[, 1], y = dane2[, 2], etykieta = rep(2, nrow(dane2)))
  
  ramka = rbind(ramka1, ramka2)
}

rysujZbior = function(zbior)
{
  plot(
    NULL,
    asp = 1, 
    xlim = c(-3, 3), 
    ylim = c(-3, 3),
    xlab = "x", 
    ylab = "y", 
    main = deparse(substitute(zbior))
  )
  points(zbior, col = zbior$etykieta)
}

obliczBlad = function(zbior, przesuniece, kat)
{
  a = tan(kat*pi/180)
  b = przesuniecie
  etyk = rep(2, nrow(zbior))
  etyk[zbior$y < a * zbior$x + b] = 1
  blad(etyk, zbior$etykieta)
}

obliczBladKwa = function(zbior, a, b, c)
{
  etyk = rep(2, nrow(zbior))
  etyk[zbior$y < a * zbior$x^2 + b * zbior$x + c] = 1
  blad(etyk, zbior$etykieta)
}


set.seed(1)
zbior = generujZbior(c(-1, -1), c(1, 1), 100)

a = 1
b = 1
c = 1
x = zbior$x

obliczBladKwa(zbior, a, b, c)
rysujZbior(zbior)
curve(a * x^2 + b * x + c, add=TRUE, col="red")

uczKwa = function(zbior, params_a, params_b, params_c, rysuj = FALSE)
{
  a_min = 0
  b_min = 0
  c_min = 0
  blad_min = 1
  
  x = zbior$x
  
  for (a in params_a)
  {
    for (b in params_b)
    {
      for (c in params_c) {
        blad = obliczBladKwa(zbior, a, b, c)
        if (a != 0) {
          if (blad < blad_min)
          {
            blad_min = blad
            a_min = a
            b_min = b
            c_min = c
          }
          if (rysuj) curve(a * x^2 + b * x + c, add=TRUE, col="red")
        }
      }
    }
  }
  list(blad = blad_min, a = a_min, b = b_min, c = c_min)
}

rysujZbior(zbior)
model = uczKwa(zbior, seq(-3, 3, 0.1), seq(-3, 3, 0.1), seq(-3, 3, 0.1), rysuj = FALSE)
model
curve(model$a * x^2 + model$b * x + model$c, add=TRUE, col="red")


obliczBladPP = function(zbior_ucz, a, b, c)
{
  model = uczKwa(zbior_ucz, a, b, c, rysuj = FALSE)
  
  x = zbior_ucz$x
  obliczBladKwa(zbior_ucz, model$a, model$b, model$c)
}

obliczBladTest = function(zbior_ucz, zbior_test, a, b, c)
{
  model = uczKwa(zbior_ucz, a, b, c, rysuj = FALSE)
  
  x = zbior_ucz$x
  obliczBladKwa(zbior_test, model$a, model$b, model$c)
}

obliczBladCV = function(zbior_ucz, a, b, c)
{
  n = nrow(zbior_ucz)
  bledy = vector("numeric", n)
  
  for (i in 1:n)
  {
    zb_test = zbior_ucz[ i, ] # jeden element (wiersz)
    zb_ucz  = zbior_ucz[-i, ]
    
    model = uczKwa(zb_ucz, a, b, c, rysuj = FALSE)
    
    bledy[i] = obliczBladKwa(zb_test, model$a, model$b, model$c)
  }
  mean(bledy)
}

set.seed(1)

zbior_ucz  = generujZbior(c(-1, -1), c(1, 1), 800)
zbior_test = generujZbior(c(-1, -1), c(1, 1), 200)

a = seq(-3, 3, 0.5)
b = seq(-3, 3, 0.5)
c = seq(-3, 3, 0.5)

obliczBladPP(zbior_ucz, a, b, c)
obliczBladTest(zbior_ucz, zbior_test, a, b, c)
obliczBladCV(zbior_ucz, a, b, c)

