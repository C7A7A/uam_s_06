library(MASS)

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

set.seed(1)
zbior = generujZbior(c(-1, -1), c(1, 1), 100)
rysujZbior(zbior)

obliczBlad = function(zbior, przesuniecie, kat)
{
  a = tan(kat*pi/180) # y=ax+b
  b = przesuniecie
  etyk = rep(2, nrow(zbior))
  etyk[zbior$y < a * zbior$x + b] = 1
  blad(etyk, zbior$etykieta)
}

rysujLinie = function(przesuniecie, kat, kolor = "blue")
{
  abline(przesuniecie, tan(kat*pi/180), col = kolor) # y=a+bx
}

przesuniecie = 0
kat = 135
obliczBlad(zbior, przesuniecie, kat)
rysujLinie(przesuniecie, kat)

uczLin = function(zbior, przesuniecia, katy, rysuj = FALSE)
{
  b_min = 1
  p_min = 0
  k_min = 0
  
  for (p in przesuniecia)
  {
    for (k in katy)
    {
      b = obliczBlad(zbior, p, k)
      if (b < b_min)
      {
        b_min = b
        p_min = p
        k_min = k
      }
      if (rysuj) rysujLinie(p, k)
    }
  }
  list(blad = b_min, przesuniecie = p_min, kat = k_min)
}

rysujZbior(zbior)
model = uczLin(zbior, -1:1, seq(0, 360, 1), rysuj = TRUE)
model


#############################


obliczBladPP = function(zbior_ucz, przesuniecia, katy)
{
  model = uczLin(zbior_ucz, przesuniecia, katy)
  
  obliczBlad(zbior_ucz, model$przesuniecie, model$kat)
}

obliczBladTest = function(zbior_ucz, zbior_test, przesuniecia, katy)
{
  model = uczLin(zbior_ucz, przesuniecia, katy)
  
  obliczBlad(zbior_test, model$przesuniecie, model$kat)
}

obliczBladCV = function(zbior_ucz, przesuniecia, katy)
{
  n = nrow(zbior_ucz)
  bledy = vector("numeric", n)
  
  for (i in 1:n)
  {
    zb_test = zbior_ucz[ i, ] # jeden element (wiersz)
    zb_ucz  = zbior_ucz[-i, ]
    
    model = uczLin(zb_ucz, przesuniecia, katy)
    
    bledy[i] = obliczBlad(zb_test, model$przesuniecie, model$kat)
  }
  mean(bledy)
}

###

set.seed(1)

zbior_ucz  = generujZbior(c(-1,-1), c(1,1), 8000)
zbior_test = generujZbior(c(-1,-1), c(1,1), 2000)

rysujZbior(zbior_ucz)
rysujZbior(zbior_test)
rysujLinie(przesuniecie, kat)

przesuniecia = 0
katy = seq(0, 360, 5)

obliczBladPP(zbior_ucz, przesuniecia, katy)
# 0.0875
obliczBladTest(zbior_ucz, zbior_test, przesuniecia, katy)
# 0.0975
obliczBladCV(zbior_ucz, przesuniecia, katy)
# 0.095

