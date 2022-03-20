# 1.            ^
# teta(X) lub teta
# 2. metoda momentów i metoda największej wiarygodności
# 3. tak
# 4. tak
# 5. nie

# 6.
# X = (X1, X2, ... Xn)' - próba o populacji o rozkładzie P0
# rozkłady P0 posiadają skończone momenty do rzędu d włącznie
#
# EMM polega na przyrównaniu kolejnych d momentów z próby:
#     mi = 1/n * sum(Xk^i), i = 1, 2, ..., d
# do odpowiednich momentów rozkładu populacji
#     E(Xi), i = 1, 2, ..., d
#
# Rozwiązując otrzymany w ten sposób układ równań uzyskujemy estyatmory metody momentów

# 7. Statystykę teta^ nazywamy estymatorem nieobciążonym parametru teta, gdy dla każdego teta zachodzi równość E(teta) = 0
# 8. W modelu jednej próby prostej z rozkładu wykładniczego, EMM i ENW parametru x postaxi x^ = 1/|X| jest obciążonym estymatorem tego parametru
# Estymator nieobciążony (o minimalnej wariancji) parametru x ma postać x^ = (n-1)/n * 1/|X|

# zmienna dyskretna (ilościowa)
errors = c(
  1, 1, 2, 0, 1, 3, 1, 4, 4, 4, 0, 1, 0, 0, 0, 2, 3,
  4, 0, 1, 5, 2, 3, 5, 3, 2, 2, 4, 0, 2, 2, 0, 2, 2,
  3, 3, 1, 3, 2, 2, 0, 0, 5, 4, 2, 1, 5, 2, 2, 0
)

# zmienna ciągła
wait_time = c(
  4.03, 11.04,  5.73, 12.36, 13.17,  0.64,  7.39, 12.49,  7.72,  6.39, 13.40,  6.35,  9.49,  8.02,  
  1.44, 12.60,  3.45,  0.59,  4.59, 13.36, 12.45,  9.70,  8.97, 13.92,  9.18,  9.92,  7.62,  8.32,  
  4.05,  2.06, 13.48, 12.63,  9.67, 11.14,  0.34,  6.69, 10.62,  3.03,  4.45, 3.24,  2.00,  5.80,  
  5.79,  5.16,  2.13,  1.94, 3.26,  6.52,  3.72, 12.01,  0.64,  6.19, 11.18,  1.71,  7.85,  2.89,  
  1.79, 10.55, 12.53,  5.24,  9.31,  1.33,  5.38,  3.84, 11.40,  6.28, 11.34, 11.37, 11.12, 6.16,
  10.56,  8.81,  9.94,  0.01,  6.65,  3.08,  5.32,  8.58,  4.93,  1.56,  3.41,  9.35,  5.85, 11.03,  
  1.44,  6.09, 13.79, 12.50, 12.41,  2.45,  1.83,  9.14, 4.81,  9.19,  4.49,  2.63, 10.95,  1.31,  6.53, 7.16
)

# 9.
plot(ppois(errors, lambda=2))
plot(ppois(wait_time, lambda=2))

# 10.
pois0_5 = ppois(errors, lambda=0.5)
pois1 = ppois(errors, lambda=1)
pois2 = ppois(errors, lambda=2)

hist(
  pois0_5,
  col="orange",
  prob=TRUE,
  main="Poisson distr of errors with density"
)
lines(
  density(pois0_5),
  lwd=2
)

hist(
  pois1,
  col="orange",
  prob=TRUE,
  main="Poisson distr of errors with density"
)
lines(
  density(pois1),
  lwd=2
)

hist(
  pois2,
  col="orange",
  prob=TRUE,
  main="Poisson distr of errors with density"
)
lines(
  density(pois2),
  lwd=2
)

# 11.
