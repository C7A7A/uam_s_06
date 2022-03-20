# 1. nie
# 2. tabelaryczny, graficzny, statystyki opisowe
# 3. nie
# 4. nie
# 5. sd()

# 6.
# rozkład empiryczny - uzyskany na podstawie badania statystycznego opis wartości przyjmowanych przez cechę statystyczną przy pomocy częstości ich występowania. 
# Rozkład empiryczny z reguły jest prezentowany jako szereg rozdzielczy (tabela).

# 7.
# model statystyczny - hipoteza lub układ hipotez, które są sformułowane w sposób matematyczny (równanie, układ równań), który przedstawia zasadnicze powiązania
# występujące pomiędzy rozpatrywanymi zjawiskami rzeczywistymi.

# 8.
# statystyka - każda (mierzalna) funkcja próby. Służy do wyodrębnienia istotnych cech danych doświadczalnych. Pojęcie statystyki w statystyce matematycznej jest 
# odpowiednikiem zmiennej losowej w rachunku prawdopodobieństwa.


# 9.
moment = function(r, values) {
  sum = 0;
  for (val in values) {
    sum = sum + val^r;
  }
  
  sum / length(values)
}


# zmienna ciągła
wait_time = c(4.03, 11.04,  5.73, 12.36, 13.17,  0.64,  7.39, 12.49,  7.72,  6.39, 13.40,  6.35,  9.49,  8.02,  
              1.44, 12.60,  3.45,  0.59,  4.59, 13.36, 12.45,  9.70,  8.97, 13.92,  9.18,  9.92,  7.62,  8.32,  
              4.05,  2.06, 13.48, 12.63,  9.67, 11.14,  0.34,  6.69, 10.62,  3.03,  4.45, 3.24,  2.00,  5.80,  
              5.79,  5.16,  2.13,  1.94, 3.26,  6.52,  3.72, 12.01,  0.64,  6.19, 11.18,  1.71,  7.85,  2.89,  
              1.79, 10.55, 12.53,  5.24,  9.31,  1.33,  5.38,  3.84, 11.40,  6.28, 11.34, 11.37, 11.12, 6.16,
              10.56,  8.81,  9.94,  0.01,  6.65,  3.08,  5.32,  8.58,  4.93,  1.56,  3.41,  9.35,  5.85, 11.03,  
              1.44,  6.09, 13.79, 12.50, 12.41,  2.45,  1.83,  9.14, 4.81,  9.19,  4.49,  2.63, 10.95,  1.31,  6.53, 7.16)

# 10.
# a)
w_t_mean = mean(wait_time)
w_t_median = median(wait_time)
w_t_sd = sd(wait_time)
w_t_cv = w_t_sd / w_t_mean

# b)
hist(wait_time)

# c)
w_t_den = density(wait_time)
plot(w_t_den)

# d)
hist(
  wait_time,
  col = "orange",
  prob = TRUE,
  main = "Histogram of wait_time with density"
)
lines(
  density(wait_time),
  lwd = 4
)

# e)
boxplot(
  wait_time,
  col = "orange",
  border = "brown",
  horizontal = TRUE,
  notch = TRUE  
)
