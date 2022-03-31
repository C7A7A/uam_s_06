show_means = function(n) {
  mean_vec = vector("numeric", n)
  
  for (i in 1:n) {
    mean_vec[i] = mean(
        rnorm(1:i)
      )
  }
  
  plot(
    mean_vec,
    type="l"
  )
  
}

show_means(5000)


# u(0, b) - rozklad jednostany
# X - proba
# trust_level =  1 - alfa
# alfa = 1 - trust_level
# przedział ufności

czas_oczekiwania = c(4.03, 11.04,  5.73, 12.36, 13.17,  0.64,  7.39, 12.49,  7.72,  6.39, 13.40,  6.35,  9.49,  8.02,  
                     1.44, 12.60,  3.45,  0.59,  4.59, 13.36, 12.45,  9.70,  8.97, 13.92,  9.18,  9.92,  7.62,  8.32,  
                     4.05,  2.06, 13.48, 12.63,  9.67, 11.14,  0.34,  6.69, 10.62,  3.03,  4.45, 3.24,  2.00,  5.80,  
                     5.79,  5.16,  2.13,  1.94, 3.26,  6.52,  3.72, 12.01,  0.64,  6.19, 11.18,  1.71,  7.85,  2.89,  
                     1.79, 10.55, 12.53,  5.24,  9.31,  1.33,  5.38,  3.84, 11.40,  6.28, 11.34, 11.37, 11.12, 6.16,
                     10.56,  8.81,  9.94,  0.01,  6.65,  3.08,  5.32,  8.58,  4.93,  1.56,  3.41,  9.35,  5.85, 11.03,  
                     1.44,  6.09, 13.79, 12.50, 12.41,  2.45,  1.83,  9.14, 4.81,  9.19,  4.49,  2.63, 10.95,  1.31,  6.53, 7.16)


confidence_interval = function(X, trust_level) {
  alfa = 1 - trust_level
  n = length(X)
  
  left_interval = max(X) / ((1 - (alfa / 2)) ^ (1 / n))
  righ_interval = max(X) / ((alfa / 2) ^ (1 / n))
  
  list(left_interval, righ_interval)
}

confidence_interval(czas_oczekiwania, 0.95)

conf_levels = seq(0.01, 0.99, 0.01)
conf_intervals = confidence_interval(czas_oczekiwania, conf_levels)

plot(
  conf_levels,
  conf_intervals[[1]],
  type="l",
  ylim=c(13.5, 15.5)
)

points(
  conf_levels,
  conf_intervals[[2]],
  type="l"
)








