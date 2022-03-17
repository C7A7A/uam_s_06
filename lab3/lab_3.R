# rozkład dwumianowy 
# b(m, p) m - ilość losowań p - prawopodobieństwo
# X ~ b(10, 1/3)

K = 0:15

barplot(
  dbinom(K, 15, 1/10),
  names.arg = K,
  xlab = "K",
  ylab = "P(X=K)",
  main = "Rozkład Prawdopodobieństwa"
)

# rozkład jednostajny
# U(a, b) a < b

curve(
  dunif(x, min=1.5, max=3),
  1,
  7,
  xlab = "x",
  ylab = "F(x)",
  main = "Funkcja gęstości"
)

# rozkład normalny

curve(
  dnorm,
  -4,
  4
)

curve(
  dnorm(x, mean=0.5, sd=0.4),
  -10,
  10,
  col = "purple",
  add=TRUE
)

curve(
  dnorm(x, mean=mean(K), sd=0.5),
  -10,
  10,
  col = "orange",
  add=TRUE
)

curve(
  dnorm(x, mean=1, sd=0.8),
  -10,
  10,
  col = "red",
  add=TRUE
)

# estymacja 

# zmienna dyskretna (ilościowa)
liczba_bledow = c(
  1, 1, 2, 0, 1, 3, 1, 4, 4, 4, 0, 1, 0, 0, 0, 2, 3,
  4, 0, 1, 5, 2, 3, 5, 3, 2, 2, 4, 0, 2, 2, 0, 2, 2,
  3, 3, 1, 3, 2, 2, 0, 0, 5, 4, 2, 1, 5, 2, 2, 0
)

probs_emp = prop.table(table(liczba_bledow))
barplot(probs_emp)

m = 18

p_est = mean(liczba_bledow) / m

probs_est = dbinom(
  sort(unique(liczba_bledow)),
  size=m,
  prob=p_est
)

barplot(probs_est)

M = matrix(
  c(probs_emp, probs_est),
  nrow=2,
  byrow=TRUE 
)

barplot(
  M,
  beside=TRUE
)















