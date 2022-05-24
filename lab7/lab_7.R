set.seed(1)

# rozkład normalny
X_1 = rnorm(100)
X_2 = rnorm(100)
X_3 = rnorm(100)

X = list(X_1, X_2, X_3)

n_all = function(X) {
  k = length(X)
  n = 0
  
  for (i in 1:k) {
    x_i = X[[i]]
    n_i = length(x_i)
    n = n + n_i
  }
  
  n
}
n_all(X)

mean_all = function(X) {
  k = length(X)
  s = 0
  
  for (i in 1:k) {
    x_i = X[[i]]
    n_i = length(x_i)
    
    for (j in 1:n_i) {
      s = s + x_i[j]
    }
  }
  
  s / n_all(X)
}
mean_all(X)


SSA = function(X) {
  k = length(X)
  s = 0
  
  for (i in 1:k) {
    x_i = X[[i]]
    n_i = length(x_i)
    s = s + (n_i * (mean(x_i) - mean_all(X))^2)
  }
  
  s
}
SSA(X)

SSE = function(X) {
  k = length(X)
  s = 0
  
  for (i in 1:k) {
    x_i = X[[i]]
    n_i = length(x_i)
    
    for (j in 1:n_i) {
      s = s + (x_i[j] - mean(x_i))^2 
    }
  }
  
  s
}
SSE(X)

MSA = function(X) {
  k = length(X)
  SSA(X) / (k - 1)
}
MSA(X)

MSE = function(X) {
  k = length(X)
  n = n_all(X)
  SSE(X) / (n - k)
}
MSE(X)

# Hipoteza zerowa: u1 = u2 = u3
# Hipoteza alternatywna: ~Hipoteza zerowa
# F = MSA / MSE

F = MSA(X) / MSE(X)
F

alfa = 0.01
k = length(X)
n = n_all(X)

crical_value = qf(1 - alfa, k - 1, n - k)
crical_value

# wartość statystyki F (0.58) nie wpada w przedział wartości krtycznej (4,68; inf), więc nie możemy odrzucić hipotezy zerowej

###

set.seed(1)
X_1 = rnorm(100)
X_2 = rnorm(100)
X_3 = rnorm(100, 1, 1)

X = list(X_1, X_2, X_3)

F = MSA(X) / MSE(X)
F

alfa = 0.01
k = length(X)
n = n_all(X)

crical_value = qf(1 - alfa, k - 1, n - k)
crical_value

# wartość statystyki F (35.95) wpada w przedział wartości krtycznej (4,68; inf), więc udrzucamy hipotezę zerową

# Procedura Fishera

post_hoc_fisher = function(X_i, X_j, n, k, mse, alfa) {
  n_i = length(X_i)
  n_j = length(X_j)
  
  t = (mean(X_i) - mean(X_j)) / sqrt(mse) * sqrt((n_i * n_j) / (n_i + n_j))
  
  critical_value = qt((1 - alfa) / 2, n - k)

  (t < -crical_value) || (t < critical_value)
}
post_hoc_fisher(X_1, X_2, n_all(X), length(X), MSE(X), 0.01)
post_hoc_fisher(X_1, X_3, n_all(X), length(X), MSE(X), 0.01)
post_hoc_fisher(X_2, X_3, n_all(X), length(X), MSE(X), 0.01)



description_table = matrix(c(
  'Pomiędzy grupami', 'SSA', 'k - 1', 'MSA', 'F', 
  'Wewnątrz grup', 'SSE', 'n - k', 'MSE', 'F',
  'Całość', 'SST', 'n - 1', 'MST', 'F'
  ), ncol=5, byrow=TRUE)
colnames(description_table) = c('Zmienność','Suma kwadratów','Liczba stopni swobody', 'Średnie kwadraty', 'Statystyka testowa')
rownames(description_table) = c(1:3)
description_table = as.table(description_table)
head(description_table)


SST = function(X) {
  k = length(X)
  s = 0
  
  for (i in 1:k) {
    x_i = X[[i]]
    n_i = length(x_i)
    
    for (j in 1:n_i) {
      s = s + (x_i[j] - mean_all(X))^2 
    }
  }
  
  s
}
SST(X)

MST = function(X) {
  n = n_all(X)
  SST(X) / (n - 1)
}
MST(X)

data_table = matrix(c(
  'Pomiędzy grupami', SSA(X), length(X) - 1, MSA(X), F, 
  'Wewnątrz grup', SSE(X), n_all(X) - length(X), MSE(X), F,
  'Całość', SST(X), n_all(X) - 1, MST(X), F
), ncol=5, byrow=TRUE)
colnames(data_table) = c('Zmienność','Suma kwadratów','Liczba stopni swobody', 'Średnie kwadraty', 'Statystyka testowa')
rownames(data_table) = c(1:3)
data_table = as.table(data_table)
head(data_table)













