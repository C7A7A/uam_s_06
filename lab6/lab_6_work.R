# 1. t = (X_ - u0 / S) * sqrt(n)
# 2. H0: u = u0
# 3. nie

# 4. obszar krytyczny to obszar odrzuceń hipotezy zerowej (jeśli wartość statystyki testowej przekracza wartość krytyczną 
#    to odrzucamy tę hipotezę).
# 5. błąd I rodzaju oznacza, że odrzucamy hipotezę zerową gdy jest ona prawdziwa
# 6. p-wartość jest najmniejszym poziomem istotności testu, przy którym odrzucamy hipotezę zerową
# 7.
# H1: u1 != u2
# H1: u1 > u2
# H1: u1 < u2
# 8.

# 9.
# Hipoteza: mean(a) = mean(b)
a = rnorm(100)
b = rnorm(100)

normalize = function(x) {
  min.x = min(x)
  max.x = max(x)
  
  x.norm = (x - min.x) / (max.x - min.x)
  x.norm
}

# normalize N(0, 1)
a = normalize(a)
a
b = normalize(b)
b

alfa = 0.01

mean_a = mean(a)
mean_b = mean(b)
