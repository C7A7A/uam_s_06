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
# w modelu z jednorodnymi wariancjami: omega1^2 = omega2^2
# w modelu z niejednorodnymi wariancjami: onmega1^2 != omega2^2

# 9.
# Hipoteza: mean(a) = mean(b)
size = 100
a = rnorm(size)
b = rnorm(size)

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
std = 1
n = size * size / (size + size)

mean_a = mean(a)
mean_b = mean(b)

T = (mean_a - mean_b) / std * sqrt(n)
T

K = qt(1 - alfa, size + size - 2)
K
