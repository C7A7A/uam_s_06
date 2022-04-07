# 1) Test 1 próby dla 1 sredniej
# 20kg - Mi0 (hipoteza zerowa, średnia)
# 150 - n (próba)
# 22kg - (średnia z próby)
# 6kg - (odchylenie standardowe z próby)
# Hipoteza zerowa: Mi = Mi0
# Hipoteza alternatwna: Mi > Mi0
# 0.05 - alfa (poziom istotności)
# rozkład t-studenta o stopniach swobody n-1 t(n-1)

T = (22 - 20) / 6 * sqrt(150)
T
K = qt(1 - 0.05, 150 - 1)
K

# 2)
# 11.5% Mi0 (srednia)
# 50 (próba)
# 10.4% (srednia z próby)
# 3.4% (odchylenie standardowe)
# 0.05 - alfa (poziom istotności)

# Hipoteza zerowa: Mi = Mi0
# Hipoteza alternatywna: Mi != Mi0

T2 = (10.4 - 11.5) / 3.4 * sqrt(50)
T2
K2_1 = -qt(1 - 0.05/2, 50 - 1)
K2_2 = qt(1 - 0.05/2, 50 - 1)

# 3)
# 2 miliony Mi0
# 5 (próba)
# 1.9 3.7 2.9 2.0 3.3
# 0.05 - alfa (poziom istotności)

Mi0 = 2
n = 5
values = c(1.9, 3.7, 2.9, 2.0, 3.3)
alfa = 0.05

mean_val = mean(values)
sd_val = round(sd(values), 1)
sd_val

# Hipoteza zerowa: Mi = Mi0
# Hipoteza alternatywna: Mi > Mi0

T3 = (mean_val - Mi0) / sd_val * sqrt(n)
T3

K3 = qt(1 - alfa, n - 1)
K3


