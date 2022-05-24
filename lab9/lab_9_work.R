# 1. tak
# 2. płaszczyzna
# 3. hiperpłaszczyzna
# 4. nauczanie nadzorowane
# 5. regresja prosta liniowa

###

# 6. f(x, B) = exp(B1x) + exp(B2x)
# 7. liczbową miarą dopasowania regresji do danych empirycznych jest współczynnik determinacji (podawany w %)
#    R^2 = 1 - (SSE/SST)
# 8. jest to wykres dla funkcji logistycznej. Krzywa ma kształ litery S.

###

year = c(1995,	1996,	1997,	1998,	1999,	2000,	2001,	2002)
cases = c(39.7,	38.2,	34.7,	33.1,	30.1,	28.4,	26.3,	24.7)

9.

data_set = data.frame(year=year, cases=cases)
plot(data_set)

model = lm(
  cases ~ year,
  data = data_set
)
abline(model, col="red")

temp_year = data.frame(
  year = seq(min(year), max(year), length=100)
)

prediction = predict(model, temp_year, interval = "confidence")

lines(temp_year$year, prediction[, 2], lty=2, col="green")
lines(temp_year$year, prediction[, 3], lty=2, col="green")





