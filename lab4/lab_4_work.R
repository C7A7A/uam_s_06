# 1. nie
# 2. tak
# 3. 0,9 0,95 0,99
# 4. nie
# 5. rozkładu chi-kwadrat

# 6. F^(x) = #{k: Xk <= x} / n
# 7. w celu wylosowania próby bootstrapowej dokonujemy n-krotnego losowania ze zwracaniem spośród wartości oryginalnej próby
# 8. zazwyczaj a i b dobieramy tak, aby
#     P(Q <= a) = P(Q >= b) = alfa / 2

# 9. 
# trust_level - 1 - alfa
# parameter - delta^2
# distribution - normal distribution
library(EnvStats)

confidence_interval = function(X, trust_level) {
  distr = enorm(
    X, 
    conf.level=trust_level,
    ci=TRUE
  )
  
  distr
}

conf_limits = confidence_interval(c(1:1000), 0.95)$interval$limits

plot(
  conf_limits,
  type="l"
)
