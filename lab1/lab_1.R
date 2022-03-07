# 1.
# interpretowany

# 2.
# 2

# 3.
# na języku S

# 4.
# tak

# 5.
# tak

# 6.
# tak

# 7.
# tak

# 8.
# tak

# 9.
# wolno

# 10.
# tak

# 11.
# =, ->

# 12.
# ;

# 13.
# tak

# 14.
# tak

# 15.
# tak

# 16.
# Przede wszystkim dlatego, że kod napisany w R jest bardzo wolny. R nie został stworzony do pisania dużych aplikacji, tylko do zaawansowanych obliczeń statystycznych.

# 17.
# Każdy może napisać bibliotekę w R i udostępnić ją światu. Dlatego niektóre z pakietów będą niskiej jakości.

# 18.
# Możemy zadeklarować zmienną typu integer używając funkcji as.integer() lub dodając sufiks L przy liczbie. Reszta zmiennych numeric jest zazwyczaj interpretowana jako double.

# 19.
# Łańcuchy znakowe to zmienne typy string lub char (napisy zawarte miedzy znakami ' ' lub " ").

# 20.
# Oba znaki reprezentują operację AND. && sprawdza tylko 1 element każdego wektora.

# 21.
# Lista to struktura danych, która przechowuje ciąg elementów o dowolnym typie.

# 22.
# Wektor to struktura danych, która przechowuje ciąg elementów tego samego typu.

# 23.
# Ramka danych to struktura danych, która przechowuje dane tabelaryczne. Ramka to lista wektorów, gdzie każdy wektor przedstawia jedną z cech danych.

# 24.
# x, y - macierze
# x * y pomnoży elementy macierze przez siebie. x %*% y  dokona mnożenia macierzy przez siebie.

# 25.
# while(true) { }

# 26.
vec = 0:10

sumVecSquares = function(vector) {
  sum = 0
  for (element in vector) {
    sum = sum + (element**2);
  } 
  sum
}

sum = sumVecSquares(vec)

# 27.
matrixX = matrix(1:4, 2, 2)
matrixY = matrix(5:8, 2, 2)


sumMatInverses = function(matrixA, matrixB) {
  solve(matrixA) + solve(matrixB)
}

sum = sumMatInverses(matrixX, matrixY)

# 28.

func = function(x) {
  x+1
}

funComposition = function(fun, compositionAmount) {
  function(x) {
    if (compositionAmount <= 1) {
      return (fun(x)) 
    } else {
      return (fun(funComposition(fun, compositionAmount - 1)(x)))
    }
  }
}


funComposition(func, 0)(5)
funComposition(func, 1)(5)
funComposition(func, 2)(5)
funComposition(func, 3)(5)
funComposition(func, 4)(5)
funComposition(func, 5)(5)
