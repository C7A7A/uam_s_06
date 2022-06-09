dane = iris[1:2]
plot(dane, pch=20, col=iris$Species)

-----

b = dist(dane)
model = hclust(b, method="average")
plot(model)

model_cut = cutree(model, 4)
plot(dane, pch=20, col=model_cut)

-----
  
skupienia = kmeans(dane, 3, nstart=100.0)
plot(dane, pch=20, col=skupienia$cluster)
