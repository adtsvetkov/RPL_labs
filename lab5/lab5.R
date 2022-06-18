path <- "C:\\R\\lab5\\nut_dataframe.csv"

df <- read.csv(path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
df <- na.omit(df[, -1])

require(ggplot2)
require(factoextra)
require(resample)

# с помощью встроенных функций

# вначале рассмотрим матрицу ковариации исходных переменных
print(cov(df), digits = 2) #тут книтр

# применяем метод главных компонент
df_pca <- prcomp(df, scale = T, center = T)
df_pca$x # матрица главных компонент
# дисперсия каждой компоненты
dispersion <- summary(df_pca)$importance[2, ]

#вручную:
mu <- colMeans(as.matrix(df))
d <- colVars(df)
df_matr <- as.matrix(df)
centered <- matrix(, nrow = nrow(df_matr), ncol = ncol(df_matr))

# центрируем и нормируем матрицу
for(j in 1:ncol(df)) centered[,j] <- (df_matr[,j] - mu[j])
# ищем матрцу ковариаций
covariation <- cov(centered)
# собственные векторы и собственные числа
eig <- eigen(covariation)
#Матрица поворота (весов)
weight <- rbind(-eig$vectors, eig$values)[, order(-eig$values)]

#матрица собственных векторов
e1 <- weight[1:(nrow(weight)-1),]

# матрица РСА

matr_pca <- centered %*% e1
colnames(matr_pca) <- colnames(df_pca$x)

#вклад дисперсии в долях
v1 <- weight[(nrow(weight)),]
disp <- v1 / tr(covariation)
disp <- setNames(disp, colnames(matr_pca))

histogram <- as.numeric(formatC(cumsum(dispersion), format = 'f', digits = 3))
# скоращение матрицы
pca_cut <- as.data.frame(df_pca$x[, 1:(length(histogram[histogram < 0.75]) + 1)])
# отрисовка
myplot <- barplot(histogram, xlim = c(0, 1.1), border = NA, ylab = "Главные компоненты", main = "Cуммирование вклада дисперсий", horiz = T)
text(y = myplot, x = histogram, label = histogram,  col = "darkgreen", pos = 2, offset = 0)
text(y = myplot, x = 0, cex = 0.8, pos = 2, offset = -1)
abline(v = 0.75, col = "red", lty = 2)
text(0.75, 0, "75%", pos=4, col = "red")

#показываем то, что вносит наибольший вклад

pl1 <- fviz_contrib(df_pca, choice = "var", axes = 1)
pl2 <- fviz_contrib(df_pca, choice = "var", axes = 2)
pl3 <- fviz_contrib(df_pca, choice = "var", axes = 3)
pl4 <- fviz_contrib(df_pca, choice = "var", axes = 4)
pl5 <- fviz_contrib(df_pca, choice = "var", axes = 5)
pl6 <- fviz_contrib(df_pca, choice = "var", axes = 6)
pl7 <- fviz_contrib(df_pca, choice = "var", axes = 7)
pl8 <- fviz_contrib(df_pca, choice = "var", axes = 8)
pl9 <- fviz_contrib(df_pca, choice = "var", axes = 9)
pl10 <- fviz_contrib(df_pca, choice = "var", axes = 10)
pl11 <- fviz_contrib(df_pca, choice = "var", axes = 11)

grid.arrange(pl1, pl2, pl3, pl4, pl5, pl6, pl7, pl8, pl9, pl10, pl11, ncol=2, nrow=6)

#веса вручную для сортировки
sort(abs(df_pca$rotation[,1]), decreasing = T)

ggplot(pca_cut, aes(x=PC1, y=PC2, color = abs(df$FloCol))) + 
  geom_point(size = 2) + 
  scale_color_gradient(low="blue", high="red", name = "FloCol") +
  labs(x=paste(sep = "", "PC1 (", format(dispersion[1]*100, digits = 4), "%)"),
       y=paste(sep = "", "PC2 (", format(dispersion[2]*100, digits = 4), "%)"))