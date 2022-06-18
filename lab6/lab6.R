path <- "C:\\R\\lab6\\nut_dataframe.csv"

dtframe <- read.csv(path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
dtframe <- na.omit(dtframe[, -1])

library(kableExtra)
library(dplyr)
library(ggcorrplot)
library(lavaan)
library(lavaanPlot)

df_tibble <- as_tibble(dtframe)

df_tibble <- df_tibble %>% select(PodWidth, 
                                  PodLength, 
                                  SeedsWeight, 
                                  SeedsNumber, 
                                  PodsNumber, 
                                  PodsWeight, 
                                  BegFEndF, 
                                  EndFBegM)
df <- as.data.frame(df_tibble)

#----------------------По корреляции между главными компонентами-----------------------
# находим значения главных компонент
pca_df <- prcomp(df, scale = T, center = T)$x
#получаем матрицу корреляций
correlation <- cor(df, pca_df)
ggcorrplot(cor(pca_df, df))
# видим по рисунку что первые три компоненты вносят наибольший вклад в наши признаки
# оставим первые 3 компоненты и по порогу вклада больше 0.5 посмотрим,
# как связаны исходные данные и три главные компоненты
three_corr <- correlation[, 1:3]
dependence <- three_corr
dependence[abs(dependence) <= 0.5] <- F
dependence[abs(dependence) > 0.5] <- T
model <- '
# measurement model
PC1 =~ SeedsWeight + SeedsNumber + PodsNumber + PodsWeight
PC2 =~ PodWidth + PodLength
PC3 =~ BegFEndF + EndFBegM'
fit <- sem(model, data = df)
summary(fit, standardized=TRUE)
lavaanPlot(model = fit, 
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE)
