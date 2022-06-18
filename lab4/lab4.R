filepath <- "C:\\R\\lab4\\nut_dataframe.csv"

require(nortest)
require(dplyr)
require(ggplot2)

nut_dataframe <- read.csv(filepath, fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# удалим строки с NA

nut_dataframe <- nut_dataframe[rowSums(is.na(nut_dataframe)) <= 0,]

#-----------------------------------Задание №1----------------------------------

#Выбрать из DataFrame 2 количественные и 2 качественные переменные

#количественные
nopodsweight <- nut_dataframe$NoPodsWeight
height <- nut_dataframe$Height

#Проверить количественные переменные на нормальность

#ставим гипотезу H_0: случайная величина Х распределены нормально

shapiro.test(nopodsweight) #критерий шапиро-уилка W - значение статистики
# при уровне значимости 0.05 такая гипотеза не отвергается, т.к. значение превосходит 0.05
shapiro.test(height)

lillie.test(nopodsweight) #критерий лиллиефорса
lillie.test(height)

#гистограмма ноподса

ggplot(nut_dataframe, aes(x = NoPodsWeight)) + 
  geom_histogram(color="black", fill="white", aes(y = ..density..)) + 
  geom_density(aes(color = "blue"), size = 2) +
  stat_function(fun = dnorm, args = list(mean = mean(nut_dataframe$NoPodsWeight), 
                                         sd = sd(nut_dataframe$NoPodsWeight)), 
                aes(color = "red"), size = 2, alpha = 0.7) +
  scale_color_identity(name = "Lines",
                       breaks = c("blue", "red"),
                       labels = c("Data density", "Normal distribution"),
                       guide = "legend") +
  ggtitle("NoPodsWeight distribution")

#гистограмма высоты

ggplot(nut_dataframe, aes(x = Height)) + 
  geom_histogram(color="black", fill="white", aes(y = ..density..)) + 
  geom_density(aes(color = "blue"), size = 2) +
  stat_function(fun = dnorm, args = list(mean = mean(nut_dataframe$Height), 
                                         sd = sd(nut_dataframe$Height)), 
                aes(color = "red"), size = 2, alpha = 0.7) +
  scale_color_identity(name = "Lines",
                       breaks = c("blue", "red"),
                       labels = c("Data density", "Normal distribution"),
                       guide = "legend") +
  ggtitle("Height distribution")

#Разбить значения количественных переменных относительно значений качественных

# взятые качественные переменные - bushshape и flowstemcol

# по 2

nut2_bushshape <- group_split(nut_dataframe %>% group_by(BushShape))
nut2_flowstemcol <- group_split(nut_dataframe %>% group_by(FlowStemCol))

check_normality <- function(tibble, mode = "shapiro")
{
  for(i in 1:length(tibble))
  {
    bushshape_in_tibble <- tibble[[i]]$BushShape
    flowstemcol_in_tibble <- tibble[[i]]$FlowStemCol
    npw_in_tibble <- tibble[[i]]$NoPodsWeight
    height_in_tibble <- tibble[[i]]$Height
    print("Sorted by: ")
    if (all(bushshape_in_tibble == bushshape_in_tibble[1]))
    {
        print(paste("Bushshape: ", bushshape_in_tibble[1]))
    }
    if (all(flowstemcol_in_tibble == flowstemcol_in_tibble[1])) 
    {
        print(paste("FlowStemCol: ", flowstemcol_in_tibble[1]))
    }
    switch(mode, 
           "shapiro" = {
             print(shapiro.test(npw_in_tibble))
             print(shapiro.test(height_in_tibble))
           },
           "lillie" = {
             print(lillie.test(npw_in_tibble))
             print(lillie.test(height_in_tibble))
           },
     )
  }
}

check_normality(nut2_bushshape, "shapiro")
check_normality(nut2_flowstemcol, "lillie")

# проведем тест Фишера равенства дисперсий: гипотеза H0, что дисперсии равны

# разделение height + bushshape

var.test(nut2_bushshape[[1]]$Height, nut2_bushshape[[2]]$Height)

# разделение nopodsweight + flowstemcol

var.test(nut2_flowstemcol[[1]]$NoPodsWeight, nut2_flowstemcol[[2]]$NoPodsWeight)

# реализовать для подвыборок t-test с помощью встроенной функции и вручную

# разделение height + bushshape

t.test(nut2_bushshape[[1]]$Height, nut2_bushshape[[2]]$Height, var.equal = F)

# разделение nopodsweight + flowstemcol

t.test(nut2_flowstemcol[[1]]$NoPodsWeight, nut2_flowstemcol[[2]]$NoPodsWeight, var.equal = T)

# Вручную (реализовав форумлу)

mytest_student <- function(sample1, sample2) return((mean(sample1)-mean(sample2))/sqrt(var(sample1)/length(sample1) + var(sample2)/length(sample2)))

# разделение height + bushshape

mytest_student(nut2_bushshape[[1]]$Height, nut2_bushshape[[2]]$Height)

# разделение nopodsweight + flowstemcol

mytest_student(nut2_flowstemcol[[1]]$NoPodsWeight, nut2_flowstemcol[[2]]$NoPodsWeight)

#плоты

ggplot(nut2_bushshape[[1]], aes(x = Height)) + 
  geom_histogram(aes(y = ..density..), color="darkgreen", fill = "darkgreen", alpha = 0.7) +
  geom_histogram(data = nut2_bushshape[[2]], aes(x=Height, y = ..density..), color="orange", fill = "orange", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(nut2_bushshape[[1]]$Height), 
                                         sd = sd(nut2_bushshape[[1]]$Height)), aes(color = "Normal distribution 1"), size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(nut2_bushshape[[2]]$Height), 
                                         sd = sd(nut2_bushshape[[2]]$Height)), aes(color = "Normal distribution 2"), size = 1.5) +
  stat_function(fun = dt, args = list(df = 387.96), aes(color = "t.test distribution"), size = 1.5) +
  ggtitle("Height for bushshape")

ggplot(nut2_flowstemcol[[1]], aes(x = NoPodsWeight)) + 
  geom_histogram(aes(y = ..density..), color="darkred", fill = "darkred", alpha = 0.7) +
  geom_histogram(data = nut2_flowstemcol[[2]], aes(x=NoPodsWeight, y = ..density..), color="blue", fill = "blue", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(nut2_flowstemcol[[1]]$NoPodsWeight), 
                                         sd = sd(nut2_flowstemcol[[1]]$NoPodsWeight)), aes(color = "Normal distribution 1"), size = 1.5) +
  stat_function(fun = dnorm, args = list(mean = mean(nut2_flowstemcol[[2]]$NoPodsWeight), 
                                         sd = sd(nut2_flowstemcol[[2]]$NoPodsWeight)), aes(color = "Normal distribution 2"), size = 1.5) +
  stat_function(fun = dt, args = list(df = 402), aes(color = "t.test distribution"), size = 1.5) +
  ggtitle("NoPodsWeight for flowstemcol")

#-----------------------------Задание №2----------------------------------------

# Анализ по критерию хи-квадрат

bushape <- nut_dataframe$BushShape
flowstemcol <- nut_dataframe$FlowStemCol

# с помощью встроенной функции

# без поправки Йетса
chisq_test <- chisq.test(table(bushape, flowstemcol), correct = F)
# c поправкой Йетса
chisq_test2 <- chisq.test(table(bushape, flowstemcol))

# вручную

my_chi_squared <- function(sample1, sample2)
{
  myframe <- data.frame(sample1 = sample1, sample2 = sample2)
  
  dataformatrix <- group_split(myframe %>% group_by(sample1, sample2))
  
  chi <- vector()
  observed <- data.frame()
  expected <- data.frame()
  
  numofdata <- length(sample1)
  
  for (i in 1:length(dataformatrix))
  {
    # таблица сопряженности
    workmatrix <- dataformatrix[[i]]
    name1 <- workmatrix$sample1[1]
    name2 <- workmatrix$sample2[1]
    
    observed[toString(name1), toString(name2)] <- length(workmatrix$sample1)
  }
  
  print("Observed matrix for chi-squated test: ")
  print(observed)
  
  # проверяем гипотезу H_0, что x не зависит от y
  for (i in 1:nrow(observed))
  {
    summa <- sum(observed[i, ])
    for (j in 1:ncol(observed))
    {
      expected[i, j] <- summa * sum(observed[, j]) / numofdata
    }
  }
  
  print("Expected matrix for chi-squated test: ")
  print(expected)
  
  for (i in 1:nrow(observed))
  {
    for(j in 1:nrow(observed)) 
    {
      chi <- append(chi, (observed[i, j] - expected[i, j])^2/expected[i, j])
    }
  }
  return(sum(chi))
}

result <- my_chi_squared(bushape, flowstemcol)

# для выбранного уровня значимости 0.05 для 1 степени свободы 3.8. Гипотеза о том, что величины независимы, отвергается

require(moonBook)
require(webr)

plot(chisq_test) + stat_function(fun = dchisq, args = list(df = 1), size = 1.3) + xlim(0, 15) + 
  geom_point(aes(x=chisq_test$statistic, y=dchisq(chisq_test$statistic, df = 1)), colour="blue", size = 4) +
  geom_vline(xintercept = qchisq(.95, df = 1), color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0) +
  geom_text(aes(x=1, y = 0.1), label = "95%")