path1 <- "C:\\R\\test_data1.txt" 
path_ <- "C:\\R\\test_data2.xlsx"
path2 <- "C:\\R\\test_data2.csv"

#--------------------подготовительные действия------------------------

#сначала сконвертируем файл xlsx в csv (как предполагалось по заданию)

library(readxl)
#считали объект типа tibble
file_2 <- read_xlsx(path_)

#можем сразу сделать так:
file22 <- as.data.frame(file_2)

#а можем записать как csv
write.csv(file_2, path2, row.names = FALSE)

#и считать данные в датафрейм
file2_copy <- read.csv(path2, row.names = 1)

#считываем второй файл
file1_copy <- read.table(path1, row.names = 1, header = T)

#-------------------задание №1-------------------------

#удалим из датафреймов те данные, которых нет в обоих
file1 <- file1_copy[rownames(file1_copy) %in% names(file2_copy), ]
file2 <- file2_copy[, names(file2_copy) %in% rownames(file1_copy)]

#-------------------задание №5-------------------------

#выводим то, что удалили
print(rownames(file1_copy[!(rownames(file1_copy) %in% rownames(file1)), ]))
print(colnames(file2_copy[, !(colnames(file2_copy) %in% colnames(file2))]))

#-------------------задание №2-------------------------

#проверим сорта, для которых не все NA
not_all_NA <- colSums(!is.na(file2)) > 0 #True or False

#заменим на нули
file2[not_all_NA][is.na(file2[not_all_NA])] <-0

#-------------------задание №3-------------------------

#вычисляем признаки, для которых не указано больше, чем 80% значений
NA_features <- colSums(!is.na(file1)) > 0.8*nrow(file1)

#удаляем их
file1 <- file1[, NA_features]

#вычисляем сорта, в которых есть NA
NA_classes <- rowSums(is.na(file1)) == 0

#удаляем
file1 <- file1[NA_classes, ]

#-------------------задание №4------------------------

#выводим средние значения для признаков

print(feature_mean <- apply(file1, 2, mean))

#выводим медиану для SNP

print(SNP_med <- apply(file2, 1, median))

#------------------final task-------------------------

newcolumns <- do.call(rbind, strsplit(rownames(file2), ":"))
colnames(newcolumns) <- c("Number", "Position")
file2 <- cbind(file2, newcolumns)

