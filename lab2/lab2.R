path_csv <- "C:\\R\\lab2\\test_data_01.csv"
path_Rdata <- "C:\\R\\lab2\\all_data.Rdata"

#-----------------Задание №1-----------------
require(data.table)
require(stringr)
require(dplyr)

# проверяем допустимые ли символы

numbers_only <- function(x) return(grepl("^[ \\.[:digit:] \\-]*$", x))

# проверяем, одна ли точка

correct_number <- function(x) return(str_count(x, "\\.") <= 1)

# функция для замены на числа

edit_data <- function(x) 
{
  if((correct_number(x) & numbers_only(x))[1]) x <- as.numeric(str_replace_all(x, fixed(" "), ""))
  return(x)
}

fix_data <- function(frame) return(as_tibble(lapply(frame, edit_data)))

# считываем файл так, потому что он битый

table <- fread(text = gsub('"', "", readLines(path_csv), fixed = TRUE))
table <- as_tibble(table)

fixed <- fix_data(table)
print(fixed)

#-----------------Задание №2-------------------

# получаем названия всех пациентов

get_all_ids <- function(data)
{
  ids <- vector()
  for (i in 1:length(data)) ids <- union(ids, data[[i]]$id)
  return(sort(ids))
}

# получаем только те id, которые встречаются везде

good_ids <- function(data, ids)
{
  bad_ids <- vector()
  for(i in 1:length(data)) bad_ids <- append(bad_ids, setdiff(ids, data[[i]]$id))
  return(sort(setdiff(ids, bad_ids)))
}

get_id <- function(data)
{
  ids <- get_all_ids(data)
  id <- good_ids(data, ids)
  
  # создаем data.frame для ответа
  
  new_frame <- data.frame(matrix(ncol = 1, nrow = length(id)))
  colnames(new_frame) <- c("mean_temp")
  rownames(new_frame) <- id
  new_frame <- cbind(id, new_frame)

  # оставляем только те данные, которые валидны по id
  
  for(i in 1:length(data)) {
    data[[i]] <- as.data.frame(data[[i]][data[[i]]$id %in% id, ])
    row.names(data[[i]]) <- data[[i]]$id
  }
  
  # считаем и записываем среднее арифметическое
  
  for(i in id)
  {
    temp <- vector()
    for(j in 1:length(data)) temp <- append(temp, data[[j]][toString(i), "temp"])
    new_frame[toString(i), "mean_temp"] <- mean(temp)
  }
  return(new_frame)
}

frame <- get_id(all_data)
print(frame)