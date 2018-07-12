# Модуль 7. Построение случайного леса с помощью пакета H2O

# Лекция 7.1. Решение задачи классификации

## 7.1.1. Подготовка данных

# устанавливаем пакет h2o
# install.packages("h2o", type = "source", 
# repos = (c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))

# загружаем пакет h2o
library(h2o)

# запускаем кластер H2O
h2o.init(nthreads = -1, max_mem_size = "8G")

# загружаем пакет data.table
library(data.table)

# загружаем пакет stringi для транслитерации
# категорий переменной living_region,
# поскольку H2O неверно отображает
# кириллический текст
library(stringi)

# загружаем данные, помечаем, что десятичным 
# разделителем является запятая
data <- fread("C:/Trees/credit_train.csv", dec = ",") 

# записываем редкие категории
job_position_rare_cat <- names(sort(table(data$job_position)))[1:4]
tariff_id_rare_cat <- names(sort(table(data$tariff_id)))[1:8]
living_region_rare_cat <- c("ЧИТИНСКАЯ ОБЛАСТЬ",
                            "РЕСПУБЛИКА ИНГУШЕТИЯ",
                            "ЧЕЧЕНСКАЯ РЕСПУБЛИКА",
                            "ЧУКОТСКИЙ АВТОНОМНЫЙ ОКРУГ")

# создаем функцию предварительной обработки
process_tinkoff_dt <- function(data, 
                               process_target = TRUE,
                               recode_tariff_id = FALSE,
                               rare_job_position = job_position_rare_cat,
                               rare_tariff_id  = tariff_id_rare_cat,
                               rare_living_region = living_region_rare_cat) {
  
  # функция для предварительной обработки набора данных Tinkoff.csv
  # https://boosters.pro/champ_3 с использованием data.table
  
  # проверка входных данных
  if(! inherits(data, "data.frame") |
     ! ncol(data) %in% c(14, 15)) {
    print("Неверный формат данных")
    return(NULL)
  }
  
  # удаляем идентификационную переменную
  data[, client_id := NULL]
  
  # приводим к единому стандарту названия регионов
  reg_pattern <- c("РЕСП АЛТАЙ", "^НЕНЕЦКИЙ АО|АО НЕНЕЦКИЙ",
                   "АЛТАЙСКИЙ", "АМУРСКАЯ", 
                   "АРХАНГЕЛЬСКАЯ", "АСТРАХАНСКАЯ", 
                   "БЕЛГОРОДСКАЯ", "БРЯНСК", 
                   "ВЛАДИМИРСКАЯ", "ВОЛГОГРАДСКАЯ", 
                   "ВОЛОГОДСКАЯ", "ВОРОНЕЖСКАЯ", 
                   "ЕВРЕЙСКАЯ", "ЗАБАЙКАЛЬСКИЙ",
                   "ИВАНОВСКАЯ", "ИРКУТСКАЯ", 
                   "КАБАРДИНО-БАЛКАРСКАЯ", "КАЛИНИНГРАДСКАЯ", 
                   "КАЛУЖСКАЯ", "КАМЧАТ", 
                   "КАРАЧАЕВО-ЧЕРКЕССКАЯ", "КЕМЕРОВСКАЯ", 
                   "КИРОВСКАЯ", "КОСТРОМСКАЯ", 
                   "КРАСНОДАРСКИЙ", "КРАСНОЯРСКИЙ",
                   "КУРГАНСКАЯ", "КУРСКАЯ", 
                   "ЛЕНИНГРАДСКАЯ", "ЛИПЕЦКАЯ", 
                   "МАГАДАНСКАЯ", "МОСКВА", 
                   "МОСКВОСКАЯ|МОСКОВСКАЯ", 
                   "МУРМАНСКАЯ", "НИЖЕГОРОДСКАЯ", 
                   "НОВГОРОДСКАЯ", "НОВОСИБИРСКАЯ", 
                   "^ОМСКАЯ$|^ОМСКАЯ|\\sОМСКАЯ", "ОРЕНБУРГСКАЯ", 
                   "ОРЛОВСКАЯ|ОРЁЛ", "ПЕНЗЕНСКАЯ", 
                   "ПЕРМСК", "ПРИМОРСКИЙ", 
                   "ПСКОВСКАЯ", "АДЫГЕЯ",  
                   "БАШКОРТОСТАН", "БУРЯТИЯ", 
                   "ДАГЕСТАН", "ИНГУШЕТИЯ", 
                   "КАЛМЫКИЯ", "КАРЕЛИЯ", 
                   "КОМИ", "МАРИЙ ЭЛ", 
                   "МОРДОВИЯ", "ЯКУТИЯ|РЕСПУБЛИКА САХА", 
                   "АЛАНИЯ", "ТАТАРСТАН", 
                   "ТЫВА", "ХАКАСИЯ", 
                   "РОСТОВСКАЯ", "РЯЗАНСКАЯ", 
                   "САМАРСКАЯ", "САНКТ-ПЕТЕРБУРГ", 
                   "САРАТОВСКАЯ", "САХАЛИНСКАЯ", 
                   "СВЕРДЛОВСКАЯ", "СМОЛЕНСКАЯ", 
                   "СТАВРОПОЛЬСКИЙ", "ТАМБОВСКАЯ", 
                   "ТВЕРСКАЯ", "ТОМСКАЯ", 
                   "ТУЛЬСКАЯ", "ТЮМЕНСКАЯ", 
                   "УДМУРТСКАЯ", "УЛЬЯНОВСКАЯ", 
                   "ХАБАРОВСКИЙ", "ЧЕЛЯБИНСКАЯ", 
                   "ЧЕЧЕНСКАЯ", "ЧИТИНСКАЯ", 
                   "ЧУВАШСКАЯ", "ЧУКОТСКИЙ", 
                   "ЯРОСЛАВСКАЯ", "74", 
                   "98", "ГОРЬКОВСКАЯ", 
                   "ГУСЬ-ХРУСТАЛЬНЫЙ Р-Н", "ДАЛЬНИЙ ВОСТОК", 
                   "МЫТИЩИНСКИЙ", "ЯМАЛО-НЕНЕЦКИЙ", 
                   "ПРИВОЛЖСКИЙ ФЕДЕРАЛЬНЫЙ ОКРУГ", "РОССИЯ", 
                   "ХАНТЫ-МАНСИЙСКИЙ", "ЭВЕНКИЙСКИЙ")
  
  reg_replace <- c("РЕСПУБЛИКА АЛТАЙ", "АРХАНГЕЛЬСКАЯ ОБЛАСТЬ",
                   "АЛТАЙСКИЙ КРАЙ", "АМУРСКАЯ ОБЛАСТЬ", 
                   "АРХАНГЕЛЬСКАЯ ОБЛАСТЬ", "АСТРАХАНСКАЯ ОБЛАСТЬ", 
                   "БЕЛГОРОДСКАЯ ОБЛАСТЬ", "БРЯНСКАЯ ОБЛАСТЬ", 
                   "ВЛАДИМИРСКАЯ ОБЛАСТЬ", "ВОЛГОГРАДСКАЯ ОБЛАСТЬ", 
                   "ВОЛОГОДСКАЯ ОБЛАСТЬ", "ВОРОНЕЖСКАЯ ОБЛАСТЬ", 
                   "ЕВРЕЙСКАЯ АВТОНОМНАЯ ОБЛАСТЬ", "ЗАБАЙКАЛЬСКИЙ КРАЙ",
                   "ИВАНОВСКАЯ ОБЛАСТЬ", "ИРКУТСКАЯ ОБЛАСТЬ", 
                   "КАБАРДИНО-БАЛКАРСКАЯ РЕСПУБЛИКА", "КАЛИНИНГРАДСКАЯ ОБЛАСТЬ", 
                   "КАЛУЖСКАЯ ОБЛАСТЬ", "КАМЧАТСКИЙ КРАЙ", 
                   "КАРАЧАЕВО-ЧЕРКЕССКАЯ РЕСПУБЛИКА", "КЕМЕРОВСКАЯ ОБЛАСТЬ", 
                   "КИРОВСКАЯ ОБЛАСТЬ", "КОСТРОМСКАЯ ОБЛАСТЬ", 
                   "КРАСНОДАРСКИЙ КРАЙ", "КРАСНОЯРСКИЙ КРАЙ",
                   "КУРГАНСКАЯ ОБЛАСТЬ", "КУРСКАЯ ОБЛАСТЬ", 
                   "ЛЕНИНГРАДСКАЯ ОБЛАСТЬ", "ЛИПЕЦКАЯ ОБЛАСТЬ", 
                   "МАГАДАНСКАЯ ОБЛАСТЬ", "МОСКВА", 
                   "МОСКОВСКАЯ ОБЛАСТЬ", 
                   "МУРМАНСКАЯ ОБЛАСТЬ", "НИЖЕГОРОДСКАЯ ОБЛАСТЬ", 
                   "НОВГОРОДСКАЯ ОБЛАСТЬ", "НОВОСИБИРСКАЯ ОБЛАСТЬ", 
                   "ОМСКАЯ ОБЛАСТЬ", "ОРЕНБУРГСКАЯ ОБЛАСТЬ",
                   "ОРЛОВСКАЯ ОБЛАСТЬ", "ПЕНЗЕНСКАЯ ОБЛАСТЬ", 
                   "ПЕРМСКИЙ КРАЙ", "ПРИМОРСКИЙ КРАЙ", 
                   "ПСКОВСКАЯ ОБЛАСТЬ", "РЕСПУБЛИКА АДЫГЕЯ", 
                   "РЕСПУБЛИКА БАШКОРТОСТАН", "РЕСПУБЛИКА БУРЯТИЯ", 
                   "РЕСПУБЛИКА ДАГЕСТАН", "РЕСПУБЛИКА ИНГУШЕТИЯ", 
                   "РЕСПУБЛИКА КАЛМЫКИЯ", "РЕСПУБЛИКА КАРЕЛИЯ",  
                   "РЕСПУБЛИКА КОМИ", "РЕСПУБЛИКА МАРИЙ ЭЛ", 
                   "РЕСПУБЛИКА МОРДОВИЯ", "РЕСПУБЛИКА САХА (ЯКУТИЯ)", 
                   "РЕСПУБЛИКА СЕВЕРНАЯ ОСЕТИЯ - АЛАНИЯ", "РЕСПУБЛИКА ТАТАРСТАН", 
                   "РЕСПУБЛИКА ТЫВА", "РЕСПУБЛИКА ХАКАСИЯ", 
                   "РОСТОВСКАЯ ОБЛАСТЬ", "РЯЗАНСКАЯ ОБЛАСТЬ", 
                   "САМАРСКАЯ ОБЛАСТЬ", "САНКТ-ПЕТЕРБУРГ", 
                   "САРАТОВСКАЯ ОБЛАСТЬ", "САХАЛИНСКАЯ ОБЛАСТЬ", 
                   "СВЕРДЛОВСКАЯ ОБЛАСТЬ", "СМОЛЕНСКАЯ ОБЛАСТЬ", 
                   "СТАВРОПОЛЬСКИЙ КРАЙ", "ТАМБОВСКАЯ ОБЛАСТЬ", 
                   "ТВЕРСКАЯ ОБЛАСТЬ", "ТОМСКАЯ ОБЛАСТЬ", 
                   "ТУЛЬСКАЯ ОБЛАСТЬ", "ТЮМЕНСКАЯ ОБЛАСТЬ",
                   "УДМУРТСКАЯ РЕСПУБЛИКА", "УЛЬЯНОВСКАЯ ОБЛАСТЬ", 
                   "ХАБАРОВСКИЙ КРАЙ", "ЧЕЛЯБИНСКАЯ ОБЛАСТЬ", 
                   "ЧЕЧЕНСКАЯ РЕСПУБЛИКА", "ЧИТИНСКАЯ ОБЛАСТЬ", 
                   "ЧУВАШСКАЯ РЕСПУБЛИКА", "ЧУКОТСКИЙ АВТОНОМНЫЙ ОКРУГ", 
                   "ЯРОСЛАВСКАЯ ОБЛАСТЬ", "ЧЕЛЯБИНСКАЯ ОБЛАСТЬ", 
                   "САНКТ-ПЕТЕРБУРГ", "НИЖЕГОРОДСКАЯ ОБЛАСТЬ",  
                   "ВЛАДИМИРСКАЯ ОБЛАСТЬ", "МОСКОВСКАЯ ОБЛАСТЬ", 
                   "МОСКОВСКАЯ ОБЛАСТЬ", "ЯМАЛО-НЕНЕЦКИЙ АВТОНОМНЫЙ ОКРУГ", 
                   "МОСКОВСКАЯ ОБЛАСТЬ", "МОСКОВСКАЯ ОБЛАСТЬ", 
                   "ТЮМЕНСКАЯ ОБЛАСТЬ", "КРАСНОЯРСКИЙ КРАЙ")
  
  tmp <- Map(function(reg_pattern, reg_replace) {
    data[grep(reg_pattern, living_region), 
         living_region := reg_replace]
    return(NULL)
    # без return(NULL) будет создан объект весом 1.4 Гб
  }, 
  reg_pattern, 
  reg_replace)
  rm(tmp)
  
  # импутируем пропуски в credit_count и overdue_credit_count значением -1
  data[is.na(credit_count), credit_count := -1]
  data[is.na(overdue_credit_count), overdue_credit_count := -1]
  
  # записываем пропуски в переменной living_region
  # в отдельную категорию "Пропуск"
  data[living_region == "", living_region := "Пропуск"]
  
  # записываем редкие категории переменных job_position, 
  # tariff_id и living_region в отдельную категорию
  data[job_position %in% rare_job_position, 
       # объединяем 4 самые редкие должности
       job_position := "OTHER"]
  data[tariff_id %in% rare_tariff_id, 
       # объединяем 8 самых редких тарифов
       tariff_id := "99"]
  data[living_region %in% rare_living_region, 
       # объединяем 4 самых редких региона
       living_region := "OTHER"]
  
  # опциональный шаг:
  # изменяем значения переменной tariff_id таким образом,
  # чтобы при конвертации в фактор и последующей обработке
  # уровней как числовых значений (пакет ranger)
  # интервалы между значениями были одинаковыми,
  # в данном случае значения будут отличаться на 1
  if (recode_tariff_id) {
    tmp <- 1:length(unique(data$tariff_id))
    names(tmp) <- sort(unique(data$tariff_id))
    data[, tariff_id := as.character(tmp[tariff_id])]
  }
  
  # опциональный шаг:
  # преобразовываем переменную open_account_flg в фактор, 
  # при этом значения 0 и 1 запишем как Class 0 и Class 1
  # только для обучающей выборки!
  if (process_target) {
    data[, open_account_flg := factor(open_account_flg, 
                                      levels = c(0, 1),
                                      labels = c("Class 0", "Class 1"))]
  }
  
  # преобразовываем остальные категориальные переменные в факторы
  data[, names(data)[sapply(data, is.character)] := lapply(.SD, as.factor),
       .SDcols = names(data)[sapply(data, is.character)]]
  
  # записываем категории living_region латиницей
  setattr(data$living_region,
          "levels",
          stringi::stri_trans_general(levels(data$living_region),
                                      "russian-latin/bgn"))
  # избавляемся от символа ʹ 
  setattr(data$living_region,
          "levels",
          gsub("\\ʹ", "", levels(data$living_region)))
}

# все изменения проведены in plaсe (по ссылке), поэтому изменен
# будет объект, переданный в качестве аргумента
# присваивание dt <- process_tinkoff_dt(data) не требуется
process_tinkoff_dt(data, recode_tariff_id = TRUE, process_target = TRUE)

# задаем стартовое значение генератора случайных чисел, чтобы
# каждый раз получать одно и то же разбиение
# на обучающую и контрольную выборки 
set.seed(42)
# разбиваем набор на обучающую и контрольную выборки
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
development <- data[ind == 1, ]
holdout <- data[ind == 2, ]

# загружаем пакет imputeMissings
library(imputeMissings)
# вычисляем медианы/моды на обучающем наборе
values <- compute(development)
# заменяем пропуски в количественных переменных
# медианами, а пропуски в категориальных 
# переменных заменяем модами
development <- impute(development, object = values)
holdout <- impute(holdout, object = values)

# загружаем пакет woeBinning
library(woeBinning)

# выполняем биннинг переменной
# tariff_id
tariff_binned <- woe.tree.binning(development, "open_account_flg", "tariff_id",
                                  min.perc.total = 0.01, min.perc.class = 0.005,
                                  stop.limit = 0.001, event.class = "Class 1")

# создаем новую переменную tariff_id.binned
# и дамми-переменные для всех уровней
# переменной tariff_id.binned
development <- woe.binning.deploy(development, tariff_binned, 
                                  add.woe.or.dum.var = "dum")
holdout <- woe.binning.deploy(holdout, tariff_binned, 
                              add.woe.or.dum.var = "dum")

# создаем копию переменной living_region
development$living_region_freq <- development$living_region
holdout$living_region_freq <- holdout$living_region

# записываем относительные частоты категорий 
# переменной living_region_freq
map_region <- table(development$living_region_freq)/nrow(development)

# заменяем категории относительными частотами 
# наблюдений в этих категориях
levels(development$living_region_freq) <- map_region
levels(holdout$living_region_freq) <- map_region

# преобразовываем в числовой вектор
development$living_region_freq <- as.numeric(
  levels(development$living_region_freq)[development$living_region_freq])
holdout$living_region_freq <- as.numeric(
  levels(holdout$living_region_freq)[holdout$living_region_freq])

# преобразовываем датафреймы во фреймы H2O
train <- as.h2o(development)
valid <- as.h2o(holdout)

# смотрим структуру обучающего фрейма
str(train)

# 7.1.2. Построение модели и работа с результатами

# строим модель случайного леса деревьев классификации, 
# все значения параметров взяты по умолчанию
rf1 <- h2o.randomForest(training_frame = train, # задаем обучающий фрейм
                        validation_frame = valid, # задаем контрольный фрейм
                        model_id = "RF_credit", # задаем идентификатор модели
                        x = c(1:13,15:25), # задаем индексы предикторов
                        y = 14, # задаем индекс зависимой переменной
                        seed = 10000 # задаем стартовое значение
)  

# выводим информацию о модели
summary(rf1)

# выводим информацию о важностях 
# всех предикторов
imp <- as.data.frame(h2o.varimp(rf1))
print(imp)

# строим таблицу выигрышей на
# обучающей выборке
h2o.gainsLift(rf1, train)

## 7.1.3. Работа с прогнозами модели

# вычисляем прогнозы модели
# для контрольной выборки
rf1_pred <- h2o.predict(rf1, newdata = valid)

# смотрим прогнозы модели
rf1_pred

# сохраняем прогнозы в csv-файл
h2o.exportFile(rf1_pred, path = "C:/Trees/predictions.csv", force=TRUE)

# строим ROC-кривую для обучающей выборки
plot(h2o.performance(rf1))

# строим ROC-кривую для контрольной выборки
plot(h2o.performance(rf1, valid=TRUE))

## 7.1.4. Использование ранней остановки при построении модели

# строим модель случайного леса из 600 деревьев,
# используя оценивание метрик по каждым 10
# деревьям и раннюю остановку
rf2 <- h2o.randomForest(training_frame = train, 
                        validation_frame = valid,
                        model_id = "RF_credit2",
                        x = c(1:13,15:25), y = 14, ntrees = 600,
                        score_tree_interval= 10,
                        stopping_metric = "AUC",
                        stopping_rounds = 10,
                        stopping_tolerance= 0.02, 
                        max_runtime_secs = 300,
                        seed = 10000)  

# выводим информацию о модели
summary(rf2)

# выводим историю вычислений полностью
history <- as.data.frame(h2o.scoreHistory(rf2))
print(history)

# строим модель с теми же самыми значениями гиперпараметров,
# не используя оценивание метрик по каждым 10
# деревьям и раннюю остановку
rf3 <- h2o.randomForest(training_frame = train, 
                        validation_frame = valid,
                        model_id = "RF_credit3",
                        x = c(1:13,15:25), y = 14, ntrees = 600,
                        seed = 10000)  

# выводим информацию о модели
summary(rf3)

## 7.1.5. Конструирование новых признаков

# загружаем игрушечные обучающий
# и контрольный наборы
train_example <- read.csv2("C:/Trees/mean_target_train.csv")
valid_example <- read.csv2("C:/Trees/mean_target_valid.csv")

# преобразовываем наборы во фреймы h2o
train_example <- as.h2o(train_example)
valid_example <- as.h2o(valid_example)

# смотрим обучающий фрейм
head(train_example, 20)

# создаем столбец с номерами блоков для каждого 
# наблюдения обучающего фрейма
train_example$fold <- h2o.kfold_column(train_example, 3, seed = 10000)
# смотрим обучающий фрейм
head(train_example, 20)

# создаем таблицу сопоставления
te_map <- h2o.target_encode_create(train_example, 
                                   x = list("feature"), 
                                   y = "target", 
                                   fold_column = "fold")

# смотрим таблицу сопоставления
te_map

# применяем таблицу сопоставления 
# к обучающему фрейму, не применяя
# сглаживание и добавление шума
te_train_example <- h2o.target_encode_apply(train_example, 
                                            x = list("feature"), 
                                            y = "target", 
                                            target_encode_map = te_map, 
                                            holdout_type = "KFold",
                                            fold_column = "fold",
                                            blended_avg = FALSE, 
                                            noise_level = 0)

# применяем таблицу сопоставления к контрольному 
# фрейму, обратите внимание, для контрольной выборки
# не надо задавать тип проверки, сглаживание среднего 
# и добавление шума
te_valid_example <- h2o.target_encode_apply(valid_example, 
                                            x = list("feature"), 
                                            y = "target", 
                                            target_encode_map = te_map, 
                                            holdout_type = "None",
                                            fold_column = "fold",
                                            blended_avg = FALSE, 
                                            noise_level = 0)

# смотрим результаты кодировки
# в обучающем фрейме
head(te_train_example, 20)

# смотрим результаты кодировки
# в контрольном фрейме
head(te_valid_example, 20)

# модифицируем функцию h2o.target_encode_apply
h2o.target_encode_apply_custom <- function(data, x, y, 
                                           target_encode_map, 
                                           holdout_type, 
                                           fold_column = NULL, 
                                           blended_avg = TRUE, 
                                           noise_level = NULL, 
                                           seed = -1) {
  
  if (missing(data)) {
    stop("argument 'data' is missing, with no default")
  }
  if (missing(target_encode_map)) {
    stop("argument 'target_encode_map' is missing, with no default")
  }
  if (!is.h2o(data)) {
    stop("argument `data` must be a valid H2OFrame")
  }  
  if (!is.logical(blended_avg)) {
    stop("`blended_avg` must be logical")
  }
  if (holdout_type == "KFold") {
    if (is.null(fold_column)) {
      stop("`fold_column` must be provided for `holdout_type = KFold")
    }
  }
  if (!is.null(noise_level)) {
    if (!is.numeric(noise_level) || length(noise_level) > 1L) {
      stop("`noise_level` must be a numeric vector of length 1")
    }  else if (noise_level < 0) {
      stop("`noise_level` must be non-negative")
    }  
  }
  if (is.numeric(y)) {
    y <- colnames(data)[y]
  }
  if (is.numeric(unlist(x))) {
    x <- sapply(x, function(i) colnames(data)[i])
  }
  if (is.numeric(fold_column)) {
    fold_column <- colnames(data)[fold_column]
  }
  
  if (is.null(noise_level)) {
    # если для `noise_level` задано NULL, значение выбирается, 
    # исходя из распределения `y`
    noise_level <- ifelse(is.factor(data[[y]]), 0.01, 
                          (max(data[[y]], na.rm = TRUE) - min(data[[y]], 
                                                              na.rm = TRUE))*0.01)
  }
  
  # удаляем столбцы-строки из `data` (см.: https://0xdata.atlassian.net/browse/PUBDEV-5266)
  dd <- h2o.describe(data)
  string_cols <- as.character(dd[which(dd$Type == "string"), "Label"])
  if (length(string_cols) > 0) {
    data <- data[setdiff(colnames(data), string_cols)]
    warning(paste0("The string columns: ", paste(string_cols, collapse = ", "), 
                   " were dropped from the dataset"))
  }
  
  te_frame <- data
  for (cols in x){
    
    x_map <- target_encode_map[[paste(cols, collapse = ":")]]
    
    if (holdout_type == "KFold") {
      
      holdout_encode_map <- NULL
      
      folds <- as.matrix(h2o.unique(x_map[[fold_column]]))[, 1]
      for (i in folds){
        out_fold <- x_map[x_map[[fold_column]] != i, ]
        
        # вычисляем сумму значений y и частоту наблюдений по каждой категории, 
        # исходя из out-of-fold данных
        out_fold <- h2o.group_by(out_fold, cols, sum("numerator"), sum("denominator"))
        colnames(out_fold)[which(colnames(out_fold) == "sum_numerator")] <- "numerator"
        colnames(out_fold)[which(colnames(out_fold) == "sum_denominator")] <- "denominator"
        out_fold$fold <- i
        colnames(out_fold)[ncol(out_fold)] <- fold_column
        
        holdout_encode_map <- h2o.rbind(holdout_encode_map, out_fold)
      }
      
      te_frame <- h2o.merge(te_frame, holdout_encode_map, 
                            by = c(cols, fold_column), all.x = TRUE)
    }
    
    if (holdout_type == "LeaveOneOut") {
      
      # объединяем таблицу сопоставления с данными
      te_frame <- h2o.merge(te_frame, x_map, by = cols, 
                            all.x = TRUE, all.y = FALSE)
      
      # вычисляем числитель и знаменатель
      te_frame$numerator <- h2o.ifelse(is.na(te_frame[[y]]), 
                                       te_frame$numerator, 
                                       te_frame$numerator - te_frame[[y]])
      
      te_frame$denominator <- h2o.ifelse(is.na(te_frame[[y]]),
                                         te_frame$denominator, 
                                         te_frame$denominator - 1)
    }
    if (holdout_type == "None") {
      
      if (!is.null(fold_column)) {
        x_map <- h2o.group_by(x_map, cols, sum("numerator"), sum("denominator"))
        colnames(x_map)[which(colnames(x_map) == "sum_denominator")] <- "denominator"
        colnames(x_map)[which(colnames(x_map) == "sum_numerator")] <- "numerator"
      }
      # объединяем таблицу сопоставления с данными
      te_frame <- h2o.merge(te_frame, x_map, by = cols, 
                            all.x = TRUE, all.y = FALSE)
    }
    
    # вычисляем групповое среднее
    if (blended_avg) {
      
      # вычисляем сглаженное групповое среднее, 
      # задаем свои значения k и f
      k <- 20
      f <- 10
      global_mean <- sum(x_map$numerator)/sum(x_map$denominator)
      lambda <- 1/(1 + exp((-1)* (te_frame$denominator - k)/f))
      te_frame$target_encode <- ((1 - lambda) * global_mean) + 
        (lambda * te_frame$numerator/te_frame$denominator)
      
    } else {
      
      # вычисляем среднее значение зависимой переменной в группе
      te_frame$target_encode  <- te_frame$numerator/te_frame$denominator
    }
    
    # добавляем случайный шум
    if (noise_level > 0) {
      # генерируем случайные числа из одномерного распределения  
      random_noise <- h2o.runif(te_frame, seed = seed)
      # масштабируем
      random_noise <- random_noise * 2 * noise_level - noise_level
      # добавляем шум к кодировке
      te_frame$target_encode  <- te_frame$target_encode  + random_noise
    }
    
    te_frame$numerator <- NULL
    te_frame$denominator <- NULL
    colnames(te_frame)[which(colnames(te_frame) == 
                               "target_encode")] <- paste0("TargetEncode_", 
                                                           paste(cols, 
                                                                 collapse = ":"))
  }
  
  
  return(te_frame)
}


# применяем таблицу сопоставления 
# к обучающему фрейму, используя
# сглаживание
te_train_example2 <- h2o.target_encode_apply_custom(train_example, 
                                                    x = list("feature"), 
                                                    y = "target", 
                                                    target_encode_map = te_map, 
                                                    holdout_type = "KFold",
                                                    fold_column = "fold",
                                                    blended_avg = TRUE, 
                                                    noise_level = 0)

# смотрим результаты новой кодировки
# в обучающем фрейме
head(te_train_example2, 20)

# создаем столбец с номера блоков для
# каждого наблюдения
train$fold <- h2o.kfold_column(train, 5, seed = 10000)

# создаем таблицу сопоставления
region_map <- h2o.target_encode_create(train, 
                                       x = list("living_region"), 
                                       y = "open_account_flg", 
                                       fold_column = "fold")

# применяем таблицу сопоставления к обучающему
# и контрольному фреймам
ext_train <- h2o.target_encode_apply(train, 
                                     x = list("living_region"), 
                                     y = "open_account_flg", 
                                     target_encode_map = region_map, 
                                     holdout_type = "KFold",
                                     fold_column = "fold",
                                     blended_avg = TRUE, 
                                     noise_level = 0.05, 
                                     seed = 10000)

ext_valid <- h2o.target_encode_apply(valid, 
                                     x = list("living_region"), 
                                     y = "open_account_flg",
                                     target_encode_map = region_map, 
                                     holdout_type = "None",
                                     fold_column = "fold",
                                     blended_avg = FALSE, 
                                     noise_level = 0)

# задаем имя зависимой переменной и
# имена предикторов
dependent <- "open_account_flg"
predictors <- setdiff(names(ext_train), 
                      c("open_account_flg", "fold", "living_region"))

# строим модель с прежними значениями гиперпараметров,
# используя фреймы с новой переменной 
# TargetEncode_living_region
rf4 <- h2o.randomForest(training_frame = ext_train, 
                        validation_frame = ext_valid,
                        model_id = "RF_credit4",
                        x = predictors, 
                        y = dependent, 
                        ntrees = 600,
                        seed = 10000)  

# выводим информацию о модели
summary(rf4)

## 7.1.6. Поиск оптимальных значений параметров с помощью решетчатого поиска

# выполняем решетчатый поиск с перекрестной проверкой,
# варьируя глубину и количество случайно отбираемых
# предикторов, при этом задаем гистограммирование 
# RoundRobin
grid <- h2o.grid(x = predictors, y = dependent, 
                 training_frame = ext_train,
                 algorithm = "drf", ntrees = 600, 
                 histogram_type = "RoundRobin",
                 grid_id = "gridresults",
                 hyper_params = list(mtries = c(3, 4), 
                                     max_depth = c(14, 16)),
                 nfolds = 5, keep_cross_validation_predictions = TRUE,
                 seed = 10000)

# выводим информацию о моделях
# решетчатого поиска
summary(grid)

# отсортируем полученные модели
# в порядке убывания AUC
sortedGrid <- h2o.getGrid("gridresults", sort_by = "auc", decreasing = TRUE)
sortedGrid

# извлекаем из списка моделей 
# наилучшую модель
best_model_id <- sortedGrid@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)
best_model

# вычисляем AUC наилучшей модели
# на контрольной выборке
perf <- h2o.performance(best_model, ext_valid)
h2o.auc(perf)

## 7.1.7. Обучение модели на всех исторических данных, 
## сохранение модели и применение к новым данным

# смотрим порядок следования имен столбцов в фреймах
train_names <- names(ext_train)
valid_names <- names(ext_valid)

col_names <- list("имена столбцов в обучающем фрейме" = train_names, 
                  "имена столбцов в контрольном фрейме" = valid_names)
col_names

# удаляем переменную fold
ext_train <- ext_train[, -2]

# задаем строгий порядок следования имен столбцов с tariff_id.binned
# по TargetEncode_living_region и размещаем в начале фрейма
cols <- c("tariff_id.binned", "dum.tariff_id.2.binned",
          "dum.tariff_id.2119.binned", "dum.tariff_id.123.binned",
          "dum.tariff_id.20.binned", "dum.tariff_id.186316.binned",
          "dum.tariff_id.15.binned", "dum.tariff_id.14.binned",
          "dum.tariff_id.misclevelpos.binned", 
          "dum.tariff_id.misclevelneg.binned",
          "living_region_freq", "TargetEncode_living_region")

ext_train <- ext_train[, c(cols, setdiff(names(ext_train), cols))]
ext_valid <- ext_valid[, c(cols, setdiff(names(ext_valid), cols))]


# смотрим порядок следования имен столбцов во фреймах
train_names <- names(ext_train)
valid_names <- names(ext_valid)

col_names <- list("имена столбцов в обучающем фрейме" = train_names, 
                  "имена столбцов в контрольном фрейме" = valid_names)
col_names

# выполняем объединение фреймов
ext_full <- h2o.rbind(ext_train, ext_valid)

str(ext_full)

# удаляем переменную TargetEncode_living_region
ext_full <- ext_full[ , -which(names(ext_full) == "TargetEncode_living_region")]

# создаем столбец с номера блоков для
# каждого наблюдения объединенного
# фрейма
ext_full$fold <- h2o.kfold_column(ext_full, 5, seed = 10000)
# создаем таблицу сопоставления
region_map <- h2o.target_encode_create(ext_full, 
                                       x = list("living_region"), 
                                       y = "open_account_flg", 
                                       fold_column = "fold")

# применяем таблицу сопоставления к
# объединенному фрейму
ext_full <- h2o.target_encode_apply(ext_full, x = list("living_region"), 
                                    y = "open_account_flg", 
                                    target_encode_map = region_map, 
                                    holdout_type = "KFold",
                                    fold_column = "fold",
                                    blended_avg = TRUE, 
                                    noise_level = 0.05, 
                                    seed = 10000)

# временно преобразовываем объединенный
# фрейм в датафрейм
ext_full <- as.data.frame(ext_full)

# создаем копию переменной living_region
# – переменную living_region_freq 
ext_full$living_region_freq <- ext_full$living_region

# вычисляем относительные частоты категорий 
# переменной living_region_freq на
# объединенном наборе
map_freq <- table(ext_full$living_region_freq)/nrow(ext_full)

# заменяем категории относительными частотами 
# наблюдений в этих категориях
levels(ext_full$living_region_freq) <- map_freq

# преобразовываем в числовой вектор
ext_full$living_region_freq <- as.numeric(
  levels(ext_full$living_region_freq)[ext_full$living_region_freq])

# преобразовываем датафрейм
# обратно во фрейм
ext_full <- as.h2o(ext_full)

# обучаем модель случайного леса с наилучшими
# значениями гиперпараметров на объединенном
# фрейме
rf_full <- h2o.randomForest(training_frame = ext_full,
                            model_id = "RF_credit_full",
                            x = predictors, 
                            y = dependent, 
                            ntrees = 600, 
                            mtries = 4, 
                            max_depth = 16,
                            histogram_type = "RoundRobin",
                            seed = 10000)

# сохраняем модель в назначенный каталог
path <- h2o.saveModel(rf_full, path="C:\\Trees\\mybest_model", force=TRUE)

# смотрим месторасположение модели
print(path)

# выгружаем ранее сохраненную модель
m_loaded <- h2o.loadModel("C:\\Trees\\mybest_model\\RF_credit_full")

# загружаем новые данные
newdata <- fread("C:/Trees/credit_test.csv", dec = ",") 

# смотрим данные
str(newdata)

# применяем функцию предобработки к новым данным
process_tinkoff_dt(newdata, recode_tariff_id = TRUE, process_target = FALSE)

# смотрим данные
str(newdata)

# смотрим частоты категорий 
# переменной living_region
sort(table(newdata$living_region, useNA = "ifany"), 
     decreasing = TRUE)

# новые категории переменной living_region, 
# отсутствующие в обучающих данных,
# записываем в категорию OTHER
library(car)
newdata$living_region <- recode(newdata$living_region, "c('G. CHELYABINSK', 
                                'MOSKOVSKIY P') = 'OTHER'")

# выводим информацию о пропусках
sapply(newdata, function(x) sum(is.na(x)))

# создаем новую переменную tariff_id.binned
# и дамми-переменные для всех уровней
# переменной tariff_id.binned
newdata <- woe.binning.deploy(newdata, tariff_binned, 
                              add.woe.or.dum.var = "dum")

# создаем копию переменной living_region 
# - переменную living_region_freq
newdata$living_region_freq <- newdata$living_region

# заменяем категории относительными частотами наблюдений в этих 
# категориях, вычисленными по всей исторической выборке
levels(newdata$living_region_freq) <- map_freq

# преобразовываем в числовой вектор
newdata$living_region_freq <- as.numeric(
  levels(newdata$living_region_freq)[newdata$living_region_freq])

# пишем функцию, которая вычисляет средние значения
# зависимой переменной open_account_flg в категориях 
# переменной living_region на всей
# исторической выборке
mean_target_encode <- function(data) {
  data$open_account_flg <- recode(data$open_account_flg, 
                                  "'Class 0'= 0; 'Class 1' = 1", 
                                  as.factor = FALSE)
    living_region_mean_target <- tapply(
    X = data$open_account_flg, 
    INDEX = data$living_region,
    mean)
  return(living_region_mean_target)
}

# вычисляет средние значения зависимой переменной 
# open_account_flg в категориях переменной 
# living_region на всей исторической выборке
living_region_mean_target <- mean_target_encode(data)

# заменяем категории переменной living_region средними значениями 
# зависимой переменной open_account_flg в этих категориях,
# вычисленными на всей исторической выборке
newdata$living_region <- living_region_mean_target[newdata$living_region]

# переименовываем переменную living_region 
# в TargetEncode_living_region
names(newdata)[names(newdata) == "living_region"] <- "TargetEncode_living_region"

# снова смотрим данные
str(newdata)

# преобразовываем датафрейм во фрейм H2O
newframe <- as.h2o(newdata)

# вычисляем прогнозы модели
# для новых данных
newdata_predictions <- h2o.predict(rf_full, newdata = newframe)

## 7.2. Решение задачи регрессии

# загружаем данные
data <- read.csv2("C:/Trees/Creddebt.csv")

str(data)

data$ed = recode(data$ed, "'Неполное среднее' = 1; 
                 'Среднее' = 2; 
                 'Среднее специальное' = 3;
                 'Незаконченное высшее' = 4; 
                 'Высшее, ученая степень'= 5", 
                 as.factor.result = FALSE)

set.seed(100)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
development <- data[ind==1, ]
holdout <- data[ind==2, ]

# преобразовываем датафреймы во фреймы H2O
dev <- as.h2o(development)
hold <- as.h2o(holdout)

# строим модель случайного леса деревьев регрессии
rf_regr <- h2o.randomForest(x=c(1:6), y=7, training_frame = dev, 
                            validation_frame = hold, ntrees=300, 
                            seed=10000) 

# выводим информацию о модели
summary(rf_regr)


## 7.3. Предварительная подготовка данных в H2O

# импортируем CSV-файл в кластер H2O
credit <- h2o.importFile(path = "C:/Trees/CredRating.csv", 
                         destination_frame = "credit")

# смотрим созданный фрейм H2O
credit

# смотрим имена столбцов
h2o.colnames(credit)

# смотрим структуру фрейма H2O
h2o.str(credit)

# преобразовываем переменные client_id и
# Credit_rating в тип enum
credit[, c(1, 7)] <- as.factor(credit[, c(1, 7)])

# выводим индексы столбцов типа enum
h2o.columns_by_type(object = credit, coltype="categorical")

# смотрим уникальные значения столбца с индексом 7
h2o.unique(credit[, 7])

# смотрим количество уникальных значений
# по столбцам типа enum
h2o.nlevels(credit)

# удаляем переменную client_id
credit <- credit[, -1]

# выводим частоты категорий для
# столбца с индексом 6
h2o.table(credit[, 6])

# выполняем случайное разбиение набора данных
# на обучающую и контрольную выборки
splits <- h2o.splitFrame(data = credit, ratios = 0.70, seed = 10000)
train_frame <- splits[[1]]
test_frame <- splits[[2]]

# выводим информацию о переменных обучающего
# фрейма train_set
options(scipen = 999, digits = 3)
h2o.describe(train_frame)

# выполняем импутацию пропусков переменной Age 
# в обучающем и контрольном фреймах средним 
# значением, вычисленным в обучающем фрейме
train_frame$Age[is.na(train_frame$Age)] <- h2o.mean(train_frame$Age, 
                                                    na.rm = TRUE)
test_frame$Age[is.na(test_frame$Age)] <- h2o.mean(train_frame$Age, 
                                                  na.rm = TRUE)

# вычисляем моды для переменных
# Income и Credit_cards
varnames <- c("Income", "Credit_cards")
lapply(as.data.frame(train_frame[, varnames]), 
       function(x) sort(table(x), decreasing = TRUE))

# выполняем импутацию пропусков переменных Income 
# и Credit_cards в обучающем и контрольном фреймах 
# модами, вычисленными в обучающем фрейме
h2o.impute(train_frame[, c("Income", "Credit_cards")], 
           values = c("Medium", "5 or more"))
h2o.impute(test_frame[, c("Income", "Credit_cards")], 
           values = c("Medium", "5 or more"))

# завершаем работу с H2O
h2o.shutdown(prompt = TRUE)
