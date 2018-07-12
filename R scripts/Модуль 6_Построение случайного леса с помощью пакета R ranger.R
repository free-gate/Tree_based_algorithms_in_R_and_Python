# Модуль 6. Построение случайного леса с помощью пакета R ranger

# Лекция 6.1. Построение ансамбля деревьев классификации

## 6.1.1. Подготовка данных

# загружаем данные, столбцы со строковыми значениями 
# обрабатываем как символьные векторы, пустые 
# строковые значения будут записаны как пропуски
data <- read.csv2("C:/Trees/credit_train.csv", 
                  stringsAsFactors = FALSE, na.strings = "")

# cмотрим типы переменных
str(data)

# удаляем идентификационную переменную
data$client_id <- NULL
# преобразовываем переменную open_account_flg в фактор, 
# при этом значения 0 и 1 запишем как NoCredit и Credit
data$open_account_flg <- factor(data$open_account_flg, levels = c(0, 1),
                                labels = c("NoCredit", "Credit"))

# смотрим типы переменных
str(data)

# смотрим уникальные значения 
# переменной living_region
unique(data$living_region)

# Приводим к единому стандарту названия регионов
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
  data$living_region[grep(reg_pattern, data$living_region)] <<- reg_replace
  return(NULL)
  # без return(NULL) будет создан объект весом 1.4 Гб
}, 
reg_pattern, 
reg_replace)
rm(tmp)

# смотрим уникальные значения 
# переменной living_region
unique(data$living_region)

# смотрим частоты по категориальным переменным 
lapply(data[sapply(data, is.character)], 
       function(x) sort(table(x, useNA = "ifany"), decreasing = TRUE))

# выводим информацию о пропусках
sapply(data, function(x) sum(is.na(x)))

# импутируем пропуски в переменных credit_count
# и overdue_credit_count -1
data$credit_count[is.na(data$credit_count)] <- -1
data$overdue_credit_count[is.na(data$overdue_credit_count)] <- -1

# записываем пропуски в переменной living_region
# в отдельную категорию "Пропуск"
data$living_region[is.na(data$living_region)] <- "Пропуск"

# заменяем пропуски в переменной 
# monthly_income медианой
data$monthly_income[is.na(data$monthly_income)] <- median(
  data$monthly_income, na.rm = TRUE)

# выводим информацию о пропусках
sapply(data, function(x) sum(is.na(x)))

# преобразовываем символьные 
# векторы в факторы
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], 
                                           factor)

# cмотрим типы переменных
str(data)

# задаем стартовое значение генератора случайных чисел, чтобы
# каждый раз получать одно и то же разбиение
# на обучающую и контрольную выборки 
set.seed(42)
# разбиваем набор на обучающую и контрольную выборки
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
development <- data[ind == 1, ]
holdout <- data[ind == 2, ]

# загружаем пакет ranger
library(ranger)

# строим ансамбль деревьев классификации
cl_model <- ranger(open_account_flg ~ ., development, seed = 152)

# выводим информацию о качестве модели
print(cl_model)

# записываем результаты модели для контрольной выборки
cl_result <- predict(cl_model, holdout, type = "response", seed = 152)

# смотрим структуру объекта result
str(cl_result)

# смотрим матрицу ошибок для контрольной выборки
table(holdout$open_account_flg, cl_result$predictions)

# Лекция 6.2. Построение случайного леса вероятностей

# строим лес вероятностей
prob_model <- ranger(open_account_flg~., development, 
                     importance="permutation", 
                     probability=TRUE, seed=152)

# выводим информацию о качестве модели
print(prob_model)

# записываем результаты модели для контрольной выборки
prob_result <- predict(prob_model, holdout, type = "response", seed = 152)

# смотрим структуру объекта result
str(prob_result)

# загружаем пакет pROC и вычисляем AUC
# для контрольной выборки
library(pROC)
roc <- roc(holdout$open_account_flg, prob_result$predictions[, 1], ci=TRUE)
roc

# посмотрим пермутированные
# важности предикторов
importance(prob_model)

# выполняем перекодировку переменной tariff_id
data$tariff_id <- as.factor(as.integer(as.factor(data$tariff_id)))
# смотрим частоты категорий переменной tariff_id в новой кодировке
sort(table(data$tariff_id, useNA = "ifany"), decreasing = TRUE)

# загружаем пакет car
library(car)
# записываем редкие категории переменной 
# tariff_id в отдельную категорию
data$tariff_id <- car::recode(data$tariff_id, "c('14', '16', '26', '15', '24', 
                                                 '27', '17', '33') = '99'")

# выводим информацию о частотах категорий
# переменных job_position и living_region
names <- c("job_position", "living_region")
lapply(data[, names], function(x) sort(table(x, useNA = "ifany"), 
                                       decreasing = TRUE))

# записываем редкие категории переменных job_position 
# и living_region в отдельную категорию
data$job_position <- car::recode(data$job_position,
                                 "c('PNS', 'HSK', 'INV', 'ONB')='OTHER'")
data$living_region <- car::recode(data$living_region, "c('ЧУКОТСКИЙ АВТОНОМНЫЙ ОКРУГ', 
                                                         'ЧЕЧЕНСКАЯ РЕСПУБЛИКА', 
                                                         'РЕСПУБЛИКА ИНГУШЕТИЯ', 
                                                         'ЧИТИНСКАЯ ОБЛАСТЬ')='OTHER'")

# задаем стартовое значение генератора случайных чисел, чтобы
# каждый раз получать одно и то же разбиение
# на обучающую и контрольную выборки 
set.seed(42)
# разбиваем набор на обучающую и контрольную выборки
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
development <- data[ind == 1, ]
holdout <- data[ind == 2, ]

# загружаем пакет CHAID
library(CHAID)

# задаем набор условий для построения дерева CHAID
params <- chaid_control(minprob = 1, minsplit = 200, 
                        minbucket = 100, alpha3=0.04, maxheight=1)

# строим модель дерева CHAID
chd  <- chaid(open_account_flg ~ tariff_id, control = params, development)

# выводим схему дерева
print(chd)


# загружаем пакет memisc
library(memisc)
# создадим новую переменную tariffcat с укрупненными
# категориями переменной tariff_id
development$tariffcat <- memisc::recode(development$tariff_id,
                                        "1" <- c("19", "5"), 
                                        "2" <- c("10", "23"),
                                        "3" <- c("13", "18", "31"),
                                        "4" <- c("1", "11", "25"),
                                        "5" <- c("20", "29", "3", "32", "6", "7"),
                                        "6" <- c("12", "28"),
                                        "7" <- c("2", "4"),
                                        "8" <- "21",
                                        "9" <- "22",
                                        "10" <- "30",
                                        "11" <- "8",
                                        otherwise = "12")

# устанавливаем пакет InformationValue
# install.packages("InformationValue")
# загружаем пакет InformationValue
library(InformationValue)

# создадим переменную target, преобразовав метки 
# зависимой переменной в 0 и 1
target <- car::recode(development$open_account_flg, 
                      "'NoCredit'='0'; 'Credit'='1'")

# отключаем экспоненциальное представление чисел
options(scipen = 999, digits = 3)

# выводим WOE и IV по каждой категории
# переменной tariffcat
WOETable(X = development$tariffcat, Y = target)

# выводим итоговое IV для
# переменной tariffcat
IV(X = development$tariffcat, Y = target)

# записываем имена факторов, за исключением
# open_account_flg
cat_var_names <- names(development[, sapply(development, is.factor)])
cat_names <- cat_var_names[! cat_var_names %in% "open_account_flg"]
# выводим IV по факторам
sapply(development[cat_names], function(x) IV(x, Y = target))

# вычислим значение хи-квадрат 
# для переменной tariffcat
chisq.test(development$tariffcat, target)

# удаляем переменную tariffcat
development$tariffcat <- NULL

# устанавливаем пакет woeBinning
# install.packages("woeBinning")
# загружаем пакет woeBinning
library(woeBinning)

str(development)

# выполняем биннинг переменной
# tariff_id
tariff_binned <- woe.tree.binning(development, "open_account_flg", "tariff_id",
                                  min.perc.total = 0.01, min.perc.class = 0.005,
                                  stop.limit = 0.001, event.class = "Credit")
tariff_binned

# создаем новую переменную tariff_id.binned
# и дамми-переменные для всех уровней
# переменной tariff_id.binned
development <- woe.binning.deploy(development, tariff_binned, 
                                  add.woe.or.dum.var = "dum")
holdout <- woe.binning.deploy(holdout, tariff_binned, 
                              add.woe.or.dum.var = "dum")

# смотрим добавленные переменные
str(development)

# строим модель, перекодировав tariff_id и укрупнив 
# категории переменных tariff_id, job_position и 
# living_region, добавив новые переменные по 
# итогам WOE-преобразования переменной tariff_id
prob_model2 <- ranger(open_account_flg~., development,
                      probability = TRUE, seed = 152)

# записываем результаты модели для контрольной выборки
prob_result2 <- predict(prob_model2, holdout,
                        type = "response", seed = 152) 

# вычисляем AUC
roc2 <- roc(holdout$open_account_flg, 
            prob_result2$predictions[, 1], ci = TRUE)
roc2

# создаем копию переменной living_region
development$living_region_freq <- development$living_region
holdout$living_region_freq <- holdout$living_region

# вычисляем относительные частоты категорий переменной living_region_freq
map_region <- table(development$living_region_freq)/nrow(development)
map_region 

# заменяем категории частотами 
# наблюдений в этих категориях
levels(development$living_region_freq) <- map_region
levels(holdout$living_region_freq) <- map_region

# преобразовываем в числовой вектор
development$living_region_freq <- as.numeric(
  levels(development$living_region_freq)[development$living_region_freq])
holdout$living_region_freq <- as.numeric(
  levels(holdout$living_region_freq)[holdout$living_region_freq])

# посмотрим на результаты преобразования
head(development[, c("living_region", "living_region_freq")], 10)

# строим модель с новыми переменными, увеличив
# минимальный размер терминального узла
prob_model3 <- ranger(open_account_flg ~ ., development, 
                      min.node.size = 120, probability = TRUE, 
                      seed = 152)

# записываем результаты модели для контрольной выборки
prob_result3 <- predict(prob_model3, holdout, 
                        type = "response", seed = 152)

# вычисляем AUC
roc3 <- roc(holdout$open_account_flg, 
            prob_result3$predictions[, 1], ci = TRUE)
roc3

# загружаем пакет caret
library(caret)

# настраиваем параметры решетчатого поиска
# для случайного леса
ctrl = trainControl(method = "cv", number = 5, classProbs = TRUE, 
                    summaryFunction = twoClassSummary)
gridSet <- expand.grid(
  mtry = c(18, 20),
  min.node.size = c(120, 160),
  splitrule = "gini"
)

set.seed(152)
# запускаем решетчатый поиск, перебираем 
# параметры mtry и min.node.size пакета
# ranger (то есть количество случайно
# обираемых предикторов для разбиения и
# минимальное количество наблюдений в 
# терминальном узле для случайного леса)
ranger_gridsearch <- train(open_account_flg ~ ., 
                           data = development,
                           method = "ranger",
                           metric = "ROC",
                           trControl = ctrl, 
                           tuneGrid = gridSet, seed=152)

# печатаем результаты решетчатого поиска
print(ranger_gridsearch)

# вычисляем AUC на тестовой выборке
score <- predict(ranger_gridsearch, holdout, type = "prob")
roc <- roc(holdout$open_account_flg, score[, 1], ci = TRUE)
roc

# Лекция 6.3. Построение случайного леса выживаемости

# загружаем данные
data <- read.csv2("C:/Trees/Telco.csv")

# смотрим типы данных
str(data)

# превращаем переменную churn в фактор
data$churn <- as.factor(data$churn)

# смотрим типы данных
str(data)

# устанавливаем пакет survival
# install.packages("survival")
# загружаем пакет survival
library(survival)

# строим ансамбль деревьев выживаемости
model <- ranger(Surv(tenure, churn) ~ ., data, seed = 152)

# выводим информацию о качестве модели
print(model)

# записываем результаты модели и смотрим структуру объекта result 
result <- predict(model, data, type = "response", seed = 152)
str(result)

# строим кривые функции выживания для наблюдений 3 и 8
plot(model$unique.death.times, 
     model$survival[3, ], 
     type = "l", col = "orange", ylim = c(0.4, 1))
lines(model$unique.death.times, model$survival[8, ], col = "blue")


# смотрим наблюдения 3 и 8
data[3, ]
data[8, ]

# строим кривые функции кумулятивного риска для наблюдений 3 и 8
plot(model$unique.death.times, 
     model$chf[3, ], 
     type = "l", col = "orange", ylim = c(0.0,1))
lines(model$unique.death.times, model$chf[8, ], col = "blue")



