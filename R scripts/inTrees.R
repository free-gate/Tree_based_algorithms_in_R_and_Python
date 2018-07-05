# загружаем данные
data <- read.csv2("C:/Trees/Telco.csv")

# превращаем переменную churn в фактор
data$churn <- as.factor(data$churn)

# смотрим типы данных
str(data)

# разбиваем данные на обучающую
# и контрольную выборки
set.seed(42)
random_number <- runif(nrow(data), 0, 1)
development <- data[random_number > 0.3, ]
holdout <- data[random_number <= 0.3, ]

# загружаем пакет randomForest
library(randomForest)

# устанавливаем пакет inTrees
# install.packages("inTrees")
# загружаем пакет inTrees
library(inTrees)

# задаем стартовое значение генератора случайных
# чисел для воспроизводимости результатов
set.seed(152)

# строим случайный лес деревьев классификации
model<-randomForest(churn ~ ., development, nodesize = 50)

# смотрим AUC на контрольной выборке
library(pROC)
prob <- predict(model, holdout, type = "prob")
roc(holdout$churn, prob[, 2], ci = TRUE)

# преобразуем деревья леса в нужный формат
treeList <- RF2List(model)

# извлекаем условия из деревьев случайного леса
ruleExec <- extractRules(treeList, 
                         ntree=500, 
                         development[,c(1:11)])
# выводим первые 10 условий
head(ruleExec, 10)

# отберем уникальные условия
ruleExec <- unique(ruleExec)

# измеряем условия
ruleMetric <- getRuleMetric(ruleExec, 
                            development[,c(1:11)], 
                            development$churn)

# выводим первые 5 условий
# с метриками качества
head(ruleMetric, 5)

# выполняем прунинг условий
prunedRules <- pruneRule(ruleMetric, 
                         development[,c(1:11)], 
                         development$churn)

# выводим первые 5 обрезанных условий
# с метриками качества
head(prunedRules, 5)

# выполняем отбор обрезанных условий c помощью
# регуляризованного случайного леса
selectedRules <- selectRuleRRF(prunedRules, 
                               development[,c(1:11)], 
                               development$churn)

# выведем первые 5 отобранных 
# обрезанных условий
head(selectedRules, 5)

# создаем модель - перечень отобранных обрезанных условий, 
# отсортированных в порядке возрастания ошибки
learner <- buildLearner(selectedRules, 
                        development[,c(1:11)], 
                        development$churn)

# смотрим первые 5 условий
head(learner, 5)

# применяем модель к контрольному набору данных
# и получаем прогнозы
pred <- applyLearner(learner, holdout[,c(1:11)])

# записываем условия в более наглядном виде
Simp_Learner <- presentRules(selectedRules,
                             colnames(development[,c(1:11)]))

# смотрим первые 5 условий
head(Simp_Learner, 5)

# загружаем пакет rcompanion
library(rcompanion)
# строим гистограмму распределения
# переменной income
plotNormalHistogram(development$income)

# строим график квантиль-квантиль
# для переменной income
qqnorm(development$income, 
       xlab="Квантили теоретического распределения",
       ylab="Квантили наблюдаемого распределения")
qqline(development$income, col="red")

# выполняем логарифмическое преобразование переменной
# income, используем константу, чтобы не брать
# логарифм нуля
a <- 0.001
var_log <- log(development$income + a)

# строим гистограмму распределения
# переменной var_log
plotNormalHistogram(var_log)

# строим график квантиль-квантиль
# для переменной var_log
qqnorm(var_log, 
       xlab = "Квантили теоретического распределения",
       ylab = "Квантили наблюдаемого распределения")
qqline(var_log, col = "red")


# выполняем преобразование корнем,
# используем модуль, чтобы не вычислять корни
# отрицательных чисел, и затем учитываем знак числа
var_sq <- sign(development$income) * abs(development$income)^(1/2)

# выводим гистограмму распределения
# переменной var_sq
plotNormalHistogram(var_sq)

# строим график квантиль-квантиль
# для переменной var_sq
qqnorm(var_sq, 
       xlab = "Квантили теоретического распределения",
       ylab = "Квантили наблюдаемого распределения")
qqline(var_sq, col = "red")


# вычисляем лямбду преобразования Бокса-Кокса
# с помощью функции powerTransform пакета car
library(car)
powerTransform(development$income)

# выполняем преобразование с помощью вычисленной лямбды,
# используя функцию bcPower пакета car
var_boxcox <-bcPower(development$income, -0.2887634)

# выводим гистограмму распределения
# переменной var_boxcox
plotNormalHistogram(var_boxcox)

# выводим график квантиль-квантиль
# для переменной var_boxcox
qqnorm(var_boxcox, 
       xlab = "Квантили теоретического распределения",
       ylab = "Квантили наблюдаемого распределения")
qqline(var_boxcox, col = "red")


# пишем функцию, которая выполняет масштабирование
StandardScaler_manually <- function(x) {
  x <- (x - mean(x)) / sd(x)
  }

# применяем функцию к переменной
var_standardscaled <- StandardScaler_manually(development$income)
head(var_standardscaled, 5)

# можно воспользоваться функцией scale
var_scaled <- scale(development$income)

# убедимся, что распределение переменной
# var_standardscaled имеет среднее 0 и
# стандартное отклонение 1
mean_value <- round(mean(var_scaled), 3)
sd_value <- round(sd(var_scaled), 3)

output <- c("Среднее" = mean_value, 
            "Стандартное отклонение" = sd_value)
output


# загружаем пакет caret
library(caret)
# строим модель – вычисляем средние и стандартные 
# отклонения переменных, необходимые для преобразования
trans <- preProcess(development, method = c("center", "scale"))
# применяем модель к обучающему и контрольному наборам
development_trans <- predict(trans, development)
holdout_trans <- predict(trans, holdout)

# пишем функцию, которая выполняет масштабирование
MinMaxScaler_manually <- function(x) {
  x <- (x - min(x)) / (max(x) - min(x))
  }

# применяем функцию к переменной
var_minmaxscaled <- MinMaxScaler_manually(development$income)
head(var_minmaxscaled, 5)

# убедимся, что распределение переменной
# var_minmaxscaled имеет
# диапазон от 0 до 1
min_value <- round(min(var_minmaxscaled), 3)
max_value <- round(max(var_minmaxscaled), 3)

output <- c("Минимум" = min_value, 
            "Максимум" = max_value)
output

# пишем функцию, которая выполняет масштабирование
RobustScaler_manually <- function(x) {
  x <- (x - quantile(x, 0.25)) / IQR(x)
}

# применяем функцию к переменной
var_robustscaled <- RobustScaler_manually(development$income)
head(var_robustscaled, 5)

# применяем преобразование Бокса-Кокса и стандартизацию
transform <- preProcess(development, 
                        method = c("BoxCox", "center", "scale"))
# применяем модель к обучающему и контрольному наборам
development_transformed <- predict(transform, development)
holdout_transformed <- predict(transform, holdout)


# загружаем библиотеку h2o
library(h2o)
h2o.init(nthreads=-1, max_mem_size = "8G")

# выполняем преобразование во фреймы h2o
train <- as.h2o(development)
valid <- as.h2o(holdout)

# строим модель логистической регрессии на данных,
# к которым не были применены преобразования,
# максимизирующие нормальность, и стандартизация
glm1 <- h2o.glm(family = "binomial", training_frame = train, 
                validation_frame = valid, 
                model_id = "logreg_churn",
                standardize = FALSE,
                x = c(1:11), y = 12, seed = 10000)

# смотрим модель
summary(glm1)


# выполняем преобразование во фреймы h2o
train_transformed <- as.h2o(development_transformed)
valid_transformed <- as.h2o(holdout_transformed)


# строим модель логистической регрессии на данных,
# к которым были применены преобразования,
# максимизирующие нормальность, и стандартизация
glm2 <- h2o.glm(family = "binomial", 
                training_frame = train_transformed, 
                validation_frame = valid_transformed, 
                model_id = "logreg_churn2",
                standardize = TRUE,
                x = c(1:11), y = 12, seed = 10000)

# смотрим модель
summary(glm2)

# смотрим условия
Simp_Learner

# создаем новые переменные на основе
# условий случайного леса
development$var <- ifelse(development$employ > 7.5 & 
                            development$custcat %in% 
                            c("БАЗОВЫЕ УСЛУГИ", "ВСЕ УСЛУГИ", "ПЛЮС УСЛУГИ"), 0, 1)
holdout$var <- ifelse(holdout$employ > 7.5 & 
                        holdout$custcat %in% 
                        c("БАЗОВЫЕ УСЛУГИ", "ВСЕ УСЛУГИ", "ПЛЮС УСЛУГИ"), 0, 1)

development$var2 <- ifelse(development$tenure > 3.5, 0, 1)
holdout$var2 <- ifelse(holdout$tenure > 3.5, 0, 1)

development$var3 <- ifelse(development$region %in% c("ЗОНА 2", "ЗОНА 3") & 
                             development$tenure > 14.5 & development$address > 5.5, 0, 1)
holdout$var3 <- ifelse(holdout$region %in% c("ЗОНА 2", "ЗОНА 3") & 
                         holdout$tenure > 14.5 & holdout$address > 5.5, 0, 1)

# применяем преобразование Бокса-Кокса и стандартизацию
transform2 <- preProcess(development, 
                         method = c("BoxCox", "center", "scale"))
# применяем модель к обучающему и контрольному наборам
development_transformed2 <- predict(transform2, development)
holdout_transformed2 <- predict(transform2, holdout)

# выполняем преобразование во фреймы h2o
train_transformed2 <- as.h2o(development_transformed2)
valid_transformed2 <- as.h2o(holdout_transformed2)

# строим модель логистической регрессии на данных,
# c новыми переменными, к которым были применены 
# преобразования, максимизирующие нормальность, 
# и стандартизация
glm3 <- h2o.glm(family = "binomial", 
                training_frame = train_transformed2, 
                validation_frame = valid_transformed2, 
                model_id = "logreg_churn3",
                standardize = TRUE,
                x = c(1:11,13:15), y = 12, seed = 10000)

# смотрим модель
summary(glm3)