library(data.table)
library(caret)
library(Boruta)
library(lubridate)
library(xgboost)
df <- fread("C:/Trees/train_rosbank.csv")

# кодируем пропуск как категорию
df[channel_type == "", channel_type := "unknown"] 
df[, channel_type := as.factor(channel_type)]

df[, PERIOD := as.factor(PERIOD)]

# удаляем самые большие платежы (в донгах и пр.)
df <- df[amount < 1000000] 

# объединяем редкие (<300 записей) валюты
# df[, .N, by = currency][order(N)][N < 300][, sum(N)] 
# [1] 3185 - меньше 1% данных
currency_rare <- df[, .N, by = currency][order(N)][N < 300][, currency]
df[currency %in% currency_rare, currency := -9999]
df[, currency := as.factor(currency)]

# конвертируем платеж в рубли по среднему курсу 2017 г.
# https://index.minfin.com.ua/reference/currency/code/
# https://ratestats.com/czech-koruna/2017/
df[currency == 203, amount := amount * 2.51338]
# https://pattayahelper.com/kurs-bata-k-rublyu-na-segodnya.html
df[currency == 764, amount := amount * 1.98]
# https://ratestats.com/dollar/2017/
df[currency == 840, amount := amount * 58.2982]
# https://ratestats.com/belarusian-ruble/2017/
df[currency == 933, amount := amount * 30.1790]
# https://ratestats.com/turkish-lira/2017/
df[currency == 949, amount := amount * 15.9885]
# https://ratestats.com/euro/2017/
df[currency == 978, amount := amount * 66.0305]
# https://ratestats.com/zloty/2017/
df[currency == 985, amount := amount * 15.5240]

# задаем формат даты и времени
df[, TRDATETIME := dmy_hms(TRDATETIME)]

df[, trx_category := as.factor(trx_category)]

df[, target_flag := factor(target_flag, 
                           levels = c(1, 0), 
                           labels = c("остался", "ушел"))]

# количество транзакций (каждая сумма в столбце amount_rub - транзакция) 
# по каждому клиенту (cl_id) в каждом периоде (PERIOD)
tmp <- df[, .N, by = c("cl_id", "PERIOD")]
df_aggr <- dcast(tmp, cl_id ~ PERIOD, value.var = "N", fill = 0)
colnames(df_aggr)[-1] <- paste0("ntran_period", 1:19)

# количество уникальных MCC-кодов по каждому клиенту (cl_id) 
# в каждом периоде (PERIOD)
tmp <- df[, .(nmcc_period = unique(MCC)), by = c("cl_id", "PERIOD")]
tmp <- dcast(tmp, 
             cl_id ~ PERIOD, 
             value.var = "nmcc_period", 
             fill = 0)
colnames(tmp)[-1] <- paste0("nmcc_period", 1:19)
df_aggr <- cbind(df_aggr, tmp[,  -c("cl_id")]) # убираем cl_id

# разница в рублях между суммами транзакции в самую раннюю и 
# самую последнюю дату (TRDATETIME) по каждому клиенту (cl_id)
# setorder осуществляет быструю сортировку по cl_id, а затем по TRDATETIME
setorder(df, cl_id, TRDATETIME)
tmp <- df[, 
          .(tran_delta = amount[.N] - amount[1]), 
          by = "cl_id"]
df_aggr <- cbind(df_aggr, tmp[, -c("cl_id")])

# разница в рублях между суммами транзакции в самую раннюю и 
# самую последнюю дату (TRDATETIME) в каждом периоде (PERIOD) 
# по каждому клиенту (cl_id)
setorder(df, cl_id, PERIOD, TRDATETIME)
tmp <- df[, 
          .(tran_delta_period = amount[.N] - amount[1]), 
          by = c("cl_id", "PERIOD")]
tmp <- dcast(tmp, 
             cl_id ~ PERIOD, 
             value.var = "tran_delta_period", 
             fill = 0)
colnames(tmp)[-1] <- paste0("tran_delta_period", 1:19)
df_aggr <- cbind(df_aggr, tmp[, -c("cl_id")])

# разница в днях между самой ранней и самой последней датой (TRDATETIME) 
# по каждому клиенту (cl_id)
setorder(df, cl_id, TRDATETIME)
tmp <- df[, 
          .(time_delta = TRDATETIME[.N] - TRDATETIME[1]), 
          by = "cl_id"]
df_aggr <- cbind(df_aggr, tmp[, -c("cl_id")])

# разница в днях между самой ранней и самой последней датой (TRDATETIME) 
# в каждом периоде (PERIOD) по каждому клиенту (cl_id)
setorder(df, cl_id, PERIOD, TRDATETIME)
tmp <- df[, 
          .(time_delta_period = TRDATETIME[.N] - TRDATETIME[1]), 
          by = c("cl_id", "PERIOD")]
tmp <- dcast(tmp, 
             cl_id ~ PERIOD, 
             value.var = "time_delta_period", 
             fill = 0)
colnames(tmp)[-1] <- paste0("time_delta_period", 1:19)
df_aggr <- cbind(df_aggr, tmp[, -c("cl_id")])

# среднее количество дней между транзакциями по каждому клиенту (cl_id)
setorder(df, cl_id, TRDATETIME)
tmp <- df[, 
          .(mean_time_delta = (TRDATETIME[.N] - TRDATETIME[1]) / .N), 
          by = "cl_id"]
df_aggr <- cbind(df_aggr, tmp[, -c("cl_id")])

# среднее количество дней между транзакциями в каждом периоде (PERIOD) 
# по каждому клиенту (cl_id)
setorder(df, cl_id, PERIOD, TRDATETIME)
tmp <- df[, 
          .(mean_time_delta_period = (TRDATETIME[.N] - TRDATETIME[1]) / .N), 
          by = c("cl_id", "PERIOD")]
tmp <- dcast(tmp, 
             cl_id ~ PERIOD, 
             value.var = "mean_time_delta_period", 
             fill = 0)
colnames(tmp)[-1] <- paste0("mean_time_delta_period", 1:19)
df_aggr <- cbind(df_aggr, tmp[, -c("cl_id")])


# бинарные переменные
dummies <- dummyVars( ~ channel_type + currency + trx_category, 
                      data = df)
df_dummy <- predict(dummies, df)
df_dummy <- cbind(df[, .(cl_id, amount)], df_dummy)
df_dummy <- cbind(df[, .(target_flag)], df_dummy)
tmp <- df_dummy[, c(list(target_flag = target_flag[1],
                         amount_sum = sum(amount),
                         amount_mean = mean(amount),
                         amount_med = median(amount)),
                    lapply(.SD, mean)),
                by = cl_id,
                .SDcols = names(df_dummy)[4:28]]
df_aggr <- cbind(df_aggr, tmp[, -c("cl_id", "target_flag")])
df_aggr <- cbind(tmp[, .(target_flag)], df_aggr)

set.seed(42)
boruta_rosbank <- Boruta(target_flag ~ ., 
                         data = df_aggr, 
                         doTrace = 2, 
                         ntree = 400,
                         maxRuns = 60)

saveRDS(boruta_rosbank,  "C:/Trees/boruta_rosbank.rds")

boruta_rosbank <- readRDS("C:/Trees/boruta_rosbank.rds")
boruta_rosbank <- TentativeRoughFix(boruta_rosbank)
boruta_rosbank
plot(boruta_rosbank)
model_formula <- getConfirmedFormula(boruta_rosbank)
model_formula

# находим имена столбцов со значениями даты и времени
difftime_cols <- names(df_aggr)[sapply(df_aggr, is.difftime)]
# преобразовываем выбранные столбцы в числовые
df_aggr[,  
        c(difftime_cols) := lapply(.SD, as.numeric),
        .SDcols = difftime_cols]

# сохраняем переменную отклика в виде числовой переменной со значениями 0/1
y_train <- as.numeric(df_aggr[, target_flag]) - 1

# отбираем признаки, исключенные алгоритмом Boruta
rej_features <- names(boruta_rosbank$finalDecision)[
  boruta_rosbank$finalDecision == "Rejected"]


h2o_data <- df_aggr[, -rej_features, with = FALSE]

# создаем объект класса xgb.DMatrix
dtrain <- xgb.DMatrix(
  as.matrix(df_aggr[, -c("target_flag", rej_features), with = FALSE]), 
  label = as.matrix(y_train))

fitControl <- trainControl(method = "cv", 
                           number = 10, 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)

tuneGridXGB <- expand.grid(
  nrounds = c(350, 400, 450),
  max_depth = c(3, 4, 5),
  eta = c(0.02),
  gamma = c(0),
  colsample_bytree = c(0.7),
  min_child_weight = c(5),
  subsample = c(0.6)
)

set.seed(12)
fit_xgb <- train(model_formula, 
                 data = df_aggr, 
                 method = "xgbTree", 
                 trControl = fitControl,
                 metric = "ROC", 
                 tuneGrid = tuneGridXGB)
saveRDS(fit_xgb,  "C:/Trees/fit_xgb.rds")

fit_xgb <- readRDS("C:/Trees/fit_xgb.rds")
fit_xgb

# воспользуемся подобранным выше оптимальными параметрами и 
# выполним обучение (вновь с перекрестной проверкой) для 
# выбора оптимального числа итераций при помощи `xgb.cv()`.

xgb_params <- list(
  colsample_bytree = 0.7, 
  subsample = 0.6, 
  booster = "gbtree",
  max_depth = 5, 
  eta = 0.02, 
  eval_metric = "auc", 
  objective = "binary:logistic",
  gamma = 0.7,
  min_child_weight = 5, 
  num_class = 1)

set.seed(42)
fit_xgb_cv <- xgb.cv(xgb_params, 
                     dtrain, 
                     early_stopping_rounds = 10, 
                     nfold = 10, 
                     nrounds = 1000,
                     print_every_n = 10)

# сохраняем оптимальное число итераций для дальнейшего использования
nrounds_best <- fit_xgb_cv$niter

# наконец, обучим нашу финальную модель.
fit_xgb_fin <- xgb.train(xgb_params,
                         dtrain,
                         nrounds = nrounds_best,
                         verbose = 1,
                         print_every_n = 10, 
                         watchlist = list(train = dtrain))


# загружаем библиотеку h2o, 
# перед загрузкой библиотеки h2o убедитесь, что библиотека h2o установлена 
# (сначала установите Java SE Development Kit 8, обратите внимание, 
# 9-я версия H2O не поддерживается, а затем после установки Java 
# установите пакет h2o с помощью команды install.packages("h2o")
# и затем загрузите библиотеку)
library(h2o)
h2o.init(nthreads=-1, max_mem_size = "8G")

# выполняем преобразование во фрейм h2o
h2o_data <- as.h2o(h2o_data)

# ищем оптимиальные гиперпараметры
hyper_parameters <- list(max_depth = c(2, 4, 6))
gbm_grid <- h2o.grid(algorithm = "gbm", grid_id = "gbm_grid", 
                     hyper_params = hyper_parameters, learn_rate = 0.05, ntrees = 150,
                     histogram_type = "QuantilesGlobal", nbins = 8,
                     col_sample_rate = 0.95, col_sample_rate_per_tree = 0.5,
                     sample_rate = 0.3, training_frame = h2o_data, 
                     x = c(2:84), y = "target_flag",
                     nfolds=10, keep_cross_validation_predictions=TRUE,
                     seed = 1000000)

# сортируем результаты по AUC
sorted_gbm_grid <- h2o.getGrid("gbm_grid", sort_by = "auc", decreasing = TRUE)
sorted_gbm_grid










