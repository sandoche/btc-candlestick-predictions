### Dependencies ###

if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if (!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if (!require(tidyquant)) install.packages("tidyquant", repos = "http://cran.us.r-project.org")
if (!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if (!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if (!require(TTR)) install.packages("TTR", repos = "http://cran.us.r-project.org")

### Global Variables ###

library(tidyverse)
library(caret)
library(httr)
library(jsonlite)
library(tidyquant)
library(patchwork)
library(randomForest)
library(TTR)

trading_pair <- "BTC-USD"
start_date <- "2024-01-01"
end_date <- "2025-03-29"
candlestick_period <- 3600
set.seed(1)

### Loading the datasets ###

candles <- read_csv(paste0("data/", trading_pair, "_candles_", start_date, "_", end_date, "_", candlestick_period, ".csv"), show_col_types = FALSE)

fear_and_greed_index <- read_csv(paste0("data/", trading_pair, "_fear_and_greed_index_", start_date, "_", end_date, ".csv"))
fear_and_greed_index <- fear_and_greed_index %>% mutate(value = as.numeric(value))

hash_rate <- jsonlite::fromJSON("data/hash-rate.json")$`hash-rate` %>%
  rename(timestamp = x, hash_rate = y) %>%
  mutate(timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  filter(timestamp >= as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC") & timestamp <= as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC"))

average_block_size <- jsonlite::fromJSON("data/avg-block-size.json")$`avg-block-size` %>%
  rename(timestamp = x, avg_block_size = y) %>%
  mutate(timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  filter(timestamp >= as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC") & timestamp <= as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC"))

n_transactions <- jsonlite::fromJSON("data/n-transactions.json")$`n-transactions` %>%
  rename(timestamp = x, n_transactions = y) %>%
  mutate(timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  filter(timestamp >= as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC") & timestamp <= as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC"))

utxo_count <- jsonlite::fromJSON("data/utxo-count.json")$`utxo-count` %>%
  rename(timestamp = x, utxo_count = y) %>%
  mutate(
    timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC"),
    timestamp = as.Date(timestamp)
  ) %>%
  filter(timestamp >= as.Date(start_date) & timestamp <= as.Date(end_date)) %>%
  group_by(timestamp) %>%
  summarise(utxo_count = mean(utxo_count)) # Take average for each date
knitr::kable(head(utxo_count), format = "simple", caption = "Overview of the BTC UTXO count dataset")


### Functions to prepare the data ###

enhance_dataset <- function(candles_data, fear_and_greed_index_data, hash_rate_data, average_block_size_data, n_transactions_data, utxo_count_data) {
  candles_enhanced <- candles_data %>%
    mutate(date_only = as.Date(time)) %>%
    left_join(fear_and_greed_index_data, by = c("date_only" = "timestamp")) %>%
    left_join(hash_rate_data, by = c("date_only" = "timestamp")) %>%
    left_join(average_block_size_data, by = c("date_only" = "timestamp")) %>%
    left_join(n_transactions_data, by = c("date_only" = "timestamp")) %>%
    left_join(utxo_count_data, by = c("date_only" = "timestamp")) %>%
    mutate(
      body_size = abs(close - open),
      upper_shadow_size = high - pmax(close, open),
      lower_shadow_size = pmin(close, open) - low,
      direction = ifelse(close > open, "up", "down"),
    ) %>%
    tq_mutate(
      select = close,
      mutate_fun = ROC,
      n = 14,
      col_rename = "roc"
    ) %>%
    tq_mutate( # https://www.keenbase-trading.com/find-best-macd-settings/#t-1719588154943
      select = close,
      mutate_fun = MACD,
      nFast = 12,
      nSlow = 26,
      nSig = 9,
      col_rename = c("macd", "signal")
    ) %>%
    tq_mutate(
      select = close,
      mutate_fun = RSI,
      n = 14,
      col_rename = "rsi"
    ) %>%
    tq_mutate(
      select = close,
      mutate_fun = BBands,
      n = 20,
      sd = 2,
      col_rename = "bband"
    )

  candles_enhanced
}

add_lagged_candles <- function(enhanced_clean_dataset, n_lag) {
  dataset_with_lagged_candles <- enhanced_clean_dataset

  for (i in 1:n_lag) {
    dataset_with_lagged_candles[[paste0("body_size_lag_", i)]] <- lag(dataset_with_lagged_candles$body_size, i)
    dataset_with_lagged_candles[[paste0("upper_shadow_size_lag_", i)]] <- lag(dataset_with_lagged_candles$upper_shadow_size, i)
    dataset_with_lagged_candles[[paste0("lower_shadow_size_lag_", i)]] <- lag(dataset_with_lagged_candles$lower_shadow_size, i)
    dataset_with_lagged_candles[[paste0("direction_lag_", i)]] <- lag(dataset_with_lagged_candles$direction, i)
    dataset_with_lagged_candles[[paste0("volume_lag_", i)]] <- lag(dataset_with_lagged_candles$volume, i)
    dataset_with_lagged_candles[[paste0("value_lag_", i)]] <- lag(dataset_with_lagged_candles$value, i)
    dataset_with_lagged_candles[[paste0("close_lag_", i)]] <- lag(dataset_with_lagged_candles$close, i)
    dataset_with_lagged_candles[[paste0("hash_rate_lag_", i)]] <- lag(dataset_with_lagged_candles$hash_rate, i)
    dataset_with_lagged_candles[[paste0("avg_block_size_lag_", i)]] <- lag(dataset_with_lagged_candles$avg_block_size, i)
    dataset_with_lagged_candles[[paste0("n_transactions_lag_", i)]] <- lag(dataset_with_lagged_candles$n_transactions, i)
    dataset_with_lagged_candles[[paste0("utxo_count_lag_", i)]] <- lag(dataset_with_lagged_candles$utxo_count, i)
    dataset_with_lagged_candles[[paste0("open_lag_", i)]] <- lag(dataset_with_lagged_candles$open, i)
    dataset_with_lagged_candles[[paste0("high_lag_", i)]] <- lag(dataset_with_lagged_candles$high, i)
    dataset_with_lagged_candles[[paste0("low_lag_", i)]] <- lag(dataset_with_lagged_candles$low, i)
    dataset_with_lagged_candles[[paste0("roc_lag_", i)]] <- lag(dataset_with_lagged_candles$roc, i)
    dataset_with_lagged_candles[[paste0("macd_lag_", i)]] <- lag(dataset_with_lagged_candles$macd, i)
    dataset_with_lagged_candles[[paste0("signal_lag_", i)]] <- lag(dataset_with_lagged_candles$signal, i)
    dataset_with_lagged_candles[[paste0("rsi_lag_", i)]] <- lag(dataset_with_lagged_candles$rsi, i)
    dataset_with_lagged_candles[[paste0("up_bband_lag_", i)]] <- lag(dataset_with_lagged_candles$up, i)
    dataset_with_lagged_candles[[paste0("mavg_lag_", i)]] <- lag(dataset_with_lagged_candles$mavg, i)
    dataset_with_lagged_candles[[paste0("dn_bband_lag_", i)]] <- lag(dataset_with_lagged_candles$dn, i)
    dataset_with_lagged_candles[[paste0("pctB_lag_", i)]] <- lag(dataset_with_lagged_candles$pctB, i)
  }

  dataset_with_lagged_candles
}

prepare_dataset <- function(candles_data, fear_and_greed_index_data, hash_rate_data, average_block_size_data, n_transactions_data, utxo_count_data) {
  enhanced_clean_dataset <- enhance_dataset(candles_data, fear_and_greed_index_data, hash_rate_data, average_block_size_data, n_transactions_data, utxo_count_data)
  enhanced_clean_dataset_without_na <- enhanced_clean_dataset %>% drop_na()
  dataset_with_lagged_candles <- add_lagged_candles(enhanced_clean_dataset_without_na, 15)
  dataset_with_lagged_candles_without_na <- dataset_with_lagged_candles %>% drop_na()
  dataset_with_lagged_candles_without_na
}

### Prepare the data ###

fear_and_greed_index_date_before_na <- fear_and_greed_index %>% filter(timestamp == as.Date("2024-10-25"))
fear_and_greed_index_date_after_na <- fear_and_greed_index %>% filter(timestamp == as.Date("2024-10-27"))
fear_and_greed_value_date_na <- mean(c(fear_and_greed_index_date_before_na$value, fear_and_greed_index_date_after_na$value))

fear_and_greed_index_corrected <- fear_and_greed_index %>%
  bind_rows(tibble(timestamp = date_na, value = fear_and_greed_value_date_na, value_classification = "Greed"))

project_dataset <- prepare_dataset(candles, fear_and_greed_index_corrected, hash_rate, average_block_size, n_transactions, utxo_count)

sum(is.na(project_dataset))
nrow(project_dataset)
nrow(candles)

test_index <- createDataPartition(y = project_dataset$direction, times = 1, p = 0.2, list = FALSE)
train_set <- project_dataset[-test_index, ]
test_set <- project_dataset[test_index, ]


### Utility functions ###

create_feature_formula <- function(feature_names, n_lags) {
  features <- c()

  for (feature_name in feature_names) {
    for (i in 1:n_lags) {
      features <- c(
        features,
        paste0(feature_name, "_lag_", i)
      )
    }
  }

  formula_str <- paste("direction ~", paste(features, collapse = " + "))

  as.formula(formula_str)
}


train_with_cache <- function(formula, train_set, method) {
  formula_hash <- digest::digest(formula)
  filepath <- paste0("models/", method, "_", formula_hash, ".rds")
  if (file.exists(filepath)) {
    model <- readRDS(filepath)
  } else {
    start_time <- Sys.time()
    if (method == "rf") {
      model <- train(formula, data = train_set, method = "rf", ntree = 100)
    } else if (method == "glm") {
      model <- train(formula, data = train_set, method = "glm", family = "binomial")
    } else if (method == "rpart") {
      model <- train(formula, data = train_set, method = "rpart")
    } else if (method == "knn") {
      model <- train(formula, data = train_set, method = "knn", preProcess = c("center", "scale"), tuneGrid = data.frame(k = seq(3, 15, 2)))
    } else if (method == "gbm") {
      model <- train(formula, data = train_set, method = "gbm")
    } else {
      stop("Invalid method")
    }
    end_time <- Sys.time()
    print(paste("Training time:", end_time - start_time))

    saveRDS(model, filepath)
  }

  model
}

### Creating the formulas ###

formula_OHLC_lag_3 <- create_feature_formula(c("open", "high", "low", "close"), 3)
formula_OHLC_lag_5 <- create_feature_formula(c("open", "high", "low", "close"), 5)
formula_OHLC_lag_7 <- create_feature_formula(c("open", "high", "low", "close"), 7)
formula_OHLC_lag_15 <- create_feature_formula(c("open", "high", "low", "close"), 15)

formula_candles_features_lag_3 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close"), 3)
formula_candles_features_lag_5 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close"), 5)
formula_candles_features_lag_7 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close"), 7)
formula_candles_features_lag_15 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close"), 15)

formula_candles_features_with_fear_and_greed_index_lag_3 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close", "value"), 3)
formula_candles_features_with_fear_and_greed_index_lag_5 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close", "value"), 5)
formula_candles_features_with_fear_and_greed_index_lag_7 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close", "value"), 7)
formula_candles_features_with_fear_and_greed_index_lag_15 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close", "value"), 15)

formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_3 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close", "value", "hash_rate", "avg_block_size", "n_transactions", "utxo_count"), 3)
formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_5 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close", "value", "hash_rate", "avg_block_size", "n_transactions", "utxo_count"), 5)
formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_7 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close", "value", "hash_rate", "avg_block_size", "n_transactions", "utxo_count"), 7)
formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_15 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close", "value", "hash_rate", "avg_block_size", "n_transactions", "utxo_count"), 15)

formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_3 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close", "value", "hash_rate", "avg_block_size", "n_transactions", "utxo_count", "roc", "macd", "signal", "rsi", "up_bband", "mavg", "dn_bband", "pctB"), 3)
formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_5 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close", "value", "hash_rate", "avg_block_size", "n_transactions", "utxo_count", "roc", "macd", "signal", "rsi", "up_bband", "mavg", "dn_bband", "pctB"), 5)
formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_7 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close", "value", "hash_rate", "avg_block_size", "n_transactions", "utxo_count", "roc", "macd", "signal", "rsi", "up_bband", "mavg", "dn_bband", "pctB"), 7)
formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_15 <- create_feature_formula(c("body_size", "upper_shadow_size", "lower_shadow_size", "direction", "close", "value", "hash_rate", "avg_block_size", "n_transactions", "utxo_count", "roc", "macd", "signal", "rsi", "up_bband", "mavg", "dn_bband", "pctB"), 15)


### Training the models ###

# OHLC

glm_model_OHLC_lag_3 <- train_with_cache(formula_OHLC_lag_3, train_set, "glm")
glm_model_OHLC_lag_5 <- train_with_cache(formula_OHLC_lag_5, train_set, "glm")
glm_model_OHLC_lag_7 <- train_with_cache(formula_OHLC_lag_7, train_set, "glm")
glm_model_OHLC_lag_15 <- train_with_cache(formula_OHLC_lag_15, train_set, "glm")

rpart_model_OHLC_lag_3 <- train_with_cache(formula_OHLC_lag_3, train_set, "rpart")
rpart_model_OHLC_lag_5 <- train_with_cache(formula_OHLC_lag_5, train_set, "rpart")
rpart_model_OHLC_lag_7 <- train_with_cache(formula_OHLC_lag_7, train_set, "rpart")
rpart_model_OHLC_lag_15 <- train_with_cache(formula_OHLC_lag_15, train_set, "rpart")

rf_model_OHLC_lag_3 <- train_with_cache(formula_OHLC_lag_3, train_set, "rf")
rf_model_OHLC_lag_5 <- train_with_cache(formula_OHLC_lag_5, train_set, "rf")
rf_model_OHLC_lag_7 <- train_with_cache(formula_OHLC_lag_7, train_set, "rf")
rf_model_OHLC_lag_15 <- train_with_cache(formula_OHLC_lag_15, train_set, "rf")

knn_model_OHLC_lag_3 <- train_with_cache(formula_OHLC_lag_3, train_set, "knn")
knn_model_OHLC_lag_5 <- train_with_cache(formula_OHLC_lag_5, train_set, "knn")
knn_model_OHLC_lag_7 <- train_with_cache(formula_OHLC_lag_7, train_set, "knn")
knn_model_OHLC_lag_15 <- train_with_cache(formula_OHLC_lag_15, train_set, "knn")

gbm_model_OHLC_lag_3 <- train_with_cache(formula_OHLC_lag_3, train_set, "gbm")
gbm_model_OHLC_lag_5 <- train_with_cache(formula_OHLC_lag_5, train_set, "gbm")
gbm_model_OHLC_lag_7 <- train_with_cache(formula_OHLC_lag_7, train_set, "gbm")
gbm_model_OHLC_lag_15 <- train_with_cache(formula_OHLC_lag_15, train_set, "gbm")

# Candles features

glm_model_candles_features_lag_3 <- train_with_cache(formula_candles_features_lag_3, train_set, "glm")
glm_model_candles_features_lag_5 <- train_with_cache(formula_candles_features_lag_5, train_set, "glm")
glm_model_candles_features_lag_7 <- train_with_cache(formula_candles_features_lag_7, train_set, "glm")
glm_model_candles_features_lag_15 <- train_with_cache(formula_candles_features_lag_15, train_set, "glm")

rpart_model_candles_features_lag_3 <- train_with_cache(formula_candles_features_lag_3, train_set, "rpart")
rpart_model_candles_features_lag_5 <- train_with_cache(formula_candles_features_lag_5, train_set, "rpart")
rpart_model_candles_features_lag_7 <- train_with_cache(formula_candles_features_lag_7, train_set, "rpart")
rpart_model_candles_features_lag_15 <- train_with_cache(formula_candles_features_lag_15, train_set, "rpart")

rf_model_candles_features_lag_3 <- train_with_cache(formula_candles_features_lag_3, train_set, "rf")
rf_model_candles_features_lag_5 <- train_with_cache(formula_candles_features_lag_5, train_set, "rf")
rf_model_candles_features_lag_7 <- train_with_cache(formula_candles_features_lag_7, train_set, "rf")
rf_model_candles_features_lag_15 <- train_with_cache(formula_candles_features_lag_15, train_set, "rf")

knn_model_candles_features_lag_3 <- train_with_cache(formula_candles_features_lag_3, train_set, "knn")
knn_model_candles_features_lag_5 <- train_with_cache(formula_candles_features_lag_5, train_set, "knn")
knn_model_candles_features_lag_7 <- train_with_cache(formula_candles_features_lag_7, train_set, "knn")
knn_model_candles_features_lag_15 <- train_with_cache(formula_candles_features_lag_15, train_set, "knn")

gbm_model_candles_features_lag_3 <- train_with_cache(formula_candles_features_lag_3, train_set, "gbm")
gbm_model_candles_features_lag_5 <- train_with_cache(formula_candles_features_lag_5, train_set, "gbm")
gbm_model_candles_features_lag_7 <- train_with_cache(formula_candles_features_lag_7, train_set, "gbm")
gbm_model_candles_features_lag_15 <- train_with_cache(formula_candles_features_lag_15, train_set, "gbm")

# Candles features with fear and greed index

glm_model_candles_features_with_fear_and_greed_index_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_3, train_set, "glm")
glm_model_candles_features_with_fear_and_greed_index_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_5, train_set, "glm")
glm_model_candles_features_with_fear_and_greed_index_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_7, train_set, "glm")
glm_model_candles_features_with_fear_and_greed_index_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_15, train_set, "glm")

rpart_model_candles_features_with_fear_and_greed_index_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_3, train_set, "rpart")
rpart_model_candles_features_with_fear_and_greed_index_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_5, train_set, "rpart")
rpart_model_candles_features_with_fear_and_greed_index_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_7, train_set, "rpart")
rpart_model_candles_features_with_fear_and_greed_index_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_15, train_set, "rpart")

rf_model_candles_features_with_fear_and_greed_index_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_3, train_set, "rf")
rf_model_candles_features_with_fear_and_greed_index_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_5, train_set, "rf")
rf_model_candles_features_with_fear_and_greed_index_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_7, train_set, "rf")
rf_model_candles_features_with_fear_and_greed_index_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_15, train_set, "rf")

knn_model_candles_features_with_fear_and_greed_index_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_3, train_set, "knn")
knn_model_candles_features_with_fear_and_greed_index_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_5, train_set, "knn")
knn_model_candles_features_with_fear_and_greed_index_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_7, train_set, "knn")
knn_model_candles_features_with_fear_and_greed_index_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_15, train_set, "knn")

gbm_model_candles_features_with_fear_and_greed_index_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_3, train_set, "gbm")
gbm_model_candles_features_with_fear_and_greed_index_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_5, train_set, "gbm")
gbm_model_candles_features_with_fear_and_greed_index_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_7, train_set, "gbm")
gbm_model_candles_features_with_fear_and_greed_index_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_lag_15, train_set, "gbm")

# Candles features with fear and greed index and chain data

glm_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_3, train_set, "glm")
glm_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_5, train_set, "glm")
glm_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_7, train_set, "glm")
glm_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_15, train_set, "glm")

rpart_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_3, train_set, "rpart")
rpart_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_5, train_set, "rpart")
rpart_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_7, train_set, "rpart")
rpart_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_15, train_set, "rpart")

rf_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_3, train_set, "rf")
rf_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_5, train_set, "rf")
rf_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_7, train_set, "rf")
rf_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_15, train_set, "rf")

knn_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_3, train_set, "knn")
knn_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_5, train_set, "knn")
knn_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_7, train_set, "knn")
knn_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_15, train_set, "knn")

gbm_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_3, train_set, "gbm")
gbm_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_5, train_set, "gbm")
gbm_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_7, train_set, "gbm")
gbm_model_candles_features_with_fear_and_greed_index_and_chain_data_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_lag_15, train_set, "gbm")

# Candles features with fear and greed index and chain data and TA

glm_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_3, train_set, "glm")
glm_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_5, train_set, "glm")
glm_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_7, train_set, "glm")
glm_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_15, train_set, "glm")

rpart_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_3, train_set, "rpart")
rpart_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_5, train_set, "rpart")
rpart_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_7, train_set, "rpart")
rpart_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_15, train_set, "rpart")

rf_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_3, train_set, "rf")
rf_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_5, train_set, "rf")
rf_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_7, train_set, "rf")
rf_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_15, train_set, "rf")

knn_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_3, train_set, "knn")
knn_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_5, train_set, "knn")
knn_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_7, train_set, "knn")
knn_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_15, train_set, "knn")

gbm_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_3 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_3, train_set, "gbm")
gbm_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_5 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_5, train_set, "gbm")
gbm_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_7 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_7, train_set, "gbm")
gbm_model_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_15 <- train_with_cache(formula_candles_features_with_fear_and_greed_index_and_chain_data_and_ta_lag_15, train_set, "gbm")


## Compare results ##



