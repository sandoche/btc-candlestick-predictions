### Dependencies ###

if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if (!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if (!require(tidyquant)) install.packages("tidyquant", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(httr)
library(jsonlite)
library(tidyquant)

### Global Variables ###

trading_pair <- "BTC-USD"
start_date <- "2024-01-01"
end_date <- "2025-03-29"
candlestick_period <- 3600

### Utilities ###

#' Get candlestick data from Coinbase Exchange API
#'
#' @param product_id Trading pair (e.g., "BTC-USD")
#' @param start_time Start date (format: "YYYY-MM-DD")
#' @param end_time End date (format: "YYYY-MM-DD")
#' @param granularity Candlestick period in seconds (60, 300, 900, 3600, 21600, 86400)
#' @return A tibble containing the candlestick data with columns: time, low, high, open, close, volume
#'
get_coinbase_candles <- function(product_id = trading_pair,
                                 start_time = start_date,
                                 end_time = end_date,
                                 granularity = candlestick_period) {
  # Convert dates to POSIXct
  start_date_posix <- as.POSIXct(start_time, tz = "UTC")
  end_date_posix <- as.POSIXct(end_time, tz = "UTC")

  # Calculate number of data points and required chunks
  # Coinbase API limit is 300 data points per request
  time_diff_seconds <- as.numeric(difftime(end_date_posix, start_date_posix, units = "secs"))
  total_candles <- time_diff_seconds / granularity
  message("Time range contains approximately ", round(total_candles), " candles")

  # Calculate chunk size in seconds (slightly under 300 candles per chunk)
  chunk_size_seconds <- 290 * granularity

  # Create sequence of dates for pagination
  date_breaks <- seq(from = start_date_posix, to = end_date_posix, by = chunk_size_seconds)
  if (tail(date_breaks, 1) < end_date_posix) {
    date_breaks <- c(date_breaks, end_date_posix)
  }

  message("Breaking request into ", length(date_breaks) - 1, " chunks")

  # Initialize empty tibble for results
  all_candles <- tibble()

  # Process each chunk
  for (i in 1:(length(date_breaks) - 1)) {
    chunk_start <- date_breaks[i]
    chunk_end <- date_breaks[i + 1]

    # Convert chunk dates to ISO 8601
    chunk_start_iso <- format(chunk_start, "%Y-%m-%dT%H:%M:%SZ")
    chunk_end_iso <- format(chunk_end, "%Y-%m-%dT%H:%M:%SZ")

    message(
      "Fetching chunk ", i, "/", length(date_breaks) - 1,
      ": ", chunk_start_iso, " to ", chunk_end_iso
    )

    # Make API request for this chunk
    response <- GET(
      url = paste0("https://api.exchange.coinbase.com/products/", product_id, "/candles"),
      query = list(
        start = chunk_start_iso,
        end = chunk_end_iso,
        granularity = granularity
      )
    )

    # Add delay to avoid rate limiting
    Sys.sleep(0.5)

    # Check response status
    if (http_status(response)$category != "Success") {
      warning("Chunk ", i, " failed: ", http_status(response)$message)
      next # Skip to next chunk instead of stopping
    }

    # Parse response
    candles_data <- tryCatch(
      {
        content(response, "text") %>% fromJSON()
      },
      error = function(e) {
        warning("Failed to parse JSON for chunk ", i, ": ", e$message)
        return(NULL)
      }
    )

    # Skip if no data or invalid format
    if (is.null(candles_data) || length(candles_data) == 0 || !is.matrix(candles_data)) {
      warning("No valid data for chunk ", i)
      next
    }

    # Process this chunk's data
    chunk_candles <- as_tibble(candles_data) %>%
      setNames(c("time", "low", "high", "open", "close", "volume")) %>%
      mutate(
        time = as.POSIXct(time, origin = "1970-01-01", tz = "UTC"),
        low = as.numeric(low),
        high = as.numeric(high),
        open = as.numeric(open),
        close = as.numeric(close),
        volume = as.numeric(volume)
      )

    # Append to results
    all_candles <- bind_rows(all_candles, chunk_candles)
  }

  # Final processing of combined data
  if (nrow(all_candles) == 0) {
    warning("No data found for the entire date range")
    return(tibble())
  }

  # Remove duplicates and sort by time
  all_candles <- all_candles %>%
    distinct() %>%
    arrange(time)

  message("Successfully retrieved ", nrow(all_candles), " candles")

  return(all_candles)
}

#' Get fear and greed index data from alternative.me
#'
#' @param start_time Start date (format: "YYYY-MM-DD")
#' @param end_time End date (format: "YYYY-MM-DD")
#' @return A tibble containing the fear and greed index data with columns: date, value
#'
get_fear_and_greed_index <- function(start_time = start_date, end_time = end_date) {
  today <- Sys.Date()
  number_of_days <- as.numeric(difftime(today, start_time, units = "days"))

  # Get the fear and greed index data from alternative.me
  response <- GET(
    url = paste0("https://api.alternative.me/fng/?limit=", number_of_days, "&date_format=kr"),
  )

  # Check response status
  if (http_status(response)$category != "Success") {
    warning("Failed to get fear and greed index: ", http_status(response)$message)
  }

  # Parse response
  response_data <- tryCatch(
    {
      content(response, "text") %>% fromJSON()
    },
    error = function(e) {
      warning("Failed to parse JSON for chunk ", i, ": ", e$message)
      return(NULL)
    }
  )

  fear_and_greed_index_data <- response_data$data %>%
    select(value, value_classification, timestamp) %>%
    mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")) %>%
    filter(timestamp >= as.POSIXct(start_time, origin = "1970-01-01", tz = "UTC") & timestamp <= as.POSIXct(end_time, origin = "1970-01-01", tz = "UTC"))

  return(fear_and_greed_index_data)
}

create_features <- function(candles_data, fear_and_greed_data) {
  candles_with_fear_and_greed_data <- candles_data %>%
    mutate(date_only = as.Date(time)) %>%
    left_join(fear_and_greed_data, by = c("date_only" = "timestamp"))

  candles_with_fear_and_greed_data <- candles_with_fear_and_greed_data %>%
    mutate(
      body_size = abs(close - open),
      upper_shadow_size = high - pmax(close, open),
      lower_shadow_size = pmin(close, open) - low,
      direction = ifelse(close > open, "up", "down"),
    )

  if (sum(is.na(candles_with_fear_and_greed_data)) > 0) {
    stop("There are NAs in the data")
  }

  # add 15 lagged candle's features
  candles_with_fear_and_greed_data <- candles_with_fear_and_greed_data %>%
    mutate(
      body_size_lag_1 = lag(body_size, 1),
      upper_shadow_size_lag_1 = lag(upper_shadow_size, 1),
      lower_shadow_size_lag_1 = lag(lower_shadow_size, 1),
      direction_lag_1 = lag(direction, 1),
      body_size_lag_2 = lag(body_size, 2),
      upper_shadow_size_lag_2 = lag(upper_shadow_size, 2),
      lower_shadow_size_lag_2 = lag(lower_shadow_size, 2),
      direction_lag_2 = lag(direction, 2),
      body_size_lag_3 = lag(body_size, 3),
      upper_shadow_size_lag_3 = lag(upper_shadow_size, 3),
      lower_shadow_size_lag_3 = lag(lower_shadow_size, 3),
      direction_lag_3 = lag(direction, 3),
      body_size_lag_4 = lag(body_size, 4),
      upper_shadow_size_lag_4 = lag(upper_shadow_size, 4),
      lower_shadow_size_lag_4 = lag(lower_shadow_size, 4),
      direction_lag_4 = lag(direction, 4),
      body_size_lag_5 = lag(body_size, 5),
      upper_shadow_size_lag_5 = lag(upper_shadow_size, 5),
      lower_shadow_size_lag_5 = lag(lower_shadow_size, 5),
      direction_lag_5 = lag(direction, 5),
      body_size_lag_6 = lag(body_size, 6),
      upper_shadow_size_lag_6 = lag(upper_shadow_size, 6),
      lower_shadow_size_lag_6 = lag(lower_shadow_size, 6),
      direction_lag_6 = lag(direction, 6),
      body_size_lag_7 = lag(body_size, 7),
      upper_shadow_size_lag_7 = lag(upper_shadow_size, 7),
      lower_shadow_size_lag_7 = lag(lower_shadow_size, 7),
      direction_lag_7 = lag(direction, 7),
      body_size_lag_8 = lag(body_size, 8),
      upper_shadow_size_lag_8 = lag(upper_shadow_size, 8),
      lower_shadow_size_lag_8 = lag(lower_shadow_size, 8),
      direction_lag_8 = lag(direction, 8),
      body_size_lag_9 = lag(body_size, 9),
      upper_shadow_size_lag_9 = lag(upper_shadow_size, 9),
      lower_shadow_size_lag_9 = lag(lower_shadow_size, 9),
      direction_lag_9 = lag(direction, 9),
      body_size_lag_10 = lag(body_size, 10),
      upper_shadow_size_lag_10 = lag(upper_shadow_size, 10),
      lower_shadow_size_lag_10 = lag(lower_shadow_size, 10),
      direction_lag_10 = lag(direction, 10),
      body_size_lag_11 = lag(body_size, 11),
      upper_shadow_size_lag_11 = lag(upper_shadow_size, 11),
      lower_shadow_size_lag_11 = lag(lower_shadow_size, 11),
      direction_lag_11 = lag(direction, 11),
      body_size_lag_12 = lag(body_size, 12),
      upper_shadow_size_lag_12 = lag(upper_shadow_size, 12),
      lower_shadow_size_lag_12 = lag(lower_shadow_size, 12),
      direction_lag_12 = lag(direction, 12),
      body_size_lag_13 = lag(body_size, 13),
      upper_shadow_size_lag_13 = lag(upper_shadow_size, 13),
      lower_shadow_size_lag_13 = lag(lower_shadow_size, 13),
      direction_lag_13 = lag(direction, 13),
      body_size_lag_14 = lag(body_size, 14),
      upper_shadow_size_lag_14 = lag(upper_shadow_size, 14),
      lower_shadow_size_lag_14 = lag(lower_shadow_size, 14),
      direction_lag_14 = lag(direction, 14),
      body_size_lag_15 = lag(body_size, 15),
      upper_shadow_size_lag_15 = lag(upper_shadow_size, 15),
      lower_shadow_size_lag_15 = lag(lower_shadow_size, 15),
      direction_lag_15 = lag(direction, 15)
    ) %>%
    drop_na()


  return(candles_with_fear_and_greed_data)
}


### Script ###

## Loading the data

# candles <- get_coinbase_candles()
# write_csv(candles, paste0("data/", trading_pair, "_candles_", start_date, "_", end_date, ".csv"))

# Loading the predownloaded data set, uncomment the code above if needed to download another data sets
candles <- read_csv(paste0("data/", trading_pair, "_candles_", start_date, "_", end_date, ".csv"))

# Explore the data
head(candles)

# fear_and_greed_index <- get_fear_and_greed_index()
# write_csv(fear_and_greed_index, paste0("data/", trading_pair, "_fear_and_greed_index_", start_date, "_", end_date, ".csv"))

# Loading the predownloaded fear and greed data set, uncomment the code above if needed to download another data sets
fear_and_greed_index <- read_csv(paste0("data/", trading_pair, "_fear_and_greed_index_", start_date, "_", end_date, ".csv"))
fear_and_greed_index <- fear_and_greed_index %>% mutate(value = as.numeric(value))

head(fear_and_greed_index)

## Visualizing the data

# Plot the data to check if everything is good
candles %>%
  ggplot(aes(x = time, y = close)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "BTC-USD Candlestick Chart",
    x = "Time",
    y = "Price"
  ) +
  scale_y_continuous(labels = scales::comma)


# Plot the candlestick chart of the last 24 candles
candles %>%
  tail(24) %>%
  mutate(direction = ifelse(close >= open, "up", "down")) %>%
  ggplot(aes(x = time, y = close, volume = volume)) +
  # The shadows (wicks)
  geom_segment(aes(xend = time, y = low, yend = high, color = direction), size = 0.5) +
  # The body
  geom_segment(aes(xend = time, y = open, yend = close, color = direction), size = 5) +
  scale_color_manual(values = c("up" = "darkgreen", "down" = "red")) +
  theme_tq() +
  theme(legend.position = "none") +
  labs(
    title = "BTC-USD Candlestick Chart (Last 24 Candles)",
    x = "Time",
    y = "Price"
  ) +
  scale_y_continuous(labels = scales::comma)

# Plotting the volume last 300 candles
candles %>%
  tail(300) %>%
  ggplot(aes(x = time, y = volume)) +
  geom_segment(aes(xend = time, yend = 0, color = volume)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "BTC-USD Candlestick Chart (Last 300 candles)", y = "Volume", x = "") +
  theme_tq() +
  theme(legend.position = "none")

# Plotting the fear and greed index evolution
fear_and_greed_index %>%
  ggplot(aes(x = timestamp, y = value)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "BTC-USD Fear and Greed Index Evolution",
    x = "Time",
    y = "Fear and Greed Index"
  ) +
  scale_y_continuous(labels = scales::comma)

## Preparing the data for the model

candles_with_fear_and_greed_index <- candles %>%
  mutate(date_only = as.Date(time)) %>%
  left_join(fear_and_greed_index, by = c("date_only" = "timestamp"))

head(candles_with_fear_and_greed_index)

## Plot the price of bitcoin and color the chart line based on the fear and greed index's value
candles_with_fear_and_greed_index %>%
  ggplot(aes(x = time, y = close, color = value)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "BTC-USD Price and Fear and Greed Index Value",
    x = "Time",
    y = "Price",
    color = "Fear and Greed Value"
  ) +
  scale_y_continuous(labels = scales::comma)

# We can see that when the price is going down the fear and greed index is low, and when the price goes up it's high
# This could be a good feature to guess the candles color

# If we look at a candle separately we can guess its feature:
# - Size of the body
# - Size of the upper shadow / wicks
# - Size of the lower shadow / wicks
# - Direction / color of the candle (up or down / green or red)

# The overall direction of the market can also be guessed by some technical indicators like the moving average, the relative strength index, the MACD, the Bollinger Bands, etc.
# Instead of using these indicators, we will use the fear and greed index and the trading volume as features

# We will create the data set with 14 previous candles features and the 15th candle's direction as the target

# Check NAs
sum(is.na(candles_with_fear_and_greed_index))

# Display the NAs
candles_with_fear_and_greed_index %>% filter(is.na(value))

# we can can see taht the fear and greed index is NA for the date of 2024-10-26
# we can use the median between the 2024-10-25 and 2024-10-27

date_na <- as.Date("2024-10-26")
fear_and_greed_index_date_before_na <- fear_and_greed_index %>% filter(timestamp == as.Date("2024-10-25"))
fear_and_greed_index_date_after_na <- fear_and_greed_index %>% filter(timestamp == as.Date("2024-10-27"))
fear_and_greed_value_date_na <- mean(c(fear_and_greed_index_date_before_na$value, fear_and_greed_index_date_after_na$value))

# Add date_na to the fear and greed index which is missing
fear_and_greed_index <- fear_and_greed_index %>%
  bind_rows(tibble(timestamp = date_na, value = fear_and_greed_value_date_na, value_classification = "Greed"))

# Check NAs again
sum(is.na(fear_and_greed_index))

candles_with_fear_and_greed_index <- candles %>%
  mutate(date_only = as.Date(time)) %>%
  left_join(fear_and_greed_index, by = c("date_only" = "timestamp"))

sum(is.na(candles_with_fear_and_greed_index))

## Prepare the data for the model

candles <- read_csv(paste0("data/", trading_pair, "_candles_", start_date, "_", end_date, ".csv"))
fear_and_greed_index <- read_csv(paste0("data/", trading_pair, "_fear_and_greed_index_", start_date, "_", end_date, ".csv"))
fear_and_greed_index <- fear_and_greed_index %>% mutate(value = as.numeric(value))

date_na <- as.Date("2024-10-26")
fear_and_greed_index_date_before_na <- fear_and_greed_index %>% filter(timestamp == as.Date("2024-10-25"))
fear_and_greed_index_date_after_na <- fear_and_greed_index %>% filter(timestamp == as.Date("2024-10-27"))
fear_and_greed_value_date_na <- mean(c(fear_and_greed_index_date_before_na$value, fear_and_greed_index_date_after_na$value))

fear_and_greed_index <- fear_and_greed_index %>%
  bind_rows(tibble(timestamp = date_na, value = fear_and_greed_value_date_na, value_classification = "Greed"))

dataset <- create_features(candles, fear_and_greed_index)

## Testing machine learning models using a very small sample of the data

# Take last 1000 candles
small_dataset <- dataset %>% tail(1000)
set.seed(1)

# Split the data into training and testing sets
test_index <- createDataPartition(y = small_dataset$direction, times = 1, p = 0.2, list = FALSE)
train_set <- small_dataset[-test_index, ]
test_set <- small_dataset[test_index, ]

# Function to create feature sets with different lags
create_feature_set <- function(n_lags) {
  features <- c()

  # Add base features for each lag
  for (i in 1:n_lags) {
    features <- c(
      features,
      paste0("body_size_lag_", i),
      paste0("upper_shadow_size_lag_", i),
      paste0("lower_shadow_size_lag_", i),
      paste0("direction_lag_", i)
    )
  }

  # Add volume and fear & greed value
  features <- c(features, "volume", "value")

  # Create formula for model training
  formula_str <- paste("direction ~", paste(features, collapse = " + "))
  return(as.formula(formula_str))
}

# Create formulas for different lag configurations
formula_3_lag <- create_feature_set(3)
formula_5_lag <- create_feature_set(5)
formula_7_lag <- create_feature_set(7)
formula_15_lag <- create_feature_set(15)

# Set seed for reproducibility
set.seed(123)

# Train GLM models
start_time <- Sys.time()
glm_3_lags <- train(formula_3_lag, data = train_set, method = "glm", family = "binomial")
end_time <- Sys.time()
print(paste("GLM 3 lags training time:", format(end_time - start_time, digits = 2)))

start_time <- Sys.time()
glm_5_lags <- train(formula_5_lag, data = train_set, method = "glm", family = "binomial")
end_time <- Sys.time()
print(paste("GLM 5 lags training time:", format(end_time - start_time, digits = 2)))

start_time <- Sys.time()
glm_7_lags <- train(formula_7_lag, data = train_set, method = "glm", family = "binomial")
end_time <- Sys.time()
print(paste("GLM 7 lags training time:", format(end_time - start_time, digits = 2)))

start_time <- Sys.time()
glm_15_lags <- train(formula_15_lag, data = train_set, method = "glm", family = "binomial")
end_time <- Sys.time()
print(paste("GLM 15 lags training time:", format(end_time - start_time, digits = 2)))

# Train Decision Tree models
start_time <- Sys.time()
tree_3_lags <- train(formula_3_lag, data = train_set, method = "rpart")
end_time <- Sys.time()
print(paste("Decision Tree 3 lags training time:", format(end_time - start_time, digits = 2)))

start_time <- Sys.time()
tree_5_lags <- train(formula_5_lag, data = train_set, method = "rpart")
end_time <- Sys.time()
print(paste("Decision Tree 5 lags training time:", format(end_time - start_time, digits = 2)))

start_time <- Sys.time()
tree_7_lags <- train(formula_7_lag, data = train_set, method = "rpart")
end_time <- Sys.time()
print(paste("Decision Tree 7 lags training time:", format(end_time - start_time, digits = 2)))

start_time <- Sys.time()
tree_15_lags <- train(formula_15_lag, data = train_set, method = "rpart")
end_time <- Sys.time()
print(paste("Decision Tree 15 lags training time:", format(end_time - start_time, digits = 2)))

# Train Random Forest models
start_time <- Sys.time()
rf_3_lags <- train(formula_3_lag, data = train_set, method = "rf", ntree = 100)
end_time <- Sys.time()
print(paste("Random Forest 3 lags training time:", format(end_time - start_time, digits = 2)))

start_time <- Sys.time()
rf_5_lags <- train(formula_5_lag, data = train_set, method = "rf", ntree = 100)
end_time <- Sys.time()
print(paste("Random Forest 5 lags training time:", format(end_time - start_time, digits = 2)))

start_time <- Sys.time()
rf_7_lags <- train(formula_7_lag, data = train_set, method = "rf", ntree = 100)
end_time <- Sys.time()
print(paste("Random Forest 7 lags training time:", format(end_time - start_time, digits = 2)))

start_time <- Sys.time()
rf_15_lags <- train(formula_15_lag, data = train_set, method = "rf", ntree = 100)
end_time <- Sys.time()
print(paste("Random Forest 15 lags training time:", format(end_time - start_time, digits = 2)))

# Train KNN models
start_time <- Sys.time()
knn_3_lags <- train(formula_3_lag,
  data = train_set, method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = seq(3, 15, 2))
)
end_time <- Sys.time()
print(paste("KNN 3 lags training time:", format(end_time - start_time, digits = 2)))

start_time <- Sys.time()
knn_5_lags <- train(formula_5_lag,
  data = train_set, method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = seq(3, 15, 2))
)
end_time <- Sys.time()
print(paste("KNN 5 lags training time:", format(end_time - start_time, digits = 2)))

start_time <- Sys.time()
knn_7_lags <- train(formula_7_lag,
  data = train_set, method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = seq(3, 15, 2))
)
end_time <- Sys.time()
print(paste("KNN 7 lags training time:", format(end_time - start_time, digits = 2)))

start_time <- Sys.time()
knn_15_lags <- train(formula_15_lag,
  data = train_set, method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = seq(3, 15, 2))
)
end_time <- Sys.time()
print(paste("KNN 15 lags training time:", format(end_time - start_time, digits = 2)))

# Calculate accuracies
results <- data.frame(
  model = character(),
  n_lags = numeric(),
  accuracy = numeric(),
  stringsAsFactors = FALSE
)

# Function to add model results
add_model_results <- function(model, model_name, n_lags) {
  predictions <- predict(model, test_set)
  accuracy <- mean(predictions == test_set$direction)
  data.frame(
    model = model_name,
    n_lags = n_lags,
    accuracy = accuracy
  )
}

# Add results for all models
results <- rbind(
  # GLM results
  add_model_results(glm_3_lags, "GLM", 3),
  add_model_results(glm_5_lags, "GLM", 5),
  add_model_results(glm_7_lags, "GLM", 7),
  add_model_results(glm_15_lags, "GLM", 15),

  # Decision Tree results
  add_model_results(tree_3_lags, "Decision Tree", 3),
  add_model_results(tree_5_lags, "Decision Tree", 5),
  add_model_results(tree_7_lags, "Decision Tree", 7),
  add_model_results(tree_15_lags, "Decision Tree", 15),

  # Random Forest results
  add_model_results(rf_3_lags, "Random Forest", 3),
  add_model_results(rf_5_lags, "Random Forest", 5),
  add_model_results(rf_7_lags, "Random Forest", 7),
  add_model_results(rf_15_lags, "Random Forest", 15),

  # KNN results
  add_model_results(knn_3_lags, "KNN", 3),
  add_model_results(knn_5_lags, "KNN", 5),
  add_model_results(knn_7_lags, "KNN", 7),
  add_model_results(knn_15_lags, "KNN", 15)
)

# Display results
results %>%
  arrange(desc(accuracy)) %>%
  mutate(accuracy = scales::percent(accuracy, accuracy = 0.01)) %>%
  print()

# Plot results
ggplot(results, aes(x = factor(n_lags), y = accuracy, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Model Performance Comparison",
    x = "Number of Lags",
    y = "Accuracy",
    fill = "Model Type"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom")

# Find best model
best_result <- results %>%
  arrange(desc(accuracy)) %>%
  slice(1)

message(sprintf(
  "\nBest model: %s with %d lags (Accuracy: %s)",
  best_result$model,
  best_result$n_lags,
  best_result$accuracy
))

# VOLUME SHOULD BE INCLUDED IN THE LAGS
# https://www.neuroquantology.com/open-access/An+Optimized+Machine+Learning+Model+for+Candlestick+Chart+Analysis+to+Predict+Stock+Market+Trends_9861/?download=true

# References
# Coinbase API to get the candlestick data: https://docs.cdp.coinbase.com/exchange/reference/exchangerestapi_getproductcandles
# https://business-science.github.io/tidyquant/articles/TQ04-charting-with-tidyquant.html
# Fear and Greed Index: https://alternative.me/crypto/fear-and-greed-index/
