### Dependencies ###

if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if (!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if (!require(tidyquant)) install.packages("tidyquant", repos = "http://cran.us.r-project.org")
if (!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if (!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if (!require(TTR)) install.packages("TTR", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(httr)
library(jsonlite)
library(tidyquant)
library(patchwork)
library(randomForest)
library(TTR)

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

create_features <- function(candles_data, fear_and_greed_data, hash_rate_data, average_block_size_data, n_transactions_data, utxo_count_data) {
  candles_with_fear_and_greed_data <- candles_data %>%
    mutate(date_only = as.Date(time)) %>%
    left_join(fear_and_greed_data, by = c("date_only" = "timestamp")) %>%
    left_join(hash_rate_data, by = c("date_only" = "timestamp")) %>%
    left_join(average_block_size_data, by = c("date_only" = "timestamp")) %>%
    left_join(n_transactions_data, by = c("date_only" = "timestamp")) %>%
    left_join(utxo_count_data, by = c("date_only" = "timestamp"))

  candles_with_fear_and_greed_data <- candles_with_fear_and_greed_data %>%
    mutate(
      body_size = abs(close - open),
      upper_shadow_size = high - pmax(close, open),
      lower_shadow_size = pmin(close, open) - low,
      direction = ifelse(close > open, "up", "down"),
    )

  # if (sum(is.na(candles_with_fear_and_greed_data)) > 0) {
  #  stop(paste("There are NAs in the data: ", sum(is.na(candles_with_fear_and_greed_data))))
  # }

  candles_with_fear_and_greed_data_and_ta <- candles_with_fear_and_greed_data %>%
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

  # add 15 lagged candle's features using a loop instead of manual listing
  for (i in 1:15) {
    candles_with_fear_and_greed_data_and_ta[[paste0("body_size_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$body_size, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("upper_shadow_size_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$upper_shadow_size, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("lower_shadow_size_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$lower_shadow_size, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("direction_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$direction, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("volume_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$volume, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("value_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$value, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("close_price_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$close, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("hash_rate_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$hash_rate, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("avg_block_size_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$avg_block_size, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("n_transactions_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$n_transactions, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("utxo_count_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$utxo_count, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("open_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$open, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("high_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$high, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("low_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$low, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("close_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$close, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("roc_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$roc, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("macd_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$macd, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("signal_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$signal, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("rsi_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$rsi, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("up_bband_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$up, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("mavg_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$mavg, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("dn_bband_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$dn, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("pctB_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$pctB, i)
  }

  candles_with_fear_and_greed_data_and_ta <- candles_with_fear_and_greed_data_and_ta %>%
    drop_na()

  return(candles_with_fear_and_greed_data_and_ta)
}


### Script ###

## Loading the data

# candles <- get_coinbase_candles()
# write_csv(candles, paste0("data/", trading_pair, "_candles_", start_date, "_", end_date, "_", candlestick_period, ".csv"))

# Loading the predownloaded data set, uncomment the code above if needed to download another data sets
candles <- read_csv(paste0("data/", trading_pair, "_candles_", start_date, "_", end_date, "_", candlestick_period, ".csv"))


# Explore the data
head(candles)

# fear_and_greed_index <- get_fear_and_greed_index()
# write_csv(fear_and_greed_index, paste0("data/", trading_pair, "_fear_and_greed_index_", start_date, "_", end_date, ".csv"))

# Loading the predownloaded fear and greed data set, uncomment the code above if needed to download another data sets
fear_and_greed_index <- read_csv(paste0("data/", trading_pair, "_fear_and_greed_index_", start_date, "_", end_date, ".csv"))
fear_and_greed_index <- fear_and_greed_index %>% mutate(value = as.numeric(value))
head(fear_and_greed_index)

hash_rate <- jsonlite::fromJSON("data/hash-rate.json")$`hash-rate` %>%
  rename(timestamp = x, hash_rate = y) %>%
  mutate(timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  filter(timestamp >= as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC") & timestamp <= as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC"))
head(hash_rate)

average_block_size <- jsonlite::fromJSON("data/avg-block-size.json")$`avg-block-size` %>%
  rename(timestamp = x, avg_block_size = y) %>%
  mutate(timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  filter(timestamp >= as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC") & timestamp <= as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC"))
head(average_block_size)

n_transactions <- jsonlite::fromJSON("data/n-transactions.json")$`n-transactions` %>%
  rename(timestamp = x, n_transactions = y) %>%
  mutate(timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  filter(timestamp >= as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC") & timestamp <= as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC"))
head(n_transactions)

utxo_count <- jsonlite::fromJSON("data/utxo-count.json")$`utxo-count` %>%
  rename(timestamp = x, utxo_count = y) %>%
  mutate(
    timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC"),
    timestamp = as.Date(timestamp)
  ) %>%
  filter(timestamp >= as.Date(start_date) & timestamp <= as.Date(end_date))
head(utxo_count)

## Visualizing the data

# Plot the data to check if everything is good

# Create the multi-panel plot
p1 <- candles %>%
  ggplot(aes(x = time, y = close)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "BTC-USD Price", y = "Price") +
  scale_y_continuous(labels = scales::comma)

p2 <- hash_rate %>%
  ggplot(aes(x = timestamp, y = hash_rate)) +
  geom_line(color = "red") +
  theme_minimal() +
  labs(title = "Hash Rate", y = "Hash Rate") +
  scale_y_continuous(labels = scales::comma)

p3 <- average_block_size %>%
  ggplot(aes(x = timestamp, y = avg_block_size)) +
  geom_line(color = "green4") +
  theme_minimal() +
  labs(title = "Average Block Size", y = "Size") +
  scale_y_continuous(labels = scales::comma)

p4 <- n_transactions %>%
  ggplot(aes(x = timestamp, y = n_transactions)) +
  geom_line(color = "purple") +
  theme_minimal() +
  labs(title = "Number of Transactions", y = "Count") +
  scale_y_continuous(labels = scales::comma)

p5 <- utxo_count %>%
  ggplot(aes(x = timestamp, y = utxo_count)) +
  geom_line(color = "orange") +
  theme_minimal() +
  labs(title = "UTXO Count", y = "Count") +
  scale_y_continuous(labels = scales::comma)

combined_plot <- (p1 / p2 / p3 / p4 / p5) +
  plot_layout(ncol = 1, heights = c(1, 1, 1, 1, 1)) +
  plot_annotation(
    title = "Bitcoin Price and Blockchain Metrics",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
  ) &
  theme(axis.title.x = element_blank())

print(combined_plot)

# Original single plot kept for reference
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
  left_join(fear_and_greed_index, by = c("date_only" = "timestamp")) %>%
  left_join(hash_rate, by = c("date_only" = "timestamp")) %>%
  left_join(average_block_size, by = c("date_only" = "timestamp")) %>%
  left_join(n_transactions, by = c("date_only" = "timestamp")) %>%
  left_join(utxo_count, by = c("date_only" = "timestamp"))

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


# Chart to compare the number of up vs down candles
candles %>%
  mutate(direction = ifelse(close >= open, "up", "down")) %>%
  summarise(
    up = sum(direction == "up"),
    down = sum(direction == "down")
  ) %>%
  pivot_longer(cols = everything(), names_to = "direction", values_to = "count") %>%
  ggplot(aes(x = direction, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Number of Up vs Down Candles",
    x = "Direction",
    y = "Count"
  )


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

candles <- read_csv(paste0("data/", trading_pair, "_candles_", start_date, "_", end_date, "_", candlestick_period, ".csv"))
fear_and_greed_index <- read_csv(paste0("data/", trading_pair, "_fear_and_greed_index_", start_date, "_", end_date, ".csv"))
fear_and_greed_index <- fear_and_greed_index %>% mutate(value = as.numeric(value))

date_na <- as.Date("2024-10-26")
fear_and_greed_index_date_before_na <- fear_and_greed_index %>% filter(timestamp == as.Date("2024-10-25"))
fear_and_greed_index_date_after_na <- fear_and_greed_index %>% filter(timestamp == as.Date("2024-10-27"))
fear_and_greed_value_date_na <- mean(c(fear_and_greed_index_date_before_na$value, fear_and_greed_index_date_after_na$value))

fear_and_greed_index <- fear_and_greed_index %>%
  bind_rows(tibble(timestamp = date_na, value = fear_and_greed_value_date_na, value_classification = "Greed"))

dataset <- create_features(candles, fear_and_greed_index, hash_rate, average_block_size, n_transactions, utxo_count)

## Testing machine learning models using a very small sample of the data

# Take last 1000 candles
small_dataset <- dataset
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
      paste0("direction_lag_", i),
      paste0("volume_lag_", i),
      paste0("value_lag_", i),
      paste0("close_price_lag_", i),
      paste0("hash_rate_lag_", i),
      paste0("avg_block_size_lag_", i),
      paste0("n_transactions_lag_", i),
      paste0("utxo_count_lag_", i),
      paste0("open_lag_", i),
      paste0("high_lag_", i),
      paste0("low_lag_", i),
      paste0("close_lag_", i),
      paste0("roc_lag_", i),
      paste0("macd_lag_", i),
      paste0("signal_lag_", i),
      paste0("rsi_lag_", i),
      paste0("up_bband_lag_", i),
      paste0("mavg_lag_", i),
      paste0("dn_bband_lag_", i),
      paste0("pctB_lag_", i)
    )
  }

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
saveRDS(glm_3_lags, paste0("models/glm_3_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
glm_5_lags <- train(formula_5_lag, data = train_set, method = "glm", family = "binomial")
end_time <- Sys.time()
print(paste("GLM 5 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(glm_5_lags, paste0("models/glm_5_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
glm_7_lags <- train(formula_7_lag, data = train_set, method = "glm", family = "binomial")
end_time <- Sys.time()
print(paste("GLM 7 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(glm_7_lags, paste0("models/glm_7_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
glm_15_lags <- train(formula_15_lag, data = train_set, method = "glm", family = "binomial")
end_time <- Sys.time()
print(paste("GLM 15 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(glm_15_lags, paste0("models/glm_15_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

# Train Decision Tree models
start_time <- Sys.time()
tree_3_lags <- train(formula_3_lag, data = train_set, method = "rpart")
end_time <- Sys.time()
print(paste("Decision Tree 3 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(tree_3_lags, paste0("models/tree_3_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
tree_5_lags <- train(formula_5_lag, data = train_set, method = "rpart")
end_time <- Sys.time()
print(paste("Decision Tree 5 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(tree_5_lags, paste0("models/tree_5_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
tree_7_lags <- train(formula_7_lag, data = train_set, method = "rpart")
end_time <- Sys.time()
print(paste("Decision Tree 7 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(tree_7_lags, paste0("models/tree_7_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
tree_15_lags <- train(formula_15_lag, data = train_set, method = "rpart")
end_time <- Sys.time()
print(paste("Decision Tree 15 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(tree_15_lags, paste0("models/tree_15_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

# Train Random Forest models
start_time <- Sys.time()
rf_3_lags <- train(formula_3_lag, data = train_set, method = "rf", ntree = 100)
end_time <- Sys.time()
print(paste("Random Forest 3 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_3_lags, paste0("models/rf_3_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
rf_5_lags <- train(formula_5_lag, data = train_set, method = "rf", ntree = 100)
end_time <- Sys.time()
print(paste("Random Forest 5 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_5_lags, paste0("models/rf_5_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

# start_time <- Sys.time()
rf_7_lags <- train(formula_7_lag, data = train_set, method = "rf", ntree = 100)
end_time <- Sys.time()
print(paste("Random Forest 7 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_7_lags, paste0("models/rf_7_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

# start_time <- Sys.time()
rf_15_lags <- train(formula_15_lag, data = train_set, method = "rf", ntree = 100)
end_time <- Sys.time()
print(paste("Random Forest 15 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_15_lags, paste0("models/rf_15_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

# # Train KNN models
start_time <- Sys.time()
knn_3_lags <- train(formula_3_lag,
  data = train_set, method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = seq(3, 15, 2))
)
end_time <- Sys.time()
print(paste("KNN 3 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(knn_3_lags, paste0("models/knn_3_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
knn_5_lags <- train(formula_5_lag,
  data = train_set, method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = seq(3, 15, 2))
)
end_time <- Sys.time()
print(paste("KNN 5 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(knn_5_lags, paste0("models/knn_5_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
knn_7_lags <- train(formula_7_lag,
  data = train_set, method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = seq(3, 15, 2))
)
end_time <- Sys.time()
print(paste("KNN 7 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(knn_7_lags, paste0("models/knn_7_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
knn_15_lags <- train(formula_15_lag,
  data = train_set, method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = seq(3, 15, 2))
)
end_time <- Sys.time()
print(paste("KNN 15 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(knn_15_lags, paste0("models/knn_15_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

# Train GBM models
start_time <- Sys.time()
gbm_3_lags <- train(formula_3_lag, data = train_set, method = "gbm")
end_time <- Sys.time()
print(paste("GBM 3 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(gbm_3_lags, paste0("models/gbm_3_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
gbm_5_lags <- train(formula_5_lag, data = train_set, method = "gbm")
end_time <- Sys.time()
print(paste("GBM 5 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(gbm_5_lags, paste0("models/gbm_5_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
gbm_7_lags <- train(formula_7_lag, data = train_set, method = "gbm")
end_time <- Sys.time()
print(paste("GBM 7 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(gbm_7_lags, paste0("models/gbm_7_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
gbm_15_lags <- train(formula_15_lag, data = train_set, method = "gbm")
end_time <- Sys.time()
print(paste("GBM 15 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(gbm_15_lags, paste0("models/gbm_15_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

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
  add_model_results(knn_15_lags, "KNN", 15),

  # GBM results
  add_model_results(gbm_3_lags, "GBM", 3),
  add_model_results(gbm_5_lags, "GBM", 5),
  add_model_results(gbm_7_lags, "GBM", 7),
  add_model_results(gbm_15_lags, "GBM", 15)
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

# According to this we should try SVM, KNN, LR, DT, RF
# https://www.neuroquantology.com/open-access/An+Optimized+Machine+Learning+Model+for+Candlestick+Chart+Analysis+to+Predict+Stock+Market+Trends_9861/?download=true

# rf, svmRadial, and gbm for classification
# https://arxiv.org/pdf/1606.00930

# LSTM (not found in caret), GBM
# https://www.diva-portal.org/smash/get/diva2:1778251/FULLTEXT03.pdf


# Other dataset
# https://www.blockchain.com/explorer/charts/total-bitcoins

# References
# Coinbase API to get the candlestick data: https://docs.cdp.coinbase.com/exchange/reference/exchangerestapi_getproductcandles
# https://business-science.github.io/tidyquant/articles/TQ04-charting-with-tidyquant.html
# Fear and Greed Index: https://alternative.me/crypto/fear-and-greed-index/


### Feature importance

# load rds
rf_5_lags <- readRDS("models/with_all_features/rf_5_lags_20250409_114643.rds")
predictions <- predict(rf_5_lags, test_set)
accuracy <- mean(predictions == test_set$direction)
accuracy

importance <- varImp(rf_5_lags, scale = FALSE)

# Extract the top 20% most important features
imp_df <- importance$importance
imp_df$Feature <- rownames(imp_df)
imp_df <- imp_df %>%
  arrange(desc(Overall))

# Calculate how many features make up 20% of the total
n_features <- nrow(imp_df)
top_n_features <- ceiling(n_features * 0.2)

# Extract the top features
top_features <- imp_df %>%
  slice_head(n = top_n_features)

print(paste("Top", top_n_features, "features (20% of total", n_features, "features):"))
print(top_features)

# Plot the top features
top_features %>%
  ggplot(aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 20% Most Important Features",
    x = "Feature",
    y = "Importance"
  )


# Train a new model with the top features
### Dependencies ###

if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if (!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if (!require(tidyquant)) install.packages("tidyquant", repos = "http://cran.us.r-project.org")
if (!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if (!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if (!require(TTR)) install.packages("TTR", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(httr)
library(jsonlite)
library(tidyquant)
library(patchwork)
library(randomForest)
library(TTR)

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

create_features <- function(candles_data, fear_and_greed_data, hash_rate_data, average_block_size_data, n_transactions_data, utxo_count_data) {
  candles_with_fear_and_greed_data <- candles_data %>%
    mutate(date_only = as.Date(time)) %>%
    left_join(fear_and_greed_data, by = c("date_only" = "timestamp")) %>%
    left_join(hash_rate_data, by = c("date_only" = "timestamp")) %>%
    left_join(average_block_size_data, by = c("date_only" = "timestamp")) %>%
    left_join(n_transactions_data, by = c("date_only" = "timestamp")) %>%
    left_join(utxo_count_data, by = c("date_only" = "timestamp"))

  candles_with_fear_and_greed_data <- candles_with_fear_and_greed_data %>%
    mutate(
      body_size = abs(close - open),
      upper_shadow_size = high - pmax(close, open),
      lower_shadow_size = pmin(close, open) - low,
      direction = ifelse(close > open, "up", "down"),
    )

  # if (sum(is.na(candles_with_fear_and_greed_data)) > 0) {
  #  stop(paste("There are NAs in the data: ", sum(is.na(candles_with_fear_and_greed_data))))
  # }

  candles_with_fear_and_greed_data_and_ta <- candles_with_fear_and_greed_data %>%
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

  # add 15 lagged candle's features using a loop instead of manual listing
  for (i in 1:15) {
    candles_with_fear_and_greed_data_and_ta[[paste0("body_size_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$body_size, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("upper_shadow_size_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$upper_shadow_size, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("lower_shadow_size_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$lower_shadow_size, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("direction_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$direction, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("volume_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$volume, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("value_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$value, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("close_price_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$close, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("hash_rate_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$hash_rate, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("avg_block_size_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$avg_block_size, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("n_transactions_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$n_transactions, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("utxo_count_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$utxo_count, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("open_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$open, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("high_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$high, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("low_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$low, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("close_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$close, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("roc_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$roc, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("macd_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$macd, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("signal_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$signal, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("rsi_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$rsi, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("up_bband_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$up, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("mavg_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$mavg, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("dn_bband_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$dn, i)
    candles_with_fear_and_greed_data_and_ta[[paste0("pctB_lag_", i)]] <- lag(candles_with_fear_and_greed_data_and_ta$pctB, i)
  }

  candles_with_fear_and_greed_data_and_ta <- candles_with_fear_and_greed_data_and_ta %>%
    drop_na()

  return(candles_with_fear_and_greed_data_and_ta)
}


### Script ###

## Loading the data

# candles <- get_coinbase_candles()
# write_csv(candles, paste0("data/", trading_pair, "_candles_", start_date, "_", end_date, "_", candlestick_period, ".csv"))

# Loading the predownloaded data set, uncomment the code above if needed to download another data sets
candles <- read_csv(paste0("data/", trading_pair, "_candles_", start_date, "_", end_date, "_", candlestick_period, ".csv"))


# Explore the data
head(candles)

# fear_and_greed_index <- get_fear_and_greed_index()
# write_csv(fear_and_greed_index, paste0("data/", trading_pair, "_fear_and_greed_index_", start_date, "_", end_date, ".csv"))

# Loading the predownloaded fear and greed data set, uncomment the code above if needed to download another data sets
fear_and_greed_index <- read_csv(paste0("data/", trading_pair, "_fear_and_greed_index_", start_date, "_", end_date, ".csv"))
fear_and_greed_index <- fear_and_greed_index %>% mutate(value = as.numeric(value))
head(fear_and_greed_index)

hash_rate <- jsonlite::fromJSON("data/hash-rate.json")$`hash-rate` %>%
  rename(timestamp = x, hash_rate = y) %>%
  mutate(timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  filter(timestamp >= as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC") & timestamp <= as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC"))
head(hash_rate)

average_block_size <- jsonlite::fromJSON("data/avg-block-size.json")$`avg-block-size` %>%
  rename(timestamp = x, avg_block_size = y) %>%
  mutate(timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  filter(timestamp >= as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC") & timestamp <= as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC"))
head(average_block_size)

n_transactions <- jsonlite::fromJSON("data/n-transactions.json")$`n-transactions` %>%
  rename(timestamp = x, n_transactions = y) %>%
  mutate(timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC")) %>%
  filter(timestamp >= as.POSIXct(start_date, origin = "1970-01-01", tz = "UTC") & timestamp <= as.POSIXct(end_date, origin = "1970-01-01", tz = "UTC"))
head(n_transactions)

utxo_count <- jsonlite::fromJSON("data/utxo-count.json")$`utxo-count` %>%
  rename(timestamp = x, utxo_count = y) %>%
  mutate(
    timestamp = as.POSIXct(timestamp / 1000, origin = "1970-01-01", tz = "UTC"),
    timestamp = as.Date(timestamp)
  ) %>%
  filter(timestamp >= as.Date(start_date) & timestamp <= as.Date(end_date))
head(utxo_count)

## Visualizing the data

# Plot the data to check if everything is good

# Create the multi-panel plot
p1 <- candles %>%
  ggplot(aes(x = time, y = close)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "BTC-USD Price", y = "Price") +
  scale_y_continuous(labels = scales::comma)

p2 <- hash_rate %>%
  ggplot(aes(x = timestamp, y = hash_rate)) +
  geom_line(color = "red") +
  theme_minimal() +
  labs(title = "Hash Rate", y = "Hash Rate") +
  scale_y_continuous(labels = scales::comma)

p3 <- average_block_size %>%
  ggplot(aes(x = timestamp, y = avg_block_size)) +
  geom_line(color = "green4") +
  theme_minimal() +
  labs(title = "Average Block Size", y = "Size") +
  scale_y_continuous(labels = scales::comma)

p4 <- n_transactions %>%
  ggplot(aes(x = timestamp, y = n_transactions)) +
  geom_line(color = "purple") +
  theme_minimal() +
  labs(title = "Number of Transactions", y = "Count") +
  scale_y_continuous(labels = scales::comma)

p5 <- utxo_count %>%
  ggplot(aes(x = timestamp, y = utxo_count)) +
  geom_line(color = "orange") +
  theme_minimal() +
  labs(title = "UTXO Count", y = "Count") +
  scale_y_continuous(labels = scales::comma)

combined_plot <- (p1 / p2 / p3 / p4 / p5) +
  plot_layout(ncol = 1, heights = c(1, 1, 1, 1, 1)) +
  plot_annotation(
    title = "Bitcoin Price and Blockchain Metrics",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
  ) &
  theme(axis.title.x = element_blank())

print(combined_plot)

# Original single plot kept for reference
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
  left_join(fear_and_greed_index, by = c("date_only" = "timestamp")) %>%
  left_join(hash_rate, by = c("date_only" = "timestamp")) %>%
  left_join(average_block_size, by = c("date_only" = "timestamp")) %>%
  left_join(n_transactions, by = c("date_only" = "timestamp")) %>%
  left_join(utxo_count, by = c("date_only" = "timestamp"))

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


# Chart to compare the number of up vs down candles
candles %>%
  mutate(direction = ifelse(close >= open, "up", "down")) %>%
  summarise(
    up = sum(direction == "up"),
    down = sum(direction == "down")
  ) %>%
  pivot_longer(cols = everything(), names_to = "direction", values_to = "count") %>%
  ggplot(aes(x = direction, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Number of Up vs Down Candles",
    x = "Direction",
    y = "Count"
  )


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

candles <- read_csv(paste0("data/", trading_pair, "_candles_", start_date, "_", end_date, "_", candlestick_period, ".csv"))
fear_and_greed_index <- read_csv(paste0("data/", trading_pair, "_fear_and_greed_index_", start_date, "_", end_date, ".csv"))
fear_and_greed_index <- fear_and_greed_index %>% mutate(value = as.numeric(value))

date_na <- as.Date("2024-10-26")
fear_and_greed_index_date_before_na <- fear_and_greed_index %>% filter(timestamp == as.Date("2024-10-25"))
fear_and_greed_index_date_after_na <- fear_and_greed_index %>% filter(timestamp == as.Date("2024-10-27"))
fear_and_greed_value_date_na <- mean(c(fear_and_greed_index_date_before_na$value, fear_and_greed_index_date_after_na$value))

fear_and_greed_index <- fear_and_greed_index %>%
  bind_rows(tibble(timestamp = date_na, value = fear_and_greed_value_date_na, value_classification = "Greed"))

dataset <- create_features(candles, fear_and_greed_index, hash_rate, average_block_size, n_transactions, utxo_count)

## Testing machine learning models using a very small sample of the data

# Take last 1000 candles
small_dataset <- dataset
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
      paste0("direction_lag_", i),
      paste0("volume_lag_", i),
      paste0("value_lag_", i),
      paste0("close_price_lag_", i),
      paste0("hash_rate_lag_", i),
      paste0("avg_block_size_lag_", i),
      paste0("n_transactions_lag_", i),
      paste0("utxo_count_lag_", i),
      paste0("open_lag_", i),
      paste0("high_lag_", i),
      paste0("low_lag_", i),
      paste0("close_lag_", i),
      paste0("roc_lag_", i),
      paste0("macd_lag_", i),
      paste0("signal_lag_", i),
      paste0("rsi_lag_", i),
      paste0("up_bband_lag_", i),
      paste0("mavg_lag_", i),
      paste0("dn_bband_lag_", i),
      paste0("pctB_lag_", i)
    )
  }

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
saveRDS(glm_3_lags, paste0("models/glm_3_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
glm_5_lags <- train(formula_5_lag, data = train_set, method = "glm", family = "binomial")
end_time <- Sys.time()
print(paste("GLM 5 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(glm_5_lags, paste0("models/glm_5_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
glm_7_lags <- train(formula_7_lag, data = train_set, method = "glm", family = "binomial")
end_time <- Sys.time()
print(paste("GLM 7 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(glm_7_lags, paste0("models/glm_7_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
glm_15_lags <- train(formula_15_lag, data = train_set, method = "glm", family = "binomial")
end_time <- Sys.time()
print(paste("GLM 15 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(glm_15_lags, paste0("models/glm_15_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

# Train Decision Tree models
start_time <- Sys.time()
tree_3_lags <- train(formula_3_lag, data = train_set, method = "rpart")
end_time <- Sys.time()
print(paste("Decision Tree 3 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(tree_3_lags, paste0("models/tree_3_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
tree_5_lags <- train(formula_5_lag, data = train_set, method = "rpart")
end_time <- Sys.time()
print(paste("Decision Tree 5 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(tree_5_lags, paste0("models/tree_5_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
tree_7_lags <- train(formula_7_lag, data = train_set, method = "rpart")
end_time <- Sys.time()
print(paste("Decision Tree 7 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(tree_7_lags, paste0("models/tree_7_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
tree_15_lags <- train(formula_15_lag, data = train_set, method = "rpart")
end_time <- Sys.time()
print(paste("Decision Tree 15 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(tree_15_lags, paste0("models/tree_15_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

# Train Random Forest models
start_time <- Sys.time()
rf_3_lags <- train(formula_3_lag, data = train_set, method = "rf", ntree = 100)
end_time <- Sys.time()
print(paste("Random Forest 3 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_3_lags, paste0("models/rf_3_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
rf_5_lags <- train(formula_5_lag, data = train_set, method = "rf", ntree = 100)
end_time <- Sys.time()
print(paste("Random Forest 5 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_5_lags, paste0("models/rf_5_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

# start_time <- Sys.time()
rf_7_lags <- train(formula_7_lag, data = train_set, method = "rf", ntree = 100)
end_time <- Sys.time()
print(paste("Random Forest 7 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_7_lags, paste0("models/rf_7_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

# start_time <- Sys.time()
rf_15_lags <- train(formula_15_lag, data = train_set, method = "rf", ntree = 100)
end_time <- Sys.time()
print(paste("Random Forest 15 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_15_lags, paste0("models/rf_15_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

# # Train KNN models
start_time <- Sys.time()
knn_3_lags <- train(formula_3_lag,
  data = train_set, method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = seq(3, 15, 2))
)
end_time <- Sys.time()
print(paste("KNN 3 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(knn_3_lags, paste0("models/knn_3_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
knn_5_lags <- train(formula_5_lag,
  data = train_set, method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = seq(3, 15, 2))
)
end_time <- Sys.time()
print(paste("KNN 5 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(knn_5_lags, paste0("models/knn_5_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
knn_7_lags <- train(formula_7_lag,
  data = train_set, method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = seq(3, 15, 2))
)
end_time <- Sys.time()
print(paste("KNN 7 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(knn_7_lags, paste0("models/knn_7_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
knn_15_lags <- train(formula_15_lag,
  data = train_set, method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = seq(3, 15, 2))
)
end_time <- Sys.time()
print(paste("KNN 15 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(knn_15_lags, paste0("models/knn_15_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

# Train GBM models
start_time <- Sys.time()
gbm_3_lags <- train(formula_3_lag, data = train_set, method = "gbm")
end_time <- Sys.time()
print(paste("GBM 3 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(gbm_3_lags, paste0("models/gbm_3_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
gbm_5_lags <- train(formula_5_lag, data = train_set, method = "gbm")
end_time <- Sys.time()
print(paste("GBM 5 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(gbm_5_lags, paste0("models/gbm_5_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
gbm_7_lags <- train(formula_7_lag, data = train_set, method = "gbm")
end_time <- Sys.time()
print(paste("GBM 7 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(gbm_7_lags, paste0("models/gbm_7_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

start_time <- Sys.time()
gbm_15_lags <- train(formula_15_lag, data = train_set, method = "gbm")
end_time <- Sys.time()
print(paste("GBM 15 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(gbm_15_lags, paste0("models/gbm_15_lags_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

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
  add_model_results(knn_15_lags, "KNN", 15),

  # GBM results
  add_model_results(gbm_3_lags, "GBM", 3),
  add_model_results(gbm_5_lags, "GBM", 5),
  add_model_results(gbm_7_lags, "GBM", 7),
  add_model_results(gbm_15_lags, "GBM", 15)
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

# According to this we should try SVM, KNN, LR, DT, RF
# https://www.neuroquantology.com/open-access/An+Optimized+Machine+Learning+Model+for+Candlestick+Chart+Analysis+to+Predict+Stock+Market+Trends_9861/?download=true

# rf, svmRadial, and gbm for classification
# https://arxiv.org/pdf/1606.00930

# LSTM (not found in caret), GBM
# https://www.diva-portal.org/smash/get/diva2:1778251/FULLTEXT03.pdf


# Other dataset
# https://www.blockchain.com/explorer/charts/total-bitcoins

# References
# Coinbase API to get the candlestick data: https://docs.cdp.coinbase.com/exchange/reference/exchangerestapi_getproductcandles
# https://business-science.github.io/tidyquant/articles/TQ04-charting-with-tidyquant.html
# Fear and Greed Index: https://alternative.me/crypto/fear-and-greed-index/


### Feature importance

# load rds
rf_5_lags <- readRDS("models/with_all_features/rf_5_lags_20250409_114643.rds")
predictions <- predict(rf_5_lags, test_set)

importance <- varImp(rf_5_lags, scale = FALSE)

# Extract the top 20% most important features
imp_df <- importance$importance
imp_df$Feature <- rownames(imp_df)
imp_df <- imp_df %>%
  arrange(desc(Overall))

# Calculate how many features make up 20% of the total
n_features <- nrow(imp_df)
top_n_features <- ceiling(n_features * 0.5)

# Extract the top features
top_features <- imp_df %>%
  slice_head(n = top_n_features)

print(paste("Top", top_n_features, "features (50% of total", n_features, "features):"))
print(top_features)

# Plot the top features
top_features %>%
  ggplot(aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Top 50% Most Important Features",
    x = "Feature",
    y = "Importance"
  )

# Train random forest with the initial features from 1 to 7 lags and store the model
formula_1_lag <- create_feature_set(1)
formula_2_lags <- create_feature_set(2)
formula_3_lags <- create_feature_set(3)
formula_4_lags <- create_feature_set(4)
formula_5_lags <- create_feature_set(5)
formula_6_lags <- create_feature_set(6)
formula_7_lags <- create_feature_set(7)

# This time I will use cross validation to train the model
# I will use 5 folds
folds <- createFolds(train_set$direction, k = 5)
train_control <- trainControl(method = "cv", number = 5)
fine

# Train the model
start_time <- Sys.time()
rf_1_lag <- train(formula_1_lag, data = train_set, method = "rf", ntree = 100, trControl = train_control)
end_time <- Sys.time()
print(paste("Random Forest 1 lag training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_1_lag, "models/rf_1_lag.rds")

start_time <- Sys.time()
rf_2_lags <- train(formula_2_lags, data = train_set, method = "rf", ntree = 100, trControl = train_control)
end_time <- Sys.time()
print(paste("Random Forest 2 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_2_lags, "models/rf_2_lags.rds")

start_time <- Sys.time()
rf_3_lags <- train(formula_3_lags, data = train_set, method = "rf", ntree = 100, trControl = train_control)
end_time <- Sys.time()
print(paste("Random Forest 3 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_3_lags, "models/rf_3_lags.rds")

start_time <- Sys.time()
rf_4_lags <- train(formula_4_lags, data = train_set, method = "rf", ntree = 100, trControl = train_control)
end_time <- Sys.time()
print(paste("Random Forest 4 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_4_lags, "models/rf_4_lags.rds")

start_time <- Sys.time()
rf_5_lags <- train(formula_5_lags, data = train_set, method = "rf", ntree = 100, trControl = train_control)
end_time <- Sys.time()
print(paste("Random Forest 5 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_5_lags, "models/rf_5_lags.rds")

start_time <- Sys.time()
rf_6_lags <- train(formula_6_lags, data = train_set, method = "rf", ntree = 100, trControl = train_control)
end_time <- Sys.time()
print(paste("Random Forest 6 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_6_lags, "models/rf_6_lags.rds")

start_time <- Sys.time()
rf_7_lags <- train(formula_7_lags, data = train_set, method = "rf", ntree = 100, trControl = train_control)
end_time <- Sys.time()
print(paste("Random Forest 7 lags training time:", format(end_time - start_time, digits = 2)))
saveRDS(rf_7_lags, "models/rf_7_lags.rds")

# plot the accuracy of the best result for each model like comparing the bestTune for each result
# Extract best accuracies for each model
accuracies <- c(
  rf_1_lag$results$Accuracy[rf_1_lag$results$mtry == rf_1_lag$bestTune$mtry],
  rf_2_lags$results$Accuracy[rf_2_lags$results$mtry == rf_2_lags$bestTune$mtry],
  rf_3_lags$results$Accuracy[rf_3_lags$results$mtry == rf_3_lags$bestTune$mtry],
  rf_4_lags$results$Accuracy[rf_4_lags$results$mtry == rf_4_lags$bestTune$mtry],
  rf_5_lags$results$Accuracy[rf_5_lags$results$mtry == rf_5_lags$bestTune$mtry],
  rf_6_lags$results$Accuracy[rf_6_lags$results$mtry == rf_6_lags$bestTune$mtry],
  rf_7_lags$results$Accuracy[rf_7_lags$results$mtry == rf_7_lags$bestTune$mtry]
)

# Create data frame for plotting
plot_data <- data.frame(
  Lags = 1:7,
  Accuracy = accuracies
)

# Create the plot
ggplot(plot_data, aes(x = Lags, y = Accuracy)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Best Accuracy by Number of Lags",
    x = "Number of Lags",
    y = "Accuracy"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:7)
