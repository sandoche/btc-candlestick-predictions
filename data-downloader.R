### Data download helpers ###

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

### Data download ###

if (is.null(trading_pair) || is.null(start_date) || is.null(end_date) || is.null(candlestick_period)) {
  stop("trading_pair, start_date, end_date and candlestick_period must be set")
}

candles <- get_coinbase_candles()
write_csv(candles, paste0("data/", trading_pair, "_candles_", start_date, "_", end_date, "_", candlestick_period, ".csv"))

fear_and_greed_index <- get_fear_and_greed_index()
write_csv(fear_and_greed_index, paste0("data/", trading_pair, "_fear_and_greed_index_", start_date, "_", end_date, ".csv"))
