# Notes

## Test 1

trading_pair <- "BTC-USD"
start_date <- "2024-01-01"
end_date <- "2025-03-29"
candlestick_period <- 86400

Without Fear and greed index

           model n_lags accuracy
1            KNN      3   58.43%
2           nnet      7   56.18%
3            GLM     15   53.93%
4  Random Forest     15   53.93%
5            KNN     15   53.93%
6            GLM      3   52.81%
7            GLM      7   52.81%
8  Random Forest      7   52.81%
9            KNN      7   52.81%
10 Decision Tree      3   51.69%
11 Decision Tree      5   51.69%
12 Decision Tree      7   51.69%
13 Decision Tree     15   51.69%
14 Random Forest      5   51.69%
15           GLM      5   49.44%
16           KNN      5   48.31%
17          nnet      5   48.31%
18 Random Forest      3   47.19%
19          nnet     15   41.57%
20          nnet      3   37.08%


With Fear and greed index

           model n_lags accuracy
1            GLM      7   59.55%
2  Random Forest      7   56.18%
3           nnet     15   55.06%
4            GLM      5   52.81%
5            GLM     15   51.69%
6  Decision Tree      3   51.69%
7  Decision Tree      5   51.69%
8  Decision Tree      7   51.69%
9  Decision Tree     15   51.69%
10          nnet      5   51.69%
11 Random Forest     15   50.56%
12           KNN      3   48.31%
13           KNN      5   48.31%
14           GLM      3   47.19%
15           KNN      7   47.19%
16          nnet      3   47.19%
17 Random Forest      5   43.82%
18          nnet      7   42.70%
19 Random Forest      3   41.57%
20           KNN     15   35.96%

## Test 2

trading_pair <- "BTC-USD"
start_date <- "2018-02-01"
end_date <- "2025-03-29"
candlestick_period <- 86400

Without Fear and greed index

           model n_lags accuracy
1  Random Forest      7   55.19%
2            GLM      5   53.46%
3            KNN      3   53.46%
4            GLM      7   53.08%
5  Random Forest     15   52.50%
6            GLM      3   52.31%
7  Random Forest      5   51.92%
8  Decision Tree      5   51.73%
9  Decision Tree      7   51.73%
10 Decision Tree     15   51.73%
11           KNN     15   51.35%
12           KNN      7   51.15%
13 Decision Tree      3   50.96%
14 Random Forest      3   50.96%
15          nnet      3   50.96%
16           GLM     15   50.77%
17          nnet      5   50.19%
18           KNN      5   49.23%
19          nnet     15   47.88%
20          nnet      7   47.69%

With Fear and greed index

           model n_lags accuracy
1            GLM      5   55.51%
2            GLM      3   54.93%
3           nnet      7   53.58%
4  Decision Tree      7   53.38%
5  Random Forest      3   52.80%
6            GLM      7   52.42%
7  Decision Tree     15   52.42%
8  Random Forest      7   52.42%
9            KNN      3   52.42%
10           KNN      5   52.22%
11           KNN      7   52.03%
12          nnet      5   51.84%
13           GLM     15   51.45%
14 Decision Tree      5   51.26%
15           KNN     15   51.06%
16 Decision Tree      3   50.87%
17          nnet      3   50.87%
18          nnet     15   50.68%
19 Random Forest      5   49.71%
20 Random Forest     15   49.52%

### With Fear and Greed Index

And close price

           model n_lags accuracy
1  Decision Tree     15   55.90%
2            GLM      3   54.93%
3            GLM      5   54.93%
4  Decision Tree      7   53.38%
5            GLM     15   53.00%
6            GLM      7   52.61%
7  Random Forest      7   52.61%
8            KNN      7   52.61%
9            KNN      5   52.03%
10 Random Forest      3   51.45%
11 Random Forest      5   51.06%
12 Random Forest     15   51.06%
13 Decision Tree      3   50.87%
14 Decision Tree      5   50.87%
15           KNN      3   50.87%
16          nnet      7   50.87%
17          nnet     15   50.87%
18           KNN     15   50.10%
19          nnet      3   49.71%
20          nnet      5   48.74%


## Test 3

WITH EVERYTHING

           model n_lags accuracy
1  Decision Tree      3   64.98%
2  Decision Tree      5   64.98%
3  Decision Tree      7   64.98%
4  Decision Tree     15   64.98%
5            GLM      3   64.65%
6            GLM      5   64.65%
7            GLM      7   64.65%
8  Random Forest      5   62.63%
9            GLM     15   60.27%
10 Random Forest      3   60.27%
11           KNN     15   59.93%
12 Random Forest      7   59.60%
13 Random Forest     15   59.60%
14           KNN      3   59.26%
15           KNN      7   56.23%
16           KNN      5   55.89%
17          nnet      3   51.18%
18          nnet      5   51.18%
19          nnet      7   51.18%
20          nnet     15   51.18%

WITHOUT 

#      paste0("hash_rate_lag_", i),
#      paste0("avg_block_size_lag_", i),
#      paste0("n_transactions_lag_", i),
#      paste0("utxo_count_lag_", i)


           model n_lags accuracy
1            GLM      3   65.32%
2  Decision Tree      3   64.98%
3  Decision Tree      5   64.98%
4  Decision Tree      7   64.98%
5  Decision Tree     15   64.98%
6            GLM      5   62.29%
7            GLM     15   61.95%
8            GLM      7   61.28%
9  Random Forest      5   60.27%
10 Random Forest      3   59.26%
11 Random Forest     15   59.26%
12 Random Forest      7   58.25%
13           KNN      3   56.90%
14           KNN      5   56.57%
15           KNN     15   55.22%
16          nnet      3   52.86%
17           KNN      7   52.53%
18          nnet      7   52.19%
19          nnet     15   52.19%
20          nnet      5   47.81%


## Test 4

trading_pair <- "BTC-USD"
start_date <- "2024-01-01"
end_date <- "2025-03-29"
candlestick_period <- 3600

    features <- c(
      features,
      paste0("body_size_lag_", i),
      paste0("upper_shadow_size_lag_", i),
      paste0("lower_shadow_size_lag_", i),
      paste0("direction_lag_", i),
      paste0("volume_lag_", i),
      paste0("value_lag_", i),
      paste0("close_price_lag_", i)
      #      paste0("hash_rate_lag_", i),
      #      paste0("avg_block_size_lag_", i),
      #      paste0("n_transactions_lag_", i),
      #      paste0("utxo_count_lag_", i)
    )
    
    
           model n_lags accuracy
1  Random Forest      5   65.57%
2  Random Forest      3   65.51%
3            KNN      3   64.58%
4  Decision Tree      3   64.11%
5  Decision Tree      5   64.11%
6  Decision Tree      7   64.11%
7  Decision Tree     15   64.11%
8            GLM      3   63.37%
9            GLM      5   63.15%
10           GLM      7   63.15%
11           GLM     15   62.93%


With all

           model n_lags accuracy
1  Random Forest      5   66.72%
2  Random Forest      3   64.58%
3  Decision Tree      3   64.11%
4  Decision Tree      5   64.11%
5  Decision Tree      7   64.11%
6  Decision Tree     15   64.11%
7            KNN      3   63.24%
8            GLM      3   63.21%
9            GLM      5   63.21%
10           GLM      7   63.12%
11           GLM     15   62.90%

with this

    features <- c(
      features,
#      paste0("body_size_lag_", i)
#      paste0("upper_shadow_size_lag_", i),
#      paste0("lower_shadow_size_lag_", i),
      paste0("direction_lag_", i),
#      paste0("volume_lag_", i),
      paste0("value_lag_", i)
#      paste0("close_price_lag_", i),
#      paste0("hash_rate_lag_", i),
#      paste0("avg_block_size_lag_", i),
#      paste0("n_transactions_lag_", i),
#      paste0("utxo_count_lag_", i)
    )
  }
  
           model n_lags accuracy
1  Random Forest      5   65.73%
2            KNN      3   65.04%
3  Random Forest      3   64.95%
4  Decision Tree      3   64.11%
5  Decision Tree      5   64.11%
6  Decision Tree      7   64.11%
7  Decision Tree     15   64.11%
8            GLM      7   63.12%
9            GLM      3   63.09%
10           GLM      5   63.09%
11           GLM     15   63.09%

### Test 5



Adding OHLC to the previous all

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
    )

           model n_lags accuracy
1  Random Forest      5   66.47%
2  Random Forest      3   65.29%
3  Decision Tree      3   64.11%
4  Decision Tree      5   64.11%
5  Decision Tree      7   64.11%
6  Decision Tree     15   64.11%
7            GLM      3   64.02%
8            GLM     15   64.02%
9            GLM      5   63.86%
10           GLM      7   63.71%
11           KNN      3   62.93%


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
      #            paste0("open_lag_", i),
      #            paste0("high_lag_", i),
      #            paste0("low_lag_", i),
      #            paste0("close_lag_", i),
      paste0("roc_lag_", i),
      paste0("macd_lag_", i),
      paste0("signal_lag_", i),
      paste0("rsi_lag_", i),
      paste0("up_bband_lag_", i),
      paste0("dn_bband_lag_", i)
    )
  }
  
  
1  Random Forest      5   66.52%
2  Random Forest      3   65.78%
3            GLM     15   65.37%
4  Decision Tree      3   65.22%
5  Decision Tree      5   65.22%
6  Decision Tree      7   65.22%
7  Decision Tree     15   65.22%
8            GBM      3   65.22%
9            GLM      3   65.12%
10           GLM      7   64.91%
11           SVM      3   64.69%
12           GLM      5   64.63%
13           KNN      3   62.36%

---

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

           model n_lags accuracy
1  Random Forest      5   67.49%
2  Random Forest      7   66.65%
3  Random Forest      3   66.52%
4  Random Forest     15   66.18%
5            GLM      7   65.96%
6            GLM      3   65.90%
7            GLM     15   65.90%
8            GBM      7   65.78%
9            GBM      3   65.65%
10           GLM      5   65.37%
11           GBM     15   65.28%
12 Decision Tree      3   65.22%
13 Decision Tree      5   65.22%
14 Decision Tree      7   65.22%
15 Decision Tree     15   65.22%
16           GBM      5   64.94%
17           KNN      3   62.36%
18           KNN     15   60.96%
19           KNN      7   60.90%
20           KNN      5   60.30%