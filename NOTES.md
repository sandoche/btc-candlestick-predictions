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
