# Notes

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

