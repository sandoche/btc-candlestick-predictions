# Bitcoin candlestick predictions using lagged features and machine learning algorithms in R

> Training machine learning algorithms to predict the next candlestick's direction of the bitcoin chart using a set of lagged features

## ⚙️ Dataset

Data are already downloaded and stored in the `data` folder for the following paramters:
```R
trading_pair <- "BTC-USD"
start_date <- "2024-01-01"
end_date <- "2025-03-29"
candlestick_period <- 3600
```

If you want to download the data for different parameters, you can run the `data-downloader.R` script.

## 📊 Report

- Report in RMarkdown: [REPORT.Rmd](REPORT.Rmd)
- Report in PDF: [REPORT.md](REPORT.pdf)

## 🖥 Analysis and weights

- Analysis script in R: [analysis.R](analysis.R)
- Weights for the models:
  - Pull them from hugging face into the `models` folder
  - Weights for the models: [Weights on hugging face](https://huggingface.co/Sandoche/btc-candlestick-predictions)
  - Run the following command to pull the weights:
    ```bash
    cd models
    huggingface-cli download Sandoche/btc-candlestick-predictions --local-dir .
    ```

## 📄 License

[MIT](LICENSE)

## 📖 Citation
```
@misc{bitcoin-candlestick-predictions,
  author = {Sandoche Adittane},
  title = {Bitcoin candlestick predictions using lagged features and machine learning algorithms in R},
  year = {2025},
  url = {\url{https://github.com/sandoche/btc-candlestick-predictions}},
}
```