
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FinancialTool

<!-- badges: start -->
<!-- badges: end -->

The goal of **FinancialTool** is to simplify the analysis of financial
data, specifically stock market data. It provides functions for
calculating profits, analyzing stock performance, and more.

## Installation

You can install the development version of FinancialTool from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("OrlandoWillig/FinancialTool")
```

## Usage

**FinancialTool** is designed to be intuitive for those familiar with
financial analysis in R. Here are a few basic examples:

### Calculating Portfolio Profits

Suppose you have a portfolio and stock data as follows:

``` r
portfolio <- data.frame(
  Stock = c("StockA", "StockB"),
  Quantity = c(100, 50)
)
stock_data <- data.frame(
  Name = c("StockA", "StockB"),
  Last_Price = c(105, 210),
  Last_Price_Yesterday = c(100, 205)
)
```

You can calculate the profit (or loss) with:

``` r
library(FinancialTool)
profits(portfolio, stock_data)
#> [1] 750
```

## Contributions

I welcome contributions, including bug reports, suggestions, and
enhancements. Please open an issue or submit a pull request on our
GitHub repository.
