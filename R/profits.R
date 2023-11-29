#' Calculate Daily Profit or Loss of a Stock Portfolio
#'
#' This function calculates the total profit or loss for a given stock portfolio
#' based on the daily price changes. It requires a portfolio data frame with
#' stock names and quantities, and a stock data data frame with current and
#' previous day prices.
#'
#' @param portfolio A data frame with columns 'Stock' (character) and 'Quantity' (numeric).
#' @param stock_data A data frame with stock information including
#'                   'Name' (character), 'Last_Price' (numeric),
#'                   and 'Last_Price_Yesterday' (numeric).
#'
#' @return A numeric value representing the total profit or loss for the portfolio.
#' @export
#'
#' @examples
#' user_portfolio <- data.frame(
#'   Stock = c("StockA", "StockB"),
#'   Quantity = c(100, 50)
#' )
#' stock_data <- data.frame(
#'   Name = c("StockA", "StockB"),
#'   Last_Price = c(105, 210),
#'   Last_Price_Yesterday = c(100, 205)
#' )
#' profits(user_portfolio, stock_data)


# Calculate Profits or Losses of a Stock Portfolio
profits <- function(portfolio, stock_data) {
  # Ensure that the portfolio and stock_data are data frames
  if (!is.data.frame(portfolio) || !is.data.frame(stock_data)) {
    stop("Both portfolio and stock_data must be data frames.")
  }

  # Check for necessary columns in portfolio and stock_data
  required_portfolio_cols <- c("Stock", "Quantity")
  required_stock_data_cols <- c("Name", "Last_Price", "Last_Price_Yesterday")

  if (!all(required_portfolio_cols %in% names(portfolio))) {
    stop("Portfolio must contain columns: ", paste(required_portfolio_cols, collapse = ", "))
  }

  if (!all(required_stock_data_cols %in% names(stock_data))) {
    stop("Stock data must contain columns: ", paste(required_stock_data_cols, collapse = ", "))
  }

  # Merge portfolio with stock data
  merged_data <- merge(portfolio, stock_data, by.x = "Stock", by.y = "Name")

  # Calculate profit or loss for each stock
  merged_data$ProfitLoss <- (merged_data$Last_Price - merged_data$Last_Price_Yesterday) * merged_data$Quantity

  # Calculate total profit or loss
  total_profit_loss <- sum(merged_data$ProfitLoss, na.rm = TRUE)

  return(total_profit_loss)
}
