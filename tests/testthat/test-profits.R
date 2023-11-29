library(testthat)
library(FinancialTool)

test_that("profits() calculates the correct profit or loss", {
  # Sample Portfolio
  user_portfolio <- data.frame(
    Stock = c("StockA", "StockB"),
    Quantity = c(100, 50)
  )

  # Correcting column names in Sample Stock Data
  stock_data <- data.frame(
    Name = c("StockA", "StockB"),
    Last_Price = c(105, 210),  # Current prices
    Last_Price_Yesterday = c(100, 205)  # Prices from the previous day
  )

  # Expected Result
  # For StockA: (105 - 100) * 100 = 500
  # For StockB: (210 - 205) * 50  = 250
  # Total Expected Profit = 500 + 250 = 750
  expected_profit_loss <- 750

  # Test
  expect_equal(profits(user_portfolio, stock_data), expected_profit_loss)
})
