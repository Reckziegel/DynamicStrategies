sfs <- solve_for_strike(strike = 100, time_to_maturity = 5 / 252,
                        stock_value = 100, stock_volatility = 0.4,
                        risk_free_rate = 0.05, initial_investment = 10000,
                        maximum_loss = 1000)
test_that("solve_for_strike works", {
  expect_type(sfs, "double")
  expect_length(sfs, 1L)
})
