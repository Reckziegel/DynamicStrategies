.delta <- delta(time_to_maturity = 5 / 252, stock_value = 100, stock_volatility = 0.4,
                strike = 100, risk_free_rate = 0.05)
test_that("delta works", {
  expect_type(.delta, "double")
  expect_length(.delta, 1L)
})
