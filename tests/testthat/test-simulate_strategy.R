utility  <- simulate_strategy(strategy = "max_utility", n_simul = 100)
cppi     <- simulate_strategy(strategy = "cppi",        n_simul = 100)
buy_hold <- simulate_strategy(strategy = "buy_hold",    n_simul = 100)
obpi     <- simulate_strategy(strategy = "obpi",        n_simul = 100)

test_that("strategy `max_utility` works", {
  # type
  expect_type(utility, "list")
  expect_s3_class(utility, "DynamicStrategies")
  # size
  expect_length(utility, 6L)
  # names
  expect_named(utility, c("time", "portfolio_series", "market_series", "percentage_series",
                          "underlying_index", "portfolio_value"))
  # col classes
  expect_equal(purrr::flatten_chr(purrr::map(utility, class)), rep("numeric", 6L))
})

test_that("strategy `cppi` works", {
  # type
  expect_type(cppi, "list")
  expect_s3_class(cppi, "DynamicStrategies")
  # size
  expect_length(cppi, 6L)
  # names
  expect_named(cppi, c("time", "portfolio_series", "market_series", "percentage_series",
                       "underlying_index", "portfolio_value"))
  # col classes
  expect_equal(purrr::flatten_chr(purrr::map(cppi, class)), rep("numeric", 6L))
})

test_that("strategy `buy_hold` works", {
  # type
  expect_type(buy_hold, "list")
  expect_s3_class(buy_hold, "DynamicStrategies")
  # size
  expect_length(buy_hold, 6L)
  # names
  expect_named(buy_hold, c("time", "portfolio_series", "market_series", "percentage_series",
                           "underlying_index", "portfolio_value"))
  # col classes
  expect_equal(purrr::flatten_chr(purrr::map(buy_hold, class)), rep("numeric", 6L))
})

test_that("strategy `obpi` works", {
  # type
  expect_type(obpi, "list")
  expect_s3_class(obpi, "DynamicStrategies")
  # size
  expect_length(obpi, 6L)
  # names
  expect_named(obpi, c("time", "portfolio_series", "market_series", "percentage_series",
                       "underlying_index", "portfolio_value"))
  # col classes
  expect_equal(purrr::flatten_chr(purrr::map(obpi, class)), rep("numeric", 6L))
})


