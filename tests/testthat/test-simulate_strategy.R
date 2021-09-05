utility  <- simulate_strategy(strategy = "max_utility", n_simul = 100)
cppi     <- simulate_strategy(strategy = "cppi",        n_simul = 100)
buy_hold <- simulate_strategy(strategy = "buy_hold",    n_simul = 100)
obpi     <- simulate_strategy(strategy = "obpi",        n_simul = 100)

test_that("strategy `max_utility` works", {
  # type
  expect_type(utility, "list")
  expect_s3_class(utility, "DynamicStrategies")
  # size
  expect_length(utility, 2L)
  expect_equal(nrow(utility), 6L)
  expect_equal(ncol(utility), 2L)
  # names
  expect_named(utility, c("name", "value"))
  # col classes
  expect_equal(purrr::flatten_chr(purrr::map(utility, class)), c("character", "list"))
})

test_that("strategy `cppi` works", {
  # type
  expect_type(cppi, "list")
  expect_s3_class(cppi, "DynamicStrategies")
  # size
  expect_length(cppi, 2L)
  expect_equal(nrow(cppi), 6L)
  expect_equal(ncol(cppi), 2L)
  # names
  expect_named(cppi, c("name", "value"))
  # col classes
  expect_equal(purrr::flatten_chr(purrr::map(cppi, class)), c("character", "list"))
})

test_that("strategy `buy_hold` works", {
  # type
  expect_type(buy_hold, "list")
  expect_s3_class(buy_hold, "DynamicStrategies")
  # size
  expect_length(buy_hold, 2L)
  expect_equal(nrow(buy_hold), 6L)
  expect_equal(ncol(buy_hold), 2L)
  # names
  expect_named(buy_hold, c("name", "value"))
  # col classes
  expect_equal(purrr::flatten_chr(purrr::map(buy_hold, class)), c("character", "list"))
})

test_that("strategy `obpi` works", {
  # type
  expect_type(obpi, "list")
  expect_s3_class(obpi, "DynamicStrategies")
  # size
  expect_length(obpi, 2L)
  expect_equal(nrow(obpi), 6L)
  expect_equal(ncol(obpi), 2L)
  # names
  expect_named(obpi, c("name", "value"))
  # col classes
  expect_equal(purrr::flatten_chr(purrr::map(obpi, class)), c("character", "list"))
})


