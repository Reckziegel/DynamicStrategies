x <- simulate_strategy()
stats <- extract_stats(x)
test_that("extract_stats works", {
  # type
  expect_type(stats, "list")
  expect_s3_class(stats, "tbl")
  # size
  expect_length(stats, 2L)
  expect_equal(nrow(stats), 6L)
  expect_equal(ncol(stats), 2L)
  # names
  expect_named(stats, c("stat", "value"))
  # col classes
  expect_equal(purrr::flatten_chr(purrr::map(stats, class)), c("factor", "numeric"))
})
