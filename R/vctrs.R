#' @importFrom vctrs obj_print_data
#' @export
obj_print_data.DynamicStrategies <- function(x, ...) {
  cat("time             :", utils::head(x$time, 3), "...", utils::tail(x$time, 3))
  cat("\n")
  cat("portfolio_series :", utils::head(x$portfolio_series, 3), "...", utils::tail(x$portfolio_series, 3))
  cat("\n")
  cat("market_series    :", utils::head(x$market_series, 3), "...", utils::tail(x$market_series, 3))
  cat("\n")
  cat("percentage_series:", utils::head(x$percentage_series, 3), "...", utils::tail(x$percentage_series, 3))
  cat("\n")
  cat("underlying_index :", utils::head(x$underlying_index, 3), "...", utils::tail(x$underlying_index, 3))
  cat("\n")
  cat("portfolio_value  :", utils::head(x$portfolio_value, 3), "...", utils::tail(x$portfolio_value, 3))
  cat("\n")
}

#' @importFrom vctrs obj_print_header
#' @export
obj_print_header.DynamicStrategies <- function(x, ...) {
  cat(crayon::cyan("<< Dynamic Strategy >>"))
  cat("\n")
}
