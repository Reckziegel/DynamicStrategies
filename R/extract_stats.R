#' Extract Portfolio Statistics from a Dynamic Simulation
#'
#' Computes the mean, standard deviation, skewness, kurtosis, Value-at-Risk (VaR)
#' and Conditional Value-at-Risk CVaR) of a strategy.
#'
#' @param simulation An object of the \code{DynamicStrategies} class.
#' @param level A number with the desired probability level. The default is
#' \code{level = 0.01}.
#'
#' @return A tibble with 3 columns.
#' @export
#'
#' @examples
#' utility <- simulate_strategy(strategy = "max_utility")
#' cppi    <- simulate_strategy(strategy = "cppi")
#' extract_stats(utility)
#' extract_stats(cppi)
extract_stats <- function(simulation, level = 0.01) {
  assertthat::assert_that(inherits(simulation, "DynamicStrategies"))
  assertthat::assert_that(assertthat::is.number(level))

  budget <- attributes(simulation)$budget
  portfolio <- as.matrix(simulation[6, 2][[1]][[1]])
  size <- nrow(portfolio)
  p    <- ffp::as_ffp(rep(1 / size, size))

  out <- ffp::empirical_stats(x = portfolio, p = p, level = level)
  out <- out[ , c("stat", "value")]
  out[5, 2] <- budget + out[5, 2]
  out[6, 2] <- budget + out[6, 2]
  stat_nms <- c("Mu", "Volatility", "Skewness", "Kurtosis", "VaR", "CVaR")
  out |>
    dplyr::mutate(stat = forcats::fct_relevel(.f = stat_nms, stat_nms))

}
