#' @keywords internal
delta <- function(time_to_maturity, stock_value, stock_volatility, strike, risk_free_rate) {

  time_to_maturity <- max(c(time_to_maturity, 10^(-9))) # avoid division by zero message

  d1 <- 1 / (stock_volatility * sqrt(time_to_maturity)) *
    (log(stock_value / strike) + (risk_free_rate * stock_volatility ^ 2 / 2) * time_to_maturity)

  D <- stats::pnorm(d1, 0, 1)
  D

}

#' @keywords internal
solve_for_strike <- function(strike, time_to_maturity, stock_value, stock_volatility,
                         risk_free_rate, initial_investment, maximum_loss) {

  d1 <- 1 / (stock_volatility * sqrt( time_to_maturity)) *
    (log(stock_value / strike) + (risk_free_rate * stock_volatility ^ 2 / 2) *  time_to_maturity)
  d2 <- d1 - stock_volatility * sqrt( time_to_maturity)

  PV <- exp(-risk_free_rate *  time_to_maturity)

  Call_Price <- stock_value * stats::pnorm(d1, 0, 1) - PV * strike * stats::pnorm(d2, 0, 1)

  Guaranteed <- initial_investment - maximum_loss
  Cash_Available <- initial_investment - PV * Guaranteed

  K <- Cash_Available - Call_Price
  K

}
