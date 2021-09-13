#' Convex vs Concave Portfolio Simulation
#'
#' This function simulates the payoff of different kinds of dynamic strategies.
#' Depending on the option chosen, the output may be concave or convex.
#'
#' @param budget A \code{double}. The initial investment.
#' @param horizon A \code{double}. The investment horizon, in years.
#' @param step A \code{double}. The time frequency, in years.
#' @param mu A \code{double}. The mean of the process, in years.
#' @param sigma A \code{double}. The volatility of the process, in years.
#' @param rf A \code{double}. The risk-free rate, in years.
#' @param n_simul A \code{integer}. The number of simulations to be conducted.
#' @param strategy A \code{character}. One of: "max_utility", "buy_hold", "cppi", "obpi".
#' @param allocation A \code{double} between \code{0} and \code{1}. Only used for
#' "max_utility" and "buy_hold".
#' @param floor A \code{double}. The amount the investor doesn't want to loose. Only
#' used if \code{strategy = "cppi"}.
#' @param multiple A \code{integer} showing how aggressive the investor should be.
#' @param aggressiveness A \code{double} between \code{0} and \code{1} showing how
#' aggressive the investor should be. Higher numbers are connected with higher convexity.
#'
#' @return An S3 \code{list} of the \code{DynamicStrategies} class.
#'
#' @export
#'
#' @examples
#' simulate_strategy()
simulate_strategy <- function(budget     = 10000,
                              horizon    = 6 / 12,
                              step       = 1 / 252,
                              mu         = 0.2,
                              sigma      = 0.4,
                              rf         = 0.04,
                              n_simul    = 10000,
                              strategy   = c("max_utility", "buy_hold", "cppi", "obpi"),
                              allocation = 0.5,
                              floor      = budget * 0.9,
                              multiple   = 10,
                              aggressiveness = 0.5) {

  assertthat::assert_that(assertthat::is.number(budget))
  assertthat::assert_that(assertthat::is.number(horizon))
  assertthat::assert_that(assertthat::is.number(step))
  assertthat::assert_that(assertthat::is.number(mu))
  assertthat::assert_that(assertthat::is.number(sigma))
  assertthat::assert_that(assertthat::is.number(rf))
  assertthat::assert_that(assertthat::is.number(n_simul))
  assertthat::assert_that(assertthat::is.number(allocation))
  assertthat::assert_that(assertthat::is.number(floor))
  assertthat::assert_that(assertthat::is.number(multiple))
  assertthat::assert_that(assertthat::is.number(aggressiveness))
  strategy <- match.arg(strategy, c("max_utility", "buy_hold", "cppi", "obpi"))[[1]]

  # initialize values
  underlying_index <- budget  # value of the underlying at starting time, normalized to equal investment
  start            <- underlying_index
  elapsed_time     <- 0
  portfolio_value  <- budget

  if (strategy == "cppi") {

    cushion                  <- max(0, portfolio_value - floor)
    underlyings_in_portfolio <- min(portfolio_value, max(0, multiple * cushion))
    cash_in_portfolio        <- portfolio_value - underlyings_in_portfolio
    underlying_in_portfolio_percent <- underlyings_in_portfolio / portfolio_value

  } else if (strategy == "obpi") {

    strike <- stats::uniroot(
      f        = solve_for_strike,
      interval = c(0, underlying_index * 10),
      horizon, underlying_index, sigma, rf, budget, aggressiveness * budget / 3
      )$root
    underlying_in_portfolio_percent <- delta(horizon - elapsed_time, underlying_index, sigma, strike, rf)

  } else {

    underlying_in_portfolio_percent <- allocation

  }

  underlyings_in_portfolio <- portfolio_value * underlying_in_portfolio_percent
  cash_in_portfolio <- portfolio_value - underlyings_in_portfolio

  # initialize parameters for the plot (no theory in this)
  portfolio_series  <- portfolio_value
  market_series     <- underlying_index
  percentage_series <- underlying_in_portfolio_percent

  # asset evolution and portfolio rebalancing
  while (elapsed_time < horizon - 1e-5) {  # add this term to avoid errors
    # time elapses...
    elapsed_time <- elapsed_time + step

    # ...asset prices evolve and portfolio takes on new value...
    Multiplicator <- exp((mu - sigma ^ 2 / 2) * step + sigma * sqrt(step) * stats::rnorm(n_simul))
    underlying_index         <- underlying_index  * Multiplicator
    underlyings_in_portfolio <- underlyings_in_portfolio * Multiplicator
    cash_in_portfolio        <- cash_in_portfolio * exp(rf * step)
    portfolio_value          <- underlyings_in_portfolio + cash_in_portfolio

    if (strategy == "max_utility") {

      underlyings_in_portfolio <- portfolio_value * underlying_in_portfolio_percent
      cash_in_portfolio        <- portfolio_value - underlyings_in_portfolio

    } else if (strategy == "buy_hold") {

      underlying_in_portfolio_percent <- underlyings_in_portfolio / portfolio_value

    } else if (strategy == "cppi") {

      floor                           <- floor * exp(rf * step)
      cushion                         <- pmax(0, (portfolio_value - floor))
      underlyings_in_portfolio        <- pmin(portfolio_value, pmax(0, multiple * cushion))
      cash_in_portfolio               <- portfolio_value - underlyings_in_portfolio
      underlying_in_portfolio_percent <- underlyings_in_portfolio / portfolio_value

    } else if (strategy == "obpi") {

      underlying_in_portfolio_percent <- delta(horizon - elapsed_time, underlying_index, sigma, strike, rf)
      underlyings_in_portfolio <- portfolio_value * underlying_in_portfolio_percent
      cash_in_portfolio        <- portfolio_value - underlyings_in_portfolio

    }

    # store one path for the movie (no theory in this)
    portfolio_series  <- c(portfolio_series, portfolio_value[1])
    market_series     <- c(market_series, underlying_index[1])
    percentage_series <- c(percentage_series, underlying_in_portfolio_percent[1])

  }

  vctrs::new_list_of(
    list(time              = seq(0, horizon, step),
         portfolio_series  = portfolio_series,
         market_series     = market_series,
         percentage_series = percentage_series,
         underlying_index  = underlying_index,
         portfolio_value   = portfolio_value
    ),
    ptype = double(),
    class = "DynamicStrategies",

    # arguments in the call as attributes
    budget         = budget,
    horizon        = horizon,
    step           = step,
    mu             = mu,
    sigma          = sigma,
    rf             = rf,
    n_simul        = n_simul,
    strategy       = strategy,
    allocation     = allocation,
    floor          = floor,
    multiple       = multiple,
    aggressiveness = aggressiveness
  )

}
