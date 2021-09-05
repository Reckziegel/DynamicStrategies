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
#' @return A tidy \code{tibble} with 6 rows and 2 columns.
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
  Underlying_Index <- budget  # value of the underlying at starting time, normalized to equal investment
  Start            <- Underlying_Index
  Elapsed_Time     <- 0
  Portfolio_Value  <- budget

  if (strategy == "cppi") {

    Cushion                  <- max(0, Portfolio_Value - floor)
    Underlyings_in_Portfolio <- min(Portfolio_Value, max(0, multiple * Cushion))
    Cash_in_Portfolio        <- Portfolio_Value - Underlyings_in_Portfolio
    Underlying_in_Portfolio_Percent <- Underlyings_in_Portfolio / Portfolio_Value

  } else if (strategy == "obpi") {

    Strike <- stats::uniroot(
      f        = solve_for_strike,
      interval = c(0, Underlying_Index * 10),
      horizon, Underlying_Index, sigma, rf, budget, aggressiveness * budget / 3
      )$root
    Underlying_in_Portfolio_Percent <- delta(horizon - Elapsed_Time, Underlying_Index, sigma, Strike, rf)

  } else {

    Underlying_in_Portfolio_Percent <- allocation

  }

  Underlyings_in_Portfolio <- Portfolio_Value * Underlying_in_Portfolio_Percent
  Cash_in_Portfolio <- Portfolio_Value - Underlyings_in_Portfolio

  # initialize parameters for the plot (no theory in this)
  Portfolio_Series  <- Portfolio_Value
  Market_Series     <- Underlying_Index
  Percentage_Series <- Underlying_in_Portfolio_Percent

  # asset evolution and portfolio rebalancing
  while (Elapsed_Time < horizon - 1e-5) {  # add this term to avoid errors
    # time elapses...
    Elapsed_Time <- Elapsed_Time + step

    # ...asset prices evolve and portfolio takes on new value...
    Multiplicator <- exp((mu - sigma ^ 2 / 2) * step + sigma * sqrt(step) * stats::rnorm(n_simul))
    Underlying_Index         <- Underlying_Index  * Multiplicator
    Underlyings_in_Portfolio <- Underlyings_in_Portfolio * Multiplicator
    Cash_in_Portfolio        <- Cash_in_Portfolio * exp(rf * step)
    Portfolio_Value          <- Underlyings_in_Portfolio + Cash_in_Portfolio

    if (strategy == "max_utility") {

      Underlyings_in_Portfolio <- Portfolio_Value * Underlying_in_Portfolio_Percent
      Cash_in_Portfolio        <- Portfolio_Value - Underlyings_in_Portfolio

    } else if (strategy == "buy_hold") {

      Underlying_in_Portfolio_Percent <- Underlyings_in_Portfolio / Portfolio_Value

    } else if (strategy == "cppi") {

      floor                           <- floor * exp(rf * step)
      Cushion                         <- pmax(0, (Portfolio_Value - floor))
      Underlyings_in_Portfolio        <- pmin(Portfolio_Value, pmax(0, multiple * Cushion))
      Cash_in_Portfolio               <- Portfolio_Value - Underlyings_in_Portfolio
      Underlying_in_Portfolio_Percent <- Underlyings_in_Portfolio / Portfolio_Value

    } else if (strategy == "obpi") {

      Underlying_in_Portfolio_Percent <- delta(horizon - Elapsed_Time, Underlying_Index, sigma, Strike, rf)
      Underlyings_in_Portfolio <- Portfolio_Value * Underlying_in_Portfolio_Percent
      Cash_in_Portfolio        <- Portfolio_Value - Underlyings_in_Portfolio

    }

    # store one path for the movie (no theory in this)
    Portfolio_Series  <- c(Portfolio_Series, Portfolio_Value[1])
    Market_Series     <- c(Market_Series, Underlying_Index[1])
    Percentage_Series <- c(Percentage_Series, Underlying_in_Portfolio_Percent[1])

  }

  tibble::new_tibble(
    tibble::enframe(
      list(Time              = seq(0, horizon, step),
           Portfolio_Series  = Portfolio_Series,
           Market_Series     = Market_Series,
           Percentage_Series = Percentage_Series,
           Underlying_Index  = Underlying_Index,
           Portfolio_Value   = Portfolio_Value
      )
    ),
    nrow           = 6L,
    class          = "DynamicStrategies",
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
