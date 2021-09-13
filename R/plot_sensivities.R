#' Stress-Test a Dynamic Strategy
#'
#' This function loops over one of the arguments in \code{\link{simulate_strategy}}
#' to make explicit the impact of a variable in the desired allocation policy.
#'
#' @param strategy An object of the \code{DynamicStrategies} class.
#' @param variable A \code{character} with the parameter to loop-over.
#' @param from An \code{integer} with the initial parameter value.
#' @param to An \code{integer} with the final parameter value.
#' @param size A \code{integer} with the total number of iterations.
#' @param ... Currently not used.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @examples
#' # stress-test the `max_utility` strategy by changing the `allocation` parameter
#' x <- simulate_strategy()
#' plot_sensivities(strategy = x, variable = "allocation", from = 0.0, to = 1, size = 20)
#'
#' # stress-test the `cppi` strategy by changing the `floor` parameter
#' y <- simulate_strategy(strategy = "cppi")
#' plot_sensivities(strategy = y, variable = "floor", from = 7000, to = 9000, size = 20)
#'
#' # stress-test on the `obpi` strategy by changing the risk-free rate
#' y <- simulate_strategy(strategy = "obpi")
#' plot_sensivities(strategy = y, variable = "rf", from = 0, to = 0.2, size = 20)
plot_sensivities <- function(strategy, variable, from, to, size, ...) {

  assertthat::assert_that(inherits(strategy, "DynamicStrategies"))
  assertthat::assert_that(assertthat::is.number(from))
  assertthat::assert_that(assertthat::is.number(to))
  assertthat::assert_that(assertthat::is.number(size))
  variable <- match.arg(variable, c("horizon", "mu", "sigma", "rf", "allocation", "floor", "multiple", "aggressiveness"))[[1L]]
  strat    <- attributes(strategy)$strategy
  lop_over <- seq(from = from, to = to, length.out = size)

  # attributes (function call)
  budget         <- attributes(strategy)$budget
  horizon        <- attributes(strategy)$horizon
  step           <- attributes(strategy)$step
  mu             <- attributes(strategy)$mu
  sigma          <- attributes(strategy)$sigma
  rf             <- attributes(strategy)$rf
  n_simul        <- attributes(strategy)$n_simul
  allocation     <- attributes(strategy)$allocation
  floor          <- attributes(strategy)$floor
  multiple       <- attributes(strategy)$multiple
  aggressiveness <- attributes(strategy)$aggressiveness

  # check if `variable` is consistent with `strategy`
  if (strat == "max_utility" & variable %in% c("floor", "multiple", "aggressiveness")) {
    stop("`", variable, "` is not an input of the `max_utility` strategy. Reajust your call.", call. = FALSE)
  }
  if (strat == "buy_hold" & variable %in% c("floor", "multiple", "aggressiveness")) {
    stop("`", variable, "` is not an input of the `buy_hold` strategy. Reajust your call.", call. = FALSE)
  }
  if (strat == "cppi" & variable %in% c("allocation", "aggressiveness")) {
    stop("`", variable, "` is not an input of the `cppi` strategy. Reajust your call.", call. = FALSE)
  }
  if (strat == "obpi" & variable %in% c("allocation", "floor", "multiple")) {
    stop("`", variable, "` is not an input of the `obpi` strategy. Reajust your call.", call. = FALSE)
  }

  # check if range is consistent with `variable`
  if (variable == "sigma" & any(lop_over < 0)) {
    stop("`sigma` cann't be negative. Readjust your call.",call. = FALSE)
  }
  if (variable == "allocation" & any(lop_over < 0)) {
    stop("`allocation` cann't be negative. Readjust your call.",call. = FALSE)
  }
  if (variable == "floor" & any(lop_over < 0)) {
    stop("`floor` cann't be negative. Readjust your call.",call. = FALSE)
  }
  if (variable == "floor" & to > budget) {
    stop("`floor` cann't be greater then `budget`. Readjust your call.",call. = FALSE)
  }
  if (variable == "multiple" & any(lop_over < 0)) {
    stop("`multiple` cann't be negative. Readjust your call.",call. = FALSE)
  }
  if (variable == "aggressiveness" & (any(lop_over < 0) | any(lop_over > 1))) {
    stop("`aggressiveness` must be between 0 and 1. Readjust your call.",call. = FALSE)
  }

  if (variable == "horizon") {
    list_ <- purrr::map(.x = lop_over,
                        .f = ~ simulate_strategy(budget = budget, horizon = .x, step = step, mu = mu, sigma = sigma,
                                                 rf = rf, n_simul = n_simul, strategy = strat, allocation = allocation,
                                                 floor = floor, multiple = multiple, aggressiveness = aggressiveness))
  } else if (variable == "mu") {
    list_ <- purrr::map(.x = lop_over,
                        .f = ~ simulate_strategy(budget = budget, horizon = budget, step = step, mu = .x, sigma = sigma,
                                                 rf = rf, n_simul = n_simul, strategy = strat, allocation = allocation,
                                                 floor = floor, multiple = multiple, aggressiveness = aggressiveness))
  } else if (variable == "sigma") {
    list_ <- purrr::map(.x = lop_over,
                        .f = ~ simulate_strategy(budget = budget, horizon = horizon, step = step, mu = mu, sigma = .x,
                                                 rf = rf, n_simul = n_simul, strategy = strat, allocation = allocation,
                                                 floor = floor, multiple = multiple, aggressiveness = aggressiveness))
  } else if (variable == "rf") {
    list_ <- purrr::map(.x = lop_over,
                        .f = ~ simulate_strategy(budget = budget, horizon = horizon, step = step, mu = mu, sigma = sigma,
                                                 rf = .x, n_simul = n_simul, strategy = strat, allocation = allocation,
                                                 floor = floor, multiple = multiple, aggressiveness = aggressiveness))
  } else if (variable == "allocation") {
    list_ <- purrr::map(.x = lop_over,
                        .f = ~ simulate_strategy(budget = budget, horizon = horizon, step = step, mu = mu, sigma = sigma,
                                                 rf = rf, n_simul = n_simul, strategy = strat, allocation = .x,
                                                 floor = floor, multiple = multiple, aggressiveness = aggressiveness))
  } else if (variable == "floor") {
    list_ <- purrr::map(.x = lop_over,
                        .f = ~ simulate_strategy(budget = budget, horizon = horizon, step = step, mu = mu, sigma = sigma,
                                                 rf = rf, n_simul = n_simul, strategy = strat, allocation = allocation,
                                                 floor = .x, multiple = multiple, aggressiveness = aggressiveness))
  } else if (variable == "multiple") {
    list_ <- purrr::map(.x = lop_over,
                        .f = ~ simulate_strategy(budget = budget, horizon = horizon, step = step, mu = mu, sigma = sigma,
                                                 rf = rf, n_simul = n_simul, strategy = strat, allocation = allocation,
                                                 floor = floor, multiple = .x, aggressiveness = aggressiveness))
  } else {
    list_ <- purrr::map(.x = lop_over,
                        .f = ~ simulate_strategy(budget = budget, horizon = budget, step = step, mu = mu, sigma = sigma,
                                                 rf = rf, n_simul = n_simul, strategy = strat, allocation = allocation,
                                                 floor = floor, multiple = multiple, aggressiveness = .x))
  }

  list_ <- tibble::enframe(x = list_, name = ".id", value = ".strategy") |>
    dplyr::mutate(.stats = purrr::map(.x = .data$.strategy, .f = extract_stats))

  # plot
  list_ |>
    dplyr::mutate(param = lop_over) |>
    tidyr::unnest(cols = .data$.stats) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$param, y = .data$value)) +
    ggplot2::geom_point(alpha = 0.1) +
    ggplot2::geom_smooth(formula = y ~ x, method = "loess") +
    ggplot2::facet_wrap(~.data$stat, scales = "free_y") +
    ggplot2::labs(x = paste("Stress-Test on:", stringr::str_to_sentence(variable)),
                  y = NULL)


}

