#' Easy Inspection of a Dynamic Strategy Simulation
#'
#' Extends the \code{autoplot} method for the \code{DynamicStrategies} class.
#'
#' @param object An object of the \code{DynamicStrategies} class.
#' @param ... Additional arguments to be passed to \code{autoplot}.
#'
#' @return A \code{ggplot2} object.
#' @export
#'
#' @importFrom ggplot2 autoplot
#' @importFrom rlang .data
#'
#' @examples
#' library(ggplot2)
#'
#' # Maximum Utility
#' simulate_strategy(strategy = "max_utility") |> autoplot()
#'
#' # cppi
#' simulate_strategy(strategy = "cppi", multiple = 3) |> autoplot()
autoplot.DynamicStrategies <- function(object, ...) {

  budget <- attributes(object)$budget
  bins   <- attributes(object)$n_simul

  .portfolio <- tibble::tibble(underlying_index = object[["underlying_index"]],
                               portfolio_value  = object[["portfolio_value"]]) |>
    dplyr::mutate(group = "Portfolio")

  .underlying <- tibble::tibble(underlying_index = object[["underlying_index"]],
                                portfolio_value  = object[["underlying_index"]]) |>
    dplyr::mutate(underlying_index = sort(.data$underlying_index),
                  portfolio_value  = .data$underlying_index,
                  group            = "Underlying")


  p <- dplyr::bind_rows(.portfolio, .underlying) |>
    ggplot2::ggplot(ggplot2::aes(.data$underlying_index, .data$portfolio_value, color = .data$group)) +
    ggplot2::geom_vline(xintercept = budget, linetype = 2, color = "white") +
    ggplot2::geom_hline(yintercept = budget, linetype = 2, color = "white") +
    ggplot2::geom_point(size = 0.1) +
    #scale_color_manual(values = c("#545454", "#576890")) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
    ggplot2::scale_x_continuous(labels = scales::dollar_format()) +
    ggplot2::theme(legend.position = "bottom", panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(color = NULL, x = "Underlying", y = "Investment")

  ggExtra::ggMarginal(
    p,
    type        = 'histogram',
    margins     = 'both',
    bins        = 20 * log(bins),
    size        = 3,
    binwidth    = 40,
    groupFill   = TRUE,
    groupColour = TRUE,
    position    = "dodge",
    yparams = list(size = 2),
    xparams = list(size = 2)
  )

}


