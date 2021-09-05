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
autoplot.DynamicStrategies <- function(object, ...) {

  budget <- attributes(object)$budget
  bins   <- attributes(object)$n_simul

  .portfolio <- object |>
    dplyr::slice(5:6) |>
    tibble::deframe() |>
    tibble::as_tibble() |>
    dplyr::mutate(group = "Portfolio")
  .underlying <- object |>
    dplyr::slice(5:6) |>
    tibble::deframe() |>
    tibble::as_tibble() |>
    dplyr::mutate(Underlying_Index = sort(.data$Underlying_Index),
                  Portfolio_Value  = .data$Underlying_Index,
                  group            = "Underlying")


  p <- dplyr::bind_rows(.portfolio, .underlying) |>
    ggplot2::ggplot(ggplot2::aes(.data$Underlying_Index, .data$Portfolio_Value, color = .data$group)) +
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


