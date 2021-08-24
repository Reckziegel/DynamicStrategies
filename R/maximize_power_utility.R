# maximize_power_utility <- function(budget     = 10000,
#                                    horizon    = 6 / 12,
#                                    step       = 1 / 252,
#                                    mu         = 0.2,
#                                    sigma      = 0.4,
#                                    rf         = 0.04,
#                                    n_simul    = 10000,
#                                    allocation = 0.50) {
#
#   ### Initialize values
#   Underlying_Index <- budget
#   Start            <- Underlying_Index
#   Elapsed_Time     <- 0
#   Portfolio_Value  <- budget
#
#   Underlying_in_Portfolio_Percent <- allocation
#
#   Underlyings_in_Portfolio <- Portfolio_Value * Underlying_in_Portfolio_Percent
#   Cash_in_Portfolio <- Portfolio_Value - Underlyings_in_Portfolio
#
#   ### Initialize parameters for the plot (no theory in this)
#   Portfolio_Series  <- Portfolio_Value
#   Market_Series     <- Underlying_Index
#   Percentage_Series <- Underlying_in_Portfolio_Percent
#
#   # asset evolution and portfolio rebalancing
#   while (Elapsed_Time < (horizon - 10e-5)) { # add this term to avoid errors
#     # time elapses...
#     Elapsed_Time <- Elapsed_Time + step
#
#     # ...asset prices evolve and portfolio takes on new value...
#     Multiplicator            <- exp((mu - sigma ^ 2 / 2) * step + sigma * sqrt(step) * stats::rnorm(n_simul))
#     Underlying_Index         <- Underlying_Index * Multiplicator
#     Underlyings_in_Portfolio <- Underlyings_in_Portfolio * Multiplicator
#     Cash_in_Portfolio        <- Cash_in_Portfolio * exp(rf * step)
#     Portfolio_Value          <- Underlyings_in_Portfolio + Cash_in_Portfolio
#
#     # ...and we rebalance our portfolio
#     #Underlying_in_Portfolio_Percent <- Underlying_in_Portfolio_Percent
#     Underlyings_in_Portfolio <- Portfolio_Value * Underlying_in_Portfolio_Percent
#     Cash_in_Portfolio        <- Portfolio_Value - Underlyings_in_Portfolio
#
#     # Portfolio_Series  <- vector("double", horizon / step + 1)
#     # Market_Series     <- vector("double", horizon / step + 1)
#     # Percentage_Series <- vector("double", horizon / step + 1)
#     # store one path for the movie (no theory in this)
#     Portfolio_Series  <- c(Portfolio_Series, Portfolio_Value[1])
#     Market_Series     <- c(Market_Series, Underlying_Index[1])
#     Percentage_Series <- c(Percentage_Series, Underlying_in_Portfolio_Percent[1])
#   }
#
#   list(Time              = seq(0, horizon, step),
#        Portfolio_Series  = Portfolio_Series,
#        Market_Series     = Market_Series,
#        Percentage_Series = Percentage_Series,
#        Underlying_Index  = Underlying_Index,
#        Portfolio_Value   = Portfolio_Value
#   )
#
# }
#
# x <- maximize_power_utility()
#
# ### Play the movie for one path
# #y_max <- max( cbind( Portfolio_Series, Market_Series) ) * 1.2
#
# p_series <- x |>
#   enframe() |>
#   slice(1:4) |>
#   deframe() |>
#   as_tibble() |>
#   pivot_longer(cols = -Time) |>
#   filter(name != "Percentage_Series") |>
#   ggplot(aes(x = Time, y= value, color = name)) +
#   geom_line()
#
# alloc_series <- x |>
#   enframe() |>
#   slice(1:4) |>
#   deframe() |>
#   as_tibble() |>
#   ggplot(aes(x = Time, y = Percentage_Series)) +
#   geom_area() +
#   ylim(c(0, 1))
#
# patchwork::wrap_plots(p_series / alloc_series)
#
# x1_simul <- x |>
#   enframe() |>
#   slice(5:6) |>
#   deframe() |>
#   as_tibble() |>
#   mutate(group = "Portfolio")
# x2_simul <- x |>
#   enframe() |>
#   slice(5:6) |>
#   deframe() |>
#   as_tibble() |>
#   mutate(Underlying_Index = sort(Underlying_Index),
#          Portfolio_Value = Underlying_Index,
#          group = "Underlying")
#
#
# p <- bind_rows(x1_simul, x2_simul) |>
#   ggplot(aes(Underlying_Index, Portfolio_Value, color = group)) +
#   geom_point(size = 0.01) +
#   geom_line(size = 1) +
#   scale_color_manual(values = c("#545454", "#576890")) +
#   scale_y_continuous(labels = scales::dollar_format()) +
#   scale_x_continuous(labels = scales::dollar_format()) +
#   theme(legend.position = "bottom") +
#   labs(color = NULL,
#        x = "Underlying Payoff",
#        y = "Position Payoff")
#
#
# ggExtra::ggMarginal(
#   p,
#   type = 'histogram',
#   margins = 'both',
#   size = 3,
#   binwidth = 40,
#   groupFill = TRUE,
#   groupColour = TRUE,
#   position = "dodge",
# )
#
# ### Plots
# # plot the scatterplot
#
# # marginals
# NumBins <- round(10 * log(n_simul))
# layout( matrix(c(1,2,2,2,1,2,2,2,1,2,2,2,0,3,3,3), 4, 4, byrow = TRUE))
# barplot( table( cut( Portfolio_Value, NumBins )), horiz=TRUE, yaxt="n")
#
# # joint scatter plot
# plot(Underlying_Index, Portfolio_Value, xlab = "underlying at horizon (~ buy & hold )", ylab = "investment at horizon" )
# so <- sort( Underlying_Index )
# lines( so, so, col = "red" )
#
# barplot( table( cut( Underlying_Index, NumBins )), yaxt="n")
