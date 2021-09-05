utility  <- simulate_strategy(strategy = "max_utility", n_simul = 100)
cppi     <- simulate_strategy(strategy = "cppi",        n_simul = 100)
buy_hold <- simulate_strategy(strategy = "buy_hold",    n_simul = 100)
obpi     <- simulate_strategy(strategy = "obpi",        n_simul = 100)

test_that("plot_sensivities() thowns an error for bad inputs", {
  # utility
  expect_error(plot_sensivities(strategy = utility, variable = "floor", from = 9000, to = 10000, size = 10))
  expect_error(plot_sensivities(strategy = utility, variable = "multiple", from = 1, to = 10, size = 10))
  expect_error(plot_sensivities(strategy = utility, variable = "aggressiveness", from = 0, to = 1, size = 10))

  # cppi
  expect_error(plot_sensivities(strategy = cppi, variable = "aggressiveness", from = 0, to = 1, size = 10))
  expect_error(plot_sensivities(strategy = cppi, variable = "allocation", from = 0, to = 1, size = 10))

  # obpi
  expect_error(plot_sensivities(strategy = obpi, variable = "allocation", from = 0, to = 1, size = 10))
  expect_error(plot_sensivities(strategy = obpi, variable = "floor", from = 9000, to = 10000, size = 10))
  expect_error(plot_sensivities(strategy = obpi, variable = "multiple", from = 1, to = 10, size = 10))

  # buy_hold
  expect_error(plot_sensivities(strategy = buy_hold, variable = "floor", from = 9000, to = 10000, size = 10))
  expect_error(plot_sensivities(strategy = buy_hold, variable = "multiple", from = 1, to = 10, size = 10))
  expect_error(plot_sensivities(strategy = buy_hold, variable = "aggressiveness", from = 0, to = 1, size = 10))

  # negative sigma
  expect_error(plot_sensivities(strategy = utility, variable = "sigma", from = -10, to = -10, size = 10))

  # self financing
  expect_error(plot_sensivities(strategy = utility, variable = "allocation", from = -1, to = 1, size = 10))

  # Floor
  expect_error(plot_sensivities(strategy = cppi, variable = "floor", from = -1, to = 1000, size = 10))
  expect_error(plot_sensivities(strategy = cppi, variable = "floor", from =  9000, to = 11000, size = 10))

  # Multiple
  expect_error(plot_sensivities(strategy = cppi, variable = "multiple", from = -1, to = 10, size = 10))

  # Agressiveness
  expect_error(plot_sensivities(strategy = obpi, variable = "aggressiveness", from = -1, to = 0.5, size = 10))
  expect_error(plot_sensivities(strategy = obpi, variable = "aggressiveness", from = 0, to = 2, size = 10))


})
