test_that("infectiousness_boundary_condition", {
  dateInput <- as.Date("2021-10-05")
  infectiousness_shift <- 12.272481
  max_infectious_days <- 24
  shape <- 20.516508
  rate <- 1.592124

  expect_equal(colnames(get_infectiousness_density(dateInput, infectiousness_shift, max_infectious_days, shape, rate)[1]), "dates")
  expect_equal(colnames(get_infectiousness_density(dateInput, infectiousness_shift, max_infectious_days, shape, rate)[2]), "distribution")
  expect_equal(ncol(get_infectiousness_density(dateInput, infectiousness_shift, max_infectious_days, shape, rate)), 2)
  expect_equal(nrow(get_infectiousness_density(dateInput, infectiousness_shift, max_infectious_days, shape, rate)), 576)
})



test_that("infectiousness_dates", {
 dateInput <- as.Date("2021-10-05")
 infectiousness_shift <- 12.272481
 max_infectious_days <- 24
 shape <- 20.516508
 rate <- 1.592124

 dateResult <- as.Date("2021-10-05") - infectiousness_shift

 expectedInput <- lapply(1:25, function(i)
   get_infectiousness_density(dateInput, infectiousness_shift, max_infectious_days, shape, rate)[i,1])

 expectedResult <- as.list(seq(as.POSIXct(dateResult, tz = "CET"),
                               by = "hour",
                               length.out = 25))

 expect_equal(expectedInput, expectedResult)
})



test_that("infectiousness_gamma", {
  dateInput <- as.Date("2021-10-05")
  infectiousness_shift <- 12.272481
  max_infectious_days <- 24
  shape <- 20.516508
  rate <- 1.592124

  expectedInput <- round(sapply(150:175, function(i)
    get_infectiousness_density(dateInput, infectiousness_shift, max_infectious_days, shape, rate)[i,2]), 5)

  expectedResult  <- c(0.00374, 0.00399, 0.00425, 0.00452, 0.00481, 0.00511, 0.00543,
                       0.00576, 0.00610, 0.00647, 0.00684, 0.00724, 0.00765, 0.00808,
                       0.00852, 0.00899, 0.00947, 0.00997, 0.01049, 0.01103, 0.01159,
                       0.01217, 0.01277, 0.01339, 0.01404, 0.01470)

  expect_equal(expectedInput, expectedResult)
})
