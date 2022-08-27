test_that("get_incubation_day_distribution_boundaryCondition", {
  max_days <- 14
  meanlog <- 1.69
  sdlog <- 0.55

  expect_equal(length(get_incubation_day_distribution(max_days, meanlog, sdlog)), 14)
})



test_that("get_incubation_day_distributions_output", {
  max_days <- 14
  meanlog <- 1.69
  sdlog <- 0.55

  expectedResult <- c(0.001060561, 0.033897039, 0.106173722, 0.149277062, 0.151364228,
                     0.131619623, 0.105746291, 0.081412298, 0.061243957, 0.045520808,
                     0.033651613, 0.024843849, 0.018363569, 0.013611971)


  expect_equal(get_incubation_day_distribution(max_days, meanlog, sdlog),expectedResult)

})


