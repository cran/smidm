test_that("serial_interval_boundary_condition", {
  dateInput <- as.Date("2021-10-05")

  expect_equal(colnames(get_serial_interval_density(dateInput)[1]), "dates")
  expect_equal(colnames(get_serial_interval_density(dateInput)[2]), "distribution")
  expect_equal(ncol(get_serial_interval_density(dateInput)), 2)
  expect_equal(nrow(get_serial_interval_density(dateInput)), 480)
})


test_that("serial_interval_dates", {
 dateInput <- as.Date("2021-10-05")

 expectedInput <- lapply(1:25, function(i)
   get_serial_interval_density(dateInput)[i,1])

 expectedResult <- as.list(seq(as.POSIXct(dateInput, tz = "CET"),
                               by = "hour",
                               length.out = 25))

 expect_equal(expectedInput, expectedResult)
})

test_that("serial_interval_gamma", {
  dateInput <- as.Date("2021-10-05")

  expectedInput <- round(sapply(1:25, function(i)
    get_serial_interval_density(dateInput)[i,2]), 9)

  expectedResult  <- c(0.000000000, 0.002857082, 0.006261424, 0.009843859,
                       0.013508132, 0.017205306, 0.020905440, 0.024588381,
                       0.028239760, 0.031848946, 0.035407873, 0.038910327,
                       0.042351470, 0.045727521, 0.049035527, 0.052273198,
                       0.055438780, 0.058530957, 0.061548782, 0.064491610,
                       0.067359055, 0.070150951, 0.072867315, 0.075508327, 0.078074303)

  expect_equal(expectedInput, expectedResult)
})
