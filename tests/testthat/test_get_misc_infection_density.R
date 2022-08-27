
test_that("misc_infection_date_boundary_condition", {
  dateInput <- as.Date("2021-10-05")
  max_incubation_days <- 14
  meanlog <- 1.69
  sdlog <- 0.55

  expect_equal(colnames(get_misc_infection_density(dateInput, 1, max_incubation_days, meanlog, sdlog)[1]), "dates")
  expect_equal(colnames(get_misc_infection_density(dateInput, 8, max_incubation_days, meanlog, sdlog)[2]), "distribution")
  expect_equal(ncol(get_misc_infection_density(dateInput, 4, max_incubation_days, meanlog, sdlog)), 2)
  expect_equal(nrow(get_misc_infection_density(dateInput, 3, max_incubation_days, meanlog, sdlog)), 336)
})


test_that("misc_infection_date_dates", {
 dateInput <- as.Date("2021-10-05")
 max_incubation_days <- 14
 meanlog <- 1.69
 sdlog <- 0.55


 dateResult <- as.Date("2021-10-05") - 14

 expectedInput <- lapply(1:25, function(i) get_misc_infection_density(dateInput, 5, max_incubation_days, meanlog, sdlog)[i,1])


 expectedResult <- as.list(seq(as.POSIXct(dateResult, tz = "CET"),
                          by = "hour",
                          length.out = 25))



 expect_equal(expectedInput, expectedResult)
})

test_that("misc_infection_date_gamma", {
  dateInput <- as.Date("2021-10-05")
  max_incubation_days <- 14
  meanlog <- 1.69
  sdlog <- 0.55

  expectedInput <- round(sapply(1:25, function(i)
                        get_misc_infection_density(dateInput, 25, max_incubation_days, meanlog, sdlog)[i,2]), 7)

  expectedResult  <- c(0.0118359, 0.0119827, 0.0121314, 0.0122820, 0.0124346,
                       0.0125891, 0.0127457, 0.0129043, 0.0130650, 0.0132277,
                       0.0133926, 0.0135597, 0.0137289, 0.0139003, 0.0140740,
                       0.0142499, 0.0144281, 0.0146086, 0.0147915, 0.0149768,
                       0.0151645, 0.0153546, 0.0155472, 0.0157424, 0.0159400)


  expect_equal(expectedInput, expectedResult)
})

test_that("misc_infection_date_gamma_same_groups", {
  dateInput <- c(as.Date("2021-10-05"), as.Date("2021-10-05"), as.Date("2021-10-05"))
  persons <- c(3,7,1)
  max_incubation_days <- 14
  meanlog <- 1.69
  sdlog <- 0.55

  expectedInput <- round(get_misc_infection_density(dateInput, persons, max_incubation_days, meanlog, sdlog)[,2], 7)
  expectedInput <- expectedInput[1:25]

  expectedResult  <- c(0.0118359, 0.0119827, 0.0121314, 0.0122820, 0.0124346, 0.0125891,
                       0.0127457, 0.0129043, 0.0130650, 0.0132277, 0.0133926, 0.0135597,
                       0.0137289, 0.0139003, 0.0140740, 0.0142499, 0.0144281, 0.0146086,
                       0.0147915, 0.0149768, 0.0151645, 0.0153546, 0.0155472, 0.0157424,
                       0.0159400)


  expect_equal(expectedInput, expectedResult)
})

test_that("misc_infection_date_gamma_different_groups", {
  dateInput <- c(as.Date("2021-10-05"), as.Date("2021-10-01"))
  persons <- c(1,1)
  max_incubation_days <- 14
  meanlog <- 1.69
  sdlog <- 0.55

  expectedInput <- round(get_misc_infection_density(dateInput, persons, max_incubation_days, meanlog, sdlog)[,2], 7)
  expectedInput <- expectedInput[1:25]

  expectedResult  <- c(0.0059179, 0.0059913, 0.0060657, 0.0061410, 0.0062173,
                       0.0062946, 0.0063728, 0.0064522, 0.0065325, 0.0066139,
                       0.0066963, 0.0067798, 0.0068644, 0.0069502, 0.0070370,
                       0.0071249, 0.0072140, 0.0073043, 0.0073958, 0.0074884,
                       0.0075822, 0.0076773, 0.0077736, 0.0078712, 0.0079700)


  expect_equal(expectedInput, expectedResult)
})


