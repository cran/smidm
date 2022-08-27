
#############PCR tests###########################

test_that("calculate_likelihood_negative_tests_data_extend_1_PCR", {
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 8
  types <- matrix(nrow = 1, ncol = 2)
  types[1, 1] <- "PCR"
  types[1, 2] <- "PCR"

  expect_equal(calculate_likelihood_negative_tests(days, types, 10, 10), c(1, 0.2, 0.04, 0.008, 0.0016,
                                                      0.00032, 0.000064, 0.0000128, 0.00000256,
                                                      0.000000512, 0.0000001024))
})

test_that("calculate_likelihood_negative_tests_data_extend_2_PCR", {
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 8
  types <- matrix(nrow = 1, ncol = 2)
  types[1, 1] <- "PCR"
  types[1, 1] <- "PCR"

  expect_equal(calculate_likelihood_negative_tests(days, types, 1, 1), c(1.0, 0.2))
})

test_that("calculate_likelihood_negative_tests_data_extend_3_PCR", {
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 8
  types <- matrix(nrow = 1, ncol = 1)
  types[1, 1] <- "PCR"

  expect_equal(calculate_likelihood_negative_tests(days, types, 25, 5), c(1.000000000, 0.840000000, 0.701333333, 0.581773913, 0.479257549,
                                                      0.391875782, 0.317869714, 0.255623842, 0.203659889, 0.160630640,
                                                      0.125313771, 0.096605681, 0.073515329, 0.055158062, 0.040749449,
                                                      0.029599115, 0.021104569, 0.014745043, 0.010075320, 0.006719566,
                                                      0.004365166, 0.002756553, 0.001689043, 0.001002667, 0.000576000,
                                                      0.000320000))
})


test_that("calculate_likelihood_negative_tests_data_days_1_PCR", {
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 0
  types <- matrix(nrow = 1, ncol = 1)
  types[1, 1] <- "PCR"

  expect_equal(calculate_likelihood_negative_tests(days, types, 25, 5), rep(1, 26))
})


#############Antigen tests###########################

test_that("calculate_likelihood_negative_tests_data_extend_1_Antigen", {
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 8
  types <- matrix(nrow = 1, ncol = 2)
  types[1, 1] <- "Antigen"
  types[1, 2] <- "Antigen"

  expect_equal(round(calculate_likelihood_negative_tests(days, types, 10, 10),7), round(c(1.000000e+00, 2.956057e-01, 8.738276e-02,
                                                                     2.583085e-02, 7.635747e-03, 2.257171e-03,
                                                                     6.672326e-04, 1.972378e-04, 5.830463e-05,
                                                                     1.723518e-05, 5.094819e-06),7))
})

test_that("calculate_likelihood_negative_tests_data_extend_2_Antigen", {
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 8
  types <- matrix(nrow = 1, ncol = 2)
  types[1, 1] <- "Antigen"
  types[1, 1] <- "Antigen"

  expect_equal(round(calculate_likelihood_negative_tests(days, types, 1, 1),7), c(1.0000000, 0.2956057))
})

test_that("calculate_likelihood_negative_tests_data_extend_3_Antigen", {
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 8
  types <- matrix(nrow = 1, ncol = 2)
  types[1, 1] <- "Antigen"
  types[1, 2] <- "Antigen"

  expect_equal(calculate_likelihood_negative_tests(days, types, 25, 5), c(1.000000000, 0.859121150, 0.734781342, 0.625461010, 0.529737894,
                                                      0.446283779, 0.373861226, 0.311320313, 0.257595367, 0.211701705,
                                                      0.172732365, 0.139854844, 0.112307836, 0.089397965, 0.070496523,
                                                      0.055036207, 0.042507851, 0.032457166, 0.024481476, 0.018226451,
                                                      0.013382846, 0.009683236, 0.006898752, 0.004835818, 0.003332886,
                                                      0.002257171))
})

test_that("calculate_likelihood_negative_tests_data_days_1_Antigen", {
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 0
  types <- matrix(nrow = 1, ncol = 1)
  types[1, 1] <- "Antigen"

  expect_equal(calculate_likelihood_negative_tests(days, types, 25, 5), rep(1, 26))
})

test_that("calculate_likelihood_negative_tests_other_tests", {
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 8
  types <- matrix(nrow = 1, ncol = 1)
  types[1, 1] <- "Plasphemie"

  df <- data.frame(Plasphemie = 1 - c(1, 1, 1, 0.96, 0.66, 0.36, 0.24, 0.21,
                                      0.20, 0.21, 0.23, 0.26, 0.29, 0.33,
                                      0.375, 0.42, 0.465, 0.51, 0.56, 0.60,
                                      0.63, 0.67))

  info <- get_test_sensitivities(df)
  expect_equal(calculate_likelihood_negative_tests(days, types, 10, 5, info), c(1.000000000, 0.600000000, 0.342222222,
                                                          0.184000000, 0.092419048, 0.043017143,
                                                          0.018483810, 0.007360000, 0.002737778,
                                                          0.000960000, 0.000320000))
})
