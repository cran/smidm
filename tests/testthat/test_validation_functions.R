###################TESTEN DER VALIDATIONS FUNKTIONEN
test_that("smidm_is_natural_number", {

  expect_error(smidm_is_natural_number(-10),
               "The input must be a natural number.")
  expect_error(smidm_is_natural_number(0),
               "The input must be a natural number.")
})

test_that("smidm_is_double_matrix", {

  matrix <- matrix(nrow = 1, ncol = 1)
  matrix[1,1] <- "character"


  expect_error(smidm_is_double_matrix(matrix),
               "The input must be a numeric matrix.")
})

test_that("smidm_is_character_matrix", {

  matrix <- matrix(nrow = 1, ncol = 1)
  matrix[1,1] <- 1


  expect_error(smidm_is_character_matrix(matrix),
               "The input must be a character matrix.")
})

test_that("smidm_is_parameter_gamma", {
  expect_error(smidm_is_parameter_gamma(-0.1),
               "The input must be a positive number for the gamma distribution.")
})

test_that("smidm_is_parameter_log_mean", {
  expect_error(smidm_is_parameter_log_mean(""),
               "The input must be a number for the mean parameter of the log distribution.")
})

test_that("smidm_is_parameter_log_sd", {
  expect_error(smidm_is_parameter_log_sd(-0.1),
               "The input must be a positive number for the standard deviation parameter of the log distribution.")
})

test_that("smidm_is_positive_vector", {
  expect_error(smidm_is_positive_vector(c("test", "test", "test")),
               "The input must be a positive vector with numbers.")
})

test_that("smidm_is_character", {
  expect_error(smidm_is_character(1),
               "The input must be of class character.")
})

test_that("smidm_is_dataframe", {
  expect_error(smidm_is_dataframe(1),
               "The input must be of class dataframe.")
})

test_that("smidm_is_date", {
  expect_error(smidm_is_date(1),
               "The input must be of class Date.")
})

test_that("smidm_is_double", {
  expect_error(smidm_is_double(-1),
               "The input must be a positive number.")
})

