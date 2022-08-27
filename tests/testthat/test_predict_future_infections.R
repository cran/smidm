test_that("predict_future_infections_all1", {
  last_day_reported_infection <- 1
  total_reported_infections <- 1
  total_expected_infections <- 1
  expect_equal(predict_future_infections(last_day_reported_infection, total_reported_infections,
                                         total_expected_infections), c(0, 0, 0))
  expect_equal(length(predict_future_infections(last_day_reported_infection, total_reported_infections,
                                                total_expected_infections)), 3)
})

test_that("predict_future_infections_mixed", {
  last_day_reported_infection <- 1
  total_reported_infections <- 2
  total_expected_infections <- 10
  expect_equal(predict_future_infections(last_day_reported_infection, total_reported_infections,
                                         total_expected_infections), c(0, 1, 1, 2, 1, 1, 1, 1, 0))
})

