test_that("calculate_posterior_no_infections_no_infect_1", {
  noInfect <- 10
  infectedP <- 3
  Type <- c("school")
  daySize <- c(10)
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 8
  types <- matrix(nrow = 1, ncol = 1)
  types[1, 1] <- "PCR"

  expect_equal(round(calculate_posterior_no_infections(noInfect, infectedP,
                                                     Type, days, types, daySize),7), 0.9494978)
})

test_that("calculate_posterior_no_infections_no_infect_2", {
  noInfect <- 20
  infectedP <- 1
  Type <- c("school")
  daySize <- c(10)
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 8
  types <- matrix(nrow = 1, ncol = 1)
  types[1, 1] <- "PCR"


  expect_equal(round(calculate_posterior_no_infections(noInfect, infectedP,
                                                           Type, days, types, daySize),7), 0.9409392)
})

test_that("calculate_posterior_no_infections_no_infect_3", {
  noInfect <- 30
  infectedP <- 1
  Type <- c("school")
  daySize <- c(10)
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 8
  types <- matrix(nrow = 1, ncol = 1)
  types[1, 1] <- "PCR"


  expect_equal(round(calculate_posterior_no_infections(noInfect, infectedP,
                                                           Type, days, types, daySize),7), 0.9232523)
})

test_that("calculate_posterior_no_infections_infected_p_1", {
  noInfect <- 10
  infectedP <- 10
  Type <- c("school")
  daySize <- c(10)
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 8
  types <- matrix(nrow = 1, ncol = 1)
  types[1, 1] <- "PCR"


  expect_equal(round(calculate_posterior_no_infections(noInfect, infectedP,
                                                           Type, days, types, daySize),7), 0.8372103)
})


test_that("calculate_posterior_no_infections_day_care_center_1", {
  noInfect <- 20
  infectedP <- 1
  Type <- c("day_care_center")
  daySize <- c(10)
  days <- matrix(nrow = 1, ncol = 2)
  days[1, 1] <- 1
  days[1, 2] <- 8
  types <- matrix(nrow = 1, ncol = 1)
  types[1, 1] <- "PCR"


  expect_equal(round(calculate_posterior_no_infections(noInfect, infectedP,
                                                           Type, days, types, daySize),7), 0.8780924)
})


test_that("calculate_posterior_no_infections_other_event_1", {

  negative_persons <- 23
  infected_persons <- 2
  event <- "work"
  subgroup_size <- c(3, 5)
  test_infos <- matrix(nrow = 2, ncol = 3)

  test_infos[1, 1] <- 1
  test_infos[1, 2] <- 2
  test_infos[2, 1] <- 2
  test_infos[2, 2] <- 4
  test_infos[2, 3] <- 6

  test_types <- matrix(nrow = 2, ncol = 2)
  test_types[1, 1] <- "PCR"
  test_types[2, 1] <- "PCR"
  test_types[2, 2] <- "Antigen"

  distribution <- c(0.624090759, 0.080692035, 0.045577237, 0.032360641, 0.025326096,
                    0.020924167, 0.017895268, 0.015676336, 0.013976750, 0.012630893,
                    0.011537260, 0.010630077, 0.009864798, 0.009210154, 0.008643541,
                    0.008148212, 0.007711506, 0.007323692, 0.006977206, 0.006666148,
                    0.006385981, 0.006133470, 0.005907169, 0.005710605)

  expect_equal(round(calculate_posterior_no_infections(negative_persons,
                                                       infected_persons,
                                                       event,
                                                       test_infos,
                                                       test_types,
                                                       subgroup_size,
                                                       distribution), 6), 0.801441)
})

test_that("calculate_posterior_no_infections_other_event_2", {

  negative_persons <- 10
  infected_persons <- 5
  event <- "uni"
  subgroup_size <- c(1)
  test_infos <- matrix(nrow = 1, ncol = 2)

  test_infos[1, 1] <- 1
  test_infos[1, 2] <- 2


  test_types <- matrix(nrow = 1, ncol = 1)
  test_types[1, 1] <- "PCR"

  distribution <- c(0.19952580, 0.11559084, 0.09281880, 0.08164757, 0.07513202, 0.07116430,
                    0.06893753, 0.06820342, 0.06916264, 0.07290075, 0.08491634)

  expect_equal(round(calculate_posterior_no_infections(negative_persons,
                                                             infected_persons,
                                                             event,
                                                             test_infos,
                                                             test_types,
                                                             subgroup_size,
                                                             distribution), 6), 0.199526)
})


test_that("calculate_posterior_no_infections_other_informations", {

  negative_persons <- 10
  infected_persons <- 5
  event <- "school"
  subgroup_size <- c(1)
  test_infos <- matrix(nrow = 1, ncol = 2)

  test_infos[1, 1] <- 1
  test_infos[1, 2] <- 2


  test_types <- matrix(nrow = 1, ncol = 1)
  test_types[1, 1] <- "PCR"

  distribution <- NULL

  df <- data.frame(paranoia = 1 - c(1, 1, 1, 0.96, 0.66, 0.36, 0.24, 0.21, 0.20, 0.21, 0.23, 0.26,
                                    0.29, 0.33, 0.375, 0.42, 0.465, 0.51, 0.56, 0.60, 0.63, 0.67))
  info <- get_test_sensitivities()

  expect_equal(round(calculate_posterior_no_infections(negative_persons,
                                                             infected_persons,
                                                             event,
                                                             test_infos,
                                                             test_types,
                                                             subgroup_size,
                                                             distribution,
                                                             info), 6), 0.52773)
})
