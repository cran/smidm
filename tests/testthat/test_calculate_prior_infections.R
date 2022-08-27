test_that("aPriori_change_group_size_school_1", {
  x <- 15
  expectedResult <- c(0.88001168, 0.07114660, 0.02608142, 0.01174895,
                      0.00566976, 0.00279340, 0.00137010, 0.00065742,
                      0.00030397, 0.00013332, 0.00005445, 0.00002020,
                      0.00000657, 0.00000177, 0.00000035, 0.00000004)
  expect_equal(round(calculate_prior_infections(x, 1, "school"),8), expectedResult)
})

test_that("aPriori_aPriori_change_group_size_school_2", {
  x <- 25
  expectedResult <- c(0.87999955, 0.07191080, 0.02572335, 0.01139736,
                      0.00547487, 0.00272675, 0.00137861, 0.00069894,
                      0.00035243, 0.00017565, 0.00008607, 0.00004127,
                      0.00001927, 0.00000872, 0.00000380, 0.00000159,
                      0.00000063, 0.00000024, 0.00000008, 0.00000003,
                      0.00000001, 0.00000000, 0.00000000, 0.00000000,
                      0.00000000, 0.00000000)
  expect_equal(round(calculate_prior_infections(x, 1, "school"),8), expectedResult)
})

test_that("aPriori_change_infected_school_1", {
  x <- 15
  expectedResult <- c(0.27853795, 0.23073158, 0.17086544, 0.11952828, 0.07994860,
                      0.05124286, 0.03141181, 0.01832953, 0.01010727, 0.00521180,
                      0.00247589, 0.00106021, 0.00039574, 0.00012175, 0.00002773, 0.00000355)
  expect_equal(round(calculate_prior_infections(x, 10, "school"), 8), expectedResult)
})

test_that("aPriori_aPriori_change_infected_school_2", {
  x <- 15
  expectedResult <- c(0.14700313, 0.18515782, 0.17710232, 0.14939623, 0.11603779,
                      0.08434605, 0.05770154, 0.03714712, 0.02240863, 0.01256108,
                      0.00645601, 0.00298056, 0.00119653, 0.00039531, 0.00009661, 0.00001328)
  expect_equal(round(calculate_prior_infections(x, 15, "school"),8), expectedResult)
})

test_that("aPriori_change_group_size_day_care_center_1", {
  x <- 15
  expectedResult <- c(0.6999996057, 0.1056182556, 0.0561696635, 0.0367762514,
                      0.0262016985, 0.0194780052, 0.0148050558, 0.0113657538,
                      0.0087333393, 0.0066624830, 0.0050021286, 0.0036545859,
                      0.0025546758, 0.0016586231, 0.0009384911, 0.0003813837)
  expect_equal(calculate_prior_infections(x, 1, "day_care_center"), expectedResult)
})

test_that("aPriori_aPriori_change_group_size_day_care_center_2", {
  x <- 24
  expectedResult <- c(0.69999830, 0.11016128, 0.05709724, 0.03628037, 0.02507941,
                      0.01811581, 0.01341915, 0.01008620, 0.00764048, 0.00580496,
                      0.00440659, 0.00333118, 0.00249998, 0.00185672, 0.00136002,
                      0.00097862, 0.00068847, 0.00047069, 0.00031023, 0.00019493,
                      0.00011488, 0.00006188, 0.00002912, 0.00001094, 0.00000255)
  expect_equal(round(calculate_prior_infections(x, 1, "day_care_center"),8), expectedResult)
})

test_that("aPriori_change_infected_day_care_center_1", {
  x <- 15
  expectedResult <- c(0.48999945, 0.14860916, 0.09066108, 0.06422575, 0.04842596,
                      0.03766384, 0.02974770, 0.02362686, 0.01872888, 0.01471330,
                      0.01136468, 0.00854074, 0.00614546, 0.00411476, 0.00241029, 0.00102211)
  expect_equal(round(calculate_prior_infections(x, 2, "day_care_center"), 8), expectedResult)
})

test_that("aPriori_aPriori_change_infected_day_care_center_2", {
  x <- 15
  expectedResult <- c(0.05764775, 0.07208438, 0.07908377, 0.08248885, 0.08358600,
                      0.08297455, 0.08098148, 0.07779972, 0.07354532, 0.06828389,
                      0.06204308, 0.05481691, 0.04656369, 0.03719592, 0.02655508, 0.01434960)
  expect_equal(round(calculate_prior_infections(x, 8, "day_care_center"),8), expectedResult)
})

test_that("aPriori_aPriori_change_event_1", {
  expectedResult <- c(0.19952580, 0.11559084, 0.09281880, 0.08164757, 0.07513202,
                      0.07116430, 0.06893753, 0.06820342, 0.06916264, 0.07290075,
                      0.08491634)
  expect_equal(round(calculate_prior_infections(10, 5, "uni", 0.15, 5.8),8), expectedResult)
})

test_that("aPriori_aPriori_change_event_2", {
  expectedResult <- c(0.09146232, 0.06264377, 0.05329392, 0.04831618, 0.04517707, 0.04303416,
                      0.04151650, 0.04043363, 0.03967886, 0.03919026, 0.03893298, 0.03889107,
                      0.03906425, 0.03946817, 0.04013854, 0.04114118, 0.04259468, 0.04472501,
                      0.04802047, 0.05382112, 0.06845587)
  expect_equal(round(calculate_prior_infections(20, 10, "work", 0.20, 6.5),8), expectedResult)
})
