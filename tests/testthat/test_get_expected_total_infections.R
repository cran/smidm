test_that("get_expected_total_infections_defaults", {
  expect_equal(get_expected_total_infections(1,1,1),1)
  expect_equal(get_expected_total_infections(10,1,1),10)
  expect_equal(get_expected_total_infections(100,1,1),100)

  expect_equal(get_expected_total_infections(1,3,1),1)
  expect_equal(get_expected_total_infections(10,3,1),8)
  expect_equal(get_expected_total_infections(100,3,1),8)

  expect_equal(get_expected_total_infections(1,5,1),1)
  expect_equal(get_expected_total_infections(10,5,1),3)
  expect_equal(get_expected_total_infections(100,5,1),3)
})

