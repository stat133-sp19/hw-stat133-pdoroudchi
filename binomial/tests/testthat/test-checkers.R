context('testing private checker functions')


# check_prob tests


test_that('check_prob is between 0 and 1 (inclusive)', {
  expect_true(check_prob(0))
  expect_true(check_prob(0.5))
  expect_true(check_prob(1))
  expect_error(check_prob(-0.5))
  expect_error(check_prob(1.5))
})


test_that('check_prob fails with invalid types', {
  expect_error(check_prob('one'))
})


# check_trials tests


test_that('check_trials works with accepted values', {
  expect_true(check_trials(0))
  expect_true(check_trials(100))
})


test_that('check_trials fails with negative and/or decimal values', {
  expect_error(check_trials(-1))
  expect_error(check_trials(41.5))
})


# check_success tests


test_that('check_success works with accepted values', {
  expect_true(check_success(success = 1, trials = 3))
  expect_true(check_success(0, 0))
})


test_that('check_success fails when success is higher than number of trials', {
  expect_error(check_success(4, 3))
})


test_that('check_success fails when either value is negative', {
  expect_error(check_success(-1, 3))
  expect_error(check_success(-1, -3))
})
