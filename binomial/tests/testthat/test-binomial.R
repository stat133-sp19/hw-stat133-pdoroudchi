context('testing main functions')


# bin_choose tests


test_that('bin_choose returns correct value', {
  expect_equal(bin_choose(n = 5, k = 2), 10)
  expect_equal(bin_choose(5, 0), 1)
})


test_that('bin_choose returns correct lengths', {
  expect_length(bin_choose(5, 2), 1)
  expect_length(bin_choose(5, 1:3), 3)
})


test_that('bin_choose fails when k is greater than n', {
  expect_error(bin_choose(n = 5, k = 6))
})


# bin_probability tests


test_that('bin_probability returns correct value', {
  expect_equal(bin_probability(success = 2, trials = 5, prob = 0.5), 0.3125)
})


test_that('bin_probability returns correct lengths', {
  expect_length(bin_probability(2, 5, 0.5), 1)
  expect_length(bin_probability(0:2, 5, 0.5), 3)
})


test_that('bin_probability fails with incorrect given values', {
  expect_error(bin_probability(-1, -2, 2))
})


# bin_distribution tests


test_that('bin_distribution is of classes "bindis" and "data.frame"', {
  expect_is(bin_distribution(trials = 5, prob = 0.5), 'bindis')
  expect_is(bin_distribution(5, 0.5), 'data.frame')
})


test_that('bin_distribution returns a data frame of length 2 (2 columns)', {
  expect_length(bin_distribution(5, 0.5), 2)
})


# bin_cumulative tests


test_that('bin_cumulative is of classes "bincum" and "data.frame"', {
  expect_is(bin_cumulative(trials = 5, prob = 0.5), 'bincum')
  expect_is(bin_cumulative(5, 0.5), 'data.frame')
})


test_that('bin_cumulative returns a data frame of length 3 (3 columns)', {
  expect_length(bin_cumulative(5, 0.5), 3)
})
