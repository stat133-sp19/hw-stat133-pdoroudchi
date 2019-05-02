context('testing private auxiliary functions')


# aux_mean tests


test_that('aux_mean returns correct value', {
  expect_equal(aux_mean(trials = 10, prob = 0.3), 3)
})


test_that('aux_mean returns correct lengths', {
  expect_length(aux_mean(10, 0.3), 1)
  expect_length(aux_mean(c(10, 100), c(0.3, 0.5)), 2)
})


test_that('aux_mean is of numeric class', {
  expect_is(aux_mean(10, 0.3), 'numeric')
})


# aux_variance tests


test_that('aux_variance returns correct value', {
  expect_equal(aux_variance(trials = 10, prob = 0.3), 2.1)
})


test_that('aux_variance returns correct lengths', {
  expect_length(aux_variance(10, 0.3), 1)
  expect_length(aux_variance(c(10, 100), c(0.3, 0.5)), 2)
})


test_that('aux_variance is of numeric class', {
  expect_is(aux_variance(10, 0.3), 'numeric')
})


# aux_mode tests


test_that('aux_mode returns correct value', {
  expect_equal(aux_mode(trials = 10, prob = 0.3), 3)
})


test_that('aux_mode returns correct lengths', {
  expect_length(aux_mode(10, 0.3), 1)
  expect_length(aux_mode(c(10, 100), c(0.3, 0.5)), 2)
})


test_that('aux_mode is of integer class', {
  expect_is(aux_mode(10, 0.3), 'integer')
})


# aux_skewness tests


test_that('aux_skewness returns correct value', {
  expect_equal(aux_skewness(trials = 10, prob = 0.3), 0.2760262)
})


test_that('aux_skewness returns correct lengths', {
  expect_length(aux_skewness(10, 0.3), 1)
  expect_length(aux_skewness(c(10, 100), c(0.3, 0.5)), 2)
})


test_that('aux_skewness is of numeric class', {
  expect_is(aux_skewness(10, 0.3), 'numeric')
})


# aux_kurtosis tests


test_that('aux_kurtosis returns correct value', {
  expect_equal(aux_kurtosis(trials = 10, prob = 0.3), -0.1238095)
})


test_that('aux_kurtosis returns correct lengths', {
  expect_length(aux_kurtosis(10, 0.3), 1)
  expect_length(aux_kurtosis(c(10, 100), c(0.3, 0.5)), 2)
})


test_that('aux_kurtosis is of numeric class', {
  expect_is(aux_kurtosis(10, 0.3), 'numeric')
})
