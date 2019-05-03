#Tests
library(testthat)
library(binomial)

#for check_prob()
test_that("prob is a number between 0 and 1", {
  expect_equal(check_prob(0), "TRUE")
  expect_equal(check_prob(0.1), "TRUE")
  expect_equal(check_prob(0.84), "TRUE")
})

test_that("prob is of length 1", {
  expect_equal(str_length(check_prob(0.4)), "TRUE")
  expect_equal(str_length(check_prob(c("0.4", "0.8"))), "FALSE")
  expect_equal(str_length(check_prob(c("0.4", "0.8", "0.5"))), "FALSE")
})

test_that("getting an error if prob is invalid", {
  expect_equal(typeof(check_prob(0.3)), double)
  expect_equal(typeof(check_prob("hello")), "FALSE")
  expect_equal(typeof(check_prob("TRUE")), "FALSE")
})


#for check_trials()
test_that("trials is a number ", {
  expect_equal(check_trials(7), "TRUE")
  expect_equal(check_trials("hello"), "FALSE")
  expect_equal(check_trials("FALSE"), "FALSE")
})

test_that("trials is a positive number", {
  expect_equal(check_trials(6), "TRUE")
  expect_equal(check_trials(-6), "FALSE")
  expect_equal(check_trials(-8), "FALSE")
})

test_that("getting an error if trials is invalid", {
  expect_equal(check_prob(3), "TRUE")
  expect_equal(check_prob(0.3), "FALSE")
  expect_equal(check_prob(1.7), "FALSE")
})


#for check_success()
test_that("success is a whole number", {
  expect_equal(check_success(8, trials), "TRUE")
  expect_equal(check_success(8.1, trials), "FALSE")
  expect_equal(check_success(0.1, trials), "FALSE")
})

test_that("success is a positive number", {
  expect_equal(check_success(8, trials), "TRUE")
  expect_equal(check_success(3, trials), "FALSE")
  expect_equal(check_success(-6, trials), "FALSE")
})

test_that("getting an error if success is invalid", {
  expect_equal(typeof(prob) == integer, "TRUE")
  expect_equal(typeof(prob) == logical, "FALSE")
  expect_equal(typeof(prob) == double, "FALSE")
})


#for aux_mean()
test_that("mean is a number", {
  expect_equal(typeof(aux_mean(c(0, 1, 2, 3))), "double")
  expect_equal(typeof(aux_mean("hello")), "FALSE")
  expect_equal(typeof(aux_mean(TRUE)), "FALSE")
})

test_that("mean is calculated correctly", {
  expect_equal(aux_mean(10, 0.2), 2)
  expect_equal(aux_mean(10, 0.3), 3)
  expect_equal(aux_mean(10, 0.4), 4)
})

test_that("mean is a double", {
  expect_equal(typeof(aux_mean(10, 0.2)), double)
  expect_equal(typeof(aux_mean(10, 0.3)), double)
  expect_equal(typeof(aux_mean(10, 0.4)), double)
})


#for aux_variance()
test_that("variance is a number", {
  expect_equal(typeof(aux_variance(c(0, 1, 2, 3))), "double")
  expect_equal(typeof(aux_variance("hello")), "FALSE")
  expect_equal(typeof(aux_variance(TRUE)), "FALSE")
})

test_that("variance is calculated correctly", {
  expect_equal(aux_variance(10, 0.2), 1.6)
  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_equal(aux_variance(10, 0.4), 2.4)
})

test_that("variance is a double", {
  expect_equal(typeof(aux_variance(10, 0.2)), double)
  expect_equal(typeof(aux_variance(10, 0.3)), double)
  expect_equal(typeof(aux_variance(10, 0.4)), double)
})


#for aux_mode()
test_that("mode is a number", {
  expect_equal(typeof(aux_mode(c(0, 1, 2, 3))), "double")
  expect_equal(typeof(aux_mode("hello")), "FALSE")
  expect_equal(typeof(aux_mode(TRUE)), "FALSE")
})

test_that("mode is calculated correctly", {
  expect_equal(aux_mode(10, 0.2), 2)
  expect_equal(aux_mode(10, 0.3), 3)
  expect_equal(aux_mode(10, 0.4), 4)
})

test_that("mode is a double", {
  expect_equal(typeof(aux_mode(10, 0.2)), double)
  expect_equal(typeof(aux_mode(10, 0.3)), double)
  expect_equal(typeof(aux_mode(10, 0.4)), double)
})


#for aux_skewness()
test_that("skewness is a number", {
  expect_equal(typeof(aux_skewness(c(0, 1, 2, 3))), "double")
  expect_equal(typeof(aux_skewness("hello")), "FALSE")
  expect_equal(typeof(aux_skewness(TRUE)), "FALSE")
})

test_that("skewness is calculated correctly", {
  expect_equal(aux_skewness(10, 0.2), 0.4743416)
  expect_equal(aux_skewness(10, 0.3), 0.2760262)
  expect_equal(aux_skewness(10, 0.4), 0.1290994)
})

test_that("skewnesss is a double", {
  expect_equal(typeof(aux_skewness(10, 0.2)), double)
  expect_equal(typeof(aux_skewness(10, 0.3)), double)
  expect_equal(typeof(aux_skewness(10, 0.4)), double)
})


#for aux_kurtosis()
test_that("kurtosis is a number", {
  expect_equal(typeof(aux_kurtosis(c(0, 1, 2, 3))), "double")
  expect_equal(typeof(aux_kurtosis("hello")), "FALSE")
  expect_equal(typeof(aux_kurtosis(TRUE)), "FALSE")
})

test_that("kurtosis is calculated correctly", {
  expect_equal(aux_kurtosis(10, 0.2), 0.025)
  expect_equal(aux_kurtosis(10, 0.3), -0.1238095)
  expect_equal(aux_kurtosis(10, 0.4), -0.1833333)
})

test_that("kurtosis is a double", {
  expect_equal(typeof(aux_kurtosis(10, 0.2)), double)
  expect_equal(typeof(aux_kurtosis(10, 0.3)), double)
  expect_equal(typeof(aux_kurtosis(10, 0.4)), double)
})


#for bin_choose()
test_that("n and k is/are number(s)", {
  expect_equal(typeof(bin_choose(3, 18)), "double")
  expect_equal(typeof(bin_choose(c("a", "b"), 18)), "FALSE")
  expect_equal(typeof(bin_choose(TRUE, 18), "FALSE")
})
  
test_that("bin_choose is calculated correctly", {
expect_equal(bin_choose(5, 2), 10)
expect_equal(bin_choose(9, 3), 84)
expect_equal(bin_choose(7, 6), 7)
})
  
test_that("bin_choose is a double", {
expect_equal(typeof(bin_choose(5, 2)), double)
expect_equal(typeof(bin_choose(9, 3)), double)
expect_equal(typeof(bin_choose(7, 6)), double)
})
  
  
#for bin_probability()
test_that("success, trials, and prob are numbers", {
expect_equal(typeof(bin_probability(3, 18, 0.4)), "double")
expect_equal(typeof(typeof(bin_probability("hi", "hello", 0.4)), "FALSE")
expect_equal(typeof(typeof(bin_probability(TRUE, 18, FALSE)), "FALSE")
})
                 
test_that("bin_probability is calculated correctly", {
expect_equal(bin_probability(2, 5, 0.5), 0.3125)
expect_equal(bin_probability(55, 100, 0.45), 0.01075277)
expect_equal(bin_probability(2, 5, 0.5), 0.1715322)
})
                 
test_that("bin_probability is a double", {
expect_equal(typeof(bin_probability(2, 5, 0.5)), double)
expect_equal(typeof(bin_probability(55, 100, 0.45)), double)
expect_equal(typeof(bin_probability(2, 5, 0.5)), double)
})
                 
                 
#for bin_distribution()
test_that("trials and prob are numbers", {
expect_equal(typeof(bin_distribution(3, 0.4)), "double")
expect_equal(typeof(bin_distribution("hello", 0.4)), "FALSE")
expect_equal(typeof(bin_distribution(TRUE, 0.4)), "FALSE")
})
                 
test_that("number of columns in bin_distribution is correctly", {
expect_equal(length(bin_distribution(trials = 5, prob = 0.5)), 2)
expect_equal(length(bin_distribution(trials = 10, prob = 0.4)), 2)
expect_equal(length(bin_distribution(trials = 15, prob = 0.7)), 2)
})
                 
test_that("bin_distribution is a df", {
expect_equal(typeof(bin_distribution(trials = 5, prob = 0.5)), df)
expect_equal(typeof(bin_distribution(trials = 10, prob = 0.4)), df)
expect_equal(typeof(bin_distribution(trials = 15, prob = 0.7)), df)
})
                 
                 
#for bin_cumulative()
test_that("trials and prob are numbers", {
expect_equal(typeof(bin_cumulative(3, 0.4)), "double")
expect_equal(typeof(bin_cumulative("hello", 0.4)), "FALSE")
expect_equal(typeof(bin_cumulative(TRUE, 0.4)), "FALSE")
})
                 
test_that("number of columns in bin_cumulative is correctly", {
expect_equal(length(bin_cumulative(trials = 5, prob = 0.5)), 2)
expect_equal(length(bin_cumulative(trials = 10, prob = 0.4)), 2)
expect_equal(length(bin_cumulative(trials = 15, prob = 0.7)), 2)
})
                 
test_that("bin_cumulative is a df", {
expect_equal(typeof(bin_cumulative(trials = 5, prob = 0.5)), df)
expect_equal(typeof(bin_cumulative(trials = 10, prob = 0.4)), df)
expect_equal(typeof(bin_cumulative(trials = 15, prob = 0.7)), df)
})
                 

