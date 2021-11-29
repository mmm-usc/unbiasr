lambda1 <- c(1, .5, .9, .7, .8)
lambda2 <- c(1, .8, .9, .7, .8)
tau1 <- c(.225, .025, .010, .240, .123)
tau2 <- c(.025, .025, .010, .240, .053)

.weighted_average_list(list(lambda1, lambda2),
                       weights = c(.7, .3))

.weighted_average_list(list(tau1, tau2),
                       weights = c(.7, .3))

test_that("Weighted averages computed correctly", {
  expect_equal(.weighted_average_list(list(lambda1, lambda2),
                                      weights = c(.7, .3)),
               lambda1 * .7 + lambda2 * .3)
})

# Check when using weights not sum to one
test_that("Weighted averages computed correctly with unscaled weights", {
  w1 <- .weighted_average_list(list(tau1, tau2),
                               weights = c(.7, .3))
  w2 <- .weighted_average_list(list(tau1, tau2),
                               weights = c(350, 150))
  expect_equal(w1, w2)
})
