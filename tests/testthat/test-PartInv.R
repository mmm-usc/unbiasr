library(testthat)
source("PartInv.R")

test_that("PartInv() returns a data frame and a graph", {
  # Create sample output
  piout <- PartInv(
    .10,
    kappa_r = 0.5,
    kappa_f = 0,
    phi_r = 1,
    lambda_r = c(.3, .5, .9, .7, .8),
    tau_r = c(.225, .025, .010, .240, .123),
    Theta_r = diag(.96, 5),
    labels = c("female", "male")
  )
  pidf <- vapply(piout, FUN = inherits, what = c("data.frame"), 
                 FUN.VALUE = logical(1))
  piplot <- vapply(piout, FUN = inherits, what = c("recordedplot"), 
                   FUN.VALUE = logical(1))
  expect_true(any(pidf))
  expect_true(any(piplot))
})

test_that("Identical selection with the same parameters", {
  # Create sample output
  piout <- PartInv(
    .10,
    kappa_r = 0.5,
    phi_r = 1,
    lambda_r = c(.3, .5, .9, .7, .8),
    tau_r = c(.225, .025, .010, .240, .123),
    Theta_r = diag(.96, 5),
    labels = c("female", "male")
  )
  expect_equal(piout$summary[ , 1], 
               piout$summary[ , 2])
  expect_equal(piout$summary["Proportion selected", 1], .10)
})
