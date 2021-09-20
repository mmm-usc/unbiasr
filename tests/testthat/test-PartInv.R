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
# Create sample output 2
piout_eq <- PartInv(
  .10,
  kappa_r = 0.5,
  phi_r = 1,
  lambda_r = c(.3, .5, .9, .7, .8),
  tau_r = c(.225, .025, .010, .240, .123),
  Theta_r = diag(.96, 5),
  labels = c("female", "male")
)

test_that("PartInv() returns a data frame and a graph", {
  pidf <- vapply(piout, FUN = inherits, what = c("data.frame"), 
                 FUN.VALUE = logical(1))
  piplot <- vapply(piout, FUN = inherits, what = c("recordedplot"), 
                   FUN.VALUE = logical(1))
  expect_true(any(pidf))
  expect_true(any(piplot))
})

test_that("Identical selection with the same parameters", {
  expect_equal(piout_eq$summary[ , 1], 
               piout_eq$summary[ , 2])
  expect_equal(piout_eq$summary["Proportion selected", 1], .10)
})

test_that("PartInv() handles matrix input", {
  piout_mat <- PartInv(
    .10,
    kappa_r = matrix(0.5),
    kappa_f = matrix(0),
    phi_r = matrix(1),
    lambda_r = matrix(c(.3, .5, .9, .7, .8)),
    tau_r = matrix(c(.225, .025, .010, .240, .123)),
    Theta_r = diag(.96, 5),
    labels = c("female", "male")
  )
  expect_equal(piout[1:4], piout_mat[1:4])
})

test_that("PartInv() handles diagonal input for Theta", {
  piout_diag <- PartInv(
    .10,
    kappa_r = matrix(0.5),
    kappa_f = matrix(0),
    phi_r = matrix(1),
    lambda_r = matrix(c(.3, .5, .9, .7, .8)),
    tau_r = matrix(c(.225, .025, .010, .240, .123)),
    Theta_r = rep(.96, 5),
    labels = c("female", "male")
  )
  expect_equal(piout[1:4], piout_diag[1:4])
})

test_that("PartInv() output passes logical test", {
  psel <- .2
  # Example favoring reference group
  piout1_pstrict <- PartInv(
    psel,
    kappa_r = 0,
    kappa_f = -0.1,
    phi_r = 1.3,
    phi_f = 1.2,
    lambda_r = c(1, .5, .9, .7, .8),
    lambda_f = c(1, .8, .9, .7, .8),
    tau_r = c(.225, .025, .010, .240, .123),
    tau_f = c(.025, .025, .010, .240, .053),
    Theta_r = diag(.96, 5),
    Theta_f = diag(c(1, .65, .75, .9, .8)),
    labels = c("reference", "focal")
  )
  piout1_strict <- PartInv(
    psel,
    kappa_r = 0,
    kappa_f = -0.1,
    phi_r = 1.3,
    phi_f = 1.2,
    lambda_r = c(1, .5, .9, .7, .8),
    tau_r = c(.225, .025, .010, .240, .123),
    Theta_r = diag(.96, 5),
    labels = c("reference", "focal")
  )
  sum_ps <- piout1_pstrict$summary
  sum_s <- piout1_strict$summary
  psel_pstrict <- unlist(sum_ps["Proportion selected", ])
  psel_strict <- unlist(sum_s["Proportion selected", ])
  expect_equal(sum(psel_pstrict), psel * 2)
  expect_equal(sum(psel_strict), psel * 2)
  expect_gt(psel_pstrict[["reference"]] - psel_strict[["reference"]], 0)
  expect_lt(abs(
    sum_ps["Specificity", "reference"] - 
      sum_ps["C (true negative)", "reference"] / 
      (sum_ps["C (true negative)", "reference"] + 
         sum_ps["B (false positive)", "reference"])),
    .0005)
})
