# Create sample output
piout <- PartInv(
  propsel = .10,
  alpha = list(0.5, 0),
  psi =  list(1, 1),
  lambda = list(c(.3, .5, .9, .7, .8), c(.3, .5, .9, .7, .8)),
  nu =  list(c(.225, .025, .010, .240, .123), c(.225, .025, .010, .240, .123)),
  theta =  list(diag(.96, 5), diag(.96, 5)),
  labels = c("female", "male")
)

# Create sample output 2
piout_eq <- PartInv(
  propsel = .10,
  alpha = list(0.5, 0.5),
  psi =  list(1, 1),
  lambda = list(c(.3, .5, .9, .7, .8), c(.3, .5, .9, .7, .8)),
  nu =  list(c(.225, .025, .010, .240, .123), c(.225, .025, .010, .240, .123)),
  theta =  list(diag(.96, 5), diag(.96, 5)),
  labels = c("female", "male")
)

test_that("PartInv() returns a data frame", {
  pidf <- vapply(piout, FUN = inherits, what = c("data.frame"),
                 FUN.VALUE = logical(1))
  expect_true(any(pidf))
})

test_that("plot.PartInv() works successfully", {
  # case where which_result=NULL, uses piout to determine which results are 
  # available
  expect_no_error(plot(piout, labels = c("female", "male")))
  expect_no_error(plot(piout))
  # case where which_result="mi" but mi results were not previously requested
  expect_error(plot(piout, which_result = "mi"))
  piout_with_mi <- PartInv(
    propsel = .10,
    alpha = list(0.5, 0.5),
    psi = list(1, 1),
    lambda = list(c(.3, .5, .9, .7, .8), c(.3, .5, .9, .7, .8)),
    nu = list(c(.225, .025, .010, .240, .123), c(.225, .025, .010, .240, .123)),
    theta = list(diag(.96, 5), diag(.96, 5)),
    labels = c("female", "male"),
    show_mi_result = TRUE
  )
  # case where which_result=mi, and mi results had been requested
  expect_no_error(plot(piout_with_mi, labels = c("female", "male"),
                    which_result = "mi"))
  # case where which_result=NULL, and mi results had been requested
  expect_no_error(plot(piout_with_mi, labels = c("female", "male")))
})

test_that("Identical selection with the same parameters", {
  expect_equal(piout_eq$summary[, 1],
               piout_eq$summary[, 2])
  expect_equal(piout_eq$summary["Proportion selected", 1], .10)
})

# if alpha or alpha_r or kappa_r or cfa_fit not provided
expect_error(PartInv(
  propsel = .10,
  psi = c(1, 1),
  lambda = list(c(.3, .5, .9, .7, .8), c(.3, .5, .9, .7, .8)),
  nu = list(c(.225, .025, .010, .240, .123),c(.225, .025, .010, .240, .123)),
  theta = list(diag(.96, 5), diag(.96, 5)),
  labels = c("female", "male"),
  show_mi_result = TRUE
)
)

test_that("Duplicated results with `show_mi_result = TRUE` when inputting invariant model", {
  piout_eq2 <- PartInv(
    propsel = .10,
    alpha = list(0.5, 0.5),
    psi =  list(1, 1),
    lambda = list(c(.3, .5, .9, .7, .8), c(.3, .5, .9, .7, .8)),
    nu =  list(c(.225, .025, .010, .240, .123), c(.225, .025, .010, .240, .123)),
    theta =  list(diag(.96, 5), diag(.96, 5)),
    labels = c("female", "male"),
    show_mi_result = TRUE
  )
  expect_equal(piout_eq2$summary[, 1:2], piout_eq2$summary_mi)
})

test_that("PartInv() handles matrix input", {
  piout_mat <- suppressWarnings( # warning for deprecated parameters suppressed
    PartInv(
      propsel = .10,
      kappa_r = matrix(0.5),
      kappa_f = matrix(0),
      phi_r = matrix(1),
      lambda_r = matrix(c(.3, .5, .9, .7, .8)),
      tau_r = matrix(c(.225, .025, .010, .240, .123)),
      Theta_r = diag(.96, 5),
      labels = c("female", "male")
    )
  )
  expect_equal(piout[1:4], piout_mat[1:4])
})

test_that("PartInv() handles diagonal input for Theta", {
  piout_diag <- suppressWarnings( # warning for deprecated parameters
    PartInv(
      propsel = .10,
      kappa_r = matrix(0.5),
      kappa_f = matrix(0),
      phi_r = matrix(1),
      lambda_r = matrix(c(.3, .5, .9, .7, .8)),
      tau_r = matrix(c(.225, .025, .010, .240, .123)),
      Theta_r = rep(.96, 5),
      labels = c("female", "male")
    )
  )
  expect_equal(piout[1:4], piout_diag[1:4])
})


test_that("PartInv() issues a deprecation warning for '_r' and '_f' parameters", {
  # Expect the specific warning message from prep_params()
  expect_warning(
    PartInv(
      propsel = .10,
      kappa_r = matrix(0.5),
      kappa_f = matrix(0),
      phi_r = matrix(1),
      lambda_r = matrix(c(.3, .5, .9, .7, .8)),
      tau_r = matrix(c(.225, .025, .010, .240, .123)),
      Theta_r = rep(.96, 5),
      labels = c("female", "male")),
    regexp = "Arguments with suffixes '_r' and '_f' are deprecated\\.",
    fixed = FALSE
  )
})


test_that("PartInv() output passes logical test", {
  psel <- .2
  # Example favoring reference group
  piout1_pstrict <- PartInv(
    propsel = psel,
    alpha = list(0, -0.1),
    psi = list(1.3, 1.2),
    lambda = list(c(1, .5, .9, .7, .8), c(1, .8, .9, .7, .8)),
    nu = list(c(.225, .025, .010, .240, .123), c(.025, .025, .010, .240, .053)),
    theta = list(diag(.96, 5), diag(c(1, .65, .75, .9, .8))),
    labels = c("reference", "focal")
  )
  piout1_strict <- PartInv(
    propsel = psel,
    alpha = list(0, -0.1),
    psi = list(1.3, 1.2),
    lambda = list(c(1, .5, .9, .7, .8), c(1, .8, .9, .7, .8)),
    nu = list(c(.225, .025, .010, .240, .123), c(.225, .025, .010, .240, .123)),
    theta = list(diag(.96, 5), diag(.96, 5)),
    labels = c("reference", "focal")
  )
  sum_ps <- piout1_pstrict$summary
  sum_s <- piout1_strict$summary
  psel_pstrict <- unlist(sum_ps["Proportion selected", 1:2])
  psel_strict <- unlist(sum_s["Proportion selected", 1:2])
  expect_equal(sum(psel_pstrict), psel * 2)
  expect_equal(sum(psel_strict), psel * 2)
  expect_gt(psel_pstrict["reference"] - psel_strict["reference"], 0)
  expect_lt(abs(
    sum_ps["Specificity", "reference"] -
      sum_ps["C (true negative)", "reference"] /
      (sum_ps["C (true negative)", "reference"] +
         sum_ps["B (false positive)", "reference"])),
    .0005)
})

test_that("`show_mi_result = TRUE` works properly", {
  psel <- .2
  lambda <- list(c(1, .5, .9, .7, .8), c(1, .8, .9, .7, .8))
  nu <- list(c(.225, .025, .010, .240, .123), c(.025, .025, .010, .240, .053))
  theta <- list(diag(.96, 5), diag(c(1, .65, .75, .9, .8)))
  # Example favoring reference group
  piout1_pstrict <- PartInv(
    propsel = psel,
    alpha = list(0, -0.1),
    psi = list(1.3, 1.2),
    lambda = lambda,
    nu = nu,
    theta = theta,
    labels = c("reference", "focal"),
    pmix = c(.2, .8),
    show_mi_result = TRUE
  )
  piout1_strict <- PartInv(
    propsel = psel,
    alpha = list(0, -0.1),
    psi = list(1.3, 1.2),
    lambda = list(lambda[[1]] * .2 + lambda[[2]] * .8, 
                  lambda[[1]] * .2 + lambda[[2]] * .8),
    nu = list(nu[[1]] * .2 + nu[[2]] * .8, 
              nu[[1]] * .2 + nu[[2]] * .8),
    theta = list(theta[[1]] * .2 + theta[[2]] * .8, 
                 theta[[1]] * .2 + theta[[2]] * .8),
    pmix = c(.2, .8),
    labels = c("reference", "focal")
  )
  expect_equal(piout1_pstrict$summary_mi, piout1_strict$summary[, 1:2])
})

test_that("`show_mi_result = TRUE` uses same cut_z if specified", {
  cut_score <- 4
  lambda <- list(c(1, .5, .9, .7, .8), c(1, .8, .9, .7, .8))
  nu <- list(c(.225, .025, .010, .240, .123), c(.025, .025, .010, .240, .053))
  theta <- list(diag(.96, 5), diag(c(1, .65, .75, .9, .8)))
  # Example favoring reference group
  piout1_pstrict <- PartInv(
    cut_z = cut_score,
    alpha = list(0, -0.1),
    psi = list(1.3, 1.2),
    lambda = lambda,
    nu = nu,
    theta = theta,
    labels = c("reference", "focal"),
    pmix = c(.2, .8),
    show_mi_result = TRUE
  )
  piout1_strict <- PartInv(
    cut_z = cut_score,
    alpha = list(0, -0.1),
    psi = list(1.3, 1.2),
    lambda = list(lambda[[1]] * .2 + lambda[[2]] * .8, 
                  lambda[[1]] * .2 + lambda[[2]] * .8),
    nu = list(nu[[1]] * .2 + nu[[2]] * .8, 
              nu[[1]] * .2 + nu[[2]] * .8),
    theta = list(theta[[1]] * .2 + theta[[2]] * .8, 
                 theta[[1]] * .2 + theta[[2]] * .8),
    pmix = c(.2, .8),
    labels = c("reference", "focal")
  )
  expect_equal(piout1_pstrict$summary_mi, piout1_strict$summary[, 1:2])
})

test_that("reference_first() works properly", {
  new_params <- reference_first(
    piout$params, labels = c("A", "B"), reference = "B")
  expect_type(new_params$pmix, "double")
})

test_that("Handle pmix_ref properly", {
  piout_pmix_ref <- suppressWarnings( # warning for deprecated parameters
    PartInv(
      propsel = .10,
      alpha = list(0.5, 0),
      psi = list(1, 1),
      lambda = rep(list(c(.3, .5, .9, .7, .8)), 2),
      nu = list(c(.225, .025, .010, .240, .123),
                c(.125, .025, .110, .140, .223)),
      theta = rep(list(diag(.96, 5)), 2),
      labels = c("female", "male"),
      pmix_ref = 0.3
    )
  )
  expect_equal(piout_pmix_ref$params$pmix, c(0.3, 0.7))
})

test_that("AI ratio is > 0 for all cells", {
  CESD_pos <- PartInv(
    cut_z = 16/60 * 12,
    weights_item = rep(1, 4),
    weights_latent = 1,
    alpha = list(0, -0.125),                 
    psi = list(0.354^2, 0.329^2),
    lambda = list(c(1.00, 1.66, 2.30, 2.29),
                  c(1.00, 1.66, 2.30, 2.29)),
    nu = list(c(1.54, 1.36, 1.16, 1.08),
              c(0.68, 1.36, 1.16, 1.08)),
    theta = list(diag(c(1.20, 0.81, 0.32, 0.32)),
                 diag(c(0.72, 0.81, 0.32, 0.32))),
    pmix = rep(4903/(1903 + 4903), 2),
    plot_contour = FALSE
  )
  
  expect_true(all(CESD_pos$`AI Ratio` > 0))
})

