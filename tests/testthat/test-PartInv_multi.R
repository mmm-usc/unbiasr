# Create sample output
pimout_eq <- PartInvMulti_we(propsel = .25,
                       weights_item = c(0.008125, 0.008125, 0.008125, 0.008125,
                                        0.044875, 0.044875, 0.044875, 0.044875,
                                        0.117325, 0.117325, 0.117325, 0.117325,
                                        -0.048775, -0.048775, -0.048775, -0.048775,
                                        0.0309, 0.0309, 0.0309, 0.0309),
                       weights_latent = c(0.0325, 0.1795, 0.4693, -0.1951, 0.1236),
                       alpha_r = result[[2]]$alpha, 
                       alpha_f = result[[2]]$alpha,
                       psi_r = result[[2]]$psi, 
                       psi_f = result[[1]]$psi,
                       lambda_r = (result[[2]]$lambda + result[[1]]$lambda) / 2,
                       nu_r = (result[[2]]$nu + result[[1]]$nu) / 2,
                       Theta_r = (result[[2]]$theta + result[[1]]$theta) / 2)

pimout <- PartInvMulti_we(propsel = .25,
                          weights_item = c(0.008125, 0.008125, 0.008125, 0.008125,
                                           0.044875, 0.044875, 0.044875, 0.044875,
                                           0.117325, 0.117325, 0.117325, 0.117325,
                                           -0.048775, -0.048775, -0.048775, -0.048775,
                                           0.0309, 0.0309, 0.0309, 0.0309),
                          # Agreeableness Conscientiousness Extraversion Neuroticism Openness
                          weights_latent = c(0.0325, 0.1795, 0.4693, -0.1951, 0.1236),
                          alpha_r = result[[2]]$alpha,
                          alpha_f = result[[1]]$alpha,
                          psi_r = result[[2]]$psi,
                          psi_f = result[[1]]$psi,
                          lambda_r = result[[2]]$lambda,
                          nu_r = result[[2]]$nu,
                          nu_f = result[[1]]$nu,
                          Theta_r = result[[2]]$theta,
                          Theta_f = result[[1]]$theta)

test_that("PartInvMulti_we() returns a data frame and a graph", {
  pimdf <- vapply(pimout, FUN = inherits, what = c("data.frame"), 
                 FUN.VALUE = logical(1))
  pimplot <- vapply(pimout, FUN = inherits, what = c("recordedplot"), 
                   FUN.VALUE = logical(1))
  expect_true(any(pimdf))
  expect_true(any(pimplot))
})

test_that("Identical selection with the same parameters", {
  expect_equal(pimout_eq$summary[ , 1], 
               pimout_eq$summary[ , 2])
  expect_equal(pimout_eq$summary["Proportion selected", 1], .10)
})

