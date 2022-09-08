# Create sample output

pmix_CESD_r <- 4903/(1903+4903)
lambda_POS_r <- lambda_POS_f <- c(1.00, 1.66, 2.30, 2.29)
nu_POS_r <- c(1.54, 1.36, 1.16, 1.08)
nu_POS_f <- c(0.68, 1.36, 1.16, 1.08)
Theta_POS_r <- diag(c(1.20, 0.81, 0.32, 0.32))
Theta_POS_f <- diag(c(0.72, 0.81, 0.32, 0.32))
alpha_POS_r <- 0
alpha_POS_f <- -0.125
psi_POS_r <- 0.354^2
psi_POS_f <- 0.329^2

CESD_pos <- item_deletion_h(cut_z = 16/60*12, 
                            weights_item = c(rep(1,4)), 
                            weights_latent = 1,
                            alpha_r = alpha_POS_r,
                            alpha_f = alpha_POS_f,
                            psi_r = psi_POS_r,
                            psi_f = psi_POS_f,
                            lambda_r = lambda_POS_r,
                            lambda_f = lambda_POS_f,
                            nu_r = nu_POS_r,
                            nu_f = nu_POS_f,
                            Theta_r = Theta_POS_r, 
                            Theta_f = Theta_POS_f,
                            pmix_ref = pmix_CESD_r, 
                            plot_contour = FALSE,
                            print_formatted = TRUE)



test_that("AI ratio is > 0 for all cells", {
  expect_true(all(CESD_pos$`AI Ratio` > 0))
})
