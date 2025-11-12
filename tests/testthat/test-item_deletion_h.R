# Create sample output

test_that("redistribute_weights works properly", {
  w <- rep(1, 20)
  new_w <- redistribute_weights(w, del_i = 8)
  expect_length(new_w, 20)
  expect_equal(redistribute_weights2(w, del_i = 8)[1], 20 / 19)
  w2 <- 1:9
  new_w2 <- redistribute_weights2(w2, del_i = 8)
  expect_equal(new_w2[8], 0)
  expect_equal(new_w2[9], 9 + 8 * 9 / sum(1:7, 9))
  w3 <- w
  new_w3 <- redistribute_weights2(w3, del_i = 8, item_which_dim = rep(1:4, c(7, 7, 4, 2)))
  expect_equal(new_w3[7], 1)
  expect_equal(new_w3[9], 1 + 1 / 6)
})






test_that("item_deletion_h() handles matrix input", {
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
    pmix = c(4903 / (1903 + 4903), 1 - 4903 / (1903 + 4903)),
    plot_contour = FALSE
  )
  CESD_pos_mat <- PartInv(
    cut_z = 16 / 60 * 12,
    weights_item = c(rep(1, 4)),
    weights_latent = 1,
    alpha = list(matrix(0), matrix(-0.125)),
    psi = list(matrix(0.354^2), matrix(0.329^2)),
    lambda = list(matrix(c(1.00, 1.66, 2.30, 2.29)), 
                  matrix(c(1.00, 1.66, 2.30, 2.29))),
    nu = list(matrix(c(1.54, 1.36, 1.16, 1.08)), 
              matrix(c(0.68, 1.36, 1.16, 1.08))),
    theta = list(diag(c(1.20, 0.81, 0.32, 0.32)), 
                 diag(c(0.72, 0.81, 0.32, 0.32))),
    pmix = c(4903 / (1903 + 4903), 1 - 4903 / (1903 + 4903)),
    plot_contour = FALSE
  )
  out_vec <- item_deletion_h(CESD_pos)
  out_mat <- item_deletion_h(CESD_pos_mat)
  
  # outputs should be equivalent aside from the function calls, remove these
  # from params
  out_vec$delete_one_outputs <- lapply(out_vec$delete_one_outputs, function(x) {
    x$params$functioncall <- NULL
    x
  })
  out_mat$delete_one_outputs <- lapply(out_mat$delete_one_outputs, function(x) {
    x$params$functioncall <- NULL
    x
  })
  expect_equal(out_vec, out_mat)
})

### Helper functions
test_that("redistribute_weights() is working properly", {
  error_ex <- c(1:12)
  one_dim_w <- c(1:7)
  multi_eq_w <- c(1:9)
  multi_uneq_w <- c(1:12)

  # checking dimension errors
  # expect_error(redistribute_weights(
  #   error_ex,
  #   n_dim = -3,
  #   n_i_per_dim = c(3, 6, 3),
  #   del_i = 2
  # ))
  # expect_error(redistribute_weights(error_ex, n_dim = 5, del_i = 2))

  # single dimension
  expect_equal(sum(one_dim_w),
               sum(redistribute_weights2(one_dim_w, del_i = 2)))
  # expect_equal(
  #   sum(one_dim_w),
  #   sum(redistribute_weights(one_dim_w, n_dim = 1, n_i_per_dim = 7, del_i = 2))
  # )
  # multidimensional, equal number of items in each dim
  new_multi_eq_w <- redistribute_weights2(
    multi_eq_w, del_i = 2, item_which_dim = rep(1:3, each = 3))
  expect_equal(new_multi_eq_w, c(1.5, 0, 4.5, 4:9))
  expect_equal(sum(multi_eq_w), sum(new_multi_eq_w))

  # multidimensional, unequal number of items in dimensions
  expect_equal(
    sum(multi_uneq_w),
    sum(redistribute_weights2(
      multi_uneq_w,
      del_i = 2,
      item_which_dim = rep(1:3, c(3, 6, 3))
    ))
  )
})


test_that("determine_biased_items() can identify biased items", {
  lambda_matrix <- matrix(0, nrow = 5, ncol = 2)
  lambda_matrix[1:2, 1] <- c(.322, .655)
  lambda_matrix[3:5, 2] <- c(.398, .745, .543)
  lambda_matrix2 <- lambda_matrix
  lambda_matrix2[1, 1] <- c(.422)
  expect_equal(c(2, 3, 4),
               determine_biased_items(lambda_r = lambda_matrix,
                                      lambda_f = lambda_matrix,
                                      nu_r = c(.225, .025, .010, .240, .125),
                                      nu_f = c(.225, -.05, .240, -.025, .125),
                                      Theta_r = diag(1, 5),
                                      Theta_f = diag(c(1, .95, .80, .75, 1))))
  expect_equal(2, determine_biased_items(lambda_r = lambda_matrix,
                                         lambda_f = lambda_matrix,
                                         nu_r = c(.225, .025, .010, .240, .125),
                                         nu_f =  c(.225, .025, .010, .240, .125),
                                         Theta_r = diag(1, 5),
                                         Theta_f = diag(c(1, .95, 1, 1, 1))))
  expect_equal(1, determine_biased_items(lambda_r = lambda_matrix,
                                         lambda_f = lambda_matrix2,
                                         nu_r = c(.225, .025, .010, .240, .125),
                                         nu_f =  c(.225, .025, .010, .240, .125),
                                         Theta_r = diag(1, 5),
                                         Theta_f = diag(1, 5)))
})


# more example outputs
propsel <- 0.72
cut_z <- NULL
weights_item <- c(rep(1,4))
weights_latent <- 1
pmix_ref <- 4903/(1903+4903)
lambda_r <- lambda_f <- c(1.00, 1.66, 2.30, 2.29)
lambda_str <- lambda_f * (1 - pmix_ref) + lambda_r * pmix_ref
nu_r <- c(1.54, 1.36, 1.16, 1.08)
nu_f <- c(0.68, 1.36, 1.16, 1.08)
nu_str <- nu_f * (1 - pmix_ref) + nu_r * pmix_ref
theta_r <- diag(c(1.20, 0.81, 0.32, 0.32))
theta_f <- diag(c(0.72, 0.81, 0.32, 0.32))
theta_str <- theta_f * (1 - pmix_ref) + theta_r * pmix_ref
alpha_r <- 0
alpha_f <- -0.125
psi_r <- 0.354^2
psi_f <- 0.329^2
labels <- c("Reference", "Focal")
n_dim <- 1
n_i_per_dim <- NULL
print_formatted <- TRUE
user_specified_items <- 2
delete_one_cutoff <- NULL
plot_contour <- FALSE

ex_strict <- PartInv(propsel = propsel,
                     cut_z = cut_z,
                     weights_item = weights_item,
                     weights_latent = weights_latent,
                     alpha = list(alpha_r, alpha_f),
                     psi = list(psi_r, psi_f),
                     lambda = list(lambda_str, lambda_str),
                     nu = list(nu_str, nu_str),
                     theta = list(theta_str, theta_str),
                     pmix = c(pmix_ref, 1 - pmix_ref),
                     plot_contour = plot_contour,
                     labels = c("Reference", "Focal"), show_mi_result = TRUE)
ex_partial <- PartInv(propsel = propsel,
                      cut_z = cut_z,
                      weights_item = weights_item,
                      weights_latent = weights_latent,
                      alpha = list(alpha_r, alpha_f),
                      psi = list(psi_r, psi_f),
                      lambda = list(lambda_r, lambda_f),
                      nu = list(nu_r, nu_f),
                      theta = list(theta_r, theta_f),
                      pmix = c(pmix_ref, 1 - pmix_ref),
                      plot_contour = plot_contour,
                      labels = c("Reference", "Focal"))
test_that("cohens_h() computed correctly for comparing the reference with Efocal", {
  out <- c(0.104203486, 0.166642094, -0.101844057, -0.180318247,  0.200493093,
           -0.161048764, 0.211048367, -0.373602451)
  expect_equal(out, cohens_h(ex_partial$summary$Reference,
                             ex_partial$summary$`E_R(Focal)`))

})
