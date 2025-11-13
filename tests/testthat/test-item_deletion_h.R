

test_that("item_deletion_h() handles matrix input", {

  CESD_v <- PartInv(
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
    pmix = c(4903/(1903 + 4903), 1 - (4903/(1903 + 4903)))
  )
  CESD_mat <- PartInv(
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
    pmix = c(4903/(1903 + 4903), 1 - (4903/(1903 + 4903)))
  )
  
  out_vec <- item_deletion_h(CESD_v)
  out_mat <- item_deletion_h(CESD_mat)

  expect_equal(out_vec, out_mat)
})

test_that("item_deletion_h() is backwards compatible", {
  CESD_v <- PartInv(
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
    pmix = c(4903/(1903 + 4903), 1 - (4903/(1903 + 4903)))
  )
  out_vec <- item_deletion_h(CESD_v)
  
  out_old <- item_deletion_h(
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
     pmix = c(4903/(1903 + 4903), 1 - (4903/(1903 + 4903))))

  expect_equal(out_vec, out_old)
})

test_that("item_deletion_h() ", {
  pmix_CESD_r <<- 4903/(1903+4903)
  
  lambda_SOM_r <<- c(1.00, 1.03, 1.18, 1.29, 1.07, 1.02, 1.26)
  lambda_SOM_f <<- c(1.00, 1.03, 1.18, 1.29, 1.07, 1.02, 1.26)
  lambda_DEP_r <<- c(1.00, 1.13, 0.82, 0.91, 1.11, 0.92, 1.06)
  lambda_DEP_f <<- c(1.00, 1.13, 0.82, 0.91, 1.11, 0.92, 1.06)
  lambda_POS_r <<- lambda_POS_f <- c(1.00, 1.66, 2.30, 2.29)
  lambda_INT_r <<- lambda_INT_f <- c(1.00, 0.94)
  
  nu_SOM_r <<- c(0.69, 0.56, 0.78, 0.81, 0.88, 0.74, 0.70)
  nu_SOM_f <<- c(0.69, 0.56, 0.78, 0.80, 0.88, 0.74, 0.70)
  nu_DEP_r <<- c(0.52, 0.55, 0.57, 0.42, 0.57, 0.50, 0.56)
  nu_DEP_f <<- c(0.52, 0.55, 0.30, 0.42, 0.57, 0.50, 0.56)
  nu_POS_r <<- c(1.54, 1.36, 1.16, 1.08)
  nu_POS_f <<- c(0.68, 1.36, 1.16, 1.08)
  nu_INT_r <<- nu_INT_f <- c(0.44, 0.41)
  
  Theta_SOM_r <<- diag(c(0.45, 0.37, 0.39, 0.40, 0.57, 0.51, 0.37))
  Theta_SOM_f <<- diag(c(0.45, 0.37, 0.39, 0.40, 0.57, 0.51, 0.37))
  Theta_DEP_r <<- diag(c(0.29, 0.30, 0.41, 0.31, 0.29, 0.27, 0.24))
  Theta_DEP_f <<- diag(c(0.29, 0.13, 0.09, 0.14, 0.29, 0.27, 0.24))
  Theta_POS_r <<- diag(c(1.20, 0.81, 0.32, 0.32))
  Theta_POS_f <<- diag(c(0.72, 0.81, 0.32, 0.32))
  Theta_INT_r <<- diag(c(0.19, 0.23))
  Theta_INT_f <<- diag(c(0.19, 0.08))
  
  alpha_SOM_r <<- alpha_DEP_r <- alpha_POS_r <- alpha_INT_r <- 0
  alpha_SOM_f <<- -0.261
  alpha_DEP_f <<- -0.259
  alpha_POS_f <<- -0.125
  alpha_INT_f <<- -0.323
  
  psi_SOM_r <<- 0.482^2
  psi_SOM_f <<- 0.324^2
  psi_DEP_r <<- 0.570^2
  psi_DEP_f <<- 0.318^2
  psi_POS_r <<- 0.354^2
  psi_POS_f <<- 0.329^2
  psi_INT_r <<- 0.574^2
  psi_INT_f <<- 0.184^2
  
  lambda_CESD_r <<- 
    rbind(cbind(lambda_SOM_r, rep(0, 7), rep(0, 7), rep(0, 7)), 			
          cbind(rep(0, 7), lambda_DEP_r, rep(0, 7), rep(0, 7)),
          cbind(rep(0, 4), rep(0, 4), lambda_POS_r, rep(0, 4)),
          cbind(rep(0, 2), rep(0, 2), rep(0, 2), lambda_INT_r))
  lambda_CESD_f <<- 
    rbind(cbind(lambda_SOM_f, rep(0, 7), rep(0, 7), rep(0, 7)),
          cbind(rep(0, 7), lambda_DEP_f, rep(0, 7), rep(0, 7)),
          cbind(rep(0, 4), rep(0, 4), lambda_POS_f, rep(0, 4)),
          cbind(rep(0, 2), rep(0, 2), rep(0, 2), lambda_INT_f))      
  
  psi_CESD_r <<- as.matrix(c(psi_SOM_r, psi_DEP_r, psi_POS_r, psi_INT_r))
  psi_CESD_f <<- as.matrix(c(psi_SOM_f, psi_DEP_f, psi_POS_f, psi_INT_f))
  
  alpha_CESD_r <<- as.matrix(c(alpha_SOM_r, alpha_DEP_r, alpha_POS_r, 
                              alpha_INT_r))
  alpha_CESD_f <<- as.matrix(c(alpha_SOM_f, alpha_DEP_f, alpha_POS_f, 
                              alpha_INT_f))
  
  Theta_CESD_r <<- diag(c(0.45, 0.37, 0.39, 0.40, 0.57, 0.51, 0.37, #SOM
                         0.29, 0.30, 0.41, 0.31, 0.29, 0.27, 0.24, #DEP
                         1.20, 0.81, 0.32, 0.32, #POS
                         0.19, 0.23)) #INT
  Theta_CESD_f <<- diag(c(0.45, 0.37, 0.39, 0.40, 0.57, 0.51, 0.37, #SOM
                         0.29, 0.13, 0.09, 0.14, 0.29, 0.27, 0.24, #DEP
                         0.72, 0.81, 0.32, 0.32, #POS
                         0.23, 0.08)) #INT                  
  
  # Intercepts for the full CES-D scale
  nu_CESD_r <<- as.matrix(c(nu_SOM_r, nu_DEP_r, nu_POS_r, nu_INT_r))
  nu_CESD_f <<- as.matrix(c(nu_SOM_f, nu_DEP_f, nu_POS_f, nu_INT_f))
  
  # From Miller et al. (1997)
  corr_CESD <<- matrix(c(1, 0.93, 0.58, 0.85, 0.93, 1, 0.64, 0.88,
                        0.61, 0.61, 1, 0.55, 0.97, 0.93, 0.63, 1), 
                      nrow = 4, ncol = 4, byrow = TRUE)
  # Compute an estimate for the variance-covariance matrix
  S <<- as.matrix(sqrt(psi_CESD_f) * (1 - pmix_CESD_r) + 
                   sqrt(psi_CESD_r) * pmix_CESD_r)
  
  # Latent mean variance-covariance matrix for the full CES-D scale
  psi_cesd <<- diag(S) * corr_CESD * diag(S)

  # Item deletion on the 20-item CES-D scale
  out <- item_deletion_h(
    cut_z = 16,
    weights_item = c(rep(1, 20)),
    weights_latent = c(7, 7, 4, 2),
    alpha_r = alpha_CESD_r,
    alpha_f =  alpha_CESD_f,
    psi_r = psi_cesd,
    psi_f = psi_cesd,
    lambda_r = lambda_CESD_r,
    lambda_f = lambda_CESD_f,
    nu_r = nu_CESD_r,
    nu_f = nu_CESD_f,
    Theta_r = Theta_CESD_r,
    Theta_f = Theta_CESD_f,
    n_dim = 4,
    n_i_per_dim = c(7, 7, 4, 2),
    pmix_ref = pmix_CESD_r,
    labels = c("Chinese", "Dutch")) 
  AI_Dutch <- c(0.8988531553, 0.8991704599, 0.8944971793, 0.9229628496, 0.8955763440,
                0.9752418908, 0.8983655197, 0.8995102810)
  expect_equal(as.numeric(out$AI), AI_Dutch)
  
  out2 <- item_deletion_h(
    cut_z = 16,
    weights_item = c(rep(1, 20)),
    weights_latent = c(7, 7, 4, 2),
    alpha = list(alpha_CESD_r, alpha_CESD_f),
    psi = list(psi_cesd, psi_cesd),
    lambda = list(lambda_CESD_r,lambda_CESD_f),
    nu = list(nu_CESD_r, nu_CESD_f),
    theta = list(Theta_CESD_r, Theta_CESD_f),
    n_dim = 4,
    n_i_per_dim = c(7, 7, 4, 2),
    pmix = c(pmix_CESD_r, 1 - pmix_CESD_r),
    labels = c("Chinese", "Dutch")) 
  
  expect_equal(as.numeric(out2$AI), AI_Dutch)
  expect_equal(out, out2)
  
  
  
  CESD_partinv <- PartInv(
    cut_z = 16,
    weights_item = c(rep(1, 20)),
    weights_latent = c(7, 7, 4, 2),
    alpha = list(alpha_CESD_r, alpha_CESD_f),
    psi = list(psi_cesd, psi_cesd),
    lambda = list(lambda_CESD_r,lambda_CESD_f),
    nu = list(nu_CESD_r, nu_CESD_f),
    theta = list(Theta_CESD_r, Theta_CESD_f),
    pmix = c(pmix_CESD_r, 1 - pmix_CESD_r),
    labels = c("Chinese", "Dutch"))
  
  out_new <- item_deletion_h(
    CESD_partinv, 
    item_which_dim = c(rep(1, 7), rep(2, 7), rep(3, 4), rep(4, 2)))
  
  expect_equal(out2, out_new)
  
  # check if item_deletion_h works properly if a PartInv object is passed along
  # with n_i_per_dim instead of item_which_dim
  out_new2 <- item_deletion_h(
    CESD_partinv, 
    n_i_per_dim = c(7, 7, 4, 2))
  
  expect_equal(out_new, out_new2)
  
})

### Helper functions
test_that("redistribute_weights2 works properly", {
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
