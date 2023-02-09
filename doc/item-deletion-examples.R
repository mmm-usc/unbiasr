## ---- include = FALSE, warning=FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(PartInvShinyUI)
library(lavaan)

## -----------------------------------------------------------------------------
# Zhang et al. (2011) BMC Medical Research Methodology, Tables 3-4 on page 7

# Reference group: Chinese, Focal group: Dutch
# 5:2 ratio, mixing proportion of 5/7

pmix_CESD_r <- 5/7

# Unstandardized factor loadings
lambda_SOM_r <- lambda_SOM_f <- c(1.00, 1.03, 1.18, 1.29, 1.07, 1.02, 1.26)
lambda_DEP_r <- lambda_DEP_f <- c(1.00, 1.13, 0.82, 0.91, 1.11, 0.92, 1.06)
lambda_POS_r <- lambda_POS_f <- c(1.00, 1.66, 2.30, 2.29)
lambda_INT_r <- lambda_INT_f <- c(1.00, 0.94)

# Unstandardized intercepts
nu_SOM_r <- c(0.69, 0.56, 0.78, 0.81, 0.88, 0.74, 0.70)
nu_SOM_f <- c(0.69, 0.56, 0.78, 0.80, 0.88, 0.74, 0.70)
nu_DEP_r <- c(0.52, 0.55, 0.57, 0.42, 0.57, 0.50, 0.56)
nu_DEP_f <- c(0.52, 0.55, 0.30, 0.42, 0.57, 0.50, 0.56)
nu_POS_r <- c(1.54, 1.36, 1.16, 1.08)
nu_POS_f <- c(0.68, 1.36, 1.16, 1.08)
nu_INT_r <- nu_INT_f <- c(0.44, 0.41)

# Unstandardized uniqueness
Theta_SOM_r <- Theta_SOM_f <- diag(c(0.45, 0.37, 0.39, 0.40, 0.57, 0.51, 0.37))
Theta_DEP_r <- diag(c(0.29, 0.30, 0.41, 0.31, 0.29, 0.27, 0.24))
Theta_DEP_f <- diag(c(0.29, 0.13, 0.09, 0.14, 0.29, 0.27, 0.24))
Theta_POS_r <- diag(c(1.20, 0.81, 0.32, 0.32))
Theta_POS_f <- diag(c(0.72, 0.81, 0.32, 0.32))
Theta_INT_r <- diag(c(0.19, 0.23))
Theta_INT_f <- diag(c(0.19, 0.08))

# Latent mean differences
alpha_SOM_r <- 0
alpha_DEP_r <- 0
alpha_POS_r <- 0
alpha_INT_r <- 0
alpha_SOM_f <- -0.261
alpha_DEP_f <- -0.259
alpha_POS_f <- -0.125
alpha_INT_f <- -0.323

# Latent mean variances
psi_SOM_r <- 0.482^2
psi_SOM_f <- 0.324^2
psi_DEP_r <- 0.570^2
psi_DEP_f <- 0.318^2
psi_POS_r <- 0.354^2
psi_POS_f <- 0.329^2
psi_INT_r <- 0.574^2
psi_INT_f <- 0.184^2

## ----ex1, include=FALSE-------------------------------------------------------
# ex1 <- item_deletion_h(weights_item = c(1, 1, 1, 1), 
#                               propsel = .14, n_dim = 1,  
#                               alpha_r = 0,
#                               alpha_f =-0.125,
#                               psi_r = 0.354^2,
#                               psi_f = 0.329^2,
#                               lambda_r = c(1.00, 1.66, 2.30, 2.29),
#                               lambda_f = c(1.00, 1.66, 2.30, 2.29),
#                               nu_r = c(1.54, 1.36, 1.16, 1.08),
#                               nu_f = c(0.68, 1.36, 1.16, 1.08),
#                               Theta_r = diag(c(1.20, 0.81, 0.32, 0.32)), 
#                               Theta_f = diag(c(0.72, 0.81, 0.32, 0.32)),
#                               weights_latent = 1,
#                               plot_contour = FALSE,
#                               return_detailed = TRUE,
#                               pmix_ref = 5/7)

## ----CESD_full, include=FALSE-------------------------------------------------
# lambda_CESD_r <- rbind(cbind(lambda_SOM_r, rep(0, 7), rep(0, 7), rep(0, 7)),
#                        cbind(rep(0, 7), lambda_DEP_r, rep(0, 7), rep(0, 7)),
#                        cbind(rep(0, 4), rep(0, 4), lambda_POS_r, rep(0, 4)),
#                        cbind(rep(0, 2), rep(0, 2), rep(0, 2), lambda_INT_r))
# lambda_CESD_f <- rbind(cbind(lambda_SOM_f, rep(0, 7), rep(0, 7), rep(0, 7)),
#                        cbind(rep(0, 7), lambda_DEP_f, rep(0, 7), rep(0, 7)),
#                        cbind(rep(0, 4), rep(0, 4), lambda_POS_f, rep(0, 4)),
#                        cbind(rep(0, 2), rep(0, 2), rep(0, 2), lambda_INT_f))
# psi_CESD_r <- as.matrix(c(psi_SOM_r, psi_DEP_r, psi_POS_r, psi_INT_r))
# psi_CESD_f <- as.matrix(c(psi_SOM_f, psi_DEP_f, psi_POS_f, psi_INT_f))
# alpha_CESD_r <- as.matrix(c(alpha_SOM_r, alpha_DEP_r, alpha_POS_r, alpha_INT_r))
# alpha_CESD_f <- as.matrix(c(alpha_SOM_f, alpha_DEP_f, alpha_POS_f, alpha_INT_f))

# Theta_CESD_r <- diag(c(0.45, 0.37, 0.39, 0.40, 0.57, 0.51, 0.37, #SOM
#                   0.29, 0.30, 0.41, 0.31, 0.29, 0.27, 0.24, #DEP
#                   1.20, 0.81, 0.32, 0.32, #POS
#                   0.19, 0.23 #INT
#                   ))
# Theta_CESD_f <- diag(c(0.45, 0.37, 0.39, 0.40, 0.57, 0.51, 0.37, #SOM
#                   0.29, 0.13, 0.09, 0.14, 0.29, 0.27, 0.24, #DEP
#                   0.72, 0.81, 0.32, 0.32, #POS
#                   0.23, 0.08 #INT
#                   ))
# nu_CESD_r <- as.matrix(c(nu_SOM_r, nu_DEP_r, nu_POS_r, nu_INT_r))
# nu_CESD_f <- as.matrix(c(nu_SOM_f, nu_DEP_f, nu_POS_f, nu_INT_f))

# CESD_full <- item_deletion_h(weights_item = c(rep(1, 20)), 
#                             propsel = .14, n_dim = 1,  
#                             alpha_r = alpha_CESD_r,
#                             alpha_f = alpha_CESD_f,
#                             psi_r = psi_CESD_r,
#                             psi_f = psi_CESD_f,
#                             lambda_r = lambda_CESD_r,
#                             lambda_f = lambda_CESD_f,
#                             nu_r = nu_CESD_r,
#                             nu_f = nu_CESD_f,
#                             Theta_r = Theta_CESD_r, 
#                             Theta_f = Theta_CESD_f,
#                             weights_latent = 1,
#                             plot_contour = FALSE,
#                             return_detailed = FALSE,
#                             pmix_ref = pmix_CESD_r)
# CESD_full

## -----------------------------------------------------------------------------
#Positive Affect Factor
#Items: good (biased), hopeful, happy, enjoyed

CESD_pos <- item_deletion_h(weights_item = c(rep(1,4)), 
                              propsel = .14, n_dim = 1,  
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
                              weights_latent = 1,
                              plot_contour = FALSE,
                              return_detailed = FALSE,
                              pmix_ref = pmix_CESD_r, 
                            biased_items_only=TRUE
                           # user_specified_items = c(1,2)
                           )
CESD_pos 

# Depressive Affect Factor 
# Items: blues, depressed, lonely, crying, sad, failure, fearful
CESD_dep <- item_deletion_h(weights_item = c(rep(1,7)), 
                              propsel = .14, n_dim = 1,  
                              alpha_r = alpha_DEP_r,
                              alpha_f = alpha_DEP_f,
                              psi_r = psi_DEP_r,
                              psi_f = psi_DEP_f,
                              lambda_r = lambda_DEP_r,
                              lambda_f = lambda_DEP_f,
                              nu_r = nu_DEP_r,
                              nu_f = nu_DEP_f,
                              Theta_r = Theta_DEP_r, 
                              Theta_f = Theta_DEP_f,
                              weights_latent = 1,
                              plot_contour = FALSE,
                              return_detailed = FALSE,
                              pmix_ref = pmix_CESD_r, 
                            biased_items_only = TRUE)
CESD_dep # items 2, 3, 4 are biased

# Somatic Complaints 
# Items: bothered, appetite, mind, effort, sleep, talk, get going
CESD_som <- item_deletion_h(weights_item = c(rep(1,7)), 
                              propsel = .14, n_dim = 1,  
                              alpha_r = alpha_SOM_r,
                              alpha_f = alpha_SOM_f,
                              psi_r = psi_SOM_r,
                              psi_f = psi_SOM_f,
                              lambda_r = lambda_SOM_r,
                              lambda_f = lambda_SOM_f,
                              nu_r = nu_SOM_r,
                              nu_f = nu_SOM_f,
                              Theta_r = Theta_SOM_r, 
                              Theta_f = Theta_SOM_f,
                              weights_latent = 1,
                              plot_contour = FALSE,
                              return_detailed = TRUE,
                              pmix_ref = pmix_CESD_r)
CESD_som #  item 4 is biased (?)

# Interpersonal Problems
# Items: unfriendly, dislike
CESD_int <- item_deletion_h(weights_item = c(rep(1,2)), 
                              propsel = .14, n_dim = 1,  
                              alpha_r = alpha_INT_r,
                              alpha_f = alpha_INT_f,
                              psi_r = psi_INT_r,
                              psi_f = psi_INT_f,
                              lambda_r = lambda_INT_r,
                              lambda_f = lambda_INT_f,
                              nu_r = nu_INT_r,
                              nu_f = nu_INT_f,
                              Theta_r = Theta_INT_r, 
                              Theta_f = Theta_INT_f,
                              weights_latent = 1,
                              plot_contour = FALSE,
                              return_detailed = TRUE,
                              pmix_ref = pmix_CESD_r)
CESD_int #item 2 is biased

## -----------------------------------------------------------------------------
# Worry subscale of the test anxiety inventory
ex2 <- item_deletion_h(weights_item = c(1, 1, 1, 1, 1, 1, 1, 1), 
                              propsel = 0.1000001,#"90% cutpoint"
                              #cut_z= 23.29952,
                             n_dim = 1,  
                             alpha_r = 0,
                             alpha_f =-0.126,
                             psi_r = 0.544,
                             psi_f = 0.477,
                             lambda_r = c(0.836, 1, 0.904, 0.808, 0.903,
                                             0.960, 0.934, 0.934),
                             lambda_f = c(0.836, 1, 1.111, 1.001, 0.903, 
                                             0.960, 0.934, 0.934),
                             nu_r = c(2.114, 2.064, 1.901, 2.004, 2.144,
                                        1.985, 2.179, 2.230),
                             nu_f= c(2.114, 2.064, 1.880, 1.985, 2.144,
                                        1.985, 2.179, 2.230),
                             Theta_r = diag(c(.517, .523, .631, .585, .481,
                                                .469,.551,.572)),
                             Theta_f = diag(c(.514, .407, .371, .475, .392,
                                                .335,.454,.457)),
                             weights_latent = 1,
                             plot_contour = FALSE,
                             return_detailed = FALSE, pmix_ref = 0.5)

## -----------------------------------------------------------------------------
ex2

## ----read data, include  = FALSE----------------------------------------------
data <- read.table("IPIPFFM.dat", header = TRUE)
head(data)
male <- data[data$sex == 1, ]
female <- data[data$sex == 2, ]

## ----specify and fit model----------------------------------------------------
model <- 'A =~  a2 + a5  + a7 + a9
          C =~  c3 + c4 + c8 + c9
          E =~ e1 + e4 + e6 + e7
          N =~ n1 + n2 + n6 + n8
          O =~ i2 + i8 + i9 + i10
          a2 ~~ a5
          e4 ~~ e7  
          i2 ~~ i10
          i8 ~~ i9
          a9 ~~ i9
          c3 ~~ e6
          a2 ~~ e7
          e7 ~~ n2'
fit_strict <- cfa(model, data = data, group = "sex",
                  group.equal = c("loadings", "Intercepts", "residuals"),
                  group.partial = c("e6 ~ 1", "n1 ~ 1", "n2 ~ 1", "a2 ~ 1", 
                                    #items  1 (A2), 7 (C8), 11 (E6), 13 (N1), 14 (N2)
                                    "n2 ~~ n2", "n1 ~~ n1", "c8 ~~ c8"),
                  estimator = "MLR", std.lv = TRUE)
result <- lavInspect(fit_strict, what = "est")

## ----IPIPFFM item deletion----------------------------------------------------
ex3 <- item_deletion_h(propsel=0.25,
              weights_item = c(3.1385, 3.1385, 3.1385, 3.1385,
                                8.3203, 8.3203, 8.3203, 8.3203,
                                5.1586, 5.1586, 5.1586, 5.1586,
                                -6.5870, -6.5870, -6.5870, -6.5870,
                                1.7957, 1.7957, 1.7957, 1.7957), 
              n_dim = 5, 
              alpha_r = result[[2]]$alpha,
              alpha_f = result[[1]]$alpha,
              psi_r = result[[2]]$psi,
              psi_f = result[[1]]$psi,
              lambda_r = result[[2]]$lambda,
              lambda_f = result[[1]]$lambda,
              nu_r = result[[2]]$nu,
              nu_f = result[[1]]$nu,
              Theta_r = result[[2]]$theta,
              Theta_f = result[[1]]$theta,
              weights_latent = c(0.12553907, 0.33281060, 0.20634298, 
                                 -0.26347936, 0.07182799),
              plot_contour = FALSE, 
              return_detailed = FALSE,
              user_specified_items = c(1,7,11,13,14))

## -----------------------------------------------------------------------------
ex3

