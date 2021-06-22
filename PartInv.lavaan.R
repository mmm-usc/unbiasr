library(lavaan)
HS.model <- '  visual =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
fit <- cfa(HS.model, 
           data = HolzingerSwineford1939, 
           group = "school", 
           group.equal = c("loadings", "intercepts"), 
           group.partial = c("x7 ~ 1", "x5 ~ 1", "x3 ~ 1"))
summary(fit)

mean1 <- colMeans(HolzingerSwineford1939[
  which(HolzingerSwineford1939$school == "Pasteur"), paste0("x", 1:9)])
cov1 <- cov(HolzingerSwineford1939[
  which(HolzingerSwineford1939$school == "Pasteur"), paste0("x", 1:9)])
fit_sum <- cfa(HS.model, 
               sample.cov = list(cov1, cov2),
               sample.mean = list(mean1, mean2),
               group.equal = c("loadings", "intercepts"), 
               group.partial = c("x7 ~ 1", "x5 ~ 1", "x3 ~ 1"))

source("PartInv.R")

# HS.model2 <- '  visual =~ NA * x1 + x2 + x3
#                textual =~ NA * x4 + x5 + x6
#                  speed =~ NA * x7 + x8 + x9 
#                 visual ~~ c(1, NA) * visual
#                textual ~~ c(1, NA) * textual 
#                  speed ~~ c(1, NA) * speed
#                 visual ~ c(0, NA) * 1 
#                textual ~ c(0, NA) * 1 
#                  speed ~ c(0, NA) * 1 '
# fit2 <- cfa(HS.model2, 
#             data = HolzingerSwineford1939, 
#             group = "school", 
#             group.equal = c("loadings", "intercepts"))
# summary(fit2)

PartInv.lavaan <- function(propsel, object, reference = 1, which_fac = 1,
                           pmix_ref = 0.5, plot_contour = TRUE, 
                           force_strict = FALSE, ...) {
  library(lavaan)  # load `lavaan` package
  stopifnot(object@pta$ngroups == 2, reference %in% c(1, 2))
  nfacs <- unlist(object@pta$nfac)
  stopifnot(diff(nfacs) == 0, nfacs[1] >= which_fac)
  focal <- 3 - reference
  est <- lavInspect(object, "est")
  kappa_r <- est[[reference]][["alpha"]]
  kappa_f <- est[[focal]][["alpha"]]
  phi_r <- est[[reference]][["psi"]]
  phi_f <- est[[focal]][["psi"]]
  lambda_r <- est[[reference]][["lambda"]]
  lambda_f <- est[[focal]][["lambda"]]
  Theta_r <- est[[reference]][["theta"]]
  Theta_f <- est[[focal]][["theta"]]
  tau_r <- est[[reference]][["nu"]]
  tau_f <- est[[focal]][["nu"]]
  if (nrow(phi_r > 1)) {
    inds <- which(lambda_r[ , which_fac] != 0)
    kappa_r <- kappa_r[which_fac, ]
    kappa_f <- kappa_f[which_fac, ]
    phi_r <- phi_r[which_fac, which_fac]
    phi_f <- phi_f[which_fac, which_fac]
    lambda_r <- lambda_r[inds, which_fac]
    lambda_f <- lambda_f[inds, which_fac]
    Theta_r <- Theta_r[inds, inds]
    Theta_f <- Theta_f[inds, inds]
    tau_r <- tau_r[inds, ]
    tau_f <- tau_f[inds, ]
  }
  if (force_strict) {
    prop_ref <- (inspect(object, "nobs") / nobs(object))[reference]
    lambda_r <- lambda_f <- prop_ref * lambda_r + (1 - prop_ref) * lambda_f
    tau_r <- tau_f <- prop_ref * tau_r + (1 - prop_ref) * tau_f
    Theta_r <- Theta_f <- prop_ref * Theta_r + (1 - prop_ref) * Theta_f
  }
  PartInv(propsel, cut_z = NULL, kappa_r, kappa_f, phi_r, phi_f, lambda_r, lambda_f, 
          Theta_r, Theta_f, tau_r, tau_f, pmix_ref, plot_contour, ...)
}

PartInv.lavaan(.25, 
               object = fit, 
               reference = 1, 
               which_fac = 2, 
               force_strict = FALSE)
