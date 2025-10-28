.old_rf_names <- c("alpha_r", "alpha_f", "nu_r", "nu_f", "Theta_r", "Theta_f",
                   "psi_r", "psi_f", "lambda_r", "lambda_f",
                   "kappa_r", "kappa_f", "phi_r", "phi_f", "tau_r", "tau_f",
                   "pmix_ref")

prep_params <- function(x) {
  if (!is.null(x$cfa_fit)) {
    cfa_params <- get_params_cfa(x$cfa_fit, x$pmix, x$labels)
    x[names(cfa_params)] <- cfa_params
    x$cfa_fit <- NULL
  } else if (any(!sapply(x[.old_rf_names], is.null))) {
    warning("The arguments with suffixes '_r' and '_f' are deprecated. ",
            "Please use 'alpha', 'nu', 'theta', 'psi', 'lambda', 'pmix' ",
            "instead. See ?PartInv for details.")
    x <- stanitize_rf_params(x)
  }
  stopifnot("Number of groups as indicated in the estimates must match." = 
              length(x$alpha) == lengths(x[c("psi", "lambda", "nu", "theta")]))
  q <- length(x$alpha[[1]])
  p <- length(x$nu[[1]])
  num_g <- length(x$alpha)
  x$alpha <- to_list_matrices(x$alpha)
  x$psi <- to_list_matrices(x$psi, dims = c(q, q))
  x$lambda <- to_list_matrices(x$lambda, dims = c(p, q))
  x$theta <- to_list_matrices(x$theta, dims = c(p, p))
  x$nu <- to_list_matrices(x$nu)
  
  #### 'weights_item' and 'weights_latent' ####
  x$weights_item <- check_weights(x$weights_item, p)
  x$weights_latent <- check_weights(x$weights_latent, q)
  
  #### 'pmix' ####
  x$pmix <- check_pmix(x$pmix, num_g)

  #### 'labels' #### 
  x$labels <- check_labels(x$labels, num_g, x$reference)
  
  #### set the reference group and reorder appropriately ####
  if (!is.null(x$reference)) {
    x <- reference_first(x, x$labels, x$reference)
  }
  
  # g_labs <- c("r", paste0("f", 1:(num_g - 1)))
  # names(alpha) <- paste("alpha", g_labs, sep = "_")
  # names(nu) <- paste("nu", g_labs, sep = "_")
  # names(lambda) <- paste("lambda", g_labs, sep = "_")
  # names(psi) <- paste("psi", g_labs, sep = "_")
  # names(theta) <- paste("theta", g_labs, sep = "_")

  x$p <- p
  x$q <- q
  x$num_g <- num_g
  return(x)
}

to_matrix <- function(x, dims = NULL) {
  if (is.null(dims)) {
    return(as.matrix(x))
  } else if (length(dims) == 2) {
    if (length(x) == dims[1] && dims[1] == dims[2]) {
      return(diag(c(x), nrow = dims[1], ncol = dims[2]))
    } else if (length(x) == prod(dims)) {
      return(matrix(x, nrow = dims[1], ncol = dims[2]))
    } else {
      stop("Length of x does not match dims.")
    }
  } else {
    stop("dims must be NULL or a vector of length 2.")
  }
}

to_list_matrices <- function(x, dims = NULL) {
  if (!is.list(x)) {
    stop("input must be a list.")
  }
  lapply(x, FUN = to_matrix, dims = dims)
}

check_weights <- function(x, len) {
  if (is.null(x)) {
    return(rep(1, len))
  } else {
    stopifnot("Please provide a weight vector of the correct length." = 
                length(x) == len)
    return(x)
  }
}

check_pmix <- function(x, num_g) {
  if (is.null(x)) {
    message("Mixing proportions not provided (pmix). Assuming equal weights.")
    return(rep(1 / num_g, num_g))
  } else {
    # if pmix not null
    stopifnot(
      "Provide the correct number of mixing proportions." = length(x) ==
        num_g
    )
    return(x)
  }
}

check_labels <- function(x, num_g, reference) {
  if (is.null(x)) {
    x <- c("Reference", paste0("Focal_", 1:(num_g - 1)))
  } else {
    stopifnot("The number of labels does not match the number of groups." = 
                length(x) == num_g)
    if (!is.null(reference)) {
      if (!reference %in% x) {
        stop("The reference group label string does not match any of the ",
             "provided/default group labels.")
      }
    }
  }
  return(x)
}

reference_first <- function(x, labels, reference) {
  ind <- which(labels == reference)
  new_order <- c(ind, seq_along(labels)[-ind])
  names_to_reorder <- c("alpha", "nu", "theta", "lambda", "psi",
                         "pmix", "custom_colors", "labels")
  for (nm in names_to_reorder) {
    if (!is.null(x[[nm]])) {
      x[[nm]] <- x[[nm]][new_order]
    } else {
      next
    }
  }
  x
}

get_params_cfa <- function(cfa_fit, pmix = NULL, labels = NULL) {
  # extract the parameter estimates from the cfa fit object
  lav_cfa <- cfa_fit@Model@GLIST
  alpha <- lav_cfa[which(names(lav_cfa) == "alpha")]
  nu <- lav_cfa[which(names(lav_cfa) == "nu")]
  theta <- lav_cfa[which(names(lav_cfa) == "theta")]
  lambda <- lav_cfa[which(names(lav_cfa) == "lambda")]
  psi <- lav_cfa[which(names(lav_cfa) == "psi")]
  if (is.null(pmix)) {
    cfa_nobs <- unlist(cfa_fit@Data@nobs)
    pmix <- cfa_nobs / sum(cfa_nobs)
  }
  if (is.null(labels)) {
    labels <- cfa_fit@Data@group.label
  }
  return(list("alpha" = alpha, "lambda" = lambda, "nu" = nu, "psi" = psi, 
              "theta" = theta, "pmix" = pmix, "labels" = labels))
}

all_null <- function(x) {
  return(all(sapply(x, is.null)))
}

all_nonnull <- function(x) {
  return(all(!sapply(x, is.null)))
}

stanitize_rf_params <- function(x) {
  if (is.null(x$alpha)) {
    if (all_null(x[c("alpha_r", "alpha_f")]) &&
        all_nonnull(x[c("kappa_r", "kappa_f")])) {
      x$alpha_r <- x$kappa_r
      x$alpha_f <- x$kappa_f
    }
    x$alpha <- list(x$alpha_r, x$alpha_f)
  }
  if (is.null(x$nu)) {
    if (all_null(x[c("nu_r", "nu_f")]) &&
        all_nonnull(x[c("tau_r", "tau_f")])) {
      x$nu_r <- x$tau_r
      x$nu_f <- x$tau_f
    }
    x$nu <- list(x$nu_r, x$nu_f)
  }
  if (is.null(x$psi)) {
    if (all_null(x[c("psi_r", "psi_f")]) &&
        all_nonnull(x[c("phi_r", "phi_f")])) {
      x$psi_r <- x$phi_r
      x$psi_f <- x$phi_f
    }
    x$psi <- list(x$psi_r, x$psi_f)
  }
  if (is.null(x$lambda)) {
    x$lambda <- list(x$lambda_r, x$lambda_f)
  }
  if (is.null(x$theta)) {
    x$theta <- list(x$Theta_r, x$Theta_f)
  }
  if (!is.null(x$pmix_ref) && is.null(x$pmix)) {
    x$pmix <- c(x$pmix_ref, 1 - x$pmix_ref)
  }
  x[.old_rf_names] <- NULL
  x
}