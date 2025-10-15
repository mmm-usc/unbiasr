prep_params <- function(x, reference = NULL) {
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
  x$labels <- check_labels(x$labels, num_g, reference)
  
  #### set the reference group and reorder appropriately ####
  if (!is.null(reference)) {
    x <- reference_first(x, x$labels, reference)
  }
  
  # g_labs <- c("r", paste0("f", 1:(num_g - 1)))
  # names(alpha) <- paste("alpha", g_labs, sep = "_")
  # names(nu) <- paste("nu", g_labs, sep = "_")
  # names(lambda) <- paste("lambda", g_labs, sep = "_")
  # names(psi) <- paste("psi", g_labs, sep = "_")
  # names(theta) <- paste("theta", g_labs, sep = "_")

  return(x)
}

to_matrix <- function(x, dims = NULL) {
  if (is.null(dims)) {
    return(as.matrix(x))
  } else if (length(dims) == 2) {
    if (length(x) == dims[1] && dims[1] == dims[2]) {
      return(diag(c(x)))
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
}

reference_first <- function(x, labels, reference) {
  ind <- which(labels == reference)
  names_to_reorder <- c("alpha", "nu", "theta", "lambda", "psi",
                         "pmix", "custom_colors", "labels")
  for (nm in names_to_reorder) {
    if (!is.null(x[[nm]])) {
      x[[nm]] <- c(list(x[[nm]][[ind]]), x[[nm]][-ind])
    } else {
      next
    }
  }
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
