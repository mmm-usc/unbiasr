#' @importFrom methods setClass

# classes: PartInv, itemdeletion


# dividers
stars <-
  "***********************************************************************"
dashes <-
  "-----------------------------------------------------------------------"

.cai_names <- c("True Positive", "False Positive", "True Negative",
                 "False Negative", "Proportion Selected",
                 "Success Ratio", "Sensitivity", "Specificity")

# Constructor
new_PartInv <- function() {
  structure(
    list(
      propsel = numeric(),
      cutpt_xi = numeric(),
      cutpt_z = numeric(),
      summary = data.frame(),
      bivar_data = list(),
      ai_ratio = numeric(),
      propsel_mi = numeric(),
      cutpt_xi_mi = numeric(),
      cutpt_z_mi = numeric(),
      summary_mi = data.frame(),
      bivar_data_mi = list(),
      params = list(alpha = matrix(),
                    psi = matrix(),
                    lambda = matrix(),
                    nu = matrix(),
                    theta = matrix(),
                    weights_item = numeric(),
                    weights_latent = numeric(),
                    labels = character(),
                    num_g = integer(),
                    pmix = numeric(),
                    functioncall = character())
    ),
    class = "PartInv"
  )
}

validate_PartInv <- function(x) {
  pl <- x$params
  if (!is.numeric(x$propsel) || length(x$propsel) != 1) {
    stop("`propsel` must be a single numeric value.")
  }
  if (!is.data.frame(x$summary)) {
    stop("`summary` must be a data frame.")
  }
  if (!is.list(x$bivar_data)) {
    stop("`bivar_data` must be a list.")
  }
  if (!is.numeric(x$ai_ratio)) {
    stop("`ai_ratio` must be numeric.")
  }
  if (!is.character(pl$labels)) {
    stop("`labels` must be a character vector.")
  }
  if (length(pl$labels) != pl$num_g || length(pl$pmix) != pl$num_g) {
    stop("Lengths of `labels` and `pmix` must match the number of groups.")
  }
  if (ncol(x$summary) != ((2 * pl$num_g - 1))) {
    stop("Number of columns in `summary` does not match expected number ",
         "of 2 * g - 1, where g is the number of groups.")
  }
  if (length(x$summary_mi) > 0 && ncol(x$summary_mi) != pl$num_g) {
    stop("Number of columns in `summary_mi` does not match the number ",
         "groups.")
  }
  x
}

# setClass("PartInv",
#   representation(
#     propsel = "numeric",
#     cutpt_xi = "numeric", cutpt_z = "numeric",
#     summary = "data.frame",
#     bivar_data = "list",
#     ai_ratio = "numeric",
#     propsel_mi = "numeric",
#     cutpt_xi_mi = "numeric", cutpt_z_mi = "numeric",
#     bivar_data_mi = "list",
#     summary_mi = "data.frame",
#     labels = "character", 
#     functioncall = "character"
#   )
# )

#' @export
print.PartInv <- function(x, digits = 3L, ...) {
  .print_partinv(x[c("propsel", "cutpt_xi", "cutpt_z", "summary", "ai_ratio")],
                 type = "pi", digits = digits, ...)
  if (length(x$summary_mi) > 0) {
    cat("\n")
    .print_partinv(x[c("propsel_mi", "cutpt_xi_mi", "cutpt_z_mi",
                      "summary_mi")],
                   type = "mi", digits = digits, ...)
  }
}

.print_partinv <- function(x, type = c("pi", "mi"), ...) {
  type <- match.arg(type)
  ps <- x[[1]]
  cut_xi <- x[[2]]
  cut_z <- x[[3]]
  summ <- x[[4]]
  rownames(summ) <- .cai_names
  first_word <- switch(type,
                       pi = "Partial",
                       mi = "Strict")
  cat(first_word, "invariance results:\n\n")
  cat("Proportion selected: ", format(ps, ...), "\n")
  cat("Cutpoint on the latent scale (xi): ",
     format(cut_xi, ...), "\n")
  cat("Cutpoint on the observed scale (Z): ",
      format(cut_z, ...), "\n")
  if (type == "pi") {
    air <- x[[5]]
    cat(paste0("Adverse impact ratio ", "(reference group: '",
               colnames(x$summary)[1], "'):\n"))
    print(air, ...)
  }
  cat("\n")
  cat("Classification Accuracy Indices:\n")
  nc <- ncol(summ)
  if (nc > 8) {
    print(summ[, 1:(ceiling(nc / 2))], ...)
    cat("\n")
    cat("Expected Results if Latent Distributions Matched the Reference Group:\n")
    print(summ[, (ceiling(nc / 2) + 1):nc], ...)
  } else {
    print(format_p(summ, ...))
  }
}

#' Format proportions
#' 
#' Trim leading zeros from numeric proportions for display.
#' 
#' @param x An object to be printed.
#' @param digits Number of decimal places to display.
#' @export
format_p <- function(x, digits) {
  UseMethod("format_p")
}

#' @export
format_p.default <- function(x, digits) {
  formatted <- sprintf(paste0("%.", digits, "f"), x)
  sub("^(-?)0\\.", "\\.", formatted)
}

#' @export
format_p.data.frame <- function(x, digits) {
  x[] <- lapply(x, format_p, digits = digits)
  x
}
  
setClass("itemdeletion",
  representation(
    AI_ratios = "data.frame",
    ACAI = "list",
    h_acai_p = "list",
    h_acai_s_p = "list",
    delta_h_acai_s_p =  "list",
    h_R_Ef = "list",
    delta_h_R_Ef = "list",
    delta_h_str_vs_par = "list",
    PartInv_outputs = "list",
    items = "vector",
    function_call = "call"
  )
)
new_itemdeletion <- function() {
  list()
}

#' @method print itemdeletion 
#' @title Print method for itemdeletion class
#' @description performs printing and formatting on an itemdeletion object.
#' @param x An object of class \code{itemdeletion}, the output from
#'  \code{item_deletion_h()}.
#' @param digits Number of decimal places to display.
#' @param cols Columns to display from the data frames. Default is
#'   `5:8`, which correspond to PS, SR, SE, and SP.
#' @param  full_result Logical; if `TRUE`, prints additional results.
#'   Default is `FALSE`, which prints a more concise summary.
#' @param ... Additional arguments passed to methods.
#' @return NULL
#' 
#' @examples
#' # Multidimensional example
#' lambda_matrix <- matrix(0, nrow = 5, ncol = 2)
#' lambda_matrix[1:2, 1] <- c(.322, .655)
#' lambda_matrix[3:5, 2] <- c(.398, .745, .543)
#' multi_dim_partinv <- PartInv(
#'   propsel = .05, n_dim = 5,
#'   weights_item = c(1/4, 1/4, 1/6, 1/6, 1/6),
#'   weights_latent = c(0.5, 0.5),
#'   alpha_r = c(0, 0),
#'   alpha_f = c(-0.3, 0.1),
#'   psi_r = matrix(c(1, 0.5, 0.5, 1), nrow = 2),
#'   lambda_r = lambda_matrix,
#'   nu_r = c(.225, .025, .010, .240, .125),
#'   nu_f = c(.225, -.05, .240, -.025, .125),
#'   Theta_r = diag(1, 5),
#'   Theta_f = diag(c(1, .95, .80, .75, 1))
#' )
#' multi_dim <- item_deletion_h(multi_dim_partinv)
#' print(multi_dim)
#' @export
print.itemdeletion <- function(x, digits = 3L, cols = 5:8,
                               full_result = FALSE, ...) {
  cat("\n", stars,
      "\nAdverse Impact ratios (AIRs) under partial invariance by group\n",
      stars, "\n", sep = "")
  print(x$AI, digits, ...)
  cat("\n(Note: AIRs equal 1 under strict invariance by definition.)\n\n",
      sep = "")
  cat(stars, "\nAGGREGATE CLASSIFICATION ACCURACY INDICES (CAI*)\n", stars,
      sep = "")
  cat("\nAggregate CAI under partial invariance:\n", sep = "")
  .print_idel_tbl(x$ACAI, digits = digits, cols = cols, ...)
  cat(dashes,
      "\nImpact of deleting an item on aggregate CAI under PFI:\n",
      sep = "")
  .print_idel_tbl(x$h_acai_p, digits = digits, cols = cols, ...)
  if (full_result) {
    cat(dashes,
        "\nImpact of deleting an item on the discrepancy between ACAI under\n",
        "SFI vs. PFI:\n", sep = "")
    .print_idel_tbl(x$delta_h_acai_s_p, digits = digits, cols = cols, ...)
  }
  cat("\n", stars,
      "\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS\n", stars,
      sep = "")
  cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n",
      sep = "")
  .print_idel_tbl(x$h_R_Ef, digits = digits, cols = cols, ...)
  cat(dashes,
      "\nImpact of deleting an item on the discrepancy between observed\n",
      "CAI for the reference group and expected CAI for the focal groups\n",
      "(Efocal):\n", sep = "")
  .print_idel_tbl(x$delta_h_R_Ef, digits = digits, cols = cols, ...)
  if (full_result) {
    cat("\nDiscrepancy between CAI under SFI vs. PFI:\n")
    .print_idel_tbl(x$h_s_p, digits = digits, cols = cols, first_is_ref = TRUE, ...)
    cat(dashes,
        "\nImpact of deleting an item on the discrepancy between CAI under\n",
        "SFI vs. PFI:\n")
    .print_idel_tbl(x$delta_h_s_p, digits = digits, cols = cols, first_is_ref = TRUE,
                    ...)
  }
  invisible(NULL)
}

.print_idel_tbl <- function(x, digits, cols = 5:8,
                            first_is_ref = FALSE, ...) {
  if (is.list(x) && length(dim(x[[1]])) >= 2) {
    lapply(seq_along(x), function(i) {
      if (i == 1 && first_is_ref) {
        glab <- "Reference"
      } else {
        glab <- "Focal"
      }
      cat(glab, " group: ", names(x[i]), "\n", sep = "")
      .print_idel_tbl(x[[i]], digits = digits, cols = cols, ...)
    })
  } else {
    df <- as.data.frame(x[, cols, drop = FALSE])
    
    # round and format numeric columns, control # of digits after the decimal
    num_cols <- sapply(df, is.numeric)
    if (any(num_cols)) { 
      df[num_cols] <- lapply(df[num_cols], function(v)
        # format = "f" to prevent scientific notation, drop0trailing = F to keep 0s
        formatC(v, digits = digits, format = "f", drop0trailing = FALSE))
    }
    
    print(df, ...)
  }
  invisible(NULL)
}
