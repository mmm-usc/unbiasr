#' @importFrom methods setClass

# classes: PartInv, itemdeletion


# dividers
stars <-
  "***********************************************************************"
dashes <-
  "-----------------------------------------------------------------------"

summary_print <- function(x, ...) {
  # rownames(x) <- c("True Positive", "False Positive", "True Negative",
  #                  "False Negative", "Proportion Selected",
  #                  "Success Ratio", "Sensitivity", "Specificity")
  print(round(x, digits = 3))
}

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
      labels = character()
    ),
    class = "PartInv"
  )
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
#' @method print PartInv 
#' @title Print method for PartInv class
#' @description performs printing and formatting on a PartInv object.
#' @param x An object of class \code{PartInv}, the output from
#'  \code{PartInv()}.
#' @param ... Additional arguments passed to methods.
#' @return NULL
#' 
#' @examples
#' lambda_matrix <- matrix(0, nrow = 15, ncol = 1)
#' lambda_matrix[1:15, 1] <- c(0.68, 0.79, -0.39, 0.74, 0.59, 0.46, 0.78, -0.30,
#'                             0.59, 0.59, 0.64, 0.66, 0.59, 0.63, 0.64);
#' nu_matrix <- nu_matrix1 <- nu_matrix2 <- nu_matrix3 <-
#'   matrix(0, nrow = 15, ncol = 1)
#' nu_matrix[1:15, 1] <- c(3.6, 3.1, 2.7, 2.9, 2.5, 2.1, 3.45, 2.62, 3.2, 2.84,
#'                         3.51, 3.26, 2.45, 3.39, 2.47);
#' nu_matrix1[1:15, 1] <- c(3.9, 3.1, 2.7, 2.9, 2.5, 2.1, 3.45, 2.62, 3.2, 2.84,
#'                          3.51, 3.26, 2.45, 3.76, 2.81);
#' nu_matrix2[1:15, 1] <- c(3.6, 3.1, 2.7, 2.9, 2.5, 2.1, 3.45, 2.62, 3.6, 3.18,
#'                          3.51, 3.54, 2.45, 3.39, 2.81);
#' nu_matrix3[1:15, 1] <- c(3.6, 3.1, 2.7, 2.6, 2.5, 2.1, 3.45, 2.62, 3.2, 2.84,
#'                          3.51, 3.26, 2.45, 3.39, 2.81);
#' theta_matrix <- c(0.35, 0.62, 0.83, 0.61, 0.81, 0.87, 0.39, 1.05, 0.84, 0.92,
#'                   0.36, 0.66, 0.8, 0.66, 0.9);
#' theta_matrix1 <- c(0.61, 0.62, 0.83, 0.61, 0.81, 0.5, 0.7, 1.05, 0.84, 0.92,
#'                    0.61, 0.66, 0.8, 0.54, 0.9);
#' theta_matrix2 <- c(0.61, 0.62, 0.826, 0.61, 0.81, 0.87, 0.5, 1.05, 0.84,
#'                    0.92, 0.61, 0.66, 0.8, 0.66, 0.9);
#' theta_matrix3 <- c(0.61, 0.62, 0.826, 0.61, 0.81, 0.5, 0.7, 1.05, 0.84, 0.92,
#'                    0.61, 0.66, 0.8, 0.66, 0.9);
#' out <- PartInv(propsel = 0.25, pmix = c(1/4, 1/4, 1/4, 1/4),
#'   alpha = list(0, -0.70, -1.05, -1.10), psi = list(1, 1.2, 1.29, 1.3),
#'   nu = list(nu_matrix, nu_matrix1, nu_matrix2, nu_matrix3),
#'   lambda = list(lambda_matrix, lambda_matrix, lambda_matrix, lambda_matrix),
#'   theta = list(theta_matrix, theta_matrix1, theta_matrix2, theta_matrix3),
#'   plot_contour = TRUE, show_mi_result = TRUE,
#'   labels = c("Group 1", "Group 2", "Group 3", "Group 4"),
#'   custom_colors = c("salmon1", "lightgreen", "skyblue1", "pink"))
#'
print.PartInv <- function(x, digits = 3L, ...) {
  .print_partinv(x[c("propsel", "cutpt_xi", "cutpt_z", "summary", "ai_ratio")],
                 type = "pi", digits = digits, ...)
  if (length(x$summary_mi) > 0) {
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
  first_word <- switch(type,
                       pi = "Partial",
                       mi = "Strict")
  cat(first_word, " invariance results:\n\n")
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
    print(summ[, (ceiling(nc / 2) + 2):nc], ...)
  } else {
    print(format_p(summ, ...))
  }
}

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

#' @method print itemdeletion 
#' @title Print method for itemdeletion class
#' @description performs printing and formatting on an itemdeletion object.
#' @param x An object of class \code{itemdeletion}, the output from
#'  \code{item_deletion_h()}.
#' @param ... Additional arguments passed to methods.
#' @return NULL
#' 
#' @examples
#' # Multidimensional example
#' lambda_matrix <- matrix(0, nrow = 5, ncol = 2)
#' lambda_matrix[1:2, 1] <- c(.322, .655)
#' lambda_matrix[3:5, 2] <- c(.398, .745, .543)
#' multi_dim <- item_deletion_h(propsel = .05, n_dim = 5,
#'                              weights_item = c(1/4, 1/4, 1/6, 1/6, 1/6),
#'                              weights_latent = c(0.5, 0.5),
#'                              alpha_r = c(0, 0),
#'                              alpha_f = c(-0.3, 0.1),
#'                              psi_r = matrix(c(1, 0.5, 0.5, 1), nrow = 2),
#'                              lambda_r = lambda_matrix,
#'                              nu_r = c(.225, .025, .010, .240, .125),
#'                              nu_f = c(.225, -.05, .240, -.025, .125),
#'                              Theta_r = diag(1, 5),
#'                              Theta_f = diag(c(1, .95, .80, .75, 1)),
#'                              plot_contour = TRUE)
#' print(multi_dim)
#'@export
print.itemdeletion <- function(x, digits = 3L, ...) {
  item_set <- x$items
  # apply rounding and get the composite index columns
  x <- c(x[1],
              lapply(x[-c(1, 10:13)], function(inner_list) {
                lapply(inner_list, function(df) {
                  df <- df[, 5:8]
                  round(df, digits)
                  })
                }),
            list(x[10:13]))
  x$AI <- round(x$AI, digits)
  cat(paste0("\n", stars, "\nAdverse Impact (AI) ratios under partial invariance by group\n", stars, "\n"))
  print(x$AI[c("Full", item_set), -1, drop = FALSE])
  cat("\n(Note: AI ratios equal 1 under strict invariance by definition.)\n\n")
  cat(paste0(stars, "\nAGGREGATE CLASSIFICATION ACCURACY INDICES (CAI*)\n", stars))
  cat("\nAggregate CAI under partial invariance:\n")
  print_dfs_from_list(x$ACAI, c("Full", item_set) )
  cat(paste0(dashes, "\nImpact of deleting an item on aggregate CAI under partial invariance:\n"))
  print_dfs_from_list(x$h_acai_p, item_set)
  cat(paste0("\n", stars, "\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS\n", stars))
  cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
  print_dfs_from_list(x$h_R_Ef, c("r_Ef", paste0("r_Ef", item_set))) 
  cat(paste0(dashes, "\nImpact of deleting an item on the discrepancy between observed CAI for the 
reference group and expected CAI for the focal groups (Efocal):\n"))
   print_dfs_from_list(x$delta_h_R_Ef, item_set)
   invisible(NULL)
}

# register the custom print method for the itemdeletion class
setMethod("print", "itemdeletion", print.itemdeletion)

#' @method summary itemdeletion 
#' @title Summary method for itemdeletion class
#' @description prints a detailed summary for an itemdeletion object.
#' @param object An object of class \code{itemdeletion}, the output from
#'  \code{item_deletion_h()}.
#' @param ... Additional arguments passed to methods.
#' @return NULL
#' 
#' @examples
#' # Multidimensional example
#' lambda_matrix <- matrix(0, nrow = 5, ncol = 2)
#' lambda_matrix[1:2, 1] <- c(.322, .655)
#' lambda_matrix[3:5, 2] <- c(.398, .745, .543)
#'
#' multi_dim <- item_deletion_h(propsel = .05, n_dim = 5,
#'                              weights_item = c(1/4, 1/4, 1/6, 1/6, 1/6),
#'                              weights_latent = c(0.5, 0.5),
#'                              alpha_r = c(0, 0),
#'                              alpha_f = c(-0.3, 0.1),
#'                              psi_r = matrix(c(1, 0.5, 0.5, 1), nrow = 2),
#'                              lambda_r = lambda_matrix,
#'                              nu_r = c(.225, .025, .010, .240, .125),
#'                              nu_f = c(.225, -.05, .240, -.025, .125),
#'                              Theta_r = diag(1, 5),
#'                              Theta_f = diag(c(1, .95, .80, .75, 1)),
#'                              plot_contour = TRUE,
#'                              # delete_items = c(1:4), # to see all items
#'                              labels = c("Male", "Female")
#'                              )
#' summary(multi_dim)
#'@export
summary.itemdeletion <- function(object, ...) {
  digits <- object$digits
  item_set <- object$items
  # apply rounding and get the composite index columns
  object <- c(object[1],
         lapply(object[-c(1, 10:13)], function(inner_list) {
           lapply(inner_list, function(df) {
             df <- df[, 5:8]
             round(df, digits)
           })
         }),
         list(object[10:13]))
  object$AI <- round(object$AI, digits)
  cat(paste0("\n", stars, "\nAdverse Impact (AI) ratios under partial invariance by group\n", stars, "\n"))
  print(object$AI[c("Full", item_set), -1, drop = FALSE])
  cat("\n(Note: AI ratios equal 1 under strict invariance by definition.)\n\n")
  cat(paste0(stars, "\nAGGREGATE CLASSIFICATION ACCURACY INDICES (CAI*)\n", stars))
  cat("\nAggregate CAI (CAI*) under partial invariance:\n")
  print_dfs_from_list(object$ACAI, c("Full", item_set) )
  cat("\nImpact of deleting an item on aggregate CAI (CAI*) under partial invariance:\n")
  print_dfs_from_list(object$h_acai_p, item_set)
  cat(paste0(dashes, "\nImpact of deleting an item on the discrepancy between ACAI under SFI vs. PFI:\n"))
  print_dfs_from_list(object$delta_h_acai_s_p, item_set)
  cat(paste0("\n", stars, "\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS\n", stars))
  cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
  print_dfs_from_list(object$h_R_Ef, c("r_Ef", paste0("r_Ef", item_set))) 
  cat(paste0(dashes, "\nImpact of deleting an item on the discrepancy between observed CAI for the 
reference group and expected CAI for the focal groups (Efocal):\n"))
  print_dfs_from_list(object$delta_h_R_Ef, item_set)
  cat("\nDiscrepancy between CAI under SFI vs. PFI:\n")
  cat(paste0("Reference group: ", names(object$h_s_p)[1], "\n"))  
  print(object$h_s_p[[1]][item_set, ])
  print_dfs_from_list(object$h_s_p[-1], c("Full", item_set))
  
  cat(paste0(dashes, "\nImpact of deleting an item on the discrepancy between CAI under SFI vs. PFI:\n"))
  cat(paste0("Reference group: ", names(object$delta_h_s_p)[1], "\n"))  
  print(object$delta_h_s_p[[1]][item_set, ])
  print_dfs_from_list(object$delta_h_s_p[-1],  item_set)
  invisible(NULL)
}
# register the custom summary method for the itemdeletion class
setMethod("summary", "itemdeletion", summary.itemdeletion)

