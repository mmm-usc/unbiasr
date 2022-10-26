#' @importFrom methods setClass

# dividers
stars <-
  "***********************************************************************"
dashes <-
  "-----------------------------------------------------------------------"

# setClass("PartInvSummary",
#          representation(
#            summary = "data.frame"
#          )
# )

print_PartInvSummary <- function(x, ...) {
  cat("Classification Accuracy Indices:\n")
  rownames(x) <- c("True Positive", "False Positive", "True Negative",
                        "False Negative",  "Proportion Selected",
                        "Success Ratio", "Sensitivity", "Specificity")
 # c("TP", "FP", "TN", "FN", "PS", "SR", "SE", "SP")
  print(round(x, digits = 3))
}

setClass("PartInv_groups",
         representation(
           tab = "data.frame"
         )
)
#'@export
print.PartInv_groups <- function(x, ...) {
  rownames(x) <- c("True Positive", "False Positive", "True Negative",
                        "False Negative",  "Proportion Selected",
                        "Success Ratio", "Sensitivity", "Specificity")
  print(round(x, 3))
}

setClass("PartInv",
         representation(
           propsel = "numeric",
           cutpt_xi = "numeric", cutpt_z = "numeric",
           summary = "data.frame",
           bivar_data = "list",
           ai_ratio = "numeric",
           propsel_mi = "numeric",
           cutpt_xi_mi = "numeric", cutpt_z_mi = "numeric",
           bivar_data_mi = "list",
           summary_mi = "data.frame"
         )
)
#'@export
print.PartInv <- function(x, ...) {
  cat("Proportion selected: ", round(x$propsel, 3), "\n")
  cat("Cutpoint on the latent scale (xi): ", round(x$cutpt_xi, 3), "\n")
  cat("Cutpoint on the observed scale (Z): ", round(x$cutpt_z, 3), "\n")
  cat("AI ratio: ", round(x$ai_ratio, 3), "\n\n")
  cat("Partial invariance results:\n\n")
  print_PartInvSummary(x$summary)
  if (!is.null(x$summary_mi)) {
    cat("\n\nStrict invariance results:\n\n")
    cat("Proportion selected: ", round(x$propsel_mi, 3), "\n")
    cat("Cutpoint on the latent scale (xi): ", round(x$cutpt_xi_mi, 3), "\n")
    cat("Cutpoint on the observed scale (Z): ", round(x$cutpt_z_mi, 3), "\n")
    print_PartInvSummary(x$summary_mi)
  }
#  if(!is.null(obj$summary_mi)) {
#  cat("\nMI ")
 #   print.PartInvSummary(obj$summary_mi)
#  }
  #if(!is.null(obj$p_mi)){
  #  print(obj$plot)
  #}
}

setClass("PartInvList",
         representation(
           outputlist = "list",
           condition = "character",
           itemset = "vector"
         )
)
#'@export
print.PartInvList <- function(x, ...) {
  if (x$condition == "partial") {
    cat(paste0("\n", stars))
    cat("\nPartInv() outputs under Partial Factorial Invariance (PFI)\n")
    cat(stars)
    cat("\n\nUnder PFI, full item set:\n\n")
    print.PartInv(x$outputlist[[1]])
    for (i in x$itemset) {
      cat(dashes)
      cat("\n\nUnder PFI, if item", i, "is dropped:\n\n")
      print.PartInv(x$outputlist[[i]])
    }
  }
    if (x$condition == "strict") {
    cat(paste0("\n", stars))
    cat("\nPartInv() outputs under Strict Factorial Invariance (SFI)\n")
    cat(stars)
    cat("\n\nUnder SFI, full item set:\n\n")
    print.PartInv(x$outputlist[[1]])
    for (i in x$itemset) {
      cat(dashes)
      cat("\n\nUnder SFI, if item", i, "is dropped:\n\n")
      print.PartInv(x$outputlist[[i]])
    }
    }
  if (x$condition == "ref") {
    cat(paste0("\n", stars))
    cat("\nCAI under SFI vs. PFI for the reference group\n")
    cat(stars)
    cat("\n\nReference group, full item set:\n")
    print.PartInv_groups(x$outputlist[[1]])
    for (i in x$itemset) {
      cat(dashes)
      cat("\n\nReference group, if item", i,"is dropped:\n")
      print.PartInv_groups(x$outputlist[[i]])
    }
  }
  if (x$condition == "foc") {
    cat(paste0("\n", stars))
    cat("\nCAI under SFI vs. PFI for the focal group\n")
    cat(stars)
    cat("\n\nFocal group, full item set:\n")
    print.PartInv_groups(x$outputlist[[1]])
    for (i in x$itemset) {
      cat(dashes)
      cat("\n\nFocal group, if item", i, "is dropped:\n")
      print.PartInv_groups(x$outputlist[[i]])
    }
  }
}
#' #'@export
#' setClass("itemdeletion",
#'   representation(
#'     h_aggregate_par = "matrix",
#'     delta_h_str_par_aggregate =  "matrix",
#'     AI = "matrix",
#'     deltaREf = "matrix",
#'     h_R_Ef = "matrix",
#'     delta_h_str_vs_par = "matrix",
#'     str_vs_par = "list",
#'     aggregate = "matrix",
#'     h_aggregate_str_par = "matrix",
#'     Ref_foc = "PartInvList",
#'     PartInv = "PartInvList",
#'     detail = "logical",
#'     return_items = "list"
#' )
#' )
#' #'@export
#' print.itemdeletion <- function(x, ...) {
#'   item_set <- x$return_items
#'   # ------------------------------------------------------------------------
#'   # DEFAULT OUTPUT: only biased items printed
#'   if (!x$detail) {
#'     cat(paste0(stars, "\nAGGREGATE CLASSIFICATION ACCURACY INDICES (ACAI)\n", stars))
#'     cat("\nACAI under PFI computed for item subsets:\n")
#'     print(round(x$`ACAI`[c(1, item_set + 1), , drop = FALSE], 3))
#'     cat("\nImpact of deleting an item on ACAI under PFI:\n")
#'     print(round(x$`h ACAI (deletion)`[c(item_set), , drop = FALSE], 3))
#'     cat(paste0("\n", stars, "\nAdverse Impact (AI) ratio for item subsets by invariance condition:\n", stars, "\n"))
#'     print(round(x$AI[c(1, item_set + 1), , drop = FALSE], 3))
#'     cat(paste0("\n", stars, "\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS\n", stars))
#'     cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
#'     print(round(x$`h CAI Ref-EF`[c(1, item_set + 1), 5:8, drop = FALSE], 3))
#'     cat(paste0(dashes, "\nImpact of deleting an item on the discrepancy between CAI of\nreference vs. Efocal groups under PFI:\n"))
#'     print(round(x$`delta h CAI Ref-EF (deletion)`[c(item_set), 5:8, drop = FALSE], 3))
#'   }
#'  # ------------------------------------------------------------------------
#'   if (x$detail) { #
#'     cat(paste0(stars, "\nAGGREGATE CLASSIFICATION ACCURACY INDICES (ACAI)\n", stars))
#'     cat("\nACAI under PFI computed for item subsets:\n")
#'     print(round(x$`ACAI`[c(1, item_set + 1), , drop = FALSE], 3))
#'     cat("\nImpact of deleting an item on ACAI under PFI:\n")
#'     print(round(x$`h ACAI (deletion)`[c(item_set), , drop = FALSE], 3))
#'     cat(paste0(dashes, "\nDiscrepancy between ACAI under SFI vs. PFI: \n"))
#'     print(round(x$`h ACAI SFI-PFI`[c(1, item_set + 1), , drop = FALSE], 3))
#'     cat("\nImpact of deleting an item on the discrepancy between ACAI \nunder SFI vs. ACAI under PFI:\n")
#'     print(round(x$`delta h ACAI SFI-PFI (deletion)`[c(item_set), , drop = FALSE], 3))
#'     cat(paste0("\n", stars, "\nAdverse Impact (AI) ratio for item subsets by invariance condition:\n", stars, "\n"))
#'     print(round(x$AI[c(1, item_set + 1), , drop = FALSE], 3))
#'     cat(paste0("\n", stars, "\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS\n", stars))
#'     cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
#'     print(round(x$`h CAI Ref-EF`[c(1, item_set + 1), , drop = FALSE], 3))
#'     cat(paste0(dashes, "\nImpact of deleting an item on the discrepancy between CAI of\nreference vs. Efocal groups under PFI:\n"))
#'     print(round(x$`delta h CAI Ref-EF (deletion)`[c(item_set), , drop = FALSE], 3))
#'     cat(paste0("\n", stars, "\nCOMPARING CAI UNDER STRICT AND PARTIAL FACTORIAL INVARIANCE\n", stars))
#'     cat("\nDiscrepancy between CAI under SFI vs. PFI for the reference group:\n")
#'     print(round(x$`h CAI SFI-PFI`[[1]][c(1, item_set + 1), , drop = FALSE], 3))
#'     cat("\nDiscrepancy between CAI under SFI vs. PFI for the focal group:\n")
#'     print(round(x$`h CAI SFI-PFI`[[2]][c(1, item_set + 1), , drop = FALSE], 3))
#'     cat(paste0(dashes, "\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the reference group:\n"))
#'     print(round(x$`delta h SFI-PFI (deletion)`[[1]][c(item_set), , drop = FALSE], 3))
#'     cat("\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the focal group:\n")
#'     print(round(x$`delta h SFI-PFI (deletion)`[[2]][c(item_set), , drop = FALSE], 3))
#'     print(x$`PartInv by groups`$reference)#$outputlist)
#'     print(x$`PartInv by groups`$focal)#$outputlist[c(1, item_set+1)])
#'     print(x$PartInv$strict)#$outputlist[c(1, item_set+1)])
#'     print(x$PartInv$partial)#$outputlist[c(1, item_set+1)])
#'   }
#' }
