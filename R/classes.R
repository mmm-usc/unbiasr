# dividers
stars <-
  "***********************************************************************"
dashes <-
  "-----------------------------------------------------------------------"

#'@export
setClass("PartInvSummary",
         representation(
           summary = "data.frame"
         )
)
#'@export
print.PartInvSummary <- function(objsum) {
  cat("Classification Accuracy Indices:\n")
  rownames(objsum) <- c("True Positive", "False Positive", "True Negative",
                        "False Negative",  "Proportion Selected",
                        "Success Ratio", "Sensitivity", "Specificity")
 # c("TP", "FP", "TN", "FN", "PS", "SR", "SE", "SP")
  print(round(objsum, 3))
}

#'@export
setClass("PartInv_groups",
         representation(
           tab = "data.frame"
         )
)
#'@export
print.PartInv_groups <- function(objsum) {
  rownames(objsum) <- c("True Positive", "False Positive", "True Negative",
                        "False Negative",  "Proportion Selected",
                        "Success Ratio", "Sensitivity", "Specificity")
  print(round(objsum, 3))
}

#'@export
setClass("PartInv",
         representation(
           propsel = "numeric",
           cutpt_xi = "numeric", cutpt_z = "numeric",
           summary = "PartInvSummary",
           ai_ratio = "numeric",
           plot = "recordedplot",
           summary_mi = "PartInvSummary",
           p_mi = "recordedplot"
         )
)
#'@export
print.PartInv <- function(obj) {
  cat("Proportion selected: ", round(obj$propsel, 3), "\n")
  cat("Cutpoint on the latent scale (xi): ", round(obj$cutpt_xi, 3), "\n")
  cat("Cutpoint on the observed scale (Z): ", round(obj$cutpt_z, 3), "\n")
  cat("AI ratio: ", round(obj$ai_ratio, 3), "\n\n")
  print.PartInvSummary(obj$summary)
  if (!is.null(obj$plot)) {
    print(obj$plot)
  }
#  if(!is.null(obj$summary_mi)) {
#  cat("\nMI ")
 #   print.PartInvSummary(obj$summary_mi)
#  }
  #if(!is.null(obj$p_mi)){
  #  print(obj$plot)
  #}
}

#'@export
setClass("PartInvList",
         representation(
           outputlist = "list",
           condition = "character",
           itemset = "vector"
         )
)
#'@export
print.PartInvList <- function(obj) {
  if (obj$condition == "partial") {
    cat(paste0("\n", stars))
    cat("\nPartInv() outputs under Partial Factorial Invariance (PFI)\n")
    cat(stars)
    cat("\n\nUnder PFI, full item set:\n\n")
    print.PartInv(obj$outputlist[[1]])
    for (i in obj$itemset) {
      cat(dashes)
      cat("\n\nUnder PFI, if item", i, "is dropped:\n\n")
      print.PartInv(obj$outputlist[[i]])
    }
  }
    if (obj$condition == "strict") {
    cat(paste0("\n", stars))
    cat("\nPartInv() outputs under Strict Factorial Invariance (SFI)\n")
    cat(stars)
    cat("\n\nUnder SFI, full item set:\n\n")
    print.PartInv(obj$outputlist[[1]])
    for (i in obj$itemset) {
      cat(dashes)
      cat("\n\nUnder SFI, if item", i, "is dropped:\n\n")
      print.PartInv(obj$outputlist[[i]])
    }
    }
  if (obj$condition == "ref") {
    cat(paste0("\n", stars))
    cat("\nCAI under SFI vs. PFI for the reference group\n")
    cat(stars)
    cat("\n\nReference group, full item set:\n")
    print.PartInv_groups(obj$outputlist[[1]])
    for (i in obj$itemset) {
      cat(dashes)
      cat("\n\nReference group, if item", i,"is dropped:\n")
      print.PartInv_groups(obj$outputlist[[i]])
    }
  }
  if (obj$condition == "foc") {
    cat(paste0("\n", stars))
    cat("\nCAI under SFI vs. PFI for the focal group\n")
    cat(stars)
    cat("\n\nFocal group, full item set:\n")
    print.PartInv_groups(obj$outputlist[[1]])
    for (i in obj$itemset) {
      cat(dashes)
      cat("\n\nFocal group, if item", i, "is dropped:\n")
      print.PartInv_groups(obj$outputlist[[i]])
    }
  }
}
#'@export
setClass("itemdeletion",
  representation(
    h_aggregate_par = "matrix",
    delta_h_str_par_aggregate =  "matrix",
    AI = "matrix",
    deltaREf = "matrix",
    h_R_Ef = "matrix",
    delta_h_str_vs_par = "matrix",
    str_vs_par = "list",
    aggregate = "matrix",
    h_aggregate_str_par = "matrix",
    Ref_foc = "PartInvList",
    PartInv = "PartInvList",
    detail = "logical",
    return_items = "list"
)
)
#'@export
print.itemdeletion <- function(obj) {
  item_set <- obj$return_items
  # ------------------------------------------------------------------------
  # DEFAULT OUTPUT: only biased items printed
  if (!obj$detail) {
    cat(paste0(stars, "\nAGGREGATE CLASSIFICATION ACCURACY INDICES (ACAI)\n", stars))
    cat("\nACAI under PFI computed for item subsets:\n")
    print(round(obj$`ACAI`[c(1, item_set + 1), , drop = FALSE], 3))
    cat("\nImpact of deleting an item on ACAI under PFI:\n")
    print(round(obj$`h ACAI (deletion)`[c(item_set), , drop = FALSE], 3))
    cat(paste0("\n", stars, "\nAdverse Impact (AI) ratio for item subsets by invariance condition:\n", stars, "\n"))
    print(round(obj$AI[c(1, item_set + 1), , drop = FALSE], 3))
    cat(paste0("\n", stars, "\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS\n", stars))
    cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
    print(round(obj$`h CAI Ref-EF`[c(1, item_set + 1), 5:8, drop = FALSE], 3))
    cat(paste0(dashes, "\nImpact of deleting an item on the discrepancy between CAI of\nreference vs. Efocal groups under PFI:\n"))
    print(round(obj$`delta h CAI Ref-EF (deletion)`[c(item_set), 5:8, drop = FALSE], 3))
  }
 # ------------------------------------------------------------------------
  if (obj$detail) { #
    cat(paste0(stars, "\nAGGREGATE CLASSIFICATION ACCURACY INDICES (ACAI)\n", stars))
    cat("\nACAI under PFI computed for item subsets:\n")
    print(round(obj$`ACAI`[c(1, item_set + 1), , drop = FALSE], 3))
    cat("\nImpact of deleting an item on ACAI under PFI:\n")
    print(round(obj$`h ACAI (deletion)`[c(item_set), , drop = FALSE], 3))
    cat(paste0(dashes, "\nDiscrepancy between ACAI under SFI vs. PFI: \n"))
    print(round(obj$`h ACAI SFI-PFI`[c(1, item_set + 1), , drop = FALSE], 3))
    cat("\nImpact of deleting an item on the discrepancy between ACAI \nunder SFI vs. ACAI under PFI:\n")
    print(round(obj$`delta h ACAI SFI-PFI (deletion)`[c(item_set), , drop = FALSE], 3))
    cat(paste0("\n", stars, "\nAdverse Impact (AI) ratio for item subsets by invariance condition:\n", stars, "\n"))
    print(round(obj$AI[c(1, item_set + 1), , drop = FALSE], 3))
    cat(paste0("\n", stars, "\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS\n", stars))
    cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
    print(round(obj$`h CAI Ref-EF`[c(1, item_set + 1), , drop = FALSE], 3))
    cat(paste0(dashes, "\nImpact of deleting an item on the discrepancy between CAI of\nreference vs. Efocal groups under PFI:\n"))
    print(round(obj$`delta h CAI Ref-EF (deletion)`[c(item_set), , drop = FALSE], 3))
    cat(paste0("\n", stars, "\nCOMPARING CAI UNDER STRICT AND PARTIAL FACTORIAL INVARIANCE\n", stars))
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the reference group:\n")
    print(round(obj$`h CAI SFI-PFI`[[1]][c(1, item_set + 1), , drop = FALSE], 3))
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the focal group:\n")
    print(round(obj$`h CAI SFI-PFI`[[2]][c(1, item_set + 1), , drop = FALSE], 3))
    cat(paste0(dashes, "\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the reference group:\n"))
    print(round(obj$`delta h SFI-PFI (deletion)`[[1]][c(item_set), , drop = FALSE], 3))
    cat("\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the focal group:\n")
    print(round(obj$`delta h SFI-PFI (deletion)`[[2]][c(item_set), , drop = FALSE], 3))
    print(obj$`PartInv by groups`$reference)#$outputlist)
    print(obj$`PartInv by groups`$focal)#$outputlist[c(1, item_set+1)])
    print(obj$PartInv$strict)#$outputlist[c(1, item_set+1)])
    print(obj$PartInv$partial)#$outputlist[c(1, item_set+1)])
  }
}
