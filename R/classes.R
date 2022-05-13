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
                        "Success Ratio","Sensitivity", "Specificity")
 # c("TP", "FP", "TN", "FN", "PS", "SR", "SE", "SP")
  print(round(objsum, 3))
}

#'@export 
setClass("PartInv",
         representation(
           propsel = "numeric", 
           cutpt_xi = "numeric",  cutpt_z = "numeric", 
           summary = "PartInvSummary", 
           ai_ratio = "numeric", 
           plot = "recordedplot",
           summary_mi = "PartInvSummary",
           p_mi = "recordedplot"
         )
)
#'@export 
print.PartInv <- function(obj, itemset = NULL) {
  cat("Proportion selected: ", round(obj$propsel, 3), "\n")
  cat("Cutpoint on the latent scale (xi): ", round(obj$cutpt_xi, 3),"\n")
  cat("Cutpoint on the observed scale (Z): ", round(obj$cutpt_z, 3),"\n")
  cat("AI ratio: ", round(obj$ai_ratio, 3),"\n\n")
  print.PartInvSummary(obj$summary)
  if(!is.null(obj$plot)){
    print(obj$plot)
  }
  if(!is.null(obj$summary_mi)) {
  cat("\nMI ")
    print.PartInvSummary(obj$summary_mi) 
  }
  if(!is.null(obj$p_mi)){
    print(obj$plot)
  }
}

#'@export 
setClass("PartInvList",
         representation(
           outputlist = "list", 
           condition = "character"
         )
)
#'@export 
print.PartInvList <- function(obj, itemset = NULL) {
  if(obj$condition=="partial"){
    cat(stars)
    cat("\nPartInv() outputs under Partial Factorial Invariance (PFI)\n")
    cat(stars)
    cat("\n\nUnder PFI, full item set:\n\n")
    print.PartInv(round(obj$outputlist[[1]],3))
    for(i in itemset){
      cat(dashes)
      cat("\n\nUnder PFI, if item", i-1,"is dropped:\n\n")
      print.PartInv(round(obj$outputlist[[i]],3))
    }
  }
    if(obj$condition=="strict"){
    cat(stars)
    cat("\nPartInv() outputs under Strict Factorial Invariance (SFI)\n")
    cat(stars)
    cat("\n\nUnder SFI, full item set:\n\n")
    print.PartInv(round(obj$outputlist[[1]],3))
    for(i in itemset){
      cat(dashes)
      cat("\n\nUnder SFI, if item", i-1,"is dropped:\n\n")
      print.PartInv(round(obj$outputlist[[i]],3))
    }
    }
  if(obj$condition=="ref") {
    cat(stars)
    cat("\nCAI under SFI vs. PFI for the reference group\n")
    cat(stars)
    cat("\n\nReference group, full item set:\n\n")
    print(round(obj$outputlist[[1]],3))
    for(i in itemset){
      cat(dashes)
      cat("\n\nReference group, if item", i-1,"is dropped:\n\n")
      print(round(obj$outputlist[[i]],3))
    }
  }
  if(obj$condition=="foc") {
    cat(stars)
    cat("\nCAI under SFI vs. PFI for the focal group\n")
    cat(stars)
    cat("\n\nFocal group, full item set:\n\n")
    print(round(obj$outputlist[[1]],3))
    for(i in itemset){
      cat(dashes)
      cat("\n\nFocal group, if item", i-1,"is dropped:\n\n")
      print(round(obj$outputlist[[i]],3))
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
    Ref_foc = "list",
    PartInv = "list",
    detail = "logical",
    return_items = "list"
)
)
#'@export 
print.itemdeletion <- function(obj){
  item_set <- obj$return_items
  # ------------------------------------------------------------------------ 
  # DEFAULT OUTPUT: only biased items printed 
  if(!is.null(item_set) & !obj$detail){
    cat(paste0(stars,"\nAGGREGATE CLASSIFICATION ACCURACY INDICES (ACAI)\n", stars))
    cat("\nACAI under PFI computed for item subsets:\n")
    print(round(obj[[8]][c(1, item_set+1),, drop = FALSE], 3))
    cat("\nImpact of deleting an item on ACAI under PFI:\n") 
    print(round(obj[[1]][c(item_set),, drop = FALSE], 3))
    cat(paste0("\n", stars,"\nAdverse Impact (AI) ratio for item subsets by invariance condition:\n",stars, "\n"))
    print(round(obj$AI[c(1, item_set+1),, drop = FALSE], 3)) #AI
    cat(paste0("\n",stars,"\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS\n", stars))
    cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
    print(round(obj[[4]][c(1, item_set+1), 5:8, drop = FALSE], 3)) #h_R_Ef
    cat(paste0(dashes,"\nImpact of deleting an item on the discrepancy between CAI of\nreference vs. Efocal groups under PFI:\n"))
    print(round(obj[[5]][c(item_set), 5:8, drop = FALSE], 3)) #delta_R_Ef
  }
 # ------------------------------------------------------------------------
  if(!is.null(item_set) & obj$detail){ # 
    cat(paste0(stars,"\nAGGREGATE CLASSIFICATION ACCURACY INDICES (ACAI)\n", stars))
    cat("\nACAI under PFI computed for item subsets:\n")
    print(round(obj[[8]][c(1, item_set+1),, drop = FALSE],3))
    cat("\nImpact of deleting an item on ACAI under PFI:\n") 
    print(round(obj[[1]][c(item_set),, drop = FALSE],3))
    cat(paste0(dashes, "\nDiscrepancy between ACAI under SFI vs. PFI: \n"))
    print(round(obj[[9]][c(1, item_set+1),, drop = FALSE],3))
    cat("\nImpact of deleting an item on the discrepancy between ACAI \nunder SFI vs. ACAI under PFI:\n")
    print(round(obj[[2]][c(item_set),, drop = FALSE],3)) 
    cat(paste0("\n", stars,"\nAdverse Impact (AI) ratio for item subsets by invariance condition:\n",stars, "\n"))
    print(round(obj$AI[c(1, item_set+1),, drop = FALSE],3)) #AI
    cat(paste0("\n",stars,"\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS\n", stars))
    cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
    print(round(obj[[4]][c(1, item_set+1),, drop = FALSE],3)) #h_R_Ef
    cat(paste0(dashes,"\nImpact of deleting an item on the discrepancy between CAI of\nreference vs. Efocal groups under PFI:\n"))
    print(round(obj[[5]][c( item_set),, drop = FALSE],3)) #delta_R_Ef
    cat(paste0("\n", stars,"\nCOMPARING CAI UNDER STRICT AND PARTIAL FACTORIAL INVARIANCE\n", stars))
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the reference group:\n")
    print(round(obj[[7]][[1]][c(1, item_set+1),, drop = FALSE],3))
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the focal group:\n")
    print(round(obj[[7]][[2]][c(1, item_set+1),, drop = FALSE],3))
    cat(paste0(dashes,"\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the reference group:\n"))
    print(round(obj[[6]][[1]][c(item_set),, drop = FALSE],3)) #str_vs_par foc
    cat("\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the focal group:\n")
    print(round(obj[[6]][[2]][c(item_set),, drop = FALSE],3)) #str_vs_par foc
    print(obj[[10]]$reference$outputlist[c(1, item_set+1) ])#h_str_vs_par_list
    print(obj[[10]]$focal$outputlist[c(1, item_set+1)])
    print(obj[[11]]$strict$outputlist[c(1, item_set+1)])#cat("\nPartInv outputs for each item set\n")
    print(obj[[11]]$partial$outputlist[c(1, item_set+1)])
  }
  # ------------------------------------------------------------------------
  if(is.null(item_set) & obj$detail==FALSE) {
    cat(paste0(stars,"\nAGGREGATE CLASSIFICATION ACCURACY INDICES (ACAI)\n", stars))
    cat("\nACAI under PFI computed for item subsets:\n")
    print(round(obj[[8]],3))
    cat("\nImpact of deleting an item on ACAI under PFI:\n") 
    print(round(obj[[1]],3))
    cat(paste0(dashes, "\n\nDiscrepancy between ACAI under SFI vs. PFI: \n"))
    print(round(obj[[9]],3))
    cat("\nImpact of deleting an item on the discrepancy between ACAI \nunder SFI vs. ACAI under PFI:\n")
    print(round(obj[[2]],3)) 
    cat(paste0(stars,"\nAdverse Impact (AI) ratio for item subsets by invariance condition:", stars))
    print(obj$AI) #AI
    cat(paste0(stars,"\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS", stars))
    cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
    print(round(obj[[4]][,5:8],3)) #h_R_Ef
    cat(paste0(dashes,"\n\nImpact of deleting an item on the discrepancy between CAI of\nreference vs. Efocal groups under PFI:\n"))
    print(round(obj[[5]][,5:8],3)) #delta_R_Ef
    cat(paste0(stars,"\nCOMPARING CAI UNDER STRICT AND PARTIAL FACTORIAL INVARIANCE",stars))
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the reference group:\n")
    print(round(obj[[7]][[1]][,5:8],3)) 
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the focal group:\n")
    print(round(obj[[7]][[2]][,5:8],3))
    cat(paste0(dashes,"\n\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the reference group:\n"))
    print(round(obj[[6]][[1]][,5:8],3)) #str_vs_par ref
    cat("\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the focal group:\n")
    print(round(obj[[6]][[2]][,5:8],3)) #str_vs_par foc
  }
  if(is.null(item_set) & obj$detail){
    cat(paste0(stars,"\nAGGREGATE CLASSIFICATION ACCURACY INDICES (ACAI)\n",stars))
    cat("ACAI under PFI computed for item subsets:\n")
    print(round(obj[[8]],3))
    cat("\nImpact of deleting an item on ACAI under PFI:\n") 
    print(round(obj[[1]],3))
    cat(dashes,"\n\nDiscrepancy between ACAI under SFI vs. PFI: \n")
    print(round(obj[[9]],3))
    cat("\nImpact of deleting an item on the discrepancy between ACAI \nunder SFI vs. ACAI under PFI:\n")
    print(round(obj[[2]],3)) 
    cat(stars,"\nAdverse Impact (AI) ratio for item subsets by invariance condition:\n",stars)
    print(round(obj$AI,3)) #AI
    cat(stars,"\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS\n",stars)
    cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
    print(round(obj[[4]],3)) #h_R_Ef
    cat(dashes,"\nImpact of deleting an item on the discrepancy between CAI of\nreference vs. Efocal groups under PFI:\n")
    print(round(obj[[5]],3)) #delta_R_Ef
    cat(stars,"\nCOMPARING CAI UNDER STRICT AND PARTIAL FACTORIAL INVARIANCE\n",stars)
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the reference group:\n")
    print(round(obj[[7]][[1]],3))
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the focal group:\n")
    print(round(obj[[7]][[2]],3))
    cat(dashes,"\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the reference group:\n")
    print(round(obj[[6]][[1]],3)) #str_vs_par ref
    cat("\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the focal group:\n")
    print(round(obj[[6]][[2]],3)) #str_vs_par foc
    print(obj[[10]]$reference)#h_str_vs_par_list
    print(obj[[10]]$focal)
    print(obj[[11]]$strict)#cat("\nPartInv outputs for each item set\n")
    print(obj[[11]]$partial)
  }
}

