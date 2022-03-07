#'@export 
setClass("PartInv",
         representation(
           propsel = "numeric", 
           cutpt_xi = "numeric",  cutpt_z = "numeric", 
           summary = "data.frame", 
           ai_ratio = "numeric", 
           plot = "recordedplot"
         )
)
#'@export 
print.PartInv <- function(obj, itemset = NULL) {
  cat("Proportion selected: ", round(obj$propsel,3), "\n")
  cat("Cutpoint on the latent scale (xi): ", round(obj$cutpt_xi, 3),"\n")
  cat("Cutpoint on the observed scale (Z): ", round(obj$cutpt_z, 3),"\n")
  cat("AI ratio: ", round(obj$ai_ratio, 3),"\n\n")
  cat("Selection Accuracy Indices:\n")
  rownames(obj$summary) <- c("True Positive", "False Positive", "True Negative",
                             "False Negative", "Prop. Selected", "Success Ratio",
                             "Sensitivity", "Specificity")
  #c("TP", "FP", "TN", "FN", "PS", "SR", "SE", "SP")
  print(obj$summary)
  if(!is.null(obj$plot)){
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
print.PartInvList <- function(obj) {
  if(obj$condition=="partial"){
    cat("\n*********************************************************************")
    cat("\nPartInv() outputs under Partial Factorial Invariance (PFI)\n")
    cat("*********************************************************************")
    cat("\n\nUnder PFI, full item set:\n\n")
    print.PartInv(obj$outputlist[[1]])
    for(i in 2:length(obj$outputlist)){
      cat("________________________________________________________________")
      cat("\n\nUnder PFI, if item", i-1,"is dropped:\n\n")
      print.PartInv(obj$outputlist[[i]])
    }
  }
    if(obj$condition=="strict"){
    cat("\n***************************************************************")
    cat("\nPartInv() outputs under Strict Factorial Invariance (SFI)\n")
    cat("*********************************************************************")
    cat("\n\nUnder SFI, full item set:\n\n")
    print.PartInv(obj$outputlist[[1]])
    for(i in 2:length(obj$outputlist)){
      cat("________________________________________________________________")
      cat("\n\nUnder SFI, if item", i-1,"is dropped:\n\n")
      print.PartInv(obj$outputlist[[i]])
    }
    }
  if(obj$condition=="ref") {
    cat("\n*********************************************************************")
    cat("\nSAI under SFI vs. PFI for the reference group\n")
    cat("*********************************************************************")
    cat("\n\nReference group, full item set:\n\n")
    print(obj$outputlist[[1]])
    for(i in 2:length(obj$outputlist)){
      cat("________________________________________________________________")
      cat("\n\nReference group, if item", i-1,"is dropped:\n\n")
      print(obj$outputlist[[i]])
    }
  }
  if(obj$condition=="foc") {
    cat("\n*********************************************************************")
    cat("\nSAI under SFI vs. PFI for the focal group\n")
    cat("*********************************************************************")
    cat("\n\nFocal group, full item set:\n\n")
    print(obj$outputlist[[1]])
    for(i in 2:length(obj$outputlist)){
      cat("________________________________________________________________")
      cat("\n\nFocal group, if item", i-1,"is dropped:\n\n")
      print(obj$outputlist[[i]])
    }
  }
}
#'@export 
setClass("itemdeletion",
  representation(
    h_overall_par = "matrix",
    delta_h_str_par_overall =  "matrix",
    AI = "matrix",
    deltaREf = "matrix",
    h_R_Ef = "matrix",
    delta_h_str_vs_par = "matrix",
    str_vs_par = "list",
    overall = "matrix",
    h_overall_str_par = "matrix",
    Ref_foc = "list",
    PartInv = "list",
    detail = "logical"
)
)
#'@export 
print.itemdeletion <- function(obj){
  if(obj$detail==FALSE) {
    cat("\n******************************************************************")
    cat("\nOVERALL SELECTION ACCURACY INDICES (SAI*)\n")
    cat("******************************************************************\n")
    cat("SAI* under PFI computed for item subsets:\n")
    print(obj[[8]])
    cat("\nImpact of deleting an item on SAI* under PFI:\n") 
    print(obj[[1]])
    cat("________________________________________________________________")
    cat("\n\nDiscrepancy between SAI* under SFI vs. PFI: \n")
    print(obj[[9]])
    cat("\nImpact of deleting an item on the discrepancy between SAI* \nunder SFI vs. SAI* under PFI:\n")
    print(obj[[2]]) 
    cat("\n******************************************************************")
    cat("\nAdverse Impact (AI) ratio for item subsets:")
    cat("\n******************************************************************\n")
    print(obj$AI) #AI
    cat("\n******************************************************************")
    cat("\nCOMPARING SAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS")
    cat("\n******************************************************************\n")
    cat("\nDiscrepancy between SAI of reference vs. Efocal groups under PFI:\n")
    print(obj[[4]][,6:8]) #h_R_Ef
    cat("________________________________________________________________")
    cat("\n\nImpact of deleting an item on the discrepancy between SAI of\nreference vs. Efocal groups under PFI:\n")
    print(obj[[5]][,6:8]) #delta_R_Ef
    cat("\n******************************************************************")
    cat("\nCOMPARING SAI UNDER STRICT AND PARTIAL FACTORIAL INVARIANCE")
    cat("\n******************************************************************\n")
    cat("\nDiscrepancy between SAI under SFI vs. PFI for the reference group:\n")
    print(obj[[7]][[1]][,6:8]) 
    cat("\nDiscrepancy between SAI under SFI vs. PFI for the focal group:\n")
    print(obj[[7]][[2]][,6:8])
    cat("\n________________________________________________________________")
    cat("\n\nImpact of deleting an item on the difference between SAI under \nSFI vs. PFI for the reference group:\n")
    print(obj[[6]][[1]][,6:8]) #str_vs_par ref
    cat("\nImpact of deleting an item on the difference between SAI under \nSFI vs. PFI for the focal group:\n")
    print(obj[[6]][[2]][,6:8]) #str_vs_par foc
  }
  if(obj$detail){
    cat("\n*********************************************************************")
    cat("\nOVERALL SELECTION ACCURACY INDICES (SAI*)\n")
    cat("*********************************************************************\n")
    cat("SAI* under PFI computed for item subsets:\n")
    print(obj[[8]])
    cat("\nImpact of deleting an item on SAI* under PFI:\n") 
    print(obj[[1]])
    cat("________________________________________________________________")
    cat("\n\nDiscrepancy between SAI* under SFI vs. PFI: \n")
    print(obj[[9]])
    cat("\nImpact of deleting an item on the discrepancy between SAI* \nunder SFI vs. SAI* under PFI:\n")
    print(obj[[2]]) 
    cat("\n*********************************************************************")
    cat("\nAdverse Impact (AI) ratio for item subsets:")
    cat("\n*********************************************************************\n")
    print(obj$AI) #AI
    cat("\n*********************************************************************")
    cat("\nCOMPARING SAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS")
    cat("\n*********************************************************************\n")
    cat("\nDiscrepancy between SAI of reference vs. Efocal groups under PFI:\n")
    print(obj[[4]]) #h_R_Ef
    cat("________________________________________________________________\n")
    cat("\nImpact of deleting an item on the discrepancy between SAI of\nreference vs. Efocal groups under PFI:\n")
    print(obj[[5]]) #delta_R_Ef
    cat("\n*********************************************************************")
    cat("\nCOMPARING SAI UNDER STRICT AND PARTIAL FACTORIAL INVARIANCE")
    cat("\n*********************************************************************\n")
    cat("\nDiscrepancy between SAI under SFI vs. PFI for the reference group:\n")
    print(obj[[7]][[1]])
    cat("\nDiscrepancy between SAI under SFI vs. PFI for the focal group:\n")
    print(obj[[7]][[2]])
    cat("\n________________________________________________________________")
    cat("\nImpact of deleting an item on the difference between SAI under \nSFI vs. PFI for the reference group:\n")
    print(obj[[6]][[1]]) #str_vs_par ref
    cat("\nImpact of deleting an item on the difference between SAI under \nSFI vs. PFI for the focal group:\n")
    print(obj[[6]][[2]]) #str_vs_par foc
    print(obj[[10]]$reference)#h_str_vs_par_list
    print(obj[[10]]$focal)
    print(obj[[11]]$strict)#cat("\nPartInv outputs for each item set\n")
    print(obj[[11]]$partial)
  }
}
