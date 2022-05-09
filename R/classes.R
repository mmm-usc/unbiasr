# dividers
stars <- 
  "***********************************************************************"
dashes <- 
  "-----------------------------------------------------------------------"
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
  cat("Classification Accuracy Indices:\n")
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
    cat(stars)
    cat("\nPartInv() outputs under Partial Factorial Invariance (PFI)\n")
    cat(stars)
    cat("\n\nUnder PFI, full item set:\n\n")
    print.PartInv(obj$outputlist[[1]])
    for(i in 2:length(obj$outputlist)){
      cat(dashes)
      cat("\n\nUnder PFI, if item", i-1,"is dropped:\n\n")
      print.PartInv(obj$outputlist[[i]])
    }
  }
    if(obj$condition=="strict"){
    cat(stars)
    cat("\nPartInv() outputs under Strict Factorial Invariance (SFI)\n")
    cat(stars)
    cat("\n\nUnder SFI, full item set:\n\n")
    print.PartInv(obj$outputlist[[1]])
    for(i in 2:length(obj$outputlist)){
      cat(dashes)
      cat("\n\nUnder SFI, if item", i-1,"is dropped:\n\n")
      print.PartInv(obj$outputlist[[i]])
    }
    }
  if(obj$condition=="ref") {
    cat(stars)
    cat("\nCAI under SFI vs. PFI for the reference group\n")
    cat(stars)
    cat("\n\nReference group, full item set:\n\n")
    print(obj$outputlist[[1]])
    for(i in 2:length(obj$outputlist)){
      cat(dashes)
      cat("\n\nReference group, if item", i-1,"is dropped:\n\n")
      print(obj$outputlist[[i]])
    }
  }
  if(obj$condition=="foc") {
    cat(stars)
    cat("\nCAI under SFI vs. PFI for the focal group\n")
    cat(stars)
    cat("\n\nFocal group, full item set:\n\n")
    print(obj$outputlist[[1]])
    for(i in 2:length(obj$outputlist)){
      cat(dashes)
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
    detail = "logical",
    return_items = "list"
)
)
#'@export 
print.itemdeletion <- function(obj){
  item_set <- obj$return_items
  if(!is.null(item_set) & !obj$detail){ #default - not detailed, only the biased items
    cat(paste0(stars,"\nCOMPOSITE CLASSIFICATION ACCURACY INDICES (CAI*)\n", stars))
    cat("\nCAI* under PFI computed for item subsets:\n")
    print(obj[[8]][c(1, item_set+1),, drop = FALSE])
    cat("\nImpact of deleting an item on CAI* under PFI:\n") 
    print(obj[[1]][c(item_set),, drop = FALSE])
    #cat(paste0(dashes, "\nDiscrepancy between CAI* under SFI vs. PFI: \n"))
    #print(obj[[9]][c(1, item_set+1),, drop = FALSE])
    #cat("\nImpact of deleting an item on the discrepancy between CAI* \nunder SFI vs. CAI* under PFI:\n")
    #print(obj[[2]][c(item_set),, drop = FALSE]) 
    cat(paste0("\n", stars,"\nAdverse Impact (AI) ratio for item subsets by invariance condition:\n",stars, "\n"))
    print(obj$AI[c(1, item_set+1),, drop = FALSE]) #AI
    cat(paste0("\n",stars,"\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS\n", stars))
    cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
    print(obj[[4]][c(1, item_set+1),5:8, drop = FALSE]) #h_R_Ef
    cat(paste0(dashes,"\nImpact of deleting an item on the discrepancy between CAI of\nreference vs. Efocal groups under PFI:\n"))
    print(obj[[5]][c(item_set),5:8, drop = FALSE]) #delta_R_Ef
  #  cat(paste0("\n", stars,"\nCOMPARING CAI UNDER STRICT AND PARTIAL FACTORIAL INVARIANCE\n", stars))
  #  cat("\nDiscrepancy between CAI under SFI vs. PFI for the reference group:\n")
  #  print(obj[[7]][[1]][c(1, item_set+1),5:8, drop = FALSE])
  #  cat("\nDiscrepancy between CAI under SFI vs. PFI for the focal group:\n")
  #  print(obj[[7]][[2]][c(1, item_set+1),5:8, drop = FALSE])
  #  cat(paste0(dashes,"\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the reference group:\n"))
  #  print(obj[[6]][[1]][c(item_set),5:8, drop = FALSE]) #str_vs_par foc
  #  cat("\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the focal group:\n")
  #  print(obj[[6]][[2]][c(item_set),5:8, drop = FALSE]) #str_vs_par foc
  }
  if(!is.null(item_set) & obj$detail){ # 
    cat(paste0(stars,"\nCOMPOSITE CLASSIFICATION ACCURACY INDICES (CAI*)\n", stars))
    cat("\nCAI* under PFI computed for item subsets:\n")
    print(obj[[8]][c(1, item_set+1),, drop = FALSE])
    cat("\nImpact of deleting an item on CAI* under PFI:\n") 
    print(obj[[1]][c(item_set),, drop = FALSE])
    cat(paste0(dashes, "\nDiscrepancy between CAI* under SFI vs. PFI: \n"))
    print(obj[[9]][c(1, item_set+1),, drop = FALSE])
    cat("\nImpact of deleting an item on the discrepancy between CAI* \nunder SFI vs. CAI* under PFI:\n")
    print(obj[[2]][c(item_set),, drop = FALSE]) 
    cat(paste0("\n", stars,"\nAdverse Impact (AI) ratio for item subsets by invariance condition:\n",stars, "\n"))
    print(obj$AI[c(1, item_set+1),, drop = FALSE]) #AI
    cat(paste0("\n",stars,"\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS\n", stars))
    cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
    print(obj[[4]][c(1, item_set+1),, drop = FALSE]) #h_R_Ef
    cat(paste0(dashes,"\nImpact of deleting an item on the discrepancy between CAI of\nreference vs. Efocal groups under PFI:\n"))
    print(obj[[5]][c( item_set),, drop = FALSE]) #delta_R_Ef
    cat(paste0("\n", stars,"\nCOMPARING CAI UNDER STRICT AND PARTIAL FACTORIAL INVARIANCE\n", stars))
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the reference group:\n")
    print(obj[[7]][[1]][c(1, item_set+1),, drop = FALSE])
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the focal group:\n")
    print(obj[[7]][[2]][c(1, item_set+1),, drop = FALSE])
    cat(paste0(dashes,"\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the reference group:\n"))
    print(obj[[6]][[1]][c(item_set),, drop = FALSE]) #str_vs_par foc
    cat("\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the focal group:\n")
    print(obj[[6]][[2]][c(item_set),, drop = FALSE]) #str_vs_par foc
    print(obj[[10]]$reference$outputlist[c(1, item_set+1) ])#h_str_vs_par_list
    print(obj[[10]]$focal$outputlist[c(1, item_set+1)])
    print(obj[[11]]$strict$outputlist[c(1, item_set+1)])#cat("\nPartInv outputs for each item set\n")
    print(obj[[11]]$partial$outputlist[c(1, item_set+1)])
  }
  if(is.null(item_set) & obj$detail==FALSE) {
    cat(paste0(stars,"\nCOMPOSITE CLASSIFICATION ACCURACY INDICES (CAI*)\n", stars))
    cat("\nCAI* under PFI computed for item subsets:\n")
    print(obj[[8]])
    cat("\nImpact of deleting an item on CAI* under PFI:\n") 
    print(obj[[1]])
    cat(paste0(dashes, "\n\nDiscrepancy between CAI* under SFI vs. PFI: \n"))
    print(obj[[9]])
    cat("\nImpact of deleting an item on the discrepancy between CAI* \nunder SFI vs. CAI* under PFI:\n")
    print(obj[[2]]) 
    cat(paste0(stars,"\nAdverse Impact (AI) ratio for item subsets by invariance condition:", stars))
    print(obj$AI) #AI
    cat(paste0(stars,"\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS", stars))
    cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
    print(obj[[4]][,5:8]) #h_R_Ef
    cat(paste0(dashes,"\n\nImpact of deleting an item on the discrepancy between CAI of\nreference vs. Efocal groups under PFI:\n"))
    print(obj[[5]][,5:8]) #delta_R_Ef
    cat(paste0(stars,"\nCOMPARING CAI UNDER STRICT AND PARTIAL FACTORIAL INVARIANCE",stars))
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the reference group:\n")
    print(obj[[7]][[1]][,5:8]) 
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the focal group:\n")
    print(obj[[7]][[2]][,5:8])
    cat(paste0(dashes,"\n\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the reference group:\n"))
    print(obj[[6]][[1]][,5:8]) #str_vs_par ref
    cat("\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the focal group:\n")
    print(obj[[6]][[2]][,5:8]) #str_vs_par foc
  }
  if(is.null(item_set) & obj$detail){
    cat(paste0(stars,"\nCOMPOSITE CLASSIFICATION ACCURACY INDICES (CAI*)\n",stars))
    cat("CAI* under PFI computed for item subsets:\n")
    print(obj[[8]])
    cat("\nImpact of deleting an item on CAI* under PFI:\n") 
    print(obj[[1]])
    cat(dashes,"\n\nDiscrepancy between CAI* under SFI vs. PFI: \n")
    print(obj[[9]])
    cat("\nImpact of deleting an item on the discrepancy between CAI* \nunder SFI vs. CAI* under PFI:\n")
    print(obj[[2]]) 
    cat(stars,"\nAdverse Impact (AI) ratio for item subsets by invariance condition:\n",stars)
    print(obj$AI) #AI
    cat(stars,"\nCOMPARING CAI FOR REFERENCE AND (EXPECTED) FOCAL GROUPS\n",stars)
    cat("\nDiscrepancy between CAI of reference vs. Efocal groups under PFI:\n")
    print(obj[[4]]) #h_R_Ef
    cat(dashes,"\nImpact of deleting an item on the discrepancy between CAI of\nreference vs. Efocal groups under PFI:\n")
    print(obj[[5]]) #delta_R_Ef
    cat(stars,"\nCOMPARING CAI UNDER STRICT AND PARTIAL FACTORIAL INVARIANCE\n",stars)
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the reference group:\n")
    print(obj[[7]][[1]])
    cat("\nDiscrepancy between CAI under SFI vs. PFI for the focal group:\n")
    print(obj[[7]][[2]])
    cat(dashes,"\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the reference group:\n")
    print(obj[[6]][[1]]) #str_vs_par ref
    cat("\nImpact of deleting an item on the difference between CAI under \nSFI vs. PFI for the focal group:\n")
    print(obj[[6]][[2]]) #str_vs_par foc
    print(obj[[10]]$reference)#h_str_vs_par_list
    print(obj[[10]]$focal)
    print(obj[[11]]$strict)#cat("\nPartInv outputs for each item set\n")
    print(obj[[11]]$partial)
  }
}

