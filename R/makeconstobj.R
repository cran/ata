#' @title User-guided Function to Create a Constraints Input for ATA Form Creation
#' @author Gulsah Gurkan (gurkangulsah@gmail.com), Michael Chajewski (mchajewski@hotmail.com)
#' @description Guides the user to create a complex list object identifying the constraints to be applied in automated test assembly functions from the \code{ata} package.
#' @keywords ata wdm lp automatest_test_assembly testform test_form test_via_wdm test_via_lp
#' @usage makeconstobj( ipool,
#'  id,
#'  empty = FALSE)
#' @param ipool Item by characteristic (or property) metadata.
#' @param id Name (not actual codes) of unique item identifier (variable).
#' @param empty Should the function return an empty list to be filled in manually. Default is \code{FALSE}.
#' @return A list object with "nC" 
#'                            "nameC" 
#'                            "lowerC" 
#'                            "upperC" 
#'                            "wC"
#'                            "nI"
#'                            "set_id"
#' @references Parshall, C. G., et al. (2002). Automated test assembly for online administration. In C. G. Parshall, J. A. Spray, J. C. Kalohn, & T. Davey, Practical considerations in computer based testing (pp.106-125). New York, NY: Springer-Verlag New York, Inc.
#' @export

makeconstobj <- function(ipool,         # Item by item characteristic item metadata pool
                         id,            # Name of unique item identifier
                         empty=FALSE){  # If TRUE, can create an empty constobj to be filled in later
  
  # turn global warnings off (will be put back on at the end of the function)
  options(warn=-1)
  
  # --------------------- 
  # Return empty constobj 
  # ---------------------
  if(empty==TRUE){
    constraints <- list(
      nC=c(),         # Number of constraints to be satisfied
      nameC=c(),      # Name of constraint; must be numeric and must reflect variable name in input
      lowerC=c(),     # Lower bound total constraint value on ATA form
      upperC=c(),     # Upper bound total constraint value on ATA form
      wC=c(),         # Constraint weight used for weighted sum of (positive) deviations St
      nI=c(),         # Number of items on the future test
      set_id=NA       # set IDs for items; default is NA.
    )
    
    # Print output
    message("An empty list was created to be filled in and applied in the ATA formation.\n")

    # Return output
    return(constraints)
    
    # Stop the loop.
    stop()
  }
  
  # -------------- 
  # Check id input 
  # -------------- 
  
  # id name matches the data input.
  if(!id %in% names(ipool)){
    stop("ID variable name does not match input data.")
  }
  
  # Check the size of the item pool based on the given item ID input.
  message(paste("Based on the ID name you submitted, '",id,"', there are ",length(unique(ipool[,id]))," items in the submitted data. ", sep = ""))
  tt <- readline(prompt=paste("Is the number of independent items correct? yes/no ", sep = ""))
  if(tt!="yes"){
    stop("Function is terminated. Please check the input.")
  }
  rm(tt)
  
  # ------------------------ 
  # Prepare an output object 
  # ------------------------ 
  
  # Empty constraints object.
  constraints <- list() 
  
  # --------------------- 
  # Number of constraints 
  # --------------------- 
  
  # Append nC.
  message("ATA form will be created based on the constraints you define.")
  # nC has to be an integer with a length of 1.
  repeat{
    tt <- as.numeric(readline(prompt="How many constraints do you have? "))
    if(is.numeric(tt) & length(tt)==1 & !is.na(tt) & tt>1){
      if(tt > dim(ipool)[2]){
        message("Number of constraints exceeds the number of variables in the submitted data.")
      }else{
        break()
      }
    }else{
      message("Number of constraints should be provided as an integer number larger than 1.")
    }
  }
  # attach nC to the list object.
  constraints["nC"] <- as.numeric(tt)
  rm(tt)
  
  # ------------------------------- 
  # Constraints variable name input 
  # ------------------------------- 
  
  # Print variable names in ipool if asked.
  message("Constraints will be selected from the data set provided.")
  tt <- readline(prompt="Do you want to see all unique column names from the submitted data? yes/no ")
  if(tt=="yes"){
    cat(paste(names(ipool)), "\n")
  }
  rm(tt)
  
  # Append nameC.
  message(paste("Among the variables in the submitted data, please specify the names of the ",constraints$nC," variables to be used as ATA constraints.",sep = ""))
  nameC <- c()
  for (j in 1:(constraints$nC)) {
    # names has to match the data
    repeat{
      tt <- as.character(readline(prompt=paste("Constraint name", j, ": ",sep = " ")))
      if(tt %in% names(ipool)){
        break()
      }else{
        message("Variable name does not match the submitted data.")
      }
    }
    nameC <- c(nameC,tt) 
  }
  # Attach constraint names to the list object.
  constraints["nameC"] <- list(nameC)
  rm(nameC,tt)
  
  # All provided constraint variables are numeric
  if(sum(apply(ipool[,match(constraints$nameC, names(ipool))],2,is.numeric))!=constraints$nC){
    stop("All constraints variables must be numeric. \nIf a content selection is desired, then the content tag has to be dummy coded to indicate each content constraint seperately.")
  }
  
  # ----------------------------------- 
  # Constraint bound values and weights 
  # ----------------------------------- 
  
  # Append lowerC.
  message("What are the lower bound values for the constraints?")
  lowerC <- c()
  for (j in 1:(constraints$nC)) {
    # Check the input for bound values.
    repeat{
      tt <- as.numeric(readline(prompt=paste("Lower bound value for constraint", constraints$nameC[j], ": ",sep = " ")))
      if(is.numeric(tt) & length(tt)==1 & !is.na(tt)){
        break()
      }else{
        message("Bound values should be provided as only one value that is numeric.")
      }
    }
    lowerC <- c(lowerC,tt)
  }
  # Attach lower bound values to the list object.
  constraints["lowerC"] <- list(lowerC)
  rm(lowerC,tt)
  
  # Append upperC.
  message("What are the upper bound values for the constraints?")
  upperC <- c()
  for (j in 1:(constraints$nC)) {
    # Check the input for bound values.
    repeat{
      tt <- as.numeric(readline(prompt=paste("Upper bound value for constraint", constraints$nameC[j], ": ",sep = " ")))
      if(is.numeric(tt) & length(tt)==1 & !is.na(tt)){
        break()
      }else{
        message("Bound values should be provided as only one value that is numeric.")
      }
    }
    upperC <- c(upperC,tt)
  }
  # Attach upper bound values to the list object.
  constraints["upperC"] <- list(upperC)
  rm(upperC,tt)
  
  # Append wC.
  message("What are the weights for the constraints?")
  wC <- c()
  for (j in 1:(constraints$nC)) {
    # Check the input for weights.
    repeat{
      tt <- as.numeric(readline(prompt=paste("Weight for constraint", constraints$nameC[j], ": ",sep = " ")))
      if(is.numeric(tt) & length(tt)==1 & !is.na(tt)){
        break()
      }else{
        message("Weights should be provided as only one value that is numeric.")
      }
    }
    wC <- c(wC,tt)
  }
  # Attach weights to the list object.
  constraints["wC"] <- list(wC)
  rm(wC,tt)
  
  # --------------- 
  # ATA form length 
  # --------------- 
  
  # Append nI.
  message("Please specify the ATA form length.")
  # Check the input for form length.
  repeat{
    tt <- as.numeric(readline(prompt="What is the total number of items to be selected? "))
    if(is.numeric(tt) & length(tt)==1 & !is.na(tt) & tt>1){
      if(tt >= dim(ipool)[1]){
        message("The input item pool must contain at least one more item than the ATA form requires.")
      }else{
        break()
      }
    }else{
      message("Number of items to be selected should be provided as an integer number larger than 1.")
    }
  }
  # Attach form length to the list object.  
  constraints["nI"] <- as.numeric(tt)
  rm(tt)
  
  # --------- 
  # Item sets 
  # --------- 
  
  # Define set_id
  message("Please specify whether there are items sets in the data.")
  yn <- readline(prompt="Would you like to aggregate the data based on an ID defined unit? yes/no ")
  if(yn=="yes"){
    # Check input if it matches the data
    repeat{
      tt <- as.character(readline(prompt="What is the set ID column name to be used for aggregation? "))
      if(tt %in% names(ipool)){
       # Check if set id is already in use 
        if(tt==id){
          message("set ID cannot be the same as item ID.")
        }else if(tt %in% constraints$nameC){
          message("set ID name is already in use as one of the contraint names.")
        }else{
          break()
        }
      }else{
        message("set ID name does not match the submitted data.")
      }
    }
    
    # if not answered yes 
  }else if(yn!="yes"){
    tt <- NA
    message("No set ID was defined.")
  }
  # Attach set_id to the list object.
  constraints["set_id"] <- tt
  rm(tt,yn)
  
  # ------------- 
  # Print status 
  # ------------- 
  message("Complex list object identifying the constraints to be applied in the ATA was created.\n")
  
  # ------------- 
  # Return output 
  # ------------- 
  return(constraints)
  
  # turn global warnings back on
  options(warn=0)

}

