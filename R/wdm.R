#' @title Automated Test Assembly via Weighted (positive) Deviations Method
#' @author Gulsah Gurkan (gurkangulsah@gmail.com), Michael Chajewski (mchajewski@hotmail.com)
#' @description Ingests item metadata jointly with target test form constraints and uses the Weighted (positive) Deviations Method (WDM) to construct a test form based on the desired objectives.
#' @keywords ata wdm automated_test_assembly "test assembly" testform test_form "test form"
#' @usage wdm( ipool,
#'  id,
#'  constraints,
#'  first = NA,
#'  refine = TRUE,
#'  permutate = FALSE,
#'  tieselect = 1,
#'  verbose = TRUE,
#'  aprioriadd = NA,
#'  posthocadd = NA )
#' @param ipool Item by characteristic (property) metadata pool.
#' @param id Name of unique item identifier.
#' @param constraints Complex list object identifying the constraints to be applied in the ATA (see \code{makeconstobj} for guided process).
#' @param first How should item selection start: \code{id} of the item to be selected first from the pool, \code{NA} (default) - select randomly from the pool.
#' @param refine Should the final test form be refined against the remaining item pool? Default is \code{TRUE}.
#' @param permutate Assemble test forms starting with each item sequentially (as many forms as items in pool) and define final test form based on eligible constraint compliant solutions; Default is \code{FALSE} (currently not available).
#' @param tieselect How should tied items be resolved: 1 (default) - select the first item in the list of candidates (sensitive to data sorting); not applicable for situations with all categorical constraints only, 0 - randomly select candidate; not applicable for situations with all categorical constraints only
#' @param verbose Should progress of \code{wdm()} be printed to the console? Default = \code{TRUE}.
#' @param aprioriadd Force item addition (via IDs) to test before ATA, which affects item selection and constraint attainment success (currently not available).
#' @param posthocadd Force item addition (via IDs) to test after ATA, which affects final form specifications (currently not available).
#' @return A complex list object with test assembly specific estimates:
#' \item{wde}{Estimates of the computational steps deriving the positive weighted deviations and item selection.}
#' \item{evaluation}{Final assembled test form additive properties across constraints.}
#' \item{considered}{Estimates of the computational steps when \code{refine = TRUE} evaluating selected items and selecting replacements.}
#' \item{excluded}{Items from pool excluded.}
#' \item{excluded_set}{Item sets excluded. Only included if input \code{constobj} includes a \code{set_id}.}
#' \item{included}{Items from pool included in new test form.}
#' \item{included_set}{Item sets from pool included in new test form. Only included if input \code{constobj} includes a \code{set_id}.}
#' \item{initial_ids}{Item sets from pool included in new test form.}
#' \item{initial_setids}{Item sets from pool included in new test form. Only included if input \code{constobj} includes a \code{set_id}.}
#' \item{final_ids}{Final item ids in the test form.}
#' \item{final_setids}{Final set ids in the test form. Only included if input \code{constobj} includes a \code{set_id}.}
#' @references Parhshall, C. G., et al. (2002). Automated test assembly for online administration. In C. G. Parhsall, J. A. Spray, J. C. Kalohn, & T. Davey, Practical considerations in computer based testing (pp.106-125). New York, NY: Springer-Verlag New York, Inc.
#' @references Sanders, P. F., & Verschoor, A. J. (1998). Parallel test construction using classical item parameters. Applied Psychological Measurement, 22, 212-223. 
#' @references Swanson, L., & Stocking, M. L. (1993). A Model and heuristic for solving Very large item selection problems. Applied Psychological Measurement, 17, 151-166. 
#' @examples
#' # Specifying constraints
#' constin <- list(
#'   nI = 5,                                     # Number of items on the future test
#'   nC = 4,                                     # Number of constraints to be satisfied
#'   nameC = c("Content_A","Content_B","p","iSx"), # Name of constraint; must be numeric and must
#'   # reflect variable name in input
#'   lowerC = c(2, 3, 3.00, 0.50),               # Lower bound total constraint value on ATA form
#'   upperC = c(2, 3, 3.50, 0.60),               # Upper bound total constraint value on ATA form
#'   wC = c(1, 1, 1, 1),                         # Constraint weight used for weighted sum of
#'                                               # (positive) deviations St
#'   set_id = NA                                 # Aggregation ID for units / sets 
#' )
#' 
#' # Running WDM example from Parshall et al. (2002)
#' testWDM <- wdm( ipool = metadata_example,
#'                 id = "Item",
#'                 constraints = constin,
#'                 first = 2)
#'
#' # Summary of results
#' summary(testWDM)
#'
#' @export

wdm <- function(ipool,        # Item by item characteristic item metadata pool
                id,           # Name of unique item identifier
                constraints,  # Complex list object identifying the constraints to
                              # be applied in the ATA (see makeconstobj for guided process)
                first=NA,     # The first item or set ID from the pool to be selected:
                              #   NA - will randomly select item from avaiable pool
                              #   [ID] - will seed with specific identified item
                              #   "minSt" - will select item with initial smallest weighted positive deviations
                              #   "maxSt" - will select item with initial largest weighted positive deviations
                refine=TRUE,  # Should the final test form be refined against the
                              # remaining item pool?
                permutate=FALSE, # Assemble test forms starting with each item sequentially (as many forms
                                 # as items in pool) and define final test form based on eligible constraint
                                 # compliant solutions [currently not available]
                tieselect=1,  # How should tied considered items be resolved
                              #   1 - select the first item in the list of candidates (senesitive to data sorting)
                              #   0 - randomly select candidate
                verbose=TRUE, # Should progress of wdm be printed to the console? 
                aprioriadd=NA,   # Force item addition (via IDs) to test before ATA, which affects item selection
                                 # and constraint attainment success. If provided "first"
                                 # will be ignored [currently not available]
                posthocadd=NA){  # Force item addition (via IDs) to test after ATA [currently not available]

  # Track process time
  start_time <- Sys.time()
  
  # -------------- 
  # Check id input 
  # -------------- 
  
  # id name matches the data input.
  if(!id %in% names(ipool)){
    stop("ID variable name does not match input data.")
  }
  
  # Render id as character if factor
  if(is.factor(ipool[,which(colnames(ipool)==id)])){
    ipool[,which(colnames(ipool)==id)] <- as.character(ipool[,which(colnames(ipool)==id)])
  } 
  
  # Unique item IDs
  if(length(unique(ipool[,id])) != dim(ipool)[1]){
    stop("All items has to have unique IDs.")
  }
  
  # ---------------------------------------------- #
  # Screening constraints objects for requirements #
  # ---------------------------------------------- #

  # Constraint object is missing elements.
  constraints_list <- c("nI","nC","nameC","lowerC","upperC","wC")
  if(any(!constraints_list %in% names(constraints))){
    stop("Constraints object is missing elements: ",list(constraints_list[which(!constraints_list %in% names(constraints))]),"\nTry creating constraints object by using makeconstobj() function")
    message("\nTry creating constraints object by using makeconstobj() function")
  }
  rm(constraints_list)
  
  # All constobj elements (except for nameC) are numeric
  if(any(unlist(
    lapply(constraints[c("nI","nC","lowerC","upperC","wC")], function (x) {
      !class(x) %in% c("numeric","integer")
    })
  ))){
    stop("Elements in constraint object (nI, nC, lowerC, upperC, wC) must be numeric. If factors, define them as numeric.")
  }
  
  # Exact number of items
  if(constraints$nI == dim(ipool)[1]){
    stop("The input item pool equals the number of items to be selected. No ATA required.")
  }
  
  # Not enough items to select from
  if(constraints$nI >= dim(ipool)[1]){
    stop("The input item pool must contain at least one more item than the ATA form requires.")
  }
  
  # Not enough items to be selected
  if(constraints$nI < 2){
    stop("At least 2 items have to be selected into a test in order to use the WDM")
  }
  
  # Not enough constraints
  if(constraints$nC < 1){
    stop("At least 1 test model constraint has to be provided in order to use the WDM")
  }
  
  # Constraints variables in input
  if(length(which(constraints$nameC %in% names(ipool)))!=constraints$nC){
    stop("Specified constraints variable names do not match input data")
  }
  
  # Constraints lower bounds provided
  if(length(constraints$lowerC)!=constraints$nC){
    stop("Number of lower bound constraints has to match total number of constraints.")
  }
  
  # Constraints upper bounds provided
  if(length(constraints$upperC)!=constraints$nC){
    stop("Number of upper bound constraints has to match total number of constraints.")
  }
  
  # Constraints weights provided
  if(length(constraints$wC)!=constraints$nC){
    stop("Number of weights has to match total number of constraints.")
  }
  
  # All provided constraints data in input are numeric
  if(sum(apply(ipool[,match(constraints$nameC, names(ipool))],2,is.numeric))!=constraints$nC){
    stop("All constraints variables must be numeric. If a content selection is desired, then the content tag has to be dummy coded to indicate each content constraint seperately.")
  }
  
  # ----------------------------- #
  # Screen input value for set_id #
  # ----------------------------- # 
  
  # Define set_id as NA if not given.
  if(is.null(constraints$set_id[1])){
    constraints["set_id"] <- NA
  }
  
  # set_id name in input
  if(is.na(constraints$set_id)==FALSE){
    
    # set_id variable name exists in data
    if(!constraints$set_id %in% names(ipool)){
      stop("Specified set ID name does not match input data.")
    }
    
    # Render set_id as character if factor
    if(is.factor(ipool[,which(colnames(ipool)==constraints$set_id)])){
      ipool[,which(colnames(ipool)==constraints$set_id)] <- as.character(ipool[,which(colnames(ipool)==constraints$set_id)])
    } 
    
    # All set_ids have a single item and thus are not sets
    if(max(by(ipool[,which(names(ipool)==constraints$set_id)],ipool[,which(names(ipool)==constraints$set_id)],length))==1){
      stop("Specified set ID accounts for a single item per set. No sets detected.")
    }
  }
  
  # -------------------------------- #
  # Screening the input for first id #
  # -------------------------------- #  
  
  if(is.na(constraints$set_id)==TRUE & is.na(first)==FALSE & !first %in% c("minSt","maxSt") & !first %in% ipool[,which(colnames(ipool)==id)]){
    stop("Specified first item id was not found in the data.")
  }
  
  if(is.na(constraints$set_id)==FALSE & is.na(first)==FALSE & !first %in% c("minSt","maxSt")){
    if(!first %in% ipool[,which(colnames(ipool)==id)] & !first %in% ipool[,which(colnames(ipool)==constraints$set_id)]){
      stop("Specified first unit id was not found in the data.")
    }else{
      if(!first %in% ipool[,which(colnames(ipool)==constraints$set_id)]){
        first <- ipool[,which(colnames(ipool)==constraints$set_id)][which(first %in% ipool[,which(colnames(ipool)==id)])]
      }
    }
  }
  
  # ---------------- #
  # Restructure data #
  # ---------------- #  

  if(is.na(constraints$set_id)==FALSE){
    # Clean and order data by the name of constraints.
    data_structured <- ipool[,match(c(id,constraints$set_id,constraints$nameC), names(ipool))]
    # Create count variable.
    data_structured$Count <- 1
    # Aggregate data.
    data_structured <- aggregate(data_structured[,-c(1,2)], by = list(data_structured[,which(names(data_structured) %in% constraints$set_id)]), FUN = sum)
  }else{
    # Clean and order data by the name of constraints.
    data_structured <- ipool[,match(c(id,constraints$nameC), names(ipool))]
    # Render id as character if factor
    if(is.factor(ipool[,which(colnames(ipool)==id)])){
      data_structured[,which(colnames(data_structured)==id)] <- as.character(ipool[,which(colnames(ipool)==id)])
    } 
    # Create count variable.
    data_structured$Count <- 1
  }

  # Rename the first column in aggregated data to match id.
  names(data_structured)[1] <- "id"

  # --------------------------- #
  # Check for null contribution #
  # --------------------------- #
  
  null_flag <- apply(data_structured[,which(colnames(data_structured) %in% constraints$nameC)],1,sum)
  
  # Filter out items with null contribution
  data_structured_null <- data_structured[which(null_flag==0),]
  
  # ----------------------- #
  # Preparing input objects #
  # ----------------------- #
  
  # Making item pool selection (ips) copy
  ips <- data_structured[which(null_flag>0),]
  
  # Making selected item test (sit)
  sit <- data.frame()

  # Final test item ids
  outid <- c()
  
  # Creating estimate output tracker
  estout <- list()
  
  # ------------------- #
  # Item selection loop #
  # ------------------- #
  
  # Start item loop
  for(i in 1:constraints$nI){

    # remove item sets with number of items that are larger than remaining quota.
    if(i==1){
      ips <- ips[ips$Count<=constraints$nI,]
    }else{
      ips <- ips[ips$Count<=(constraints$nI-sum(sit$Count)),]
    }
    
    # Exit if no items left in the pool.
    if(dim(ips)[1]==0){
      break()
    }
    
    # Creating temporary iteration estimate table 
    tempout <- data.frame(id=ips$id, stringsAsFactors = FALSE)
    
    # Start constraint loop
    for(j in 1:constraints$nC){

      # Creating temporary iteration estimate table 
      tempest <- data.frame(id=ips$id, stringsAsFactors = FALSE)
      
      # ----------------------------
      # Compute item characteristics
      # ----------------------------
      
      # Identify column with jth constraint information
      selj <- which(names(ips)==constraints$nameC[j])
      
      # Past items, S(aij)
      pI <- ifelse(i==1,0,sum(sit[,selj]))
      
      # Current item, aij
      cI <- ips[,selj]
      
      # Future item, (n-k)*vj
      fI <- sapply(1:(dim(ips)[1]), function(x){
        if(i==1){
          (constraints$nI-(ips$Count[x]))*
            (sum(ips[-x,selj])/(sum(data_structured$Count)-(ips$Count[x]))) 
        }else{
          (constraints$nI-(sum(sit$Count)+ips$Count[x]))*
            (sum(ips[-x,selj])/(sum(data_structured$Count)-(sum(sit$Count)+ips$Count[x]))) 
        }
      })
      
      # Characteristic of the entire test, qj
      tempest <- data.frame(tempest, pI, cI, fI)
      tempest <- data.frame(tempest, q=rowSums(tempest[,-1]))
      
      # Boundary positive deviations
      tempest <- data.frame(tempest,
                            dL = ifelse(tempest$q < constraints$lowerC[j], constraints$lowerC[j]-tempest$q, 0),
                            dU = ifelse(tempest$q > constraints$upperC[j], tempest$q-constraints$upperC[j], 0)
                            )
      
      # Give the tempest object constraint jth names
      names(tempest)[-1] <- paste(names(tempest)[-1],j,sep="_")
      
      # Combine characteristics with previous constraint results
      tempout <- merge(tempout,tempest)
      rm(tempest)
      
      # Message within constraint loop
      if(verbose){
        message(paste("Unit ",i,": Constraint ",j,"/",constraints$nC," evaluated.", sep=""))
      }
      
    }# End constraints loop
    
    # Computing weighted deviations
    tempout <- data.frame(tempout,
                          wdL= apply(tempout[,grepl("dL",names(tempout))],1,function(x){sum(x*constraints$wC)}),
                          wdU= apply(tempout[,grepl("dU",names(tempout))],1,function(x){sum(x*constraints$wC)}))
    
    # Computing weighted sum of (positive) deviations for test
    tempout <- data.frame(tempout,
                          St= tempout$wdL + tempout$wdU)
    
    # Selecting item for inclusion
    selid <- c()
    if(i==1){
      # Assign first item to sit
      if(is.na(first)){
        # Randomly select an item
        selid <- sample(ips$id,1)
        # Inherit designated item into sit
        sit <- ips[which(ips$id==selid),]
        # Remove item from ips
        ips <- ips[-which(ips$id==selid),]
        # Tracking final test ids
        outid <- selid
        # Identifying item
        tempout <- data.frame(tempout,
                              "Selected"=ifelse(tempout$id==selid,"*",""))
      }else if(first=="minSt" | first=="maxSt"){
        # Minimum St
        if(first=="minSt"){
          if(tieselect==1){
            selid <- tempout$id[which(tempout$St==min(tempout$St))][1]
          }else{
            selid <- ifelse(length(tempout$id[which(tempout$St==min(tempout$St))])>1,sample(tempout$id[which(tempout$St==min(tempout$St))],1),tempout$id[which(tempout$St==min(tempout$St))])
          }
        }else if(first=="maxSt"){
          if(tieselect==1){
            selid <- tempout$id[which(tempout$St==max(tempout$St))][1]
          }else{
            selid <- ifelse(length(tempout$id[which(tempout$St==max(tempout$St))])>1,sample(tempout$id[which(tempout$St==min(tempout$St))],1),tempout$id[which(tempout$St==max(tempout$St))])
          }          
        }
        outid <- c(outid, selid)
        
        # Identifying item
        tempout <- data.frame(tempout,
                              "Selected"=ifelse(tempout$id==selid,"*",""))
        
        # Reallocating item post selection
        sit <- rbind(sit,
                     ips[which(tempout$id==selid),])
        ips <- ips[-which(tempout$id==selid),]
      }else{
        # Inherit designated item into sit
        sit <- ips[which(ips$id==first),]
        # Remove item from ips
        ips <- ips[-which(ips$id==first),]
        # Tracking final test ids
        outid <- sit$id
        # Identifying item
        tempout <- data.frame(tempout,
                              "Selected"=ifelse(tempout$id==first,"*",""))
      }
    }else{
      
      # If formlength has been reached, break the repeat loop.
      if (sum(sit$Count)==constraints$nI) {  
        break()                           
      }
      # Select next item.
      if(tieselect==1){
        selid <- tempout$id[which(tempout$St==min(tempout$St))][1]
      }else{
        selid <- ifelse(length(tempout$id[which(tempout$St==min(tempout$St))])>1,sample(tempout$id[which(tempout$St==min(tempout$St))],1),tempout$id[which(tempout$St==min(tempout$St))])
      }
      outid <- c(outid, selid)
      tempout <- data.frame(tempout,
                            "Selected"=ifelse(tempout$id==selid,"*",""))
      
      # Reallocating item post selection
      sit <- rbind(sit,
                   ips[which(tempout$id==selid),])
      ips <- ips[-which(tempout$id==selid),]
    }    
    
    # Appending estimate output tracker
    estout <- c(estout,list(tempout))
    rm(tempout)
    
    # Message within item loop
    if(verbose){
      message(paste("Unit ",i," (item ",sum(sit$Count),"/",constraints$nI,") selected.", sep=""))
    }
    
  }# End of item loop

  # Return with remaining items.
  ips <- data_structured[-which(data_structured$id %in% sit$id),]
  outid <- sit$id
  
  # ---------------- #
  # Refinement cycle #
  # ---------------- #
  
  if(refine==TRUE){
    
    # Message for start of refinement
    if(verbose){
      message("Test optimization:")
    }
    
    # Making item pool selection (ips) copy
    ips <- data_structured[which(null_flag>0),]
    ips <- ips[-which(ips$id %in% sit$id),]
    
    # Filter out the sets that are in different size than the ones in the form.
    #First, save the ones left out to be returned in ips
    left <- ips[which(!ips$Count %in% unique(sit$Count)),]
    # Filter to be used in refinement
    ips <- ips[which(ips$Count %in% unique(sit$Count)),]
    
    # Copy item pool objects 
    sit_r <- sit
    ips_r <- ips
    
    # Copy of final item objects to be inherited for return
    sit_r2 <- sit
    ips_r2 <- ips
    
    # Creating estimate output tracker
    estout_r <- list()
    refout_r <- list()
    
    # Refinement criterion
    stopref <- 0
    rcnt <- 0
    
    # Item selection loop #
    while(stopref==0 & rcnt <= dim(ips_r)[1]){
      
      # Creating temporary iteration estimate table 
      tempout <- data.frame(id=ips_r$id, stringsAsFactors = FALSE)
      
      # Start constraint loop
      for(j in 1:constraints$nC){
        
        # Creating temporary iteration estimate table 
        tempest <- data.frame(id=ips_r$id, stringsAsFactors = FALSE)
        
        # Compute item characteristics
        
        # Identify column with jth constraint information
        selj <- which(names(ips_r)==constraints$nameC[j])
        
        # Past items, S(aij)
        pI <- sum(sit_r[,selj])
        
        # Current item, aij
        cI <- ips_r[,selj]
        
        # Characteristic of the entire test, qj
        tempest <- data.frame(tempest, pI, cI)
        tempest <- data.frame(tempest, q=rowSums(tempest[,-1]))
        
        # Boundariy positive deviations
        tempest <- data.frame(tempest,
                              dL = ifelse(tempest$q < constraints$lowerC[j], constraints$lowerC[j]-tempest$q, 0),
                              dU = ifelse(tempest$q > constraints$upperC[j], tempest$q-constraints$upperC[j], 0)
        )
        
        # Give the tempest object constraint jth names
        names(tempest)[-1] <- paste(names(tempest)[-1],j,sep="_")
        
        # Combine characteristics with previous constraint results
        tempout <- merge(tempout,tempest)
        rm(tempest)
        
        # Message within constraint loop
        if(verbose){
          message(paste("Test constraint ",j,"/",constraints$nC," evaluated.", sep=""))
        }
        
      }# End constraints loop
      
      # Computing weighted deviations
      tempout <- data.frame(tempout,
                            wdL= apply(tempout[,grepl("dL",names(tempout))],1,function(x){sum(x*constraints$wC)}),
                            wdU= apply(tempout[,grepl("dU",names(tempout))],1,function(x){sum(x*constraints$wC)}))
      
      # Computing weighted sum of (positive) deviations for test
      tempout <- data.frame(tempout,
                            St= tempout$wdL + tempout$wdU)
      
      # Selecting item for refinement inclusion
      if(tieselect==1){
        selid <- tempout$id[which(tempout$St==min(tempout$St))][1]  
      }else{
        selid <- ifelse(length(tempout$id[which(tempout$St==min(tempout$St))])>1,sample(tempout$id[which(tempout$St==min(tempout$St))],1),tempout$id[which(tempout$St==min(tempout$St))])
      }
      tempout <- data.frame(tempout,
                            "Considered"=ifelse(tempout$id==selid,"*",""))
      
      # Appending estimate output tracker
      estout_r <- c(estout_r,list(tempout))
      rm(tempout)
      
      # ---------------------
      # Refinement evaluation
      # ---------------------
      
      # Adding considered item to selected test
      sit_r <- rbind(sit_r, ips_r[which(ips_r$id==selid),])
      
      # Creating temporary iteration estimate table 
      tempout_r<- data.frame(id=sit_r$id, stringsAsFactors = FALSE)
      
      # Start constraint loop
      for(j in 1:constraints$nC){
        
        # Creating temporary iteration estimate table 
        tempest_r <- data.frame(id=sit_r$id, stringsAsFactors = FALSE)
        
        # Compute item characteristics
        
        # Identify column with jth constraint information
        selj <- which(names(sit_r)==constraints$nameC[j])
        
        # Past items, S(aij)
        pI <- sum(sit_r[,selj])
        
        # Current item, aij
        cI <- sit_r[,selj]
        
        # Characteristic of the entire test, qj
        tempest_r <- data.frame(tempest_r, pI, cI)
        tempest_r <- data.frame(tempest_r, q=tempest_r$pI-tempest_r$cI)
        
        # Boundariy positive deviations
        tempest_r <- data.frame(tempest_r,
                                dL = ifelse(tempest_r$q < constraints$lowerC[j], constraints$lowerC[j]-tempest_r$q, 0),
                                dU = ifelse(tempest_r$q > constraints$upperC[j], tempest_r$q-constraints$upperC[j], 0)
        )
        
        # Give the tempest_r object constraint jth names
        names(tempest_r)[-1] <- paste(names(tempest_r)[-1],j,sep="_")
        
        # Combine characteristics with previous constraint results
        tempout_r<- merge(tempout_r,tempest_r)
        rm(tempest_r)
        
        # Message within constraint loop
        if(verbose){
          message(paste("Candidate ",rcnt+1," search: Constraint ",j,"/",constraints$nC," evaluated.", sep=""))
        }
        
      }# End constraints loop
      
      # Computing weighted deviations
      tempout_r<- data.frame(tempout_r,
                             wdL= apply(tempout_r[,grepl("dL",names(tempout_r))],1,function(x){sum(x*constraints$wC)}),
                             wdU= apply(tempout_r[,grepl("dU",names(tempout_r))],1,function(x){sum(x*constraints$wC)}))
      
      # Computing weighted sum of (positive) deviations for test
      tempout_r<- data.frame(tempout_r,
                             St= tempout_r$wdL + tempout_r$wdU)
      
      # Selecting item for refinement exclusion
      if(tieselect==1){
        selid_r <- tempout_r$id[which(tempout_r$St==min(tempout_r$St))][1] 
      }else{
        selid_r <- ifelse(length(tempout_r$id[which(tempout_r$St==min(tempout_r$St))]) > 1, sample(tempout_r$id[which(tempout_r$St==min(tempout_r$St))],1),tempout_r$id[which(tempout_r$St==min(tempout_r$St))])
      }
      
      tempout_r<- data.frame(tempout_r,
                             "Considered"=ifelse(tempout_r$id==selid_r,"*",""))
      
      # Appending estimate output tracker
      refout_r <- c(refout_r,list(tempout_r))
      
      # ------------------------------
      # Removing less influential item
      # ------------------------------
      
      # Checking if added item is item to be removed
      if(selid_r==selid){
        
        # Inherit item objects
        sit <- sit_r2
        ips <- ips_r2
        
        # Incrementing trackers
        stopref <- 1
        rcnt <- rcnt+1
        
        # Message within item loop
        if(verbose){
          message(paste("Candidate", rcnt,"was removed.", sep=" "))
        }
      
      }else{
        
        # Checking if added item and the item to be removed has the same count
        if(sit_r$Count[which(sit_r$id==selid_r)]==sit_r$Count[which(sit_r$id==selid)]){
          
          # Add removed item from considered sit to final store ips
          ips_r2 <- rbind(ips_r2, sit_r[which(sit_r$id==selid_r),])
          
          # Remove item which was considered and retained from final ips
          ips_r2 <- ips_r2[-which(ips_r2$id==selid),]
          
          # Remove considered item now to be included from pool of considered
          ips_r <- ips_r[-which(ips_r$id==selid),]
          
          # Remove previous item that was replaced
          sit_r <- sit_r[-which(sit_r$id==selid_r),]
          sit_r2 <- sit_r
          
          # Incrementing trackers
          rcnt <- rcnt+1
          
          # Message within item loop
          if(verbose){
            message(paste("Candidate", rcnt,"was used for refinement.", sep=" "))
          }
        }else{
          # Inherit original sit object
          sit_r <- sit
          
          # Remove considered item from pool of considered
          ips_r <- ips_r[-which(ips_r$id==selid),]
          
          # Incrementing trackers
          rcnt <- rcnt+1
          
          # Message within item loop
          if(verbose){
            message(paste("Candidate", rcnt,"was removed.", sep=" "))
          }
        }
        
      }
      
    } # End of while loop
    
    # Include different sized sets back to ips
    ips <- rbind(ips,left)
    
    # Message after refinement
    if(verbose){
      message(paste(rcnt,"candidates in total were considered.", sep=" "))
    }
    
  } # End refine loop
    
  # ------------- #
  # Final output  #
  # ------------- #
  
  # Attach data_structured_null back to ips
  ips <- rbind(ips,data_structured_null)
  
  # Disaggregate data if aggregated
  if(is.na(constraints$set_id)==FALSE){
    # set-level data
    outsetid <- outid
    sit_sets <- sit
    ips_sets <- ips
    # item-level data
    outid <- ipool[which( ipool[,which(names(ipool) %in% constraints$set_id)]  %in% as.character(outsetid)), which(names(ipool) %in% id) ]
    ips <- ipool[ which( !ipool[,which(names(ipool) %in% constraints$set_id)]  %in% as.character(sit_sets$id)), match(c(id,constraints$set_id,constraints$nameC), names(ipool)) ]
    names(ips)[1] <- "id"
    sit <- ipool[which( ipool[,which(names(ipool) %in% constraints$set_id)]  %in% as.character(sit_sets$id)), match(c(id,constraints$set_id,constraints$nameC), names(ipool)) ]
    names(sit)[1] <- "id"
  }else{
    sit <- sit[,-c(dim(sit)[2])] #remove "count" column.
    ips <- ips[,-c(dim(ips)[2])] #remove "count" column.
  }
  
  # Evaluation statistics
  if(is.na(constraints$set_id)==FALSE){
    evalout <- data.frame(rbind(
      constraints$lowerC,constraints$upperC)) 
    names(evalout) <- c(constraints$nameC)
    evalout <- rbind(evalout,
                     colSums(sit[,-c(1,2)]),
                     colMeans(sit[,-c(1,2)]),
                     colSums(ips[,-c(1,2)]),
                     colMeans(ips[,-c(1,2)]))
    evalout <- data.frame(
      "Object"=c("Constraints","Constraints","Included","Included","Excluded","Excluded"),
      "Type"=c("Lower","Upper","Sum", "Average","Sum", "Average"),
      evalout)
    
  }else{
    evalout <- data.frame(rbind(
      constraints$lowerC,constraints$upperC)) 
    names(evalout) <- c(constraints$nameC)
    evalout <- rbind(evalout,
                     colSums(sit[,-1]),
                     colMeans(sit[,-1]),
                     colSums(ips[,-1]),
                     colMeans(ips[,-1]))
    evalout <- data.frame(
      "Object"=c("Constraints","Constraints","Included","Included","Excluded","Excluded"),
      "Type"=c("Lower","Upper","Sum", "Average","Sum", "Average"),
      evalout)
    
  }
  
  
  # Rename item selection iterations   
  names(estout) <- 1:length(estout)
  
  # Rename item consideration cycles
  if(refine==TRUE){
    names(estout_r) <- 1:length(estout_r)
    names(refout_r) <- 1:length(refout_r)
  }
  
  # Record time spent.
  end_time <- Sys.time()

  # Append selected and remaining item statistics
  if(refine==TRUE){
    
    if(is.na(constraints$set_id)==TRUE){
      estout <- c("wde"=list(estout),
                  "evaluation"=list(evalout),
                  "considered"=list(estout_r),
                  "refined"=list(refout_r),
                  "excluded"=list(ips),
                  "included"=list(sit),
                  "initial_ids"=list(outid),
                  "final_ids"=list(sit$id)
      )
    }else{
      estout <- c("wde"=list(estout),
                  "evaluation"=list(evalout),
                  "considered"=list(estout_r),
                  "refined"=list(refout_r),
                  "excluded"=list(ips),
                  "excluded_sets"=list(ips_sets),
                  "included"=list(sit),
                  "included_sets"=list(sit_sets),
                  "initial_ids"=list(outid),
                  "initial_setids"=list(outsetid),
                  "final_ids"=list(sit$id),
                  "final_setids"=list(sit_sets$id) 
      )
      }
    
  }else{
    
    if(is.na(constraints$set_id)==TRUE){
      estout <- c("wde"=list(estout),
                  "evaluation"=list(evalout),
                  "excluded"=list(ips),
                  "included"=list(sit),
                  "final_ids"=list(outid)
      )
    }else{
      estout <- c("wde"=list(estout),
                  "evaluation"=list(evalout),
                  "excluded"=list(ips),
                  "excluded_sets"=list(ips_sets),
                  "included"=list(sit),
                  "included_sets"=list(sit_sets),
                  "final_ids"=list(outid),
                  "final_setids"=list(outsetid)
      )
    }
  }

  # Termination message
  if(verbose){
    print(difftime(end_time,start_time))
    message("ATA terminated successfully.")
  }
  
  # --------------------------------
  # Final analysis object attributes
  # --------------------------------
  
  # Aggregation
  attr(estout, "aggregated") <- ifelse(is.na(constraints$set_id), FALSE, TRUE)
  
  # Refinement
  attr(estout, "refined") <- refine
  
  # Permutated
  attr(estout, "permutated") <- permutate
  
  # Runtime
  attr(estout, "runtime") <- end_time-start_time
  
  # Method
  attr(estout, "method") <- "wdm"

  # Type
  attr(estout, "type") <- "deviation"
  
  # Class
  attr(estout, "class") <- "ata"
  
  # Return
  return(estout)
}
