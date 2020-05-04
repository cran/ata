#' @title Automated Test Assembly via Linear Constrained Programming
#' @author Michael Chajewski (mchajewski@hotmail.com), Gulsah Gurkan (gurkangulsah@gmail.com)
#' @description Ingests item metadata jointly with target test form constraints, and can be parametarized to uses either Boolean (0-1) Integer Linear Programming (ILP) or Mixed Integer Linear Programming (MILP) to construct a test form based on the desired objectives. When MILP is desired the selection of the objective function type should be changed.
#' @keywords ata lp automatest_test_assembly "automatest test assembly" testform test_form "test form" "assessment form" test_via_lp
#' @usage atalp( ipool,
#'       id,
#'       constraints,
#'       refine = FALSE,
#'       permutate = FALSE,
#'       sorttimes = 999,
#'       tieselect = -1,
#'       type = "const",
#'       verbose = TRUE,
#'       aprioriadd = NA,
#'       posthocadd = NA )
#' @param ipool Item by characteristic (property) metadata pool.
#' @param id Name of unique item identifier.
#' @param constraints Complex list object identifying the constraints to be applied in the ATA (see \code{makeconstobj} for guided process).
#' @param refine Creates a final test form from permutated solutions, refined to attempt a deviation balance between the observed form and the constraints. Option only effective if \code{permutate} is \code{TRUE} and \code{type = const} in which the constraint weights have meaning; Default is \code{FALSE}.
#' @param permutate Requests the test form to be assembled by resorting (\code{sorttimes}) the metadata and selecting the most frequently occurring item combination satisfying the constraints. Relevant only for \code{type = const}; Default is \code{FALSE}.
#' @param sorttimes Number of how often the original input metadata should be resorted. Only functional if \code{permutate} is \code{TRUE} and \code{type = const}; default \code{999}, so that \code{sorttimes} + main analysis account for a total of 1,000 selection versions.
#' @param tieselect How should tied items be resolved: -1 (default) - do not manipulate items (which allows for identically functioning items to be included), 1 - select the first item in the list of candidates (sensitive to data sorting); not applicable for situations with all categorical constraints only, 0 - randomly select candidate; not applicable for situations with all categorical constraints only.
#' @param type Type of objective function: \code{const} - constraint based only (default), \code{parmin} - constraint + minimum non-categorical parameter combination, \code{parmax} - constraint + maximum non-categorical parameter combination.
#' @param verbose Should progress be printed to the console? Default \code{TRUE}. 
#' @param aprioriadd Force item addition (via IDs) to test form before ATA, which affects item selection and constraint attainment success (currently not available).
#' @param posthocadd Force item addition (via IDs) to test form after ATA, which affects final form specifications (currently not available).
#' @return A complex list object with test assembly specific estimates:
#' \item{objective}{Constrained objective function value.}
#' \item{items_removed}{Removed items from item pool when \code{tieselect} is not \code{-1}.}
#' \item{excluded}{Items from pool excluded.}
#' \item{excluded_set}{Item sets excluded. Only included if input \code{constobj} includes a \code{set_id}.}
#' \item{included}{Items from pool included in new test form.}
#' \item{included_set}{Item sets from pool included in new test form. Only included if input \code{constobj} includes a \code{set_id}.}
#' \item{final_ids}{Final item ids in the test form.}
#' \item{final_setids}{Final set ids in the test form. Only included if input \code{constobj} includes a \code{set_id}.}
#' @references Chen, P. (2017). Should we stop developing heuristics and only rely on mixed integer programming solvers in automated test assembly? Applied Psychological Measurement, 41, 227-240.
#' @references Diao, Q., & van der Linden, W. J. (2011). Automated test assembly using lp_Solve Version 5.5 in R. Applied Psychological Measurement, 35, 398-409.
#' @references Shao, C., Liu, S., Yang, H., & Tsai, T. (2019). Automated test assembly using SAS operations research software in a medical licensing examination. Applied Psychological Measurement, 00, 1-15.
#' @references van der Linden, W. J. (2005). A comparison of item-selection methods for adaptive tests with content constraints. Journal of Educational Measurement, 42, 283-302.
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
#' # Running atalp
#' testLP <- atalp(ipool = metadata_example,
#'                 id = "Item",
#'                 constraints = constin)
#'
#' # Summary of results
#' summary(testLP)
#'
#' @import lpSolve stats
#' @export

atalp <- function(ipool,        # Item by item characteristic metadata pool
                  id,           # Name of unique item identifier
                  constraints,  # Complex list object identifying the constraints to
                                # be applied in the ATA (see makeconstobj for guided process)
                  refine=FALSE, # Should the final test form selection be from permutated solutions be refined to
                                # attempt a deviation balance between the observed form and the constraints? Only
                                # effective if permutate is TRUE. Also, only instance for "type" = const in which
                                # the constraint weights have meaning
                  permutate=FALSE, # Assemble test forms starting with each item sequentially (as many forms
                                   # as items in pool) and define final test form based on eligible constraint
                                   # compliant solutions
                  sorttimes=999,   # Number of resorting the original input metadata. Only functional if "permutate"
                                   # is TRUE and "type" is "const"
                  tieselect=-1, # How should tied items be resolved
                                #   -1 - do not manipulate items (which allows for identically functioning items to be included)
                                #    1 - select the first item in the list of candidates (sensitive to data sorting); not
                                #        applicable for situations with all categorical constraints only
                                #    0 - randomly select candidate; not applicable for situations with all categorical
                                #        constraints only
                  type="const", # Type of objective function
                                #   "const" - constraint based only
                                #   "parmin" - constraint + minimum non-categorical parameter combination
                                #   "parmax" - constraint + maximum non-categorical parameter combination
                  verbose=TRUE, # Should progress of wdm be printed to the console? 
                  aprioriadd=NA,   # Force item addition (via IDs) to test before ATA, which affects item selection
                                   # and constraint attainment success [currently not available]
                  posthocadd=NA){  # Force item addition (via IDs) to test after ATA [currently not available]
  
  # lpSolve() required
  #require("lpSolve")
  
  # Track process time
  start_time <- Sys.time()
  
  # Adding error hold to prevent browse mode on stop()
  options(error=NULL)
  
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
    stop("All items have to have a unique ID.")
  }
  
  # ----------------------------------------------
  # Screening constraints objects for requirements
  # ----------------------------------------------
  
  # Constraint object is missing an element.
  constraints_list <- c("nI","nC","nameC","lowerC","upperC","wC")
  if(any(!constraints_list %in% names(constraints))){
    stop("Constraints object is missing elements: ",list(constraints_list[which(!constraints_list %in% names(constraints))]), ". Try creating constraints object by using makeconstobj() function.")
  }
  rm(constraints_list)
  
  # All constobj elements (except for nameC) are numeric
  if(any(unlist(
    lapply(constraints[c("nI","nC","lowerC","upperC","wC")], function (x) {
      !class(x) %in% c("numeric","integer")
    })
  ))){
    stop("Elements in constraint object (nI, nC, lowerC, upperC, wC) must be numeric. If factors, define them as numeric dummy codes.")
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
  
  # Consstraints upper bounds provided
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
  
  # ----------------------------- 
  # Screen input value for set_id 
  # ----------------------------- 
  
  # Define set_id as NA if not given.
  if(is.null(constraints$set_id[1])){
    constraints["set_id"] <- NA
  }
  
  # set_id name in input
  if(is.na(constraints$set_id)==FALSE){
    
    # set_id variable name exists in data
    if(!constraints$set_id %in% names(ipool)){
      stop("Specified set ID name does not appear in input data.")
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
  
  # ---------------------------------
  # Parse categorical dummy variables
  # ---------------------------------
  
  # Identify dummies
  catitem <- sapply(constraints$nameC, function(x){ifelse(length(unique(ipool[,which(colnames(ipool)==x)]))==2 & min(ipool[,which(colnames(ipool)==x)])==0 & max(ipool[,which(colnames(ipool)==x)])==1, x,NA)})
  catitem <- as.character(unlist(na.omit(catitem)))
  
  # ---------------- 
  # Restructure data 
  # ----------------   
  
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
  
  # Save item count in item sets.
  Count <- data_structured$Count
  
  # Rename the first column in aggregated data to match id.
  names(data_structured)[1] <- id
  
  # ----------------------------------
  # Preparing input and output objects
  # ----------------------------------
  
  # Making item pool selection (ips) copy
  ips <- data_structured[,c(id,constraints$nameC)]

  # IDs removed for duplication
  id_removed <- NA
  
  # ---------
  # Item ties 
  # ---------
  
  # Resolve ties / functional multiples (only if constraints are NOT all categorical)
  if(tieselect==1 & length(catitem)<constraints$nC & all(duplicated(ips[,-which(colnames(ips)==id)],nmax=1))==FALSE){  # First selected
    id_removed <- ips[which(duplicated(ips[,-which(colnames(ips)==id)],nmax=1)==TRUE),1]
    ips <- ips[which(duplicated(ips[,-which(colnames(ips)==id)],nmax=1)==FALSE),]
  }else if(tieselect==0 & length(catitem)<constraints$nC & all(duplicated(ips[,-which(colnames(ips)==id)],nmax=1))==FALSE){ # Random selected
    # Find all identical pairs
    grpids <- c()    
    for(i in 1:dim(ips)[1]){
      for(ii in 1:dim(ips)[1]){
        if(i < ii & all(ips[i,-1]==ips[ii,-1])==TRUE){
          grpids <- c(grpids, list(c(i,ii))) 
        }
      }
    }
    # Remove duplicates in pairs
    grpids <- do.call(rbind,grpids)
    remrow <- rep(0,dim(grpids)[1])
    for(iii in 1:dim(grpids)[1]){
      for(iiii in 1:dim(grpids)[1]){
        if(iii < iiii & grpids[iii,2]==grpids[iiii,1])
          remrow[iiii] <- 1
      }
    }
    grpids <- grpids[-which(remrow==1),]
    # Identify cases to kill and keep
    keepcase <- ips[c(unlist(by(grpids,grpids[,1],function(x){sample(c(unlist(x)),1)}))),which(colnames(ips)==id)]
    id_removed <- ips[unique(c(unlist(grpids))),which(colnames(ips)==id)]
    id_removed <- id_removed[-which(id_removed %in% keepcase)]
    ips <- ips[-which(ips[,which(colnames(ips)==id)] %in% id_removed),]
  }

# Messaging item removal
  if(verbose==TRUE & tieselect!=-1){
    message(ifelse(is.na(id_removed[1]),"No",length(id_removed))," item(s) were removed as duplicates.",sep="")
  }
  
  
  # Rename id variable into "id"
  names(ips)[which(names(ips)==id)] <- "id"
  
  # Making selected item test (sit)
  sit <- data.frame()
  
  # --------------
  # Item selection
  # -------------- 
  
  # Make constraint matrix and summative total (right-hand-side) value inputs
  # which include the minimum number of items to be selected
  useconstmat <- Count  #reflect item count within sets
  useconstdir <- c("=")
  useconstrhs <- c(constraints$nI)
  
  # Flatten constraint matrix
  for(j in constraints$nameC){
    useconstmat <- rbind( useconstmat,                   # Old matrix
                          ips[,which(colnames(ips)==j)], # Equation values for lower bound 
                          ips[,which(colnames(ips)==j)]) # Equation values for upper bound
    useconstdir <- c(useconstdir,">=","<=")
    useconstrhs <- c(useconstrhs,constraints$lowerC[which(constraints$nameC==j)], constraints$upperC[which(constraints$nameC==j)])
  }
  
  # Create objective function values
  useobjin <- if(type=="const" & permutate==TRUE){             # Item counts
     rep(1, dim(ips)[1])
  }else if((type=="parmin" | type=="parmax") & length(catitem)==0){ # Provides within constraint standardized and weighted composite
     colSums(apply(
       apply(ips[,-1],2,function(x){(x-mean(x))/sd(x)}), # Between item standardization (within constraint)
     1, function(y){y*constraints$wC[match(colnames(ips)[-1],constraints$nameC)]})) # Within item total variations (between constraints)
  }else if((type=="parmin" | type=="parmax") & length(catitem)>0){ # Provides within constraint standardized and weighted composite
    colSums(apply(
      apply(ips[,-c(1,which(names(ips) %in% catitem))],2,function(x){(x-mean(x))/sd(x)}), # Between item standardization (within constraint)
      1, function(y){y*constraints$wC[match(colnames(ips)[-c(1,which(names(ips) %in% catitem))],constraints$nameC)]})) # Within item total variations (between constraints)
  }else{
    rep(1, dim(ips)[1]) # Defaults to items counts
  }
  
  # Future addition of just refine 
  #else if(type=="const" & permutate==FALSE & refine==TRUE & length(catitem)>0){
  #  colSums(apply(
  #    apply(ips[,-c(1,which(names(ips) %in% catitem))],1,function(x){(constraints$lowerC[-which(constin$nameC %in% catitem)]+((constraints$upperC[-which(constin$nameC %in% catitem)]-constraints$lowerC[-which(constin$nameC %in% catitem)])/2))-x}), # Between item constraint distance
  #    2, function(y){y*constraints$wC[-which(constin$nameC %in% catitem)]})) # Within item total constraint distance
  #}
  
  #print(useobjin);
  #print(useconstmat);
  #print(useconstdir);
  #print(useconstrhs);
  
  # Run integer linear programming 
  lpout <- lp( direction = ifelse(type=="const" | type=="parmin", "min", ifelse(type=="parmax", "max","min")),
               objective.in = useobjin,
               const.mat = useconstmat,
               const.dir = useconstdir,
               const.rhs = useconstrhs,
               all.bin = TRUE
               )
  
  # Terminate if no possible solution
  if(lpout$status != 0){
    stop("No feasible solution found. Check constraints / expectations against inputs.")
  }
  
  # ------------------
  # Create permutation 
  # ------------------
  
  # List object to hold permutations
  alllps <- list()
  
  # Scramble item orders (relevant for type = const only)
  if(type=="const" & permutate==TRUE & sorttimes > 0){
    
    # Add base solution
    alllps <- list(
                  "ids"=ips$id[which(lpout$solution>0)],
                  "eval"=colSums(ips[which(lpout$solution>0),-1])
                )
    
    # Permutation loop
    for(q in 1:sorttimes){
      
      # Run lp
      neword <- sample(1:(dim(ips)[1]),dim(ips)[1],replace=FALSE)
      lpout2 <- lp( direction = "min",
                    objective.in = useobjin,
                    const.mat = useconstmat[,neword],
                    const.dir = useconstdir,
                    const.rhs = useconstrhs,
                    all.bin = TRUE,
                    scale = 0)
      
      # Add solution if valid
      if(lpout2$status == 0){
            alllps <- c(alllps,
                  list(
                    "ids"=sort(ips$id[neword][which(lpout2$solution>0)]),
                    "eval"=colSums(ips[neword,][which(lpout2$solution>0),-1])
                  ))
      }
      
      # Progress tracking
      if(verbose==TRUE){
        message("Permutation ",q," / ",sorttimes," complete.",sep="")
      }
        
    } # Close permutation loop
    
  } # Close permutation routine 
  
  # -----------------
  # Evaluate solution
  # -----------------
  
  if(type=="const" & permutate==TRUE & sorttimes > 0){
  
    # Combine all evaluations (for refine)
    alleval <- do.call(rbind,alllps[which(names(alllps)=="eval")])
    
    # Combine all selected solution IDs
    allids <- do.call(rbind,alllps[which(names(alllps)=="ids")])
    
    # ID profiles
    idprofile <- apply(allids,1,function(x){paste(x, collapse = " ")})
    tabidprof <- table(idprofile)
    
    # Compute "worst" constraint ratios
    evalU <- t(apply(alleval,1,function(x){constraints$upperC-unlist(x)}))
    #print(evalU)
    evalL <- t(apply(alleval,1,function(x){unlist(x)-constraints$lowerC}))
    #print(evalL)
    evalD <- (evalU+evalL)
    #print(evalD)
    evalW <- ifelse(evalU<=evalL,evalU,evalL)
    #print(evalW)
    
    # Weight distance deviations
    w_evalW <- ifelse(evalW==evalD,0,.5-(evalW/evalD))
    #print(w_evalW)
    w_evalW <- t(apply(w_evalW,1,function(x){unlist(x)*constraints$wC}))
    #print(w_evalW)
    w_evalSum <- rowSums(w_evalW)
    #print(w_evalSum)
    
    # Messaging
    if(verbose==TRUE){
      if(length(idprofile) < (sorttimes+1)){
        message((sorttimes+1)-length(idprofile),"lp() solutions failed.",sep=" ")
      }
      if(refine==FALSE){
        message("Selected item solution occured ",round(max(tabidprof)/sum(tabidprof)*100,2),"% across permutations, with ",length(tabidprof)," unique item group profiles.",sep=" ")
      }else if(refine==TRUE){
        message("Selected item solution occured ",round(length(which(w_evalSum==min(w_evalSum)[1]))/sum(tabidprof)*100,2),"% across permutations, with ",length(unique(w_evalSum))," unique deviation profiles.", sep=" ")
      }
      
    }

    # Final test item ids
    if(refine==FALSE){
      # IDs selected based on frequency of occurences
      outid <- allids[which(idprofile==names(tabidprof)[which(tabidprof==max(tabidprof))][1])[1],]
    }else if(refine==TRUE){
      outid <- allids[which(w_evalSum==min(w_evalSum))[1],]
    }
    
    # Selected item metadata
    sit <- ips[which(ips$id %in% outid),]
    
    # Remaining items in metadata
    ips <- ips[-which(ips$id %in% outid),]
        
  }else{
    
    # Final test item ids
    outid <- ips$id[which(lpout$solution>0)]
    
    # Selected item metadata
    sit <- ips[which(lpout$solution>0),]
    
    # Remaining items in metadata
    ips <- ips[which(lpout$solution<1),]
  }
  
  # ------------
  # Final output
  # ------------
  
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
  }#else{
  #  sit <- sit[,-c(dim(sit)[2])] #remove "count" column.
  #  ips <- ips[,-c(dim(ips)[2])] #remove "count" column.
  #}
  
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
  
  # Record time spent.
  end_time <- Sys.time()
  
  # Append selected and remaining item statistics
  if(length(catitem)<constraints$nC & (tieselect==1 | tieselect==0)){
    if(is.na(constraints$set_id)==TRUE){
      estout <- c("objective"=list(lpout$objval),
                  "items_removed" = list(id_removed),
                  "evaluation"=list(evalout),
                  "excluded"=list(ips),
                  "included"=list(sit),
                  "final_ids"=list(sit$id)
      )
    }else{
      estout <- c("objective"=list(lpout$objval),
                  "items_removed" = list(id_removed),
                  "evaluation"=list(evalout),
                  "excluded"=list(ips),
                  "excluded_sets"=list(ips_sets),
                  "included"=list(sit),
                  "included_sets"=list(sit_sets),
                  "final_ids"=list(sit$id),
                  "final_setids"=list(sit_sets$id)
      )
    }
    
  }else{
    if(is.na(constraints$set_id)==TRUE){
      estout <- c("objective"=list(lpout$objval),
                  "evaluation"=list(evalout),
                  "excluded"=list(ips),
                  "included"=list(sit),
                  "final_ids"=list(sit$id)
      )
    }else{
      estout <- c("objective"=list(lpout$objval),
                  "evaluation"=list(evalout),
                  "excluded"=list(ips),
                  "excluded_sets"=list(ips_sets),
                  "included"=list(sit),
                  "included_sets"=list(sit_sets),
                  "final_ids"=list(sit$id),
                  "final_setids"=list(sit_sets$id)
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
  if(type=="const" & permutate==TRUE){
    attr(estout, "refined") <- refine
  }else{
    attr(estout, "refined") <- FALSE 
  }
  
  # Permutated
  if(type=="const"){
    attr(estout, "permutated") <- permutate
  }else{
    attr(estout, "permutated") <- FALSE
  }
  # Runtime
  attr(estout, "runtime") <- end_time-start_time
  
  # Method
  attr(estout, "method") <- "lp"

  # Type
  attr(estout, "type") <- type
  
  # Class
  attr(estout, "class") <- "ata"
  
  # Return
  return(estout)
  
}
