#' @export
#' @title User-level function for Wald-test based SubgroupSEM
#' @description todo 
#' @param model a lavaan model syntax (a character vector)
#' @param data a data frame
#' @param constraints a lavaan syntax-based constraint of parameters for the 
#' Wald test. (a character vector)
#' @param predictors a character vector of variable names, which are used as 
#' covariates/predictors in the subgroup discovery (variables in data)
#' @importFrom lavaan sem
#' @importFrom lavaan lavInspect
#' @importFrom lavaan lavTestWald
subsem_wald <- function(model, data, constraints, predictors = NULL){
  
  # Extract covariates names
  predictors  <- subsem_get_predictor_names(model, data, predictors)
  
  f_fit <- function(sg, dat){
    # Add subgroup to dataset (from logical to numeric)
    sg <- as.numeric(sg)
    dat$subgroup <- sg 
    
    # if all participants in subgroup return -1
    if (all(sg == 1)){
      rval <- 0
      return(rval)
    }
    
    rval <- tryCatch({
      # Fit Model
      fit <- sem(model, data = dat, group = "subgroup", missing="fiml", cluster = "ID_class_7")
      
      stopifnot(lavInspect(fit, "post.check"))
      # Compute interestingness measure
      lavwald <- lavTestWald(fit, constraints)
      rval <- lavwald$stat
    }, error = function(e) -1)
    
    if(!is.numeric(rval) | length(rval) > 1){rval <- -1}
    return(rval)
  }
  
  # Search for subgroups
  cat("Searching for subgroups...")
  task <- tryCatch({
    subgroupsem(f_fit = f_fit,
                dat = data,
                columns = predictors,
                search_depth = 2,
                max_n_subgroups = 10,
                generalization_aware = FALSE)
  }, error = function(e) -1)
  cat("Done.\n")
  return(task)
}


#' @export
#' @title User-level function for LRT-based SubgroupSEM
#' @description todo 
#' @param model a lavaan model syntax (a character vector)
#' @param data a data frame
#' @param predictors a character vector of variable names, which are used as 
#' covariates/predictors in the subgroup discovery (variables in data)
#' @importFrom lavaan sem
#' @importFrom lavaan lavInspect
#' @importFrom lavaan lavaanify
subsem_lrt <- function(model, data, predictors = NULL){
  
  # Extract covariates names
  predictors  <- subsem_get_predictor_names(model, data, predictors)
  
  
  get_single_group_partable <- function(model){
    pt <- lavaanify(model = model, ngroups = 2L, 
                    # default options for sem/cfa call
                    int.ov.free     = TRUE,
                    int.lv.free     = FALSE,
                    auto.fix.first  = TRUE, 
                    auto.fix.single = TRUE,
                    auto.var        = TRUE,
                    auto.cov.lv.x   = TRUE,
                    auto.cov.y      = TRUE,
                    auto.th         = TRUE,
                    auto.delta      = TRUE,
                    auto.efa        = TRUE,
                    meanstructure   = TRUE)
    model_single_group <- pt[pt$group == 1,]
    return(model_single_group)
  }
  
  baselinefit <- sem(model = get_single_group_partable(model), 
                             data = data)
  
  # Interestingness Measure in Baseline Fit
  basefit <- lavInspect(baselinefit, "fit")
  basefitlog <- -2*basefit["logl"]
  
  f_fit <- function(sg, dat){
    # Add subgroup to dataset (from logical to numeric)
    sg <- as.numeric(sg)
    dat$subgroup <- sg 
    
    # if all participants in subgroup return -1
    if (all(sg == 1)){
      rval <- 0
      return(rval)
    }
    
    rval <- tryCatch({
      # Fit Model
      fit <- sem(model, data = dat, group = "subgroup")
      stopifnot(lavInspect(fit, "post.check"))
      
      # Compute interestingness measure
      tmp <- lavInspect(fit, "fit")
      rval <- abs(-2*tmp["logl"] - basefitlog)
    }, error = function(e) -1)
    
    if(!is.numeric(rval) | length(rval) > 1){rval <- -1}
    
    return(rval)
  }
  
  # Search for subgroups
  cat("Searching for subgroups...")
  task <- tryCatch({
            subgroupsem(f_fit = f_fit,
                        dat = data,
                        columns = predictors,
                        search_depth = 4,
                        max_n_subgroups = 10,
                        generalization_aware = FALSE)
  }, error = function(e) -1)
  cat("Done.\n")
  return(task)
  
}

#' @noRd
#' @keywords internal
subsem_get_predictor_names <- function(model, data, predictors){
  if (is.null(predictors)){
    predictors <- names(data)[!names(data) %in% lavNames(lavaanify(model, ngroups = 2L))] 
  } else if (is.character(predictors)){
    predictors <- predictors
  } else {
    stop("You have not correctly specified the predictor variables.")
  }
  return(predictors)
}




