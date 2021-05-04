#' @export
#' @title Function \code{subgroupsem()}.
#' @description Interface to the python module pysubgroup for efficiently 
#' finding subgroups.
#' @param f_fit Function to be fitted. Must take at least two arguments. 
#' \code{f_fit} has the signature \code{function(group, dat, ...)}. \code{group} 
#' is a numeric vector. The length of this vector equals the rows in the data 
#' frame \code{dat} and is to be interpreted as an additional column indicating 
#' the group assignment. \code{f_fit} returns the interestingness measure. 
#' Returned values should be greater than \code{min_quality} in case of sucess
#' and smaller in case of failure (e.g., non-convergence, error).
#' @param dat A data frame.
#' @param columns Column names of the provided data frame which are to be 
#' analysed. Columns must have ordinal or nominal scale.
#' @param ignore Optional argument. If \code{columns = NULL}, \code{ignore} will 
#' be used to select every column that is not in ignore.
#' @param max_n_subgroups Maximum number of subgroups. Default is 10.
#' @param search_depth Maximum number of attribute combinations. Default is 3.
#' @param min_quality Minimum value of interestingness measure. Values below 
#' will not be considered. Default is 0.
#' @param weighting_attr Column name of a weighting attribute. Default is NULL 
#' (disabled). Not implemented yet.
#' @param generalization_aware Boolean. If specified, redundancy reduction is 
#' used. Default is TRUE.
#' @param na_rm Boolean. Default is FALSE. If set to TRUE, cases with NA values on any column 
#' will be set to FALSE in the \code{sg} vector. If set to FALSE, the regarding 
#' in the \code{sg} vector will be also \code{NA}.
#' @param ... Additional arguments to be passed to \code{f_fit}.
#' @return List containing the time consumed and the groups.
#' @examples
#' if (FALSE){
#' library(EffectLiteR)
#' 
#' dat <- read.csv(system.file("extdata", 
#'                             "data_kirchmann.csv", 
#'                             package="subgroupsem"))
#' 
#' # columns <- names(dat)[!(names(dat) %in% c("ind", "CESD_3", "CESD_1", "x"))]
#' columns <- c("Educ_d1", "Educ_d2", "Educ_d3", "Educ_d4")
#' 
#' f_fit <- function(sg, dat) {
#'     dat <- dat[sg == 1, ]
#'     
#'     res <- tryCatch({
#'         fit <- effectLite(y="CESD_3", x="x", data=dat)
#'         abs(fit@results@Egx[[1]])
#'     }, error = function(e) -1)
#'
#'     return(res)
#' }
#' 
#' task <- subgroupsem(f_fit = f_fit,
#'                     dat = dat,
#'                     columns = columns)
#' 
#' summary(task)
#' 
#' plot(task)
#' }
#' 
subgroupsem <- function(f_fit,
                        dat,
                        columns = names(dat),
                        ignore  = NULL,
                        max_n_subgroups = 10,
                        search_depth = 3,
                        min_quality = 0,
                        weighting_attr = NULL,
                        generalization_aware = TRUE,
                        na_rm = FALSE,
                        ...) {
    
    ## check if reticulate has been properly loaded
    if (!.pkgglobalenv$reticulate_loaded) {
        stop(
            "reticulate has not beend loaded properly. Function init_reticulate() may help you fix the issue.",
            call. = FALSE
        )
    }
    
    ## push data, functions and parameters to python
    py$data <- dat
    
    if (!is.null(ignore)) {
        columns <- columns[!(columns %in% ignore)]
    }
    py$ignore_names <- names(dat)[!(names(dat) %in% columns)]
    
    ## get matrix of NAs
    has_na <- sapply(columns, function(column) is.na(dat[,column]))
    
    f_fit_internal <- function(sg, selectors = NULL) {
        ## if selectors for subgroup is not NULL
        if (!na_rm && !is.null(selectors)) {
            ## if case has NA in one of the selectors, insert NA
            has_na_selectors <- apply(has_na[,selectors,drop=F], 1, any)
            sg <- ifelse(has_na_selectors, NA, sg)
        }
        ## pass sg and dat to user specified function
        return(f_fit(sg, dat, ...))
    }
    py$f_fit <- f_fit_internal
    
    ## calculate interestingness measure for the whole data
    quality_global <- f_fit_internal(rep_len(1, nrow(dat)))
    
    ## run pysubgroup package
    py_run_string("target = SEMTarget()")
    
    py_run_string("searchSpace = ps.create_selectors(data, ignore=ignore_names)")
    # py_run_string(paste0(
    #     "task = ps.SubgroupDiscoveryTask(data, target, searchSpace, ",
    #     "result_set_size=", max_n_subgroups,
    #     ", depth=", search_depth,
    #     ", min_quality=", min_quality,
    #     ", weighting_attribute=",
    #         if (is.null(weighting_attr)) "None" else paste0('"', weighting_attr, '"'),
    #     ", qf=",
    #         if (generalization_aware) "GeneralizationAwareQF(TestQF())" else "TestQF()",
    #     ")",
    # ))
    py_run_string(paste0(
        "task = ps.SubgroupDiscoveryTask(data, target, searchSpace, ",
        "result_set_size=", max_n_subgroups,
        ", depth=", search_depth,
        ", min_quality=", min_quality,
        ", weighting_attribute=",
        if (is.null(weighting_attr)) "None" else paste0('"', weighting_attr, '"'),
        ", qf=",
        if (generalization_aware) "GeneralizationAwareQF(TestQF())" else "TestQF()",
        ")"
    ))
    
    py_run_string("start = timer()")
    py_run_string("result = ps.SimpleDFS().execute(task)")
    py_run_string("end = timer()")
    
    
    ## collect results
    py_run_string("
summary = []
for i in range(len(result)):
    summary.append([result[i][0], result[i][1].subgroup_description.to_string(), result[i][1].subgroup_description.covers(data)])
")
    groups <- py$summary
    groups <- lapply(1:length(groups), function(index) {
        group <- groups[[index]]
        group <- c(index = index, group)
        names(group) <- c("index", "quality", "description", "cases")
        group
    })
    names(groups) <- paste0("Subgroup", 1:length(groups))
    
    # print('q: '+ str(q) + '\t Subgroup: ' + str(sg.subgroup_description) + '\t Size: ' + str(sg.statistics['size_sg']))"
    results <- list(time_elapsed = py$end - py$start,
                    quality_global = quality_global,
                    subgroups = groups)
    
    class(results) <- "subgroupsem"
    
    return(results)
}
