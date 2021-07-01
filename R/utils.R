#' @keywords internal
attach_reticulate <- function() {
    
    ## try to load reticulate
    tryCatch({
        
        detach_reticulate()
        
        load_reticulate()
        
        activate_condaenv()
        
        install_py_module(modulename = "numpy")
        
        install_py_module(modulename = paste0(
            "pysubgroup",
            if (!is.null(.pkgglobalenv$pysubgroup_version))
                paste0("==", .pkgglobalenv$pysubgroup_version)
            else
                ""
        ))
        
        message("Importing python modules...")
        py_run_string("import pysubgroup as ps")
        py_run_string("from timeit import default_timer as timer")
        
        message("Sourcing additional python classes...")
        py_run_string(get_py_classes())
        
        message("Library reticulate successfully attached! Yaaay :-)")
        .pkgglobalenv$reticulate_loaded <- TRUE
        
        return(list(status = TRUE, message = ""))
        
    }, error = function(e) {
        detach_reticulate()
        
        return(list(status = FALSE, message = e))
    })
}

#' @keywords internal
activate_condaenv <- function() {
    message(paste0("Activating conda environment ", 
                   .pkgglobalenv$envname, 
                   "..."))
    if (!(.pkgglobalenv$envname %in% conda_list()$name)) {
        message("conda environment r-reticulate not found, creating environment...")
        conda_create(.pkgglobalenv$envname, 
                     conda = .pkgglobalenv$conda_path)
    }
    use_condaenv(.pkgglobalenv$envname, 
                 conda = .pkgglobalenv$conda_path, 
                 required = TRUE)
}

#' @keywords internal
load_reticulate <- function() {
    if (!is_reticulate_attached()) {
        message("Attaching library reticulate...")
        requireNamespace("reticulate")
    }
}

#' @export
init_reticulate <- function() {
    
    if (!(status <- attach_reticulate())$status) {
        message(paste0("\nRSubgroup error:\n",
                       "reticulate could not be initialized with error: \n", 
                       "    ", status$message, "\n\n",
                       "Did you properly install Anaconda? You may want to change the conda path using init_reticulate()."))
    }
    
}

#' @export
repair_reticulate <- function() {
    
    remove_condaenv()
    
    detach_reticulate()
    
    init_reticulate()
}

#' @keywords internal
is_reticulate_attached <- function() {
    return("reticulate" %in% (.packages()))
}

#' @keywords internal
detach_reticulate <- function() {
    if(is_reticulate_attached()){
        message("Detaching library reticulate...")
        detach("package:reticulate", unload=TRUE) 
    }
    .pkgglobalenv$reticulate_loaded <- FALSE
}

#' @export
remove_condaenv <- function() {
    
    is_attached <- is_reticulate_attached()
    
    load_reticulate()
    
    message(paste0("Removing conda environment ", .pkgglobalenv$envname, "..."))
    conda_remove(.pkgglobalenv$envname, 
                 conda = .pkgglobalenv$conda_path)
    
    if (!is_attached) {
        detach_reticulate()
    }
}

#' @export
install_py_module <- function(modulename) {
    
    if (!py_module_available(modulename)) {
        message(paste0("Python module ",
                       modulename,
                       " missing, installing..."))
        
        conda_install(envname = .pkgglobalenv$envname, 
                      modulename, 
                      pip = TRUE, 
                      pip_ignore_installed = TRUE,
                      conda = .pkgglobalenv$conda_path)
    }
    
}

#' @export
config_rsubgroup <- function(py_path,
                             conda_path,
                             envname,
                             reload = FALSE) {
    if (!missing(py_path)) {
        .pkgglobalenv$py_path <- py_path
    }
    
    if (!missing(conda_path)) {
        .pkgglobalenv$conda_path <- conda_path
    }
    
    if (!missing(envname)) {
        .pkgglobalenv$envname <- envname
    }
    
    if (reload) {
        init_reticulate()
    }
}

#' @export
get_rsubgroup_config <- function() {
    return(list(reticulate_loaded = .pkgglobalenv$reticulate_loaded,
                py_path = .pkgglobalenv$py_path,
                conda_path = .pkgglobalenv$conda_path,
                envname = .pkgglobalenv$envname))
}


