.pkgglobalenv <- new.env(parent=emptyenv())
.pkgglobalenv$reticulate_loaded <- FALSE
.pkgglobalenv$py_path <- NULL
.pkgglobalenv$conda_path <- "auto"
.pkgglobalenv$envname <- "subgroupsem"
.pkgglobalenv$pysubgroup_version <- "0.6.1"

.onLoad <- function(libname, pkgname){
    init_reticulate()
}
