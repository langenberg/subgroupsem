#' @export
plot.subgroupsem <- function(x, ...) {
    obj <- x
    x <- 1:length(obj$subgroups)
    y <- sapply(obj$subgroups, function(element) element$quality)
    plot(
        x,y,
        xlab = "Subgroup index",
        ylab = "Interestingness measure",
        col = "black",
        type = "b", 
        pch = 21, 
        ylim = c(min(c(y,obj$quality_global)), max(c(y,obj$quality_global))),
        main = "Interestingness of subgroups"
    )
    abline(a = obj$quality_global, b = 0, col = "blue")
}

#' @export
summary.subgroupsem <- function(object, ...) {
    obj <- object
    cat("General information:")
    cat("\n\n")
    
    cat("Elapsed time: ")
    cat(obj$time_elapsed)
    cat("\n")
    
    cat("Interestingness measure full sample: ")
    cat(obj$quality_global)
    
    for (index in 1:length(obj$subgroups)) {
        sg <- obj$subgroups[[index]]
        
        cat("\n\n------------------------------------------\n\n")
        
        cat("Subgroup #")
        cat(sg$index)
        cat(":\n\n")
        
        cat("Subgroup description: ")
        cat(sg$description)
        cat("\n")
        
        cat("Size of subgroup: ")
        cat(length(which(sg$cases)))
        cat("\n")
        
        cat("Interestingness measure subgroup:    ")
        cat(sg$quality)
        cat("\n")
        
        cat("Interestingness measure full sample: ")
        cat(obj$quality_global)
        cat("\n")
        
        cat("Difference interestingness measure:  ")
        cat(sg$quality - obj$quality_global)
    }
}
