#' Plot kernel density estimates for quantitative variable of a
#' list/data.frame
#'
#' Plot kernel density estimates for not 0-1 only quantitative
#' variable of a list/data.frame.
#' 
#' @param x a data.frame
#' @param na.rm remove NA values from density calculation
#' @param ... further arguments passed to \code{density}
#' @examples
#' par(mfrow = c(2, 3))
#' quant_density(airquality)
#' @export
quant_density <- function(x, na.rm = TRUE, ...){
    stopifnot(is.list(x))
    ## plotted density: not 0-1 only numeric variable
    filter <- function(x)is.numeric(x) && (!all(x %in% c(NA, 0, 1)))
    x <- Filter(f = filter, x = x)
    var_names <- if (is.character(names(x))) names(x) else ''
    plot_fun <- function(data, varname) 
        graphics::plot(stats::density(x = data, na.rm = na.rm, ...),
                       main = varname)
    Map(plot_fun, x, var_names)
    invisible(NULL)
}
