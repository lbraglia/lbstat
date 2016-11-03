#' Plot kernel density estimates for quantitative variable of a
#' list/data.frame
#'
#' Plot kernel density estimates for not 0-1 only quantitative
#' variable of a list/data.frame.
#' 
#' @param x a data.frame
#' @param na.rm remove NA values from density calculation
#' @param quiet if FALSE tells number and names of variables plotted
#' @param ... further arguments passed to \code{density}
#' @examples
#' par(mfrow = c(2, 3))
#' quant_density(airquality)
#' @export
quant_density <- function(x, na.rm = TRUE, quiet = TRUE, ...){
    stopifnot(is.list(x))
    ## plotted density: not 0-1 only numeric variable
    x <- Filter(f = lbmisc::is.quantitative, x = x)
    var_names <- if (is.character(names(x))) names(x) else ''
    if (!quiet){
        msg <- sprintf('plotting densities for %d variables:\n',
                       sum(var_names %nin% ''))
        cat(msg)
        cat(paste(var_names[var_names %nin% ''], col = '\n'))
    }
    
    plot_fun <- function(data, varname){
        
        graphics::plot(stats::density(x = data, na.rm = na.rm, ...),
                       main = varname)
        graphics::abline(v = mean(x = data, na.rm = na.rm),
                         col = lbmisc::col2hex('red', alpha = 0.5),
                         lty = 'dotted')
        graphics::abline(v = stats::median(x = data, na.rm = na.rm),
                         col = lbmisc::col2hex('blue', alpha = 0.5),
                         lty = 'dashed')
    }
    Map(plot_fun, x, var_names)
    invisible(NULL)
}
