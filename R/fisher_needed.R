#' Is Fisher's exact test needed
#'
#' Check wether Fisher's exact test is needed (or chi-square is ok)
#' 
#' @param x first variable
#' @param y second variable
#' @param threshold cutoff value under indipendence
#' @param full_report return a full report or only the
#'     \code{TRUE}/\code{FALSE} answer
#' @export
fisher_needed <- function(x = NULL,
                          y = NULL,
                          threshold = 5L,
                          full_report = FALSE)
{

    if(length(x) != length(y))
        stop("x and y don't have the same length")
    
    ## Result list
    res$table <- table(x, y)

    ## Under indipendence...
    cross <- prop.table(table(x)) %o% prop.table(table(y))
    res$indipendence <- cross * length(x)
    res$use_fisher <- any(res$indipendence <= threshold)
    
    if (full_report) {
        res
    } else
        res$use_fisher

}
