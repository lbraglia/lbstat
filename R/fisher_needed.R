#' Is Fisher's exact test needed
#'
#' Check wether Fisher's exact test is needed (or chi-square is ok)
#' 
#' @param x first variable, a table or a matrix
#' @param y second variable
#' @param threshold cutoff value under indipendence
#' @param full_report return a full report or only the
#'     \code{TRUE}/\code{FALSE} answer
#' @examples
#' test <- matrix(c(1, 20, 15, 3), byrow = TRUE, ncol = 2)
#' fisher_needed(test)
#' fisher_needed(as.table(test))
#'
#' TeaTasting <- matrix(c(3, 1, 1, 3), nrow = 2,
#'                      dimnames = list(Guess = c("Milk", "Tea"),
#'                                      Truth = c("Milk", "Tea")))
#' fisher_needed(TeaTasting)
#' @export
fisher_needed <- function(x = NULL,
                          y = NULL,
                          threshold = 5L,
                          full_report = FALSE)
{

    ## Result list
    res <- list()

    ## Test input and decide what to do
    if (!is.null(y)){  ## both x and y are given
       
        stopifnot(length(x) == length(y))
        res$table <- table(x, y)
        cross <- prop.table(table(x)) %o% prop.table(table(y))
        res$indipendence <- cross * length(x)
        res$use_fisher <- any(res$indipendence <= threshold)

    } else { ## if y is NULL, x must be a matrix or a table

        stopifnot(is.matrix(x) || is.table(x))
        res$table <- as.table(x)
        col_sums <- colSums(x)
        row_sums <- rowSums(x)
        cross <- prop.table(row_sums) %o% prop.table(col_sums)
        res$indipendence <- cross * sum(x)
        res$use_fisher <- any(res$indipendence <= threshold)

    }
    
    if (full_report) {
        res
    } else
        res$use_fisher

}
