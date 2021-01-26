#' Quick median with bootstrap confidence interval for
#' numeric and ordered factor
#' 
#' @param x data: an integer, numeric or ordered factor
#' @param N bootstrap repetitions
#'
#'@export 
median_ci <- function(x, N = 10000){ # x ordered factor
    if (is.ordered(x)) {
        type <- 1
        f <- function(x) quantile(x, probs = 0.5, type = type)
    } else {
        type <- 7
        f <- stats::median
    }
    res <- NULL
    ## loop over n
    for (i in seq_len(N)) {
        data <- sample(x, replace = TRUE)
        res[i] <- f(data)
    }
    res <- if (is.ordered(x))
               factor(as.character(res), levels = levels(x), ordered = TRUE)
           else
               res
    ci <- as.list(quantile(res, probs = c(0.025, 0.975), type = type))
    res <- data.frame('median' = f(x), ci)
    setNames(res, c('median', 'lower boot CI', 'upper boot CI'))
}
