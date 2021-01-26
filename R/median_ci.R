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
        fun <- function(x) quantile(x, probs = 0.5, type = type, na.rm = TRUE)
    } else {
        type <- 7
        fun <- function(x) stats::median(x, na.rm = TRUE)
    }
    res <- NULL
    ## loop over n
    for (i in seq_len(N)) {
        bootsample <- sample(x, replace = TRUE)
        res[i] <- fun(bootsample)
    }
    
    ## per qualche motivo nell'assegnazione viene downgradato
    ## ad intero, lo riporto in factor
    if (is.ordered(x)) {
        res <- factor(res,
                      levels = seq_len(nlevels(x)),
                      labels = levels(x),
                      ordered = TRUE)
    }

    ci <- as.list(quantile(res, probs = c(0.025, 0.975),
                           type = type, na.rm = TRUE))
    res <- data.frame('median' = fun(x), ci)
    setNames(res, c('median', 'lower boot CI', 'upper boot CI'))
}
