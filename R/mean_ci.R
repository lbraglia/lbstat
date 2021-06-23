#' Mean with confidence interval
#' 
#' mean with confidence interval and some descriptive statistics
#' 
#' @param x a vector
#' 
#' @export
mean_ci <- function(x, label = 'Yes'){
    N <- length(x)
    NAS <- sum(is.na(x))
    Available <- N - NAS
    test <- t.test(x)
    res <- matrix(c(N, NAS, Available,
                    test$estimate, test$conf.int), nrow = 1)
    setNames(data.frame(res),
             c('N', 'NA', 'Available', 
               'Mean', 'Low.Ci', 'Up.Ci'))
}
