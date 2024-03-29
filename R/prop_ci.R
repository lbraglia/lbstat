#' Proportion of a label with confidence interval
#' 
#' Proportion of a label with confidence interval
#' @param x a vector (factor tipically)
#' @param label the label of the desider percentage
#' 
#' @export
prop_ci <- function(x, label = 'Yes'){
    N <- length(x)
    NAS <- sum(is.na(x))
    Available <- N - NAS
    n <- sum(x %in% label)
    test <- binom.test(x = n, n = Available)
    res <- matrix(c(N, NAS, Available, n,
                    test$estimate*100, test$conf.int*100), nrow = 1)
    setNames(data.frame(res),
             c('N', 'NA', 'Available', sprintf('n %s', label),
               'Prop', 'Low.Ci', 'Up.Ci'))
}


#' Proportions of each label with confidence intervals
#' 
#' Proportion of a label with confidence interval
#' @param x a vector (factor tipically)
#' @examples
#' props_cis(gl(2, 10))
#' 
#' @export
props_cis <- function(x){
    tmp <- dummify(x)
    res <- lapply(tmp, function(y) 
        prop_ci(factor(y, levels = c(0, 1), labels = c("no ev", "ev")), 
                label = 'ev'))
    do.call(rbind, res)
}
