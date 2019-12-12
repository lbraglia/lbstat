#' cor.test method for data.frame
#'
#' Calculates all coefficients and confidence intervals for the
#' combinations of variable available in the data.frame
#' 
#' @param x a data.frame
#' @param alternative cor.test alternative parameter
#' @param method cor.test method parameter
#' @param exact cor.test exact parameter
#' @param conf.level  cor.test conf.level parameter
#' @param continuity cor.test continuity parameter
#' @param ... further arguments passed to cor.test
#' @examples cor.test(airquality)
#' @export
cor.test.data.frame <-
    function(x, 
             alternative = c('two.sided', 'less', 'greater'),
             method = c("pearson", "kendall", "spearman"),
             exact = NULL, conf.level = 0.95, 
             continuity = FALSE,
             ...)
{
    ## generate all the combination of variable in a reasonable way
    indexes <- as.list(as.data.frame((combn(seq_len(ncol(x)), 2))))
    cor_ci_f <- function(x) 
        setNames(c(x$estimate, x$conf.int), c('est', 'low', 'up'))
    res <- lapply(indexes, function(id) {
        var1_name <- names(x)[id[1]]
        var2_name <- names(x)[id[2]]
        var1 <- x[, var1_name]
        var2 <- x[, var2_name]
        test <- stats::cor.test(x = var1, y = var2, 
                                alternative = alternative,
                                method = method,
                                exact = exact,
                                conf.level = conf.level,
                                continuity = continuity,
                                ...)
        test_ci <- as.list(cor_ci_f(test))
        data.frame('v1' = var1_name, 
                   'v2' = var2_name, 
                   'est' = test_ci[1], 
                   'low' = test_ci[2], 
                   'up'  = test_ci[3])
    })
    res <- do.call(rbind, res)
    rownames(res) <- NULL
    res
}
