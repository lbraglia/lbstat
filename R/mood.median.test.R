#' Mood median test
#' 
#' @param x first group of data
#' @param y second group of data
#' @export
mood.median.test <- function(x, y){
    data <- c(x, y)
    group <- factor(c(rep(1, length(x)), rep(2, length(y))))
    med <- median(data, na.rm = TRUE)
    data2 <- factor(data >= med, c(FALSE, TRUE), 
                    labels = c('< med', '>= med'))
    tab <- table(data2, group)
    test <- if (fisher_needed(tab)){
                fisher.test(tab)
            } else {
                chisq.test(tab)
            }
    list(tab = tab, p.value = test$p.value)
}
