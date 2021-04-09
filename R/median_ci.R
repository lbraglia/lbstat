#' Median bootstrap confidence interval (BCA or parametric for ordered
#' factors) for numeric and ordered factor
#' 
#' @param x data: an integer, numeric or ordered factor
#' @param conf confidence.level
#' @param R bootstrap repetitions
#'
#'@export 
median_ci <- function(x, conf = 0.95, R = 10000){
    ord <- is.ordered(x)
    f <- if (ord) {
             function(y) stats::quantile(y, probs = 0.5, type = 1, na.rm = TRUE)
         } else { 
             function(y) stats::median(y, na.rm = TRUE)
         }
    boot_f <- function(data, i) f(data[i])
    res <-  boot::boot(data = x, statistic = boot_f, R = R)
    ci <- boot::boot.ci(res, type = if (ord) 'perc' else 'bca', conf = conf)
    est <- f(x)
    boot_ci <- ci[[4]][4:5]
    res <- c(est, boot_ci)
    nm <- c('Est', 'Lower', "Upper")
    if (ord) setNames(levels(x)[res], nm = nm)
    else setNames(res, nm = nm)
}

#' Median bootstrap confidence interval (BCA or parametric for ordered
#' factors) for numeric and ordered factor
#' 
#' @param x data from the first group
#' @param y data from the second group
#' @param R bootstrap repetition
#' @param test add the mood test?
#'
#'@export 
median_diff_ci <- function(x, y, R = 10000, 
                           test = c('mood', 'none')){
    if (is.ordered(x) || is.ordered(y)) 
        stop("Only numeric variables")
    test <- match.arg(test)
    ## questa funzione non usa data.frame e la roba perché
    ## richiede che abbiano la stessa lunghezza x e y
    ## estimante
    f <- function(z) stats::median(z, na.rm = TRUE)
    est <- f(y) - f(x) 
    ## percentile confidence interval
    res <- NULL
    for(i in seq_len(R)){
        ## crea campioni bootstrap
        x_boot <- sample(x, replace = TRUE)
        y_boot <- sample(y, replace = TRUE)
        res <- c(res, f(y_boot) - f(x_boot))
    }
    boot_ci <- quantile(res, probs = c(0.025, 0.975), na.rm = TRUE)
    ## intervallo di confidenza
    rval <- c(est, boot_ci)
    nm <- c('Est', 'Lower', "Upper")
    rval <- setNames(rval, nm = nm)
    rval <- as.data.frame(as.list(rval))
    ## test di mood
    if (test == 'mood'){
        rval$p_value <- pretty_pval(mood.test(x = x, y = y)$p.value)
    }
    rval
}
