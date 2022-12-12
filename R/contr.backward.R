#' Backward Difference Coding
#' 
#' mean with confidence interval and some descriptive statistics
#' 
#' @param k number of levels of a factor
#' @references
#' \url{https://stats.oarc.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/}
#' @examples
#' \dontrun{
#' contr.backward(4)
#' hsb2 <- read.table('https://stats.idre.ucla.edu/stat/data/hsb2.csv',
#'                   header = TRUE, sep = ",")
#'
#' ## creating the factor variable race.f
#' races <- c("Hispanic", "Asian", "African-Am", "Caucasian")
#' hsb2$race.f <- factor(hsb2$race, labels = races)
#'
#' ## estimates
#' point_est <- coefficients(summary(lm(write ~ race.f - 1, data = hsb2)))
#' coef_trt <- coefficients(summary(lm(write ~ race.f, data = hsb2)))
#' contrasts(hsb2$race.f) <- contr.backward(4)
#' coef_back <- coefficients(summary(lm(write ~ race.f, data = hsb2)))
#' diff(point_est[,1])
#' }
#' 
#' @export
contr.backward <- function(k){
    
    ## 
    
    ncols <- k - 1  # numero di variabili stimate
    seq_vars <- seq_len(ncols)

    # numeratori sopra la diagonale
    nums_a <- - (k - seq_vars)
    part_a <- do.call(cbind, Map(function(x, n){
        c(rep(x = x, times = n), rep(0, k - n))
    }, nums_a, seq_vars))

    
    ## numeratori sotto la diagonale
    nums_b <- seq_vars
    part_b <- do.call(cbind, Map(function(x, n){
        c(rep(0, k - n), rep(x = x, times = n))
    }, nums_b, rev(nums_b)))

    # somma e dividi per k
    (part_a + part_b)/k
}

