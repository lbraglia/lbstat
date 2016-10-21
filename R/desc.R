## parametrized as in http://www.stata.com/manuals13/rsummarize.pdf
## r-th moment about the mean (helper function for skewness and kurtosis)
m_r <- function(x, r = NULL, na.rm = FALSE){
    if (is.null(r))
        stop("n can't be NULL")
    if (na.rm)
        x <- x[!is.na(x)]
    n <- length(x)
    sum(  (x - mean(x, na.rm = na.rm))^r )/n
}


#' skewness calculator
#' 
#' skewness calculator, parametrized as in
#' \url{http://www.stata.com/manuals13/rsummarize.pdf}
#' @param x a quantitative variable
#' @param na.rm remove not available values
#' @export
skewness <- function(x, na.rm = FALSE){
    m_r(x = x, r = 3L, na.rm = na.rm) * 
    (m_r(x = x, r = 2L, na.rm = na.rm)^(-3/2))
}

#' kurtosis calculator
#' 
#' kurtosis calculator, parametrized as in
#' \url{http://www.stata.com/manuals13/rsummarize.pdf}
#' @param x a quantitative variable
#' @param na.rm remove not available values
#' @export
kurtosis <- function(x, na.rm = FALSE){
    m_r(x = x, r = 4L, na.rm = na.rm) * 
    (m_r(x = x, r = 2L, na.rm = na.rm)^(-2))
}

#' A vector of common descriptive statistics for quantitative data
#'
#' A vector of common descriptive statistics for quantitative data
#' @param x a quantitative variable
#' @param na.rm remove not available values
#' @param exclude which statistic to exclude? character vector 
#' @export
desc <- function(x, na.rm = TRUE, exclude = ''){
    qq <- unname(stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = na.rm))

    exclude <- tolower(exclude)
    Exclude <- c(any(exclude %in% 'n'),
                 any(exclude %in% c('na',NA)),
                 any(exclude %in% c('min')),
                 any(exclude %in% c('1st qu.')),
                 any(exclude %in% c('median')),
                 any(exclude %in% c('mean')),
                 any(exclude %in% c('3rd qu.')),
                 any(exclude %in% c('max')),
                 any(exclude %in% c('std. dev.')))

    c('n' = length(x),
      'NA' = sum(is.na(x)),
      'Min' = min(x, na.rm = na.rm),
      '1st Qu.' = qq[1],
      'Median' = qq[2],
      'Mean' = mean(x, na.rm = na.rm),
      '3rd Qu.' = qq[3],
      'Max' = max(x, na.rm = na.rm), 
      'Std. Dev.' = sd(x, na.rm = na.rm)
     )[!Exclude]
}
