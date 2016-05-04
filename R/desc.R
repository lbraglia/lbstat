#' A vector of common descriptive statistics for quantitative data
#'
#' A vector of common descriptive statistics for quantitative data
#' @param x a quantitative variable
#' @param na.rm remove not available values
#' @export
desc <- function(x, na.rm = TRUE){
    qq <- unname(stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = na.rm))
    
    c('n' = length(x),
      'NA' = sum(is.na(x)),
      'Min' = min(x, na.rm = na.rm),
      '1st Qu.' = qq[1],
      'Median' = qq[2],
      'Mean' = mean(x, na.rm = na.rm),
      '3rd Qu.' = qq[3],
      'Max' = max(x, na.rm = na.rm), 
      'StdDev' = sd(x, na.rm = na.rm)
     )
}
