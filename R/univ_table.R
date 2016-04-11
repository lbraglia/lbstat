#' Univariate table with absolute, relative, cumulative frequencies.
#' 
#' Univariate table with absolute, relative, cumulative frequencies.
#' 
#' @param var vector
#' @param totals print totals?
#' @param useNA print NA?
#' @param NA_string character used for NA's columns title
#' @param round_digits number of rounding digits
#' @param sorting sorting can be "\code{asc} or "\code{desc}"
#' @return A matrix with frequencies.
#' @export
univ_table <- function(var = NULL,
                      totals = TRUE,
                      useNA = "ifany",
                      NA_string = "NA",
                      round_digits = 3,
                      sorting = NULL)
{
    
    abs_freq <- table(var, useNA = useNA)
    
    if( is.null(sorting) ) {
        ## do nothing
    } else if(sorting == 'desc') {
        ## descending ordered frequencies
        abs_freq <- rev(sort(abs_freq))
    } else if( sorting == 'asc') {
        ## ascending ordered frequencies
        abs_freq <- sort(abs_freq)
    }   ## otherwise, do nothing
	
    rel_freq <- prop.table(abs_freq)
    cum_freq <- cumsum(rel_freq)
    result <- cbind(abs_freq, rel_freq, cum_freq)
    colnames(result) <- c('Abs', 'Rel', 'Cum')

    if(totals) {
        ## row totals
        Sum <- c(colSums(result)[1:2], NA)
        result <- rbind(result, Sum)
    }

    ## NA
    rownames(result)[is.na(rownames(result))] <- NA_string 
        
    ## formatting
    round(result, round_digits)
}
