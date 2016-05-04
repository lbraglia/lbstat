#' Cross tabulation and table creation
#' 
#' This is a wrapper around table (using \code{useNA = "ifany"} by
#' default) and addsmargins. 
#' 
#' @param ... Arguments to be passed to table.
#' @param useNA display NA counts
#' @param f function to be used for summaries
#' @return The function return same results of table with NA (if present) and
#' margins.
#' @examples
#' with(airquality, Table(Month, Day))
#' @export
Table <- function(..., useNA = 'ifany', f = list('Sum' = sum))
    addmargins(base::table(useNA = useNA, ...),
               FUN = f)



#' Univariate table for categorical data.
#' 
#' @param x a discrete quantitative variable, a character or a factor
#' @param totals print totals?
#' @param useNA print NA?
#' @param NA_string character used for NA's columns title
#' @param round_digits number of rounding digits
#' @param sorting sorting can be "\code{asc}" or "\code{desc}"
#' @param latex output the table using xtable::xtable
#' @param label latex label
#' @param caption latex caption
#' @export
univ_quali <- function(x = NULL,
                       totals = TRUE,
                       useNA = 'ifany',
                       NA_string = 'NA',
                       round_digits = 3,
                       sorting = NULL,
                       latex = FALSE,
                       label = NULL,
                       caption = NULL)
{
    
    abs_freq <- table(x, useNA = useNA)
    
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
    rval <- cbind(abs_freq, rel_freq, cum_freq)
    colnames(rval) <- c('Abs', 'Rel', 'Cum')

    if(totals) {
        ## row totals
        Sum <- c(colSums(rval)[1:2], NA)
        rval <- rbind(rval, Sum)
    }

    ## NA
    rownames(rval)[is.na(rownames(rval))] <- NA_string 

    ## output
    if (latex){
        xt <- xtable::xtable(rval, label = label, caption = caption)
        xtable::print.xtable(xt)
        invisible(rval)
    } else {
        return(rval)
    }
        
}


#' Univariate table for quantitative data.
#' 
#' @param x a quantitative variable
#' @param latex output the table using xtable::xtable
#' @param label latex label
#' @param caption latex caption
#' @export
univ_quant <- function(x, latex = FALSE, label = NULL, caption = NULL){
    rval <- desc(x)
    if (latex) {
        names_ <- names(rval)
        m <- t(matrix(rval))
        colnames(m) <- names_
        xt <- xtable::xtable(m, label = label, caption = caption)
        xtable::print.xtable(xt, include.rownames = FALSE)
        invisible(rval)
    } else {
        return(rval)
    }
}

## #' Percentages table for multiple categorical responses
## #' 
## #' @param x a quantitative variable
## #' @param latex output the table using xtable::xtable
## #' @param label latex label
## #' @param caption latex caption
## #' @export
## xuniv_mr <- function(x, latex = FALSE, label = NULL, caption = NULL){
##     if (!is.data.frame(x))
##         stop('x must be a data.frame')
##     if (all(x  %in% c(0,1,NA)))
##         stop('x must only include 0, 1, NA')
##     not_NA <- unlist(lapply(x, function(x) sum(!is.na(x))))
##     s <- colSums(x, na.rm = TRUE)
##     tab <- cbind('n' = s, '% ' = round((s/not_NA)*100, 2))
##     row.names(tab) <- paste0(row.names(tab), ' (n = ', not_NA, ')')
##     row.names(tab) <- gsub('_', ' ', row.names(tab))
##     tab <- tab[order(- tab[,1]), ]
##     xtab <- xtable(tab, caption = caption, label = label)
##     print(xtab)
##     invisible(tab)
## }





## xbiv_quali <- function(x, y, label = NULL, caption = NULL){
##     ## statistiche descrittive per x (qualitativo) per y (qualitativo di stratificazione), 
##     ## con percentuali di colonna esportate via xtable
    
    
## }

## xbiv_quant <- function(x, y, f = desc, label = NULL, caption = NULL){
##     ## statistiche descrittive per x (quantitativo) per y (qualitativo di stratificazione), 
##     ## con percentuali di colonna esportate via xtable
##     spl <- split(x, list(y))
##     res <- lapply(spl, f)
##     res <- do.call(rbind, res)
##     print(xtable(res, label = label, caption = caption))
##     invisible(res)
## }
