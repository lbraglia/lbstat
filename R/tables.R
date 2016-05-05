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
    addmargins(base::table(useNA = useNA, ...), FUN = f)


#' Univariate table for categorical data.
#' 
#' @param x a discrete quantitative variable, a character or a factor
#' @param totals print totals?
#' @param useNA print NA?
#' @param NA_string character used for NA's columns title
#' @param round_digits number of rounding digits
#' @param sorting sorting can be "\code{asc}" or "\code{desc}"
#' @param latex output the table using \code{xtable::xtable}
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

#' Bivariate table for categorical data.
#' 
#' @param x row variable: a discrete quantitative variable, a
#'     character or a factor
#' @param y column variable: a discrete quantitative variable, a
#'     character or a factor
#' @param totals print totals?
#' @param useNA print NA?
#' @param NA_string character used for NA's columns title
#' @param round_digits number of rounding digits
#' @param sorting sorting can be "\code{asc}" or "\code{desc}", it is
#'     done by row sum
#' @param latex output the table using \code{xtable::xtable}
#' @param label latex label
#' @param caption latex caption
#' @examples
#' with(airquality, biv_quali(x = (OzHi = Ozone > 80), y = Month))
#' @export
biv_quali <- function(x = NULL,
                      y = NULL,
                      totals = TRUE,
                      useNA = 'ifany',
                      NA_string = 'NA',
                      round_digits = 3,
                      sorting = NULL,
                      latex = FALSE,
                      label = NULL,
                      caption = NULL)
{
    
    abs_freq <- table(x, y, useNA = useNA)
    row_sums <- rowSums(abs_freq)
        
    if( is.null(sorting) ) {
        ## do nothing
    } else if(sorting == 'desc') {
        ## descending ordered frequencies
        abs_freq <- abs_freq[rev(order(row_sums)), ]
    } else if( sorting == 'asc') {
        ## ascending ordered frequencies
        abs_freq <- abs_freq[order(row_sums), ]
    }   ## otherwise, do nothing

    ## column percentages
    rel_freq <- prop.table(abs_freq, margin = 2)
    colnames(rel_freq) <- rep('%', ncol(rel_freq))

    ## cbind together
    rval <- cbind(abs_freq, rel_freq)
   
    ## a little trick for column right ordering
    id_seq <- matrix(c(1:ncol(rval)), nrow = 2, byrow = TRUE)
    dim(id_seq) <- NULL
    rval <- rval[, id_seq]

    if(totals) {

        ## rows totals
        row_sums <- rowSums(abs_freq)
        col_tot_perc <- row_sums / sum(row_sums)
        rval <- cbind(rval, 'Tot' = row_sums, '%' = col_tot_perc)

        ## columns totals
        col_sums <- colSums(rval)
        rval <- rbind(rval, 'Tot' = col_sums)
    }

    ## NA label handling
    rownames(rval)[is.na(rownames(rval))] <- NA_string 
    colnames(rval)[is.na(colnames(rval))] <- NA_string 

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
#' @param latex output the table using \code{xtable::xtable}
#' @param label latex label
#' @param caption latex caption
#' @export
univ_quant <- function(x, latex = FALSE, label = NULL, caption = NULL)
{
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


#' Bivariate table for a main quantitative vector 
#' 
#' @param x a quantitative variable
#' @param y a discrete quantitative variable, a character or a factor
#' @param na.rm exclude missing value group for y
#' @param add_all add "All" row
#' @param latex output the table using \code{xtable::xtable}
#' @param label latex label
#' @param caption latex caption
#' @export
biv_quant <- function(x, y,
                      na.rm = FALSE,
                      add_all = TRUE,
                      latex = FALSE,
                      label = NULL,
                      caption = NULL)
{
    y <- factor(y, exclude = if (na.rm) NA else NULL)
    spl <- split(x, list(y))
    if (add_all) {
        all_ <- split(x, list('All'))
        spl <- c(spl, all_)
    }
    rval <- lapply(spl, desc)
    rval <- do.call(rbind, rval)
    if (latex) {
        xtab <- xtable::xtable(rval, caption = caption, label = label)
        xtable::print.xtable(xtab)
        invisible(rval)
    } else {
        return(rval)
    }
}



#' Percentages table for multiple categorical responses
#' 
#' @param x a (chunk of) data.frame encoding multiple responses (aka
#'      all composed of 0-1 variables)
#' @param latex output the table using \code{xtable::xtable}
#' @param label latex label
#' @param caption latex caption
#' @export
univ_mr <- function(x, latex = FALSE, label = NULL, caption = NULL)
{
    if (!is.data.frame(x))
        stop('x must be a data.frame')
    if (all(x  %in% c(0, 1, NA)))
        stop('x must only include 0, 1, NA')
    not_NA <- unlist(lapply(x, function(x) sum(!is.na(x))))
    s <- colSums(x, na.rm = TRUE)
    rval <- cbind('n' = s, '%' = round((s/not_NA)*100, 2))
    row.names(rval) <- paste0(row.names(rval), ' (n = ', not_NA, ')')
    row.names(rval) <- gsub('_', ' ', row.names(rval))
    rval <- rval[order(- rval[,1]), ]
    if (latex) {
        xtab <- xtable::xtable(rval, caption = caption, label = label)
        xtable::print.xtable(xtab)
        invisible(rval)
    } else {
        return(rval)
    }
}
