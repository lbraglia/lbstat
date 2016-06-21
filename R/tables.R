#' Cross tabulation and table creation
#' 
#' This is a wrapper around table (using \code{useNA = "ifany"} by
#' default) and addsmargins. 
#' 
#' @param ... Arguments to be passed to table.
#' @param useNA display NA counts
#' @param f function to be used for summaries
#' @param quiet addmargins quiet parameter
#' @param margin addmargins margin parameter
#' @return The function return same results of table with NA (if present) and
#' margins.
#' @examples
#' with(airquality, Table(Month, Day))
#' @export
Table <- function(..., useNA = 'ifany', f = list('Sum' = sum),
                  margin = NULL, quiet = TRUE){
    tab <- base::table(useNA = useNA, ...)
    if (is.null(margin))
        margin <- seq_along(dim(tab))
    addmargins(tab, FUN = f, quiet = quiet, margin = margin)
}

##
## helper for exporting tables ... very raw for now
##
xlsx_table <- function(tab, wb, sheet, label, caption, varname) {

    if (sheet == '')
        sheet <- varname
    if (label == '')
        label <- sprintf('tab:%s', varname)
    if (caption == '')
        caption <- gsub('_', ' ', varname)

    sheet <- strtrim(sheet, 31)
    
    ## todo: use label and caption
    openxlsx::addWorksheet(wb = wb, sheetName = sheet)
    openxlsx::writeData(wb = wb, sheet = sheet, x = tab,
                        rowNames = TRUE)
}



#' Univariate table for categorical data.
#' 
#' @param x a discrete quantitative variable, a character or a factor
#' @param totals print totals?
#' @param useNA print NA?
#' @param NA_string character used for NA's columns title
#' @param freq_sorting freq based sorting: can be \code{NA} (no freq
#'     based sorting) "\code{desc}" (descending) or "\code{asc}"
#'     (ascending).
#' @param latex output the table using \code{xtable::xtable}
#' @param latex_floating use floating environment for
#' latex printing (default = TRUE)
#' @param latex_placement table placement for latex printing
#' @param label latex label
#' @param caption latex caption
#' @param wb an openxlsx Workbook; if not NULL the table will be saved
#'     in the workbook too, aside printing
#' @param sheets optional sheet names (same length as the number of tables)
#' @examples
#'  univ_quali(x = airquality$Month)
#'  univ_quali(x = airquality[, c('Month')])
#'  univ_quali(x = airquality[, c('Month', 'Day')])
#'  univ_quali(x = airquality[, c('Month', 'Day')], latex = TRUE)
#'  univ_quali(x = airquality[, c('Month', 'Day')], latex = TRUE,
#'             label = c('tab:airq_month', 'tab:airq_day'),
#'             caption = c('airquality month', 'airquality day')
#'  )
#'  univ_quali(list('a' = rep(LETTERS[1:5],2),
#'                  'b' = rep(letters[1:5],2)))
#' @export
univ_quali <- function(x = NULL,
                       totals = TRUE,
                       useNA = 'ifany',
                       NA_string = 'NA',
                       freq_sorting = c(NA, 'desc', 'asc'),
                       latex = FALSE,
                       latex_floating = TRUE,
                       latex_placement = 'ht',
                       label = NULL,
                       caption = NULL,
                       wb = NULL,
                       sheets = NULL)
{

    ## input normalization
    freq_sorting <- match.arg(freq_sorting)
    
    if (is.null(label))
        label <- ''
    if (is.null(caption))
        caption <- ''
    if (is.null(sheets))
        sheets <- ''
    
    if (is.data.frame(x)){
        x <- as.list(x)
    } else if (is.list(x)){
        # do nothing
    } else {
        xname <- deparse(substitute(x))
        xname <-  gsub('^.+\\$', '', xname)
        x <- list(x)
        names(x) <- xname
    }

    rval <- lapply(x, function(z)
        univ_quali_worker(y = z,
                          totals = totals,
                          useNA = useNA,
                          NA_string = NA_string,
                          freq_sorting = freq_sorting))

    
    ## Workbook handling
    if (methods::is(wb, "Workbook")){
        mapply(xlsx_table,
               rval,
               list(wb),
               as.list(sheets),
               as.list(label),
               as.list(caption),
               as.list(names(rval)))
    }

    ## Print and return
    if (latex){
        mapply(univ_quali_latex_printer,
               rval,
               as.list(label),
               as.list(caption),
               as.list(names(rval)),
               as.list(latex_floating),
               as.list(latex_placement)
               )
        invisible(rval)
    } else {
        ## if it's a list of length 1, return the only element not as
        ## list element
        if (length(rval) == 1L)
            return(rval[[1]])
        else
            return(rval)
    }

}

univ_quali_worker <- function(y,
                              totals,
                              useNA,
                              NA_string,
                              freq_sorting
                              )
{

    abs_freq <- table(y, useNA = useNA)

    if( is.na(freq_sorting) ) {
        ## do nothing
    } else if(freq_sorting == 'desc') {
        ## descending ordered frequencies
        abs_freq <- rev(sort(abs_freq))
    } else if( freq_sorting == 'asc') {
        ## ascending ordered frequencies
        abs_freq <- sort(abs_freq)
    }   ## otherwise, do nothing

    rel_freq <- prop.table(abs_freq) * 100
    cum_freq <- cumsum(rel_freq)
    rval <- cbind(abs_freq, rel_freq, cum_freq)
    ## colnames(rval) <- c('Abs', 'Rel', 'Cum')
    colnames(rval) <- c('n', '%', 'cum. %')

    if(totals) {
        ## row totals
        Sum <- c(colSums(rval)[1:2], NA)
        rval <- rbind(rval, Sum)
    }

    ## NA
    rownames(rval)[is.na(rownames(rval))] <- NA_string 
    
    return(rval)

}


univ_quali_latex_printer <- function(y,
                                     label,
                                     caption,
                                     varname,
                                     latex_floating,
                                     latex_placement
                                     )
{

    if (label == '')
        label <- sprintf('tab:%s', varname)
    if (caption == '')
        caption <- gsub('_', ' ', varname)
    
    xt <- xtable::xtable(y,
                         ## align = 'ccc',
                         digits = c(0, 0, 2, 2),
                         label = label,
                         caption = caption)
    xtable::print.xtable(xt,
                         floating = latex_floating,
                         table.placement = latex_placement)

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
#' @param freq_sorting freq based sorting: can be \code{NA} (no freq based
#'     sorting) "\code{desc}" (descending) or "\code{asc}"
#'     (ascending). Sorting based on row totals.
#' @param latex output the table using \code{xtable::xtable}
#' @param latex_floating use floating environment for
#' latex printing (default = TRUE)
#' @param latex_placement table placement for latex printing
#' @param label latex label
#' @param caption latex caption
#' @param wb an openxlsx Workbook; if not NULL the table will be saved
#'     in the workbook too, aside printing
#' @param sheets optional sheet names (same length as the number of tables)
#' @examples
#' with(airquality, biv_quali(x = (OzHi = Ozone > 80), y = Month))
#' @export
biv_quali <- function(x = NULL,
                      y = NULL,
                      totals = TRUE,
                      useNA = 'ifany',
                      NA_string = 'NA',
                      ## round_digits = 3,
                      freq_sorting = c(NA, 'desc', 'asc'),
                      latex = FALSE,
                      latex_floating = TRUE,
                      latex_placement = 'ht',
                      label = NULL,
                      caption = NULL,
                      wb = NULL,
                      sheets = NULL)
{
    xname <- gsub('^.+\\$', '', deparse(substitute(x)))
    yname <- gsub('^.+\\$', '', deparse(substitute(y)))
    varnames <- strtrim(paste(xname, yname, sep = '_'), 31)

    if (is.null(label))
        label <- ''
    if (is.null(caption))
        caption <- ''
    if (is.null(sheets))
        sheets <- ''

    freq_sorting <- match.arg(freq_sorting)
    abs_freq <- table(x, y, useNA = useNA)
    row_sums <- rowSums(abs_freq)

    if( is.na(freq_sorting) ) {
        ## do nothing
    } else if(freq_sorting == 'desc') {
        ## descending ordered frequencies
        abs_freq <- abs_freq[rev(order(row_sums)), ]
    } else if( freq_sorting == 'asc') {
        ## ascending ordered frequencies
        abs_freq <- abs_freq[order(row_sums), ]
    }   ## otherwise, do nothing

    ## column percentages
    rel_freq <- prop.table(abs_freq, margin = 2) * 100
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
        col_tot_perc <- (row_sums / sum(row_sums)) * 100
        rval <- cbind(rval, 'Tot' = row_sums, '%' = col_tot_perc)

        ## columns totals
        col_sums <- colSums(rval)
        rval <- rbind(rval, 'Tot' = col_sums)
    }

    ## NA label handling
    rownames(rval)[is.na(rownames(rval))] <- NA_string 
    colnames(rval)[is.na(colnames(rval))] <- NA_string 

    ## Workbook handling
    if (methods::is(wb, "Workbook")){
        xlsx_table(rval,
                   wb,
                   sheets,
                   label,
                   caption,
                   varnames)
    }

    ## output
    if (latex){
        ## alternate 0,2 for number of columns/2
        digits <- rep(c(0,2), ncol(rval)/2)
        xt <- xtable::xtable(rval,
                             ## align = 'todo',
                             digits = c(0, digits),
                             label = label, caption = caption)
        xtable::print.xtable(xt,
                             floating = latex_floating,
                             table.placement = latex_placement
                             )
        invisible(rval)
    } else {
        return(rval)
    }

}

#' Univariate table for quantitative data.
#' 
#' @param x a quantitative variable, a data.frame or a list
#' @param latex output the table using \code{xtable::xtable}
#' @param latex_floating use floating environment for
#' latex printing (default = TRUE)
#' @param latex_placement table placement for latex printing
#' @param label latex label
#' @param caption latex caption
#' @param wb an openxlsx Workbook; if not NULL the table will be saved
#'     in the workbook too, aside printing
#' @param sheets optional sheet names (same length as the number of tables)
#' @examples
#'    univ_quant(x = airquality$Ozone)
#'    univ_quant(x = airquality[, c('Ozone')])
#'    univ_quant(x = airquality[, c('Ozone', 'Temp')])
#'    univ_quant(list('a' = 1:10, 'b' = 2:20))
#' @export
univ_quant <- function(x,
                       latex = FALSE,
                       latex_floating = TRUE,
                       latex_placement = 'ht',
                       label = NULL,
                       caption = NULL,
                       wb = NULL,
                       sheets = NULL)
{
    if (is.null(label))
        label <- ''
    if (is.null(caption))
        caption <- ''
    if (is.null(sheets))
        sheets <- ''


    if (is.data.frame(x)){
        x <- as.list(x)
    } else if (is.list(x)){
        # do nothing
    } else {
        xname <- deparse(substitute(x))
        xname <-  gsub('^.+\\$', '', xname)
        x <- list(x)
        names(x) <- xname
    }

    rval <- lapply(x, desc)
    rval <- do.call(rbind, rval)

    varnames <- strtrim(paste(rownames(rval), collapse = '_'), 31)
        
    ## Workbook handling
    if (methods::is(wb, "Workbook")){
        xlsx_table(rval,
                   wb,
                   sheets,
                   label,
                   caption,
                   varnames)
    }
    
    ## rval <- desc(x)
    if (latex) {
        ## 0 for n and NA, 2 for the others
        digits <- (!(colnames(rval) %in% c('n', 'NA')))*2
        xt <- xtable::xtable(rval,
                             ## align = 'c',
                             digits = c(0, digits),
                             label = label,
                             caption = caption)
        xtable::print.xtable(xt,
                             floating = latex_floating,
                             table.placement = latex_placement
                             )
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
#' @param latex_floating use floating environment for
#' latex printing (default = TRUE)
#' @param latex_placement table placement for latex printing
#' @param label latex label
#' @param caption latex caption
#' @param wb an openxlsx Workbook; if not NULL the table will be saved
#'     in the workbook too, aside printing
#' @param sheets optional sheet names (same length as the number of
#'     tables)
#' @export
biv_quant <- function(x, y,
                      na.rm = FALSE,
                      add_all = TRUE,
                      latex = FALSE,
                      latex_floating = TRUE,
                      latex_placement = 'ht',
                      label = NULL,
                      caption = NULL,
                      wb = NULL,
                      sheets = NULL)
{
    xname <- gsub('^.+\\$', '', deparse(substitute(x)))
    yname <- gsub('^.+\\$', '', deparse(substitute(y)))
    varnames <- strtrim(paste(xname, yname, sep = '_'), 31)

    if (is.null(label))
        label <- ''
    if (is.null(caption))
        caption <- ''
    if (is.null(sheets))
        sheets <- ''

    y <- factor(y, exclude = if (na.rm) NA else NULL)
    spl <- split(x, list(y))
    if (add_all) {
        all_ <- split(x, list('All'))
        spl <- c(spl, all_)
    }
    rval <- lapply(spl, desc
                 #, exclude = 'NA'
                   )
    rval <- do.call(rbind, rval)

    ## Workbook handling
    if (methods::is(wb, "Workbook")){
        xlsx_table(rval,
                   wb,
                   sheets,
                   label,
                   caption,
                   varnames)
    }

    if (latex) {
        ## 0 for n and NA, 2 for the others
        digits <- (!(colnames(rval) %in% c('n', 'NA')))*2
        xtab <- xtable::xtable(rval,
                               ## align = 'c',
                               digits = c(0, digits),
                               caption = caption,
                               label = label)
        xtable::print.xtable(xtab,
                             floating = latex_floating,
                             table.placement = latex_placement)
        invisible(rval)
    } else {
        return(rval)
    }
}

#' Percentages table for multiple categorical responses
#' 
#' @param x a (chunk of) data.frame encoding multiple responses (aka
#'     all composed of 0-1 variables)
#' @param latex output the table using \code{xtable::xtable}
#' @param label latex label
#' @param caption latex caption
#' @param wb an openxlsx Workbook; if not NULL the table will be saved
#'     in the workbook too, aside printing
#' @param sheets optional sheet names (same length as the number of
#'     tables)
#' @export
univ_mr <- function(x, latex = FALSE, label = NULL, caption = NULL,
                    wb = NULL, sheets = NULL)
{
    if (!is.data.frame(x))
        stop('x must be a data.frame')
    if (all(x  %in% c(0, 1, NA)))
        stop('x must only include 0, 1, NA')

    if (is.null(label))
        label <- ''
    if (is.null(caption))
        caption <- ''
    if (is.null(sheets))
        sheets <- ''

    not_NA <- unlist(lapply(x, function(x) sum(!is.na(x))))
    s <- colSums(x, na.rm = TRUE)
    rval <- cbind('n' = s, '%' = round((s/not_NA)*100, 2))
    rownames(rval) <- paste0(rownames(rval), ' (N = ', not_NA, ')')
    rownames(rval) <- gsub('_', ' ', rownames(rval))
    rval <- rval[order(- rval[,1]), ]

    varnames <- strtrim(paste(rownames(rval), collapse = '_'), 31)
        
    ## Workbook handling
    if (methods::is(wb, "Workbook")){
        xlsx_table(rval,
                   wb,
                   sheets,
                   label,
                   caption,
                   varnames)
    }

    if (latex) {
        xtab <- xtable::xtable(rval,
                               ## align = 'cc',
                               digits = c(0, 0, 2),
                               caption = caption,
                               label = label)
        xtable::print.xtable(xtab)
        invisible(rval)
    } else {
        return(rval)
    }
}
