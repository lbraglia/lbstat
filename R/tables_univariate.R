## -----------------------------------------------------------------------
## UNIVARIATE STUFF
## -----------------------------------------------------------------------

#' Dispatcher for (qualitative/quantitative) univariate tables and
#' statistics.
#'
#' @param x a \code{data.frame}
#' @param wb a WorkBook
#' @param latex use latex for printing (default = TRUE)
#' @param style if 'raw' display variables in the order given, if
#'     'type' display variable by type
#' @param exclude character vector of data.frame name to be ignored
#' @param date_preproc function to preprocess Date columns. By default
#'     make them Year-month factors; currently only factor, numerics
#'     and 0-1 variable are not ignored, therefore date_preproc should
#'     be a coercer to those types
#' @param mr_prefixes variable name prefixes (character vector); each
#'     of these identify a set of variable who belong to the same
#'     multiple response question (if done with smarty_mr_splitter
#'     this should end with "_")
#' @param univ_perc_params other options (named list) for univ_perc
#' @param univ_quant_params other options (named list) for univ_quant
#' @param univ_quali_params other options (named list) for univ_quali
#' @examples
#' 
#' wb = openxlsx::createWorkbook()
#' univariate_tables(airquality, wb = wb)
#' lbmisc::wb_to_xl(wb = wb, file = '/tmp/univariate_tables.xlsx')
#' 
#' @export
univariate_tables <-
    function(x,
             wb = NULL,
             latex = TRUE,
             exclude = NULL,
             style = c('raw', 'type'),
             date_preproc = function(d) factor(format(d, '%Y-%m')),
             mr_prefixes = NULL,
             univ_perc_params = list(),
             univ_quant_params = list(),
             univ_quali_params = list())
{
    stopifnot(is.data.frame(x) && all(dim(x) > 0L))
    style <- match.arg(style)

    ## exclude
    x <- x[, names(x) %without% exclude, drop = FALSE]
    
    ## preprocess dates keeping comments
    dates <- unlist(lapply(x, lbmisc::is.Date))
    x[, dates] <- preprocess_dates(x[, dates, drop = FALSE], date_preproc)
    
    ## logical vectors
    zero_ones <- unlist(lapply(x, lbmisc::is.percentage))
    numerics  <- unlist(lapply(x, lbmisc::is.quantitative))
    factors   <- unlist(lapply(x, lbmisc::is.qualitative))
    mr        <- names(x) %in% gsub("_$", "", mr_prefixes)

    ## check variables to be ignored
    all_na        <- unlist(lapply(x, function(y) all(is.na(y))))
    ignored_type  <- ! (zero_ones | numerics | factors | mr)
    ignored       <- ignored_type | all_na
    ignored_names <- NULL
    if (any(ignored)){
        ignored_names <- names(x)[ignored]
        message('Some variables were ignored due to missingness (or type): ',
                paste(ignored_names, collapse = ", "), '\n\n')
        x         <- x[, !ignored, drop = FALSE]
        zero_ones <- zero_ones[!ignored]
        numerics  <- numerics[!ignored]
        factors   <- factors[!ignored]
    }

    ## same pointed variables, but vector of chars names
    x_names <- names(x)
    zero_ones_c <- x_names[zero_ones]
    numerics_c  <- x_names[numerics]
    factors_c   <- x_names[factors]

    mr_vars <- NULL
    if (!is.null(mr_prefixes)){
        mr_patterns <- paste0("^", mr_prefixes)
        mr_vars <- lapply(mr_patterns,
                          function(y) grep(y, x_names, value = TRUE))
        names(mr_vars) <- mr_prefixes
    }
    
    common_params <- list(wb = wb, latex = latex)
    univ_perc_params  <- c(common_params, univ_perc_params)
    univ_quant_params <- c(common_params, univ_quant_params)
    univ_quali_params <- c(common_params, univ_quali_params)
    
    if (style == 'raw'){
        raw_dispatcher(x = x,
                       zero_ones_c = zero_ones_c,
                       numerics_c = numerics_c, 
                       factors_c = factors_c,  
                       mr_prefixes = mr_prefixes,
                       mr_vars = mr_vars,
                       perc_f = univ_mr,
                       quant_f = univ_quant,
                       quali_f = univ_quali,
                       perc_f_params  = univ_perc_params, 
                       quant_f_params = univ_quant_params, 
                       quali_f_params = univ_quali_params)
                   
    } else if (style == 'type') {
        if (any(zero_ones)) {
            data <- x[, zero_ones, drop = FALSE]
            params <- c(list(x = data), univ_perc_params)
            do.call(univ_perc, params)
        }
        if (any(numerics)){
            data <- x[, numerics, drop = FALSE]
            params <- c(list(x = data), univ_quant_params)
            do.call(univ_quant, params)
        }
        if (any(factors)){
            data <- x[, factors, drop = FALSE]
            params <- c(list(x = data), univ_quali_params)
            do.call(univ_quali, params)
        }                                   
    }
    
    invisible(ignored_names)
}



#' Univariate table for quantitative data.
#' 
#' @param x a quantitative variable, a data.frame or a list
#' @param latex output the table using \code{xtable::xtable}
#' @param latex_placement table placement for latex printing
#' @param label latex label
#' @param caption latex caption
#' @param use_comments use comments for row (variable) names, if available
#' @param wb an openxlsx Workbook; if not NULL the table will be saved
#'     in the workbook too, aside printing
#' @param sheets optional sheet names (same length as the number of tables)
#' @examples
#' 
#' wb = openxlsx::createWorkbook()
#' univ_quant(x = airquality$Ozone, sheet = 'ozone', wb = wb)
#' univ_quant(x = airquality[, c('Ozone')], wb = wb)
#' univ_quant(x = airquality[, c('Ozone', 'Temp')], wb = wb)
#' univ_quant(list('a' = 1:10, 'b' = 2:20), wb = wb)
#' lbmisc::wb_to_xl(wb = wb, file = '/tmp/univ_quant.xlsx')
#' 
#' @export
univ_quant <- function(x,
                       latex = TRUE,
                       latex_placement = 'ht',
                       label = NULL,
                       caption = NULL,
                       use_comments = TRUE,
                       wb = NULL,
                       sheets = NULL)
{
    if (is.null(label))
        label <- ''
    if (is.null(caption))
        caption <- ''
    if (is.null(sheets))
        sheets <- ''

    comments <- get_comments(x)

    ## he's making a list ...
    if (is.data.frame(x)){
        x <- as.list(x)
    } else if (is.list(x)){
        ## do nothing
    } else {
        xname <- deparse(substitute(x))
        xname <-  gsub('^.+\\$', '', xname)
        x <- list(x)
        names(x) <- xname
    }

    ## data frame of results
    rval <- do.call(rbind, lapply(x, desc))

    if (use_comments){
        usable <- comments %nin% ''
        if (any(usable)){
            ## if only 1 usable, use it as caption (if it's currently '')
            ## otherwise use them as comments
            if (sum(usable) == 1L){
                if (caption == '') caption <- comments[usable]
            } else {
                rownames(rval)[usable] <- comments[usable]
            }
        }
    }
    
    varnames <- paste(rownames(rval), collapse = '_')
        
    ## excel exporting
    if (methods::is(wb, "Workbook")){
        xlsx_table(rval,
                   NULL, # test
                   wb,
                   sheets,
                   ## label,
                   caption,
                   varnames)
    }
    
    ## latex printing
    if (latex) {
        univ_quant_latex_printer(rval,
                                 label,
                                 caption,
                                 varnames,
                                 latex_placement)
        invisible(rval)
    } else {
        return(rval)
    }
}

univ_quant_latex_printer <- function(y,
                                     label,
                                     caption,
                                     varname,
                                     latex_placement)
{

    ## make rownames not to long for printing
    tmp <- y
    rownames(tmp) <- strtrim(rownames(tmp), width = 32)

    ## 0 for n and NA, 2 for the others
    digits <- (!(colnames(y) %in% c('n', 'NA', 'Avail')))*2

    if (label == '') label <- sprintf('tab:%s', varname)
    if (caption %in% '') caption <- gsub('_', ' ', varname)
    
    ## print
    xt <- xtable::xtable(tmp,
                         ## align = 'c',
                         digits = c(0, digits),
                         label = label,
                         caption = caption)
    xtable::print.xtable(xt,
                         table.placement = latex_placement,
                         caption.placement = "top")

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
#' @param latex_placement table placement for latex printing
#' @param label latex label
#' @param caption latex caption
#' @param use_comments use variable comments for caption if available
#'     (and none specified as comment=)
#' @param wb an openxlsx Workbook; if not NULL the table will be saved
#'     in the workbook too, aside printing
#' @param sheets optional sheet names (same length as the number of tables)
#' @examples
#'
#' wb = openxlsx::createWorkbook()
#' univ_quali(x = airquality$Month, wb = wb)
#' univ_quali(x = airquality[, c('Month')], wb = wb)
#' univ_quali(x = airquality[, c('Month', 'Day')], wb = wb)
#' univ_quali(x = airquality[, c('Month', 'Day')], latex = TRUE, wb = wb)
#' univ_quali(x = airquality[, c('Month', 'Day')], latex = TRUE,
#'            label = c('tab:airq_month', 'tab:airq_day'),
#'            caption = c('airquality month', 'airquality day'),
#'            wb = wb)
#' univ_quali(list('a' = rep(LETTERS[1:5],2), 'b' = rep(letters[1:5],2)),
#'            wb = wb)
#' lbmisc::wb_to_xl(wb = wb, file = '/tmp/univ_quali.xlsx')
#' 
#' @export
univ_quali <- function(x = NULL,
                       totals = TRUE,
                       useNA = 'ifany',
                       NA_string = 'NA',
                       freq_sorting = c(NA, 'desc', 'asc'),
                       latex = TRUE,
                       latex_placement = 'ht',
                       label = NULL,
                       caption = NULL,
                       use_comments = TRUE,
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

    if (use_comments){
        comments <- get_comments(x)
        usable <- comments %nin% ''
        if (any(usable)) caption[usable] <- comments[usable]
        caption <- replace(caption, is.na(caption), '')
    }
    
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
    
    ## excel exporting
    if (methods::is(wb, "Workbook")){
        mapply(xlsx_table,
               rval,
               list(NULL), # test
               list(wb),
               as.list(sheets),
               ## as.list(label),
               as.list(caption),
               as.list(names(rval)))
    }

    ## latex printing and return
    if (latex){
        mapply(univ_quali_latex_printer,
               rval,
               as.list(label),
               as.list(caption),
               as.list(names(rval)),
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
                              freq_sorting)
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

    ## rel_freq: rm NA from percentage count
    rel_freq <- prop.table(abs_freq) * 100
    rel_freq[is.na(names(abs_freq))] <- NA
    rel_freq <- (rel_freq / sum(rel_freq, na.rm = TRUE))*100

    ## cum_freq
    cum_freq <- cumsum(rel_freq)
    rval <- cbind(abs_freq, rel_freq, cum_freq)
    ## colnames(rval) <- c('Abs', 'Rel', 'Cum')
    colnames(rval) <- c('n', '%', 'cum. %')

    if(totals) {
        ## row totals (only for N)
        Sum <- c(colSums(rval)[1], rep(NA, 2))
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
                                     latex_placement)
{

    if (label == '') label <- sprintf('tab:%s', varname)
    if (caption %in% '') caption <- gsub('_', ' ', varname)
    
    xt <- xtable::xtable(y,
                         ## align = 'ccc',
                         digits = c(0, 0, 2, 2),
                         label = label,
                         caption = caption)
    xtable::print.xtable(xt,
                         table.placement = latex_placement,
                         caption.placement = "top")
}


#' Percentages table for 0-1 variables
#'
#' Previously called \code{univ_mr} (for multiple responses) the function make
#' descriptive statistics (n, percentages) for 0-1 variables
#' 
#' @param x a (chunk of) data.frame encoding multiple responses (aka
#'     all composed of 0-1 variables)
#' @param sort_freq sort by frequencies (descending)?
#' @param only_non_zero_perc print only non zero percentages rows
#' @param latex output the table using \code{xtable::xtable}
#' @param latex_placement table placement for latex printing
#' @param label latex label
#' @param caption latex caption
#' @param use_comments use comments for row (variable) names, if available
#' @param wb an openxlsx Workbook; if not NULL the table will be saved
#'     in the workbook too, aside printing
#' @param sheets optional sheet names (same length as the number of
#'     tables)
#' @examples
#' set.seed(1)
#' data <- as.data.frame(matrix(rbinom(100, 1, p = 0.5), ncol = 10))
#' univ_perc(x = data)
#' @export
univ_perc <- function(x,
                      xname = NULL,
                      sort_freq = TRUE,
                      only_non_zero_perc = TRUE,
                      latex = TRUE,
                      latex_placement = 'ht',
                      label = NULL,
                      caption = NULL,
                      use_comments = TRUE,
                      wb = NULL,
                      sheets = NULL)
{
    if (!is.data.frame(x))
        stop('x must be a data.frame')
    if (! all(unlist((lapply(x, function(x) all(x  %in% c(0, 1, NA)))))))
        stop('x must only include 0, 1, NA')

    if (is.null(xname)) xname <- gsub('^.+\\$', '', deparse(substitute(x)))[1L]## for safeness
    
    if (is.null(label))
        label <- paste('tab', xname, sep = ':')
    if (is.null(caption))
        caption <- if (!is.null(comment(x))) comment(x) else ''
    if (is.null(sheets))
        sheets <- ''

    not_NA <- unlist(lapply(x, function(x) sum(!is.na(x))))
    s <- colSums(x, na.rm = TRUE)
    rval <- data.frame(cbind(s, (s/not_NA)*100))
    names(rval) <- c('n', '%')
    ## substitute rownames if comment are available and we use them
    if (use_comments){
        comments <- get_comments(x)
        usable <- comments %nin% ''
        if (any(usable))
            rownames(rval)[usable] <- comments[usable]
    }

    if (length(unique(not_NA)) == 1L){
        ## tutte le variabili hanno la stessa numerosità (di non mancanti)
        colnames(rval)[1] <- sprintf('n (N = %d)', as.integer(not_NA[1]))
    } else {
        ## numerosità diverse da variabile a variabile
        rownames(rval) <- paste0(rownames(rval), ' (N = ', not_NA, ')')
    }

    rownames(rval) <- gsub('_', ' ', rownames(rval))
    if (sort_freq){
        rval <- rval[order(- rval[, 1]), , drop = FALSE]
    }
    if (only_non_zero_perc){
        rval <- rval[rval$n > 0, , drop = FALSE]
    }

    ## browser()
    varnames <- paste(rownames(rval), collapse = '_')
        
    ## Workbook handling
    if (methods::is(wb, "Workbook")){
        xlsx_table(tab = rval,
                   test_df = NULL, # table
                   wb = wb,
                   sheet = sheets,
                   caption = caption,
                   varname = varnames)
    }

    if (latex) {
        ## make rownames not to long for printing
        tmp <- rval
        rownames(tmp) <- strtrim(rownames(tmp), width = 32)
        xtab <- xtable::xtable(tmp,
                               digits = c(0, 0, 2),
                               caption = caption,
                               label = label)
        xtable::print.xtable(xtab,
                             table.placement = latex_placement,
                             caption.placement = "top")
        invisible(rval)
    } else {
        return(rval)
    }
}



#' A wrapper around univ_perc lightly optimized for multiple response
#'
#' @param mr_prefixes the single multiple response
#' @param ... other parameters passed to univ_perc with exception of
#' sort_freq (TRUE) and only_non_zero_perc (TRUE)
#' @export
univ_mr <- function(mr_prefixes = '', ...){
    ## clean prefix from the data.frame
    dots <- list(...)
    x <- dots$x
    ## y <- dots$y
    if (!is.null(dim(x))) names(x) <- clean_mr_names(names(x), mr_prefixes[1])
    ## if (!is.null(dim(y))) names(y) <- clean_mr_names(names(y))
    ## dots$y <- y
    dots$x <- x
    params <- list(sort_freq = TRUE,
                   only_non_zero_perc = TRUE,
                   label = paste0('tab:', mr_prefixes),
                   sheets = mr_prefixes)
    do.call(univ_perc, c(params, dots))
}

