## -----------------------------------------------------------------------
## HELPER FUNCTIONS
## -----------------------------------------------------------------------

xlsx_caption <- function(x){
    ## latex report caption to excel table one
    ## remove test
    x <- gsub('\\s{0,}\\(.+\\){1}$', '', x, perl = TRUE)
    ## remove $$ for LaTeX formulas
    x <- gsub('\\$', '', x)
    lbmisc::rm_spaces(x)
}

##
## helper for exporting tables ... very raw for now
##
xlsx_table <- function(tab, test_df, wb, sheet, caption, varname, rowNames = TRUE)
{

    if (sheet == '')
        sheet <- strtrim(varname, 31)
    
    ## find a proper sheet name (C style)
    original_sheet <- sheet
    attempts <- 1L
    sheet <- make_sheetname(original_sheet, attempts)
    while(sheet %in% openxlsx::sheets(wb)){
        sheet <- make_sheetname(original_sheet, attempts)
        attempts <- attempts + 1L
    }
    
    ## workbook setup
    openxlsx::addWorksheet(wb = wb, sheetName = sheet)
    spacing <- 1
    
    ## caption
    if (caption != ''){
        caption <- xlsx_caption(caption)
        caption <- data.frame('caption' = caption)
        openxlsx::writeData(wb = wb, sheet = sheet, x = caption,
                            colNames = FALSE)
        this_table_rows <- 1
        next_row <- this_table_rows + spacing + 1
    } else
        next_row <- 1
    
    ## data
    openxlsx::writeData(wb = wb, sheet = sheet, x = tab, rowNames = rowNames,
                        startRow = next_row)
    this_table_rows <- nrow(tab) + 1 ## +1 for table header
    next_row <- next_row + this_table_rows + spacing
        
    ## test
    if (is.data.frame(test_df))
        openxlsx::writeData(wb = wb, sheet = sheet, x = test_df,
                            rowNames = FALSE, startRow = next_row)

}

## helper of xlsx_table: find a sensible sheetname
make_sheetname <- function(sheet, attempt){
    if (attempt == 1L)
        strtrim(sheet, 31)
    else {
        needed_digits <- floor(log10(attempt)) + 1L
        paste0(strtrim(sheet, 31 - needed_digits), as.character(attempt))
    }
}

## helper function to extract comment from a dataframe, list or vector
## and replace NULL with ''; returned vector has the same length as
## given data.frame
get_comments <- function(x) {

    extractor <- function(y){
        cy <- comment(y)
        if (is.null(cy)) '' else cy
    }
    
    if (is.list(x)) unlist(lapply(x, extractor)) else extractor(x)
}


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
#'     multiple response question
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

    ## check variables to be ignored
    all_na        <- unlist(lapply(x, function(y) all(is.na(y))))
    ignored_type  <- ! (zero_ones | numerics | factors)
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
    
    ## sono arrivato qui
    common_params <- list(wb = wb, latex = latex)
    univ_perc_params  <- c(common_params, univ_perc_params)
    univ_quant_params <- c(common_params, univ_quant_params)
    univ_quali_params <- c(common_params, univ_quali_params)
    
    if (style == 'raw'){
        raw_worker(x = x,
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
            params <- c(list(x = data, wb = wb, latex = latex),
                        univ_perc_params)
            do.call(univ_perc, params)
        }
        if (any(numerics)){
            data <- x[, numerics, drop = FALSE]
            params <- c(list(x = data, wb = wb, latex = latex),
                        univ_quant_params)
            do.call(univ_quant, params)
        }
        if (any(factors)){
            data <- x[, factors, drop = FALSE]
            params <- c(list(x = data, wb = wb, latex = latex),
                        univ_quali_params)
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
    
    ## varnames <- strtrim(paste(rownames(rval), collapse = '_'), 31)
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
    digits <- (!(colnames(y) %in% c('n', 'NA')))*2

    if (label == '') label <- sprintf('tab:%s', varname)
    if (caption %in% '') caption <- gsub('_', ' ', varname)
    
    ## print
    xt <- xtable::xtable(tmp,
                         ## align = 'c',
                         digits = c(0, digits),
                         label = label,
                         caption = caption)
    xtable::print.xtable(xt, table.placement = latex_placement)

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
                         table.placement = latex_placement)
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

    if (is.null(label))
        label <- ''
    if (is.null(caption))
        caption <- ''
    if (is.null(sheets))
        sheets <- ''

    not_NA <- unlist(lapply(x, function(x) sum(!is.na(x))))
    s <- colSums(x, na.rm = TRUE)
    rval <- data.frame(cbind('n' = s, '%' = round((s/not_NA)*100, 2)))
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
        
    
    ## varnames <- strtrim(paste(rownames(rval), collapse = '_'), 31)
    varnames <- paste(rownames(rval), collapse = '_')
        
    ## Workbook handling
    if (methods::is(wb, "Workbook")){
        xlsx_table(rval,
                   NULL, # table
                   wb,
                   sheets,
                   ## label,
                   caption,
                   varnames)
    }

    if (latex) {
        ## make rownames not to long for printing
        tmp <- rval
        rownames(tmp) <- strtrim(rownames(tmp), width = 32)
        xtab <- xtable::xtable(tmp,
                               digits = c(0, 0, 2),
                               caption = caption,
                               label = label)
        xtable::print.xtable(xtab, table.placement = latex_placement)
        invisible(rval)
    } else {
        return(rval)
    }
}


## -----------------------------------------------------------------------
## BIVARIATE STUFF
## -----------------------------------------------------------------------

#' Dispatcher for (qualitative/quantitative) bivariate tables and
#' statistics.
#'
#' @param x a \code{data.frame}
#' @param group a data.frame of 1 column
#' @param wb a WorkBook
#' @param latex use latex for printing (default = TRUE)
#' @param exclude character vector of data.frame name to be ignored
#' @param style if 'raw' display variables in the order given, if
#'     'type' display variable by type
#' @param analysis_name label prefix
#' @param date_preproc function to preprocess Date columns. By default
#'     make them Year-month factors; currently only factor, numerics
#'     and 0-1 variable are not ignored, therefore date_preproc should
#'     be a coercer to those types
#' @param mr_prefixes variable name prefixes (character vector); each
#'     of these identify a set of variable who belong to the same
#'     multiple response question (if done with smarty_mr_splitter
#'     this should end with "_")
#' @param biv_perc_params other options (named list) for biv_perc
#' @param biv_quant_params other options (named list) for biv_quant
#' @param biv_quali_params other options (named list) for biv_quali
#' @export
bivariate_tables <- function(x, group,
                             wb = NULL,
                             latex = TRUE,
                             exclude = NULL,
                             style = c('raw', 'type'),
                             analysis_name = '',
                             date_preproc = function(d) factor(format(d, '%Y-%m')),
                             mr_prefixes = NULL,
                             biv_perc_params = list(),
                             biv_quant_params = list(test = 'none'),
                             biv_quali_params = list(test = 'auto'))
{
    stopifnot(is.data.frame(x) && all(dim(x) > 0L))
    stopifnot(is.data.frame(group) && ncol(group) == 1L)
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
    x_names     <- names(x)
    zero_ones_c <- x_names[zero_ones]
    numerics_c  <- x_names[numerics]
    factors_c   <- x_names[factors]

    if (!is.null(mr_prefixes)){
        mr_patterns <- paste0("^", mr_prefixes)
        mr_vars <- lapply(mr_patterns,
                          function(y) grep(y, x_names, value = TRUE))
        names(mr_vars) <- mr_prefixes
    }

    common_params <- list(wb = wb, latex = latex, y = group[, 1], yname = names(group))
    biv_perc_params  <- c(common_params, biv_perc_params)
    biv_quant_params <- c(common_params, biv_quant_params)
    biv_quali_params <- c(common_params, biv_quali_params)

    if (style == 'raw') {
        raw_worker(x = x,
                   zero_ones_c = zero_ones_c,
                   numerics_c = numerics_c, 
                   factors_c = factors_c,  
                   mr_prefixes = mr_prefixes,
                   mr_vars = mr_vars,
                   perc_f  = biv_mr,
                   quant_f = biv_quant,
                   quali_f = biv_quali,
                   perc_f_params  = biv_perc_params, 
                   quant_f_params = biv_quant_params, 
                   quali_f_params = biv_quali_params)
    } else
        stop("Not implemented yet")

    invisible(ignored)
}


raw_worker <- function(x, ## dataset
                       zero_ones_c,
                       numerics_c, 
                       factors_c,  
                       mr_prefixes,
                       mr_vars,
                       perc_f,        #eg=univ_perc 
                       quant_f,       #eg=univ_quant
                       quali_f,       #eg=univ_quali
                       perc_f_params, #eg=univ_perc_params
                       quant_f_params,#eg=univ_quant_params
                       quali_f_params #eg=univ_quali_params
                       )
{
    x_names <- names(x)
    for (varname in x_names){
        if(varname %in% names(x)){
            ## it may not be the case since x is modifyied by mr stuff
            data <- x[, varname, drop = FALSE]
            if (varname %in% zero_ones_c){
                if (!is.null(mr_prefixes)){
                    ## check if it's a mr,
                    ## consider the full dataset of related mr
                    ## remove the other mr for the next cycle
                    finder <- function(x) varname %in% x
                    involved_mr <- Filter(finder, mr_vars)
                    involved_prefix <- names(involved_mr)
                    involved_mr <- unlist(involved_mr)
                    ## adjust various data.frame
                    if (length(involved_mr) > 0L){
                        ## update data to include all mr
                        data <- x[, involved_mr, drop = FALSE]
                        ## set comment from varname
                        rm_ptrn <- sprintf('^(%s)(.+)', involved_prefix)
                        comments <- gsub(rm_ptrn, '\\2', involved_mr)
                        clean <- function(x){
                            x <- gsub('[\\._]', ' ', x)
                            x <- rm_spaces(x)
                        }
                        set_com <- function(dat, com) {
                            ## set and return
                            com <- clean(com)
                            comment(dat) <- com
                            dat
                        }
                        data[, involved_mr] <- Map(set_com,
                                                   data[, involved_mr, drop = FALSE],
                                                   comments)
                        ## Setup caption from the original multiple responses variable
                        possible_caption_comment_variable <- gsub("_$", "", involved_prefix)
                        cm <- comment(x[, possible_caption_comment_variable])
                        perc_f_params$caption <- if (is.null(cm)) clean(involved_prefix) else cm
                        ## perc_f_params$caption <- "clean(involvedprefix)"
                        
                        ## rm these variables from the cycle
                        x <- x[, names(x) %without% involved_mr, drop = FALSE]
                    }
                }
                ## TODOHERE
                params <- c(list(x = as.data.frame(data), mr_prefixes = involved_prefix),
                            perc_f_params)
                do.call(perc_f, params)
            } else if (varname %in% numerics_c){
                params <- c(list(x = data), quant_f_params)
                do.call(quant_f, params)
            } else if (varname %in% factors_c){
                params <- c(list(x = data), quali_f_params)
                do.call(quali_f, params)
            }
        }
    }
}




#' Bivariate table for categorical data.
#' 
#' @param x row variable: a discrete quantitative variable, a
#'     character or a factor
#' @param y column variable: a discrete quantitative variable, a
#'     character or a factor
#' @param xname a string used to identify x variable (used for latex captions and excel sheets)
#' @param yname a string used to identify x variable (used for latex captions and excel sheets)
#' @param totals print totals?
#' @param tot_row_label label for rows total
#' @param tot_col_label label for columns total
#' @param useNA print NA?
#' @param perc print column percentage (default = TRUE)
#' @param exclude_NA_perc remove NA from percentages (default = TRUE)
#' @param NA_string character used for NA's columns title
#' @param freq_sorting freq based sorting: can be \code{NA} (no freq
#'     based sorting) "\code{desc}" (descending) or "\code{asc}"
#'     (ascending). Sorting based on row totals.
#' @param test one of 'auto', 'none', 'fisher', 'chi': if \code{auto}
#'     (default) fisher or chi square test will be performed (using
#'     \code{fisher_needed} to decide which one)
#' @param test_params a list of parameters to be passed to the test
#'     performing function
#' @param latex output the table using \code{xtable::xtable}
#' @param latex_placement table placement for latex printing
#' @param label latex label
#' @param caption latex caption
#' @param wb an openxlsx Workbook; if not NULL the table will be saved
#'     in the workbook too, aside printing
#' @param sheets optional sheet names (same length as the number of
#'     tables)
#' @examples
#' x <- airquality$Ozone < 80
#' y <- airquality$Month 
#' biv_quali(x = x, y = y, latex = FALSE, test = 'none')
#' biv_quali(x = x, y = y, latex = FALSE, test = 'none', freq_sort = 'asc')
#' biv_quali(x = x, y = y, latex = FALSE, test = 'none', freq_sort = 'desc')
#' biv_quali(x = x, y = y, latex = FALSE, test = 'none', perc = FALSE )
#' @export
biv_quali <- function(x = NULL,
                      y = NULL,
                      xname = NULL,
                      yname = NULL,
                      totals = TRUE,
                      tot_row_label = 'Tot',
                      tot_col_label = 'Tot',
                      useNA = 'ifany',
                      perc = TRUE,
                      exclude_NA_perc = TRUE,
                      NA_string = 'NA',
                      ## round_digits = 3,
                      freq_sorting = c(NA, 'desc', 'asc'),
                      test = c('auto', 'none', 'fisher', 'chisq'),
                      test_params = list(),
                      latex = TRUE,
                      latex_placement = 'ht',
                      label = NULL,
                      caption = NULL,
                      wb = NULL,
                      sheets = NULL)
{
    test <- match.arg(test)

    ## handle 1 col data.frame
    one_col_x <- is.data.frame(x) && ncol(x) == 1L
    one_col_y <- is.data.frame(y) && ncol(y) == 1L

    if (one_col_x){
        if (is.null(xname)) xname <- names(x)
        x <- x[, 1]
    }

    if (one_col_y){
        if (is.null(yname)) yname <- names(y)
        y <- y[, 1]
    }
    
    if (is.null(xname)) xname <- gsub('^.+\\$', '', deparse(substitute(x)))[1L]## for safeness
    if (is.null(yname)) yname <- gsub('^.+\\$', '', deparse(substitute(y)))[1L]## for safeness 
    ## varnames <- strtrim(paste(yname, xname, sep = '_'), 31)
    varnames <- paste(yname, xname, sep = '_')
    
    if (is.null(label))
        label <- paste('tab', varnames, sep = ':')
    if (is.null(caption))
        caption <- if (!is.null(comment(x))) comment(x) else ''
    if (is.null(sheets))
        sheets <- ''

    freq_sorting <- match.arg(freq_sorting)
    abs_freq <- table(x, y, useNA = useNA)
    row_sums <- rowSums(abs_freq)
    test <- match.arg(test)

    if (freq_sorting %in% c('asc', 'desc')){
        there_are_NA <- is.na(rownames(abs_freq)[nrow(abs_freq)])
        row_indexes <- if (!there_are_NA) {
                           order(row_sums, decreasing = freq_sorting == 'desc')
                       } else {
                           ## remove the last row from order determination
                           c(order(row_sums[-length(row_sums)], decreasing = freq_sorting == 'desc'),
                             length(row_sums))
                       }
        
        abs_freq <- abs_freq[row_indexes, ]
    }
    
    ## column percentages 
    rel_freq <- prop.table(abs_freq, margin = 2) * 100
    ## rm row NA percentage and back percentages to 100
    if (exclude_NA_perc){
        na_row <- is.na(rownames(rel_freq))
        rel_freq[na_row, ] <- NA
        rel_freq <- apply(rel_freq, 2,
                          function(x) 100 * x/sum(x, na.rm = TRUE))
    }

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
        ## exclude NA perc
        if (exclude_NA_perc){
            col_tot_perc[na_row] <- NA
            col_tot_perc <- 100 *
                (col_tot_perc / sum(col_tot_perc, na.rm = TRUE))
        }
        rval <- cbind(rval, 'Tot' = row_sums, '%' = col_tot_perc)
        colnames(rval)[ncol(rval) - 1] <- tot_col_label

        ## columns totals
        col_sums <- colSums(rval)
        ## put NA to percentage sums (100%) which is obvious: odd columns
        col_sums[seq(from = 2, to = length(col_sums), by = 2)] <- NA
        rval <- rbind(rval, 'Tot' = col_sums)
        rownames(rval)[nrow(rval)] <- tot_row_label
    }

    ## print percentages?
    if (! perc){
        perc_cols <- seq(from = 2, to = ncol(rval), by = 2)
        rval <- rval[, -perc_cols]
    }
    
    ## NA label handling
    rownames(rval)[is.na(rownames(rval))] <- NA_string 
    colnames(rval)[is.na(colnames(rval))] <- NA_string 

    ## test handling
    if (test %in% c('auto', 'fisher', 'chisq')){
        ## handle auto
        if ('auto' == test)
            test <- if (fisher_needed(x = x, y = y)) 'fisher' else 'chisq'
      
        ## handle fisher or chisq
        if ('fisher' == test){
            test_fun <- stats::fisher.test
            test_name <- 'Fisher'
        } else if ('chisq' == test) {
            test_fun <- stats::chisq.test
            test_name <- 'Chi square'
        } else
            stop('At this point test should be fisher or chisq ...')

        ## add parameters
        test <- if (length(test_params) > 0L)
                    do.call(test_fun, c(list(x = x, y = y), test_params))
                else
                    do.call(test_fun, list(x = x, y = y))
        
        ## evaluate test and extract useful things
        test_p <- lbmisc::pretty_pval(test$p.value)
        test_string <- sprintf('%s test p-value: %s', test_name, test_p)
        test_df <- data.frame('p-value' = test_p, 'Test' = test_name)
        caption <- paste0(caption, ' (', test_string, ')')
    } else {
        test_df <- NULL
    }
    
    ## Workbook handling
    if (methods::is(wb, "Workbook")){
        xlsx_table(rval,
                   test_df,
                   wb,
                   sheets,
                   ## label,
                   caption,
                   varnames)
    }

    ## output
    if (latex){
        if (perc) {
            ## alternate 0,2 for number of columns/2
            digits <- rep(c(0,2), ncol(rval)/2)
        } else {
            ## no percentages, all integers
            digits <- rep(0L, ncol(rval))
        }
        xt <- xtable::xtable(rval,
                             ## align = 'todo',
                             digits = c(0, digits),
                             label = label, caption = caption)
        xtable::print.xtable(xt,
                             table.placement = latex_placement)
        invisible(rval)
    } else {
        ## normal printing
        message(caption)
        print(rval)
        invisible(rval)
    }

}


#' Bivariate table for a main quantitative vector 
#' 
#' @param x a quantitative variable
#' @param y a discrete quantitative variable, a character or a factor
#' @param xname a string used to identify x variable (used for latex captions and excel sheets)
#' @param yname a string used to identify x variable (used for latex captions and excel sheets)
#' @param na.rm exclude missing value group for y
#' @param add_all add "All" row
#' @param test one of \code{'none'}, \code{'anova'}, or \code{'kruskal.test'}
#' @param latex output the table using \code{xtable::xtable}
#' @param latex_placement table placement for latex printing
#' @param label latex label
#' @param caption latex caption
#' @param wb an openxlsx Workbook; if not NULL the table will be saved
#'     in the workbook too, aside printing
#' @param sheets optional sheet names (same length as the number of
#'     tables)
#' @export
biv_quant <- function(x, y,
                      xname = NULL,
                      yname = NULL,
                      na.rm = FALSE,
                      add_all = TRUE,
                      test = c('none', 'anova', 'kruskal.test'),
                      latex = TRUE,
                      latex_placement = 'ht',
                      label = NULL,
                      caption = NULL,
                      wb = NULL,
                      sheets = NULL)
{
    test <- match.arg(test)

    ## handle 1 col data.frame
    one_col_x <- is.data.frame(x) && ncol(x) == 1L
    one_col_y <- is.data.frame(y) && ncol(y) == 1L

    if (one_col_x){
        if (is.null(xname)) xname <- names(x)
        x <- x[, 1]
    }

    if (one_col_y){
        if (is.null(yname)) yname <- names(y)
        y <- y[, 1]
    }
    
    if (is.null(xname)) xname <- gsub('^.+\\$', '', deparse(substitute(x)))[1L]## for safeness
    if (is.null(yname)) yname <- gsub('^.+\\$', '', deparse(substitute(y)))[1L]## for safeness
    ## varnames <- strtrim(paste(yname, xname, sep = '_'), 31)
    varnames <- paste(yname, xname, sep = '_')
    
    if (is.null(label))
        label <- paste('tab', varnames, sep = ':')
    if (is.null(caption))
        caption <- if (!is.null(comment(x))) comment(x) else ''
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

    ## test handling
    if (test %in% c('anova', 'kruskal.test')){
        db <- data.frame(x = x, y = factor(y))
        if ('anova' == test){
            test <- stats::oneway.test(x ~ y, data = db, var.equal = FALSE)
            test_name <- 'Anova'
            test_p <- lbmisc::pretty_pval(test$p.value)
        } else if ('kruskal.test' == test){
            test <- stats::kruskal.test(x ~ y, data = db)
            test_name <- 'Kruskal-Wallis'
            test_p <- lbmisc::pretty_pval(test$p.value)
        } else
            stop('Something strange happened in testing')
        
        test_string <- sprintf('%s test p-value: %s', test_name, test_p)
        test_df <- data.frame('p-value' = test_p, 'Test' = test_name)
        caption <- paste0(caption, ' (', test_string, ')')
    } else {
        test_df <- NULL
    }
    
    ## Workbook handling
    if (methods::is(wb, "Workbook")){
        xlsx_table(rval,
                   test_df, # test
                   wb,
                   sheets,
                   ## label,
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
                             table.placement = latex_placement)
        invisible(rval)
    } else {
        message(caption)
        print(rval)
        invisible(rval)
    }
}




#' Percentages table for 0-1 variables
#'
#' The function make descriptive statistics (n, percentages) for 0-1
#' (or factor) x variables stratifying for the y variable
#' 
#' @param x a single quantitative variable or a data.frame of
#'     qualitative variable
#' @param y a single qualitative variable or a data.frame of
#'     qualitative variable
#' @param collapse_binary_y regarding y, keep in row only the 1 in 0-1
#'     variable or the last level for factors
#' @param only_non_zero_perc print only non zero percentages rows
#' @param style print different groups in different columns
#' @param sort_freq decreasing frequencies ordered table
#' @param latex output the table using \code{xtable::xtable}
#' @param latex_placement table placement for latex printing
#' @param label latex label
#' @param caption latex caption
#' @param wb an openxlsx Workbook; if not NULL the table will be saved
#'     in the workbook too, aside printing
#' @param sheets optional sheet names
#' @export
biv_perc <- function(x = NULL,
                     y = NULL,
                     xname = NULL,
                     yname = NULL,
                     collapse_binary_y = FALSE,
                     only_non_zero_perc = FALSE,
                     style = c('long', 'wide'),
                     sort_freq = FALSE,
                     latex = TRUE,
                     latex_placement = 'ht',
                     label = NULL,
                     caption = NULL,
                     wb = NULL,
                     sheets = NULL)
{

    style <- match.arg(style)
    if (is.null(xname)) xname <- gsub('^.+\\$', '', deparse(substitute(x)))[1L]## for safeness
    if (is.null(yname)) yname <- gsub('^.+\\$', '', deparse(substitute(y)))[1L]## for safeness
    ## varnames <- strtrim(paste(xname, yname, sep = '_'), 31)
    varnames <- paste(xname, yname, sep = '_')
    
    if (is.null(label))
        label <- paste('tab', varnames, sep = ':')
    if (is.null(caption))
        caption <- if (!is.null(comment(x))) comment(x) else ''
    if (is.null(sheets))
        sheets <- ''
    
    ## browser()
    ## comunque nel seguito ci si aspetta che y sia un data.frame
    ## per splitting e simili, quindi normalizziamo
    x <- as.data.frame(x)
    ## y deve essere a due livelli e viene calcolata la percentuale del 
    ## livello piu alto, alternativamente utilizzare dummify
    defactorize <- function(f){
        if (!is.factor(f)) f
        else if (nlevels(f) == 2L) as.integer(f) - 1
        else lbmisc::dummify(if (anyNA(f)) addNA(f) else f)
    }
    x <- lapply(x, defactorize)
    x <- if (length(x) > 1) as.data.frame(do.call(cbind, x))
         else data.frame(x[[1]])
    ## normalizzo gli x rappresentati da un data.frame di una
    ## variabile ad una variabile
    if (!is.null(dim(y)) && ncol(y) == 1L) y <- y[, 1]
    ## per vari motivi assumiamo che nel seguito y
    ## sia un factor
    dimy <- dim(y)
    y <- if (is.null(dimy)) as.factor(y)
         else {
             y <- lapply(y, factor)
             if (length(y) > 1) do.call(cbind.data.frame, y)
             else data.frame(y[[1]])
         }
    ## il contatore conta degli 0 e degli 1
    the_counter <- function(x){
        stopifnot(all(x %in% c(0, 1, NA)))
        N <- sum(!is.na(x))
        n <- sum(x, na.rm = TRUE)
        perc <- (n/N)*100
        data.frame(N, n,  perc)
    }
    has_two_levels <- function(x) {
        x <- x %without% NA
        x <- droplevels(x)
        all(x %in% c(0,1)) || nlevels(x) == 2L
    }
    ## browser()
    ## l'analisi varia a seconda che x sia una variabile o un dataframe
    if(is.null(dimy)){ # y è variabile
        ## se i livelli di y sono solo 2 (e si è specificato
        ## collapse_binary_y) non splittare ma considera l'1 o 
        ## il livello più alto, altrimenti splitta
        analysis_dbs <- if (has_two_levels(y) && collapse_binary_y){
                            ## se solo due livelli
                            selector <- if (is.numeric(y)) 1 else levels(y)[2]
                            list(x[y %in% selector, , drop = FALSE])
                       } else ## se più di due livelli splitta
                           split(x, f = y)
        ## Applicare la funzione di conta per ogni gruppo
        rval <-  lapply(analysis_dbs,
                        function(g) # per ogni gruppo (ossia modalita)
                            do.call(rbind, 
                                    ## per ogni variabile entro ogni
                                    ## modalita fai la conta
                                    lapply(g, function (v) the_counter(v))))
    } else if (dimy[2] > 1){ # y è dataframe
        ## a turno per ogni colonna di y splittare la x in base alla y
        ## in questo modo group è una variabile
        rval <- lapply(y, function(yvar){
            analysis_dbs <- 
                if (has_two_levels(yvar) && collapse_binary_y){
                    ## fai il subset del gruppo di interesse gestendo e
                    selector <- if (is.numeric(yvar)) 1L else levels(yvar)[2]
                    ## dato che x dovrebbe essere sempre un data.frame...
                    ## if (is.data.frame(x))
                    list(x[yvar %in% selector, , drop = FALSE])
                    ## else list(data.frame(y[xvar %in% selector]))
                } else { ## se più di due livelli splitta
                    split(x, f = yvar)
                }
            ## per ogni dataset così splittato
            do.call(rbind, lapply(analysis_dbs, function(single_dataset){
                ## a turno per ogni variabile fai il conteggio
                tmp <- lapply(single_dataset, the_counter)
                ## e collassa con rbind per mantenere gestibilità della cosa
                do.call(rbind, tmp)
            }))
        })
        ## da qui ne esce una lista con un elemento per ogni variabile x, che
        ## andiamo a collassare per avere un unico data.frame in seguito
    } else stop('x deve essere una variabile o un dataframe')

    ## tengo solo le righe dove si ha almeno una corrispondenza per pulire
    ## l'output e ordino per frequenze decrescenti
    if (only_non_zero_perc)
        rval <- lapply(rval, function(r) r[r$n > 0, , drop = FALSE])
    if (sort_freq)
        rval <- lapply(rval, function(r) r[with(r, order(N, n, decreasing = TRUE)), , drop = FALSE])
    ## e ora metto tutto assieme
    if (style == 'long') {
        rval <- do.call(rbind, rval)
        ## rowname as first column
        rval <- cbind(data.frame(gsub("\\.", " - ", rownames(rval))), rval)
        rownames(rval) <- NULL
        names(rval)[1] <- ""
    } else {
        ## cbind, filling for na
        max_nrow <- max(unlist(lapply(rval, nrow)))
        ## fill for na
        rval <- lapply(rval, function(x){
            original_rownames <- rownames(x)
            ## add missing
            if (nrow(x) < max_nrow) x[max_nrow, ] <- NA
            ## now handle rownames making them the first column
            x <- cbind(data.frame("rn" = rownames(x)), x)
            ## clean some autogenerated rownames (numbers)
            x[x$rn %nin% original_rownames , "rn"] <- NA
            rownames(x) <- NULL
            x
        })
        rval <- do.call(cbind, rval)
        names(rval) <- gsub("\\.", " - ", names(rval))
        ## ora togli il nome della prima colonna ogni 4 (blocco unico) che è di scarto
        names(rval)[seq(1, ncol(rval), by = 4)] <- ""
    }
    
    ## Workbook handling
    if (methods::is(wb, "Workbook")){
        xlsx_table(rval,
                   test_df = NULL,
                   wb,
                   sheets,
                   ## label,
                   caption,
                   varnames,
                   rowNames = FALSE)
    }

    ## output
    if (latex){
        xt <- xtable::xtable(rval,
                             ## align = 'todo',
                             ## digits = c(0, digits),
                             label = label,
                             caption = caption)
        xtable::print.xtable(xt,
                             include.rownames = FALSE,
                             table.placement = latex_placement)
        invisible(rval)
    } else {
        ## normal printing
        message(caption)
        print(rval)
        invisible(rval)
    }

}


#' A wrapper around biv_perc lightly optimized for multiple response
#'
#' @param mr_prefixes the single multiple response
#' @param ... other parameters passed to biv_perc with exception of
#' sort_freq (TRUE) and only_non_zero_perc (TRUE)
#' @export
biv_mr <- function(mr_prefixes = '', caption, ...){
    ## clean prefix from the data.frame
    dots <- list(...)
    x <- dots$x
    y <- dots$y
    ## ## per la caption cerca il commento della variabile x con nome pari al prefisso (senza _ finale)
    ## possible_caption_comment_variable <- gsub("_$", "", mr_prefixes)
    ## cm <- comment(x[, possible_caption_comment_variable])
    ## caption <- if (is.null(cm)) '' else cm
    ## modifica i nomi per i conti
    if (!is.null(dim(x))) names(x) <- clean_mr_names(names(x), mr_prefixes[1])
    if (!is.null(dim(y))) names(y) <- clean_mr_names(names(y), mr_prefixes[1])
    dots$y <- y
    dots$x <- x
    params <- list(sort_freq = TRUE,
                   only_non_zero_perc = TRUE,
                   caption = caption,
                   label = paste0('tab:', mr_prefixes),
                   sheets = mr_prefixes,
                   style = 'wide')
    do.call(biv_perc, c(params, dots))
}

## Funzione di supporto per pulire le mr e lasciare solo le modalita
clean_mr_names <- function(z, mr_prefixes) {
    ## remove mr prefixes
    z <- gsub(mr_prefixes, '', z)
    ## replace underscore with spaces
    z <- gsub("_", " ", z)
    ## remove duplicate spaces or trailing/ending spaces
    lbmisc::rm_spaces(z)
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
                  margin = NULL, quiet = TRUE)
{
    tab <- base::table(useNA = useNA, ...)
    if (is.null(margin))
        margin <- seq_along(dim(tab))
    addmargins(tab, FUN = f, quiet = quiet, margin = margin)
}


## funzione per il preprocessng delle date per bivariate_tables
## e univariate_tables
preprocess_dates <- function(x, f){
    comments <- lapply(x, comment)
    x <- lapply(x, f)
    Map(function(d, c) {comment(d) <- c; d}, as.list(x), comments)
}
