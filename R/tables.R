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
xlsx_table <- function(tab, test_df, wb, sheet, caption, varname)
{

    if (sheet == '')
        sheet <- varname

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
    openxlsx::writeData(wb = wb, sheet = sheet, x = tab, rowNames = TRUE,
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
#' @param style if 'raw' display variables in the order given, if 'type'
#'              display variable by type
#' @param mr_prefixes variable name prefixes (character vector); each of these
#'     identify a set of variable who belong to the same multiple response
#'     question
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
univariate_tables <- function(x, wb = NULL, latex = TRUE,
                              style = c('raw', 'type'),
                              mr_prefixes = NULL,
                              univ_perc_params = list(),
                              univ_quant_params = list(),
                              univ_quali_params = list()      
                              ){
    stopifnot(is.data.frame(x) && all(dim(x) > 0L))
    style <- match.arg(style)
    ## logical vectors
    zero_ones <- unlist(lapply(x, function(y) all(y %in% c(NA, 0, 1))))
    numerics  <- unlist(lapply(x, is.numeric)) & (! zero_ones)
    factors   <- unlist(lapply(x, is.factor))

    ## check variables to be ignored
    all_na    <- unlist(lapply(x, function(y) all(is.na(y))))
    ignored_type   <- ! (zero_ones | numerics | factors)
    ignored <- ignored_type | all_na
    if (any(ignored)){
        ignored_names <- names(x)[ignored]
        warning('Some variables were ignored due to missingness (or type): ',
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

    if (!is.null(mr_prefixes)){
        mr_patterns <- paste0("^", mr_prefixes)
        mr_vars <- lapply(mr_patterns,
                          function(y) grep(y, x_names, value = TRUE))
        names(mr_vars) <- mr_prefixes
    }
        
    if (style == 'raw'){
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
                                                       data[, involved_mr],
                                                       comments)

                            univ_perc_params$caption <- clean(involved_prefix)
                            ## rm these variables from the cycle
                            x <- x[, names(x) %without% involved_mr]
                        }
                    }
                    params <- c(list(x = as.data.frame(data),
                                     wb = wb, latex = latex),
                                univ_perc_params)
                    do.call(univ_perc, params)
                } else if (varname %in% numerics_c){
                    params <- c(list(x = data, wb = wb, latex = latex),
                                univ_quant_params)
                    do.call(univ_quant, params)
                } else if (varname %in% factors_c){
                    params <- c(list(x = data, wb = wb, latex = latex),
                                univ_quali_params)
                    do.call(univ_quali, params)
                }
            }
        }
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
    
    invisible(NULL)
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
    
    varnames <- strtrim(paste(rownames(rval), collapse = '_'), 31)
        
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
#' @param latex output the table using \code{xtable::xtable}
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
                      sort_freq = FALSE,
                      latex = TRUE,
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
    rval <- cbind('n' = s, '%' = round((s/not_NA)*100, 2))
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
        rval <- rval[order(- rval[,1]), ]
    }
    
    varnames <- strtrim(paste(rownames(rval), collapse = '_'), 31)
        
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


## -----------------------------------------------------------------------
## BIVARIATE STUFF
## -----------------------------------------------------------------------

#' Dispatcher for (qualitative/quantitative) bivariate tables and
#' statistics.
#'
#' @param x a \code{data.frame}
#' @param group a data.frame of 1 column
#' @param analysis_name label prefix
#' @param wb a WorkBook
#' @param quant_test test for quantitative variables (biv_quant's test param)
#' @param quali_test test for qualitative variables (biv_quali's test param)
#' @export
bivariate_tables <- function(x, group, analysis_name, wb,
                             quant_test = 'none', quali_test = 'none'){
    worker <- function(x,   # analyzed var
                       g,   # grouping var
                       xn,  # variable name
                       gn,  # grouping var name
                       analysis_name # overall name, for sheet
                       )
    {
        ## print(class(x))
        ## print(class(g))
        ## print(xn)
        ## print(gn)
        ## print(analysis_name)
        sheet_name <- paste(analysis_name, gn, xn, sep = '_')
        label <- paste('tab', sheet_name, sep = ':')
        caption <- if (!is.null(comment(x))) comment(x) else ''
        if (lbmisc::is.qualitative(x)) {
            biv_quali(x = x, y = g, wb = wb, sheets = sheet_name,
                      label = label, caption = caption,
                      test = quali_test)
        } else if (lbmisc::is.quantitative(x)) {
            biv_quant(x = x, y = g, wb = wb, sheets = sheet_name,
                      label = label, caption = caption,
                      test = quant_test)
        } else 
            stop(sprintf('%s: tipo variabile non contemplato', xn))
    }
    invisible(Map(worker, 
                  x, 
                  list(group[, 1]), 
                  as.list(names(x)),
                  as.list(names(group)),
                  list(analysis_name)))
}



#' Bivariate table for categorical data.
#' 
#' @param x row variable: a discrete quantitative variable, a
#'     character or a factor
#' @param y column variable: a discrete quantitative variable, a
#'     character or a factor
#' @param totals print totals?
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
                      totals = TRUE,
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

        ## columns totals
        col_sums <- colSums(rval)
        ## put NA to percentage sums (100%) which is obvious: odd columns
        col_sums[seq(from = 2, to = length(col_sums), by = 2)] <- NA
        rval <- rbind(rval, 'Tot' = col_sums)
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
    xname <- gsub('^.+\\$', '', deparse(substitute(x)))
    yname <- gsub('^.+\\$', '', deparse(substitute(y)))
    varnames <- strtrim(paste(xname, yname, sep = '_'), 31)
    test <- match.arg(test)
    
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
