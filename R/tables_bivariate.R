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
#' @return The function return same results of table with NA (if
#'     present) and margins.
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
bivariate_tables <-
    function(x, group,
             wb = NULL,
             latex = TRUE,
             exclude = NULL,
             style = c('raw', 'type'),
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

    mr_vars <- NULL
    if (!is.null(mr_prefixes)){
        mr_patterns <- paste0("^", mr_prefixes)
        mr_vars <- lapply(mr_patterns,
                          function(y) grep(y, x_names, value = TRUE))
        names(mr_vars) <- mr_prefixes
    }

    common_params <- list(wb = wb, latex = latex, y = group[, 1],
                          yname = names(group))
    biv_perc_params  <- c(common_params, biv_perc_params)
    biv_quant_params <- c(common_params, biv_quant_params)
    biv_quali_params <- c(common_params, biv_quali_params)

    if (style == 'raw') {
        raw_dispatcher(x = x,
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



#' Bivariate table for categorical data.
#' 
#' @param x row variable: a discrete quantitative variable, a
#'     character or a factor
#' @param y column variable: a discrete quantitative variable, a
#'     character or a factor
#' @param xname a string used to identify x variable (used for latex
#'     captions and excel sheets)
#' @param yname a string used to identify x variable (used for latex
#'     captions and excel sheets)
#' @param totals print totals?
#' @param tot_row_label label for rows total
#' @param tot_col_label label for columns total
#' @param useNA print NA?
#' @param perc type of percentages
#' @param exclude_NA_perc remove NA from percentages (default = TRUE)
#' @param NA_string character used for NA's columns title
#' @param freq_sorting freq based sorting: can be \code{NA} (no freq
#'     based sorting) "\code{desc}" (descending) or "\code{asc}"
#'     (ascending). Sorting based on row totals.
#' @param test one of 'auto', 'none', 'fisher', 'chisq', 'mcnemar': if
#'     \code{auto} (default) fisher or chi square test will be
#'     performed (using \code{fisher_needed} to decide which one)
#' @param test_params a list of parameters to be passed to the test
#'     performing function
#' @param latex output the table using \code{xtable::xtable}
#' @param latex_placement table placement for latex printing
#' @param label_prefix latex label prefix (useful for a common prefix
#'     in univariate_tables and multivariate_tables). It's ignored if
#'     label is specified (eg the latter is considered
#'     full/complete/correct)
#' @param label latex label
#' @param caption_prefix a prefix for latex caption (useful for a
#'     common prefix in univariate_tables and multivariate_tables)
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
                      perc = c("col", "row", "none"),
                      ## perc = TRUE,
                      ## col_perc = TRUE,
                      exclude_NA_perc = TRUE,
                      NA_string = 'NA',
                      ## round_digits = 3,
                      freq_sorting = c(NA, 'desc', 'asc'),
                      test = c('auto', 'none', 'fisher', 'chisq', 'mcnemar'),
                      test_params = list(),
                      latex = TRUE,
                      latex_placement = 'ht',
                      label_prefix = '',
                      label = '',
                      caption_prefix = '',
                      caption = '',
                      wb = NULL,
                      sheets = '')
{
    test <- match.arg(test)
    perc <- match.arg(perc)

    ## to be compliant with the past code
    if (perc == 'col') {
        perc <- TRUE
        col_perc <- TRUE
    } else if (perc == 'row') {
        perc <- TRUE
        col_perc <- FALSE
    } else if (perc == 'none') {
        perc <- FALSE
        col_perc <- TRUE
    }
    
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
    
    if (is.null(xname))
        xname <- gsub('^.+\\$', '', deparse(substitute(x)))[1L]## for safeness
    if (is.null(yname))
        yname <- gsub('^.+\\$', '', deparse(substitute(y)))[1L]## for safeness 
    varnames <- paste(yname, xname, sep = '_')

    label <- latex_label_maker(label = label,
                               label_prefix = label_prefix,
                               varnames = varnames)

    caption <- latex_caption_maker(caption = caption,
                                   caption_prefix = caption_prefix,
                                   x_comment = comment(x),
                                   y_comment = comment(y))
   
    freq_sorting <- match.arg(freq_sorting)
    abs_freq <- table(x, y, useNA = useNA)
    row_sums <- rowSums(abs_freq)
    ## test <- match.arg(test)

    if (freq_sorting %in% c('asc', 'desc')){
        there_are_NA <- is.na(rownames(abs_freq)[nrow(abs_freq)])
        desc_sort <- freq_sorting == 'desc'
        row_indexes <-
            if (!there_are_NA) {
                order(row_sums, decreasing = desc_sort)
            } else {
                ## remove the last row from order determination
                c(order(row_sums[-length(row_sums)], decreasing = desc_sort),
                  length(row_sums))
            }
        abs_freq <- abs_freq[row_indexes, ]
    }
    
    ## column percentages 
    rel_freq <- prop.table(abs_freq, margin = if (col_perc) 2 else 1) * 100
    ## rm row NA percentage and back percentages to 100
    na_row <- is.na(rownames(rel_freq))
    na_col <- is.na(colnames(rel_freq))
    if (exclude_NA_perc){
        if (col_perc) {
            rel_freq[na_row, ] <- NA
        } else {
            rel_freq <- rel_freq[, !na_col]
        }
        rel_freq <- apply(rel_freq,
                          if (col_perc) 2 else 1,
                          function(x) 100 * x/sum(x, na.rm = TRUE))
        ## transpose to fix dimension with row percentages
        if (!col_perc) rel_freq <- t(rel_freq)
    }
    colnames(rel_freq) <- rep('%', ncol(rel_freq))

    ## cbind together
    rval <- cbind(abs_freq, rel_freq)

    ## a little trick for column right ordering
    if  (((!col_perc) && exclude_NA_perc && any(na_col)))
        id_seq <- matrix(c(1:(ncol(rval)+1)), nrow = 2, byrow = TRUE)
    else id_seq <- matrix(c(1:ncol(rval)), nrow = 2, byrow = TRUE)
    dim(id_seq) <- NULL
    ## remove the last index which is the first by recycling when using
    ## row percentages
    if ((!col_perc) && exclude_NA_perc && any(na_col))
        id_seq <- id_seq[-length(id_seq)]
    rval <- rval[, id_seq]

    if(totals) {
        if (col_perc){
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
        } else {
            ## columns totals
            col_sums <- colSums(abs_freq)
            row_tot_perc <- (col_sums / sum(col_sums)) * 100
            ## exclude NA perc
            if (exclude_NA_perc){
                row_tot_perc[na_col] <- NA
                row_tot_perc <- 100 *
                    (row_tot_perc / sum(row_tot_perc, na.rm = TRUE))
            }
            tot_row <- matrix(c(col_sums, row_tot_perc),
                              byrow = TRUE, nrow = 2)
            dim(tot_row) <- NULL
            ## se exclude_NA_perc e vi sono NA elimina l'ultimo elemento (NA
            ## che è stato lasciato per creare la matrice di sopra)
            if (exclude_NA_perc && any(na_col))
                tot_row <- tot_row[- length(tot_row)]
            ## todohere
            # browser()
            ## o qui lo si toglie
            rval <- rbind(rval, 'Tot' = tot_row)
            rownames(rval)[nrow(rval)] <- tot_row_label
            ## rows totals
            row_sums <- rowSums(abs_freq)
            overall_tot <- sum(row_sums)
            rval <- cbind(rval, 'Tot' = c(row_sums, overall_tot))
            colnames(rval)[ncol(rval)] <- tot_col_label
        }
        
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
    if (test %in% c('auto', 'fisher', 'chisq', 'mcnemar')){
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
        } else if ('mcnemar' == test) {
            test_fun <- stats::mcnemar.test
            test_name <- 'McNemar'
        } else
            stop('At this point test should be decided ...')

        ## ## add parameters
        ## test <- if (length(test_params) > 0L)
        ##             do.call(test_fun, c(list(x = x, y = y), test_params))
        ##         else
        ##             do.call(test_fun, list(x = x, y = y))
        
        if (length(test_params) > 0L) {
            test_params <- c(list(x = x, y = y), test_params)
        } else {
            test_params <- list(x = x, y = y)
        }
            
        ## do the test and handle shit
        test <- tryCatch(do.call(test_fun, test_params),
                         error = function(x){
                             ## nel caso di fisher aggiungi il simulate.p.value
                             ## se consigliato
                             if (test_name == 'Fisher'){
                                 sim_p <- grepl('simulate.p.value', x$message)
                                 if (sim_p){
                                     test_params <- c(
                                         test_params,
                                         list(simulate.p.value = TRUE))
                                     do.call(test_fun, test_params)
                                 }
                             }
                         },
                         warning = function(x){
                             ## nel caso di chisquare aggiungi il
                             ## simulate.p.value se l'approssimazione non
                             ## è corretta
                             ## https://stats.stackexchange.com/questions/81483
                             if (test_name == 'Chi square'){
                                 msg <- 'Chi-squared approximation may be incorrect'
                                 apprx <- grepl(msg, x$message)
                                 if (apprx){
                                     test_params <- c(
                                         test_params,
                                         list(simulate.p.value = TRUE))
                                     do.call(test_fun, test_params)
                                 }
                             }

                         })
        
        ## evaluate test and extract useful things
        test_p <- lbmisc::pretty_pval(test$p.value)
        test_string <- sprintf('%s test p-value: %s', test_name, test_p)
        test_df <- data.frame('Test' = test_name, 'p-value' = test$p.value)
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
            # browser()
            ## alternate 0,2 for number of columns/2
            digits <- rep(c(0,2), sum(!na_col))
            ## GESTIONE NA
            ## -----------
            ## se percentuali di colonna e ci sono dei NA aggiungi un c(0,2)
            if (col_perc && any(na_col)) digits <- c(digits, c(0,2))
            if ((!col_perc) && any(na_col) && (!exclude_NA_perc))
                digits <- c(digits, c(0, 2))
            if ((!col_perc) && any(na_col) && (exclude_NA_perc))
                digits <- c(digits, c(0))
            
            ## GESTIONE TOT (aggiungi 0,2 se percentuali di colonna, 0 se +
            ## di riga
            ## -----------
            if (col_perc) digits <- c(digits, c(0,2))
            else digits <- c(digits, 0)
        
        } else {
            ## no percentages, all integers
            digits <- rep(0L, ncol(rval))
        }
        xt <- xtable::xtable(rval,
                             ## align = 'todo',
                             digits = c(0, digits),
                             label = label, caption = caption)
        xtable::print.xtable(xt,
                             table.placement = latex_placement,
                             caption.placement = "top")
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
#' @param xname a string used to identify x variable (used for latex
#'     captions and excel sheets)
#' @param yname a string used to identify x variable (used for latex
#'     captions and excel sheets)
#' @param na.rm exclude missing value group for y
#' @param add_all add "All" row
#' @param test one of \code{'none'}, \code{'anova'}, or
#'     \code{'kruskal.test'}
#' @param latex output the table using \code{xtable::xtable}
#' @param latex_placement table placement for latex printing
#' @param label_prefix latex label prefix (useful for a common prefix
#'     in univariate_tables and multivariate_tables). It's ignored if
#'     label is specified (eg the latter is considered
#'     full/complete/correct)
#' @param label latex label
#' @param caption_prefix a prefix for latex caption (useful for a
#'     common prefix in univariate_tables and multivariate_tables)
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
                      label_prefix = '', 
                      label = '',
                      caption_prefix = '',
                      caption = '',
                      wb = NULL,
                      sheets = '')
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
    
    if (is.null(xname))
        xname <- gsub('^.+\\$', '', deparse(substitute(x)))[1L]## for safeness
    if (is.null(yname))
        yname <- gsub('^.+\\$', '', deparse(substitute(y)))[1L]## for safeness
    varnames <- paste(yname, xname, sep = '_')
    
    label <- latex_label_maker(label = label,
                               label_prefix = label_prefix,
                               varnames = varnames)

    caption <- latex_caption_maker(caption = caption,
                                   caption_prefix = caption_prefix,
                                   x_comment = comment(x),
                                   y_comment = comment(y))

    y <- factor(y, exclude = if (na.rm) NA else NULL)
    spl <- split(x, list(y))
    if (add_all) {
        all_ <- split(x, list('All'))
        spl <- c(spl, all_)
    }
    rval <- lapply(spl, desc)
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
        test_df <- data.frame('Test' = test_name, 'p-value' = test$p.value)
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
                             table.placement = latex_placement,
                             caption.placement = "top")
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
#' @param label_prefix latex label prefix (useful for a common prefix
#'     in univariate_tables and multivariate_tables). It's ignored if
#'     label is specified (eg the latter is considered
#'     full/complete/correct)
#' @param label latex label
#' @param caption_prefix a prefix for latex caption (useful for a
#'     common prefix in univariate_tables and multivariate_tables)
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
                     label_prefix = '',
                     label = '',
                     caption_prefix = '',
                     caption = '',
                     wb = NULL,
                     sheets = '')
{

    style <- match.arg(style)
    if (is.null(xname))
        xname <- gsub('^.+\\$', '', deparse(substitute(x)))[1L]## for safeness
    if (is.null(yname))
        yname <- gsub('^.+\\$', '', deparse(substitute(y)))[1L]## for safeness
    varnames <- paste(xname, yname, sep = '_')

    label <- latex_label_maker(label = label,
                               label_prefix = label_prefix,
                               varnames = varnames)

    caption <- latex_caption_maker(caption = caption,
                                   caption_prefix = caption_prefix,
                                   x_comment = comment(x),
                                   y_comment = comment(y))
    
    ## if (is.null(label))
    ##     label <- paste('tab', varnames, sep = ':')
    ## if (is.null(caption))
    ##     caption <- if (!is.null(comment(x))) comment(x) else ''

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
        rval <- lapply(rval, function(r) {
            r[with(r, order(N, n, decreasing = TRUE)), , drop = FALSE]
        })
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
        ## ora togli il nome della prima colonna ogni 4 (blocco unico)
        ## che è di scarto
        names(rval)[seq(1, ncol(rval), by = 4)] <- ""
    }
    
    ## Workbook handling
    if (methods::is(wb, "Workbook")){
        xlsx_table(tab = rval,
                   test_df = NULL,
                   wb = wb,
                   sheet = sheets,
                   caption = caption,
                   varname = varnames,
                   rowNames = FALSE)
    }

    ## output
    if (latex){
        digits <- c(0, # rowname non stampato
                    rep(c(0, 0, 0, 2), ncol(rval) / 4)) # set di 4 colonne 
        xt <- xtable::xtable(rval,
                             ## align = 'todo',
                             digits = digits,
                             label = label,
                             caption = caption)
        xtable::print.xtable(xt,
                             include.rownames = FALSE,
                             table.placement = latex_placement,
                             caption.placement = "top")
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
biv_mr <- function(mr_prefixes = '',
                   # caption,
                   ...){
    ## clean prefix from the data.frame
    dots <- list(...)
    x <- dots$x
    y <- dots$y
    ## ## per la caption cerca il commento della variabile x con nome
    ## ## pari al prefisso (senza _ finale)
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
                   ## caption = caption,
                   label = paste0('tab:', mr_prefixes),
                   sheets = mr_prefixes,
                   style = 'wide')
    do.call(biv_perc, c(params, dots))
}
