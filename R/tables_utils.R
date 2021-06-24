## converter latex caption -> excel export 
xlsx_caption <- function(x){ ## x is a latex generated caption
    ## remove test string
    ## x <- gsub('\\s{0,}\\(.+\\){1}$', '', x, perl = TRUE)
    ## remove $$ for LaTeX formulas
    x <- gsub('\\$', '', x)
    lbmisc::rm_spaces(x)
}

## helper for exporting tables ... very raw for now
xlsx_table <- function(tab, test_df, wb, sheet, caption,
                       varname, rowNames = TRUE)
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


## Funzione di supporto per pulire le mr e lasciare solo le modalita
clean_mr_names <- function(z, mr_prefixes) {
    ## remove mr prefixes
    z <- gsub(mr_prefixes, '', z)
    ## replace underscore with spaces
    z <- gsub("_", " ", z)
    ## remove duplicate spaces or trailing/ending spaces
    lbmisc::rm_spaces(z)
}


## funzione per il preprocessing delle date per bivariate_tables
## e univariate_tables
preprocess_dates <- function(x, f){
    comments <- lapply(x, comment)
    x <- lapply(x, f)
    Map(function(d, c) {comment(d) <- c; d}, as.list(x), comments)
}


## the dispatcher called both by univariate and bivariate_tables if the style
## is 'raw'
raw_dispatcher <- function(x, ## dataset
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
                    ## TODOHERE
                    ## browser()
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
                        data[, involved_mr] <- Map(
                            set_com,
                            data[, involved_mr, drop = FALSE],
                            comments)
                        ## Setup caption from the original multiple
                        ## responses variable
                        possible_caption_comment_variable <-
                            gsub("_$", "", involved_prefix)
                        cm <- tryCatch(
                            comment(x[, possible_caption_comment_variable]),
                            error = function(m) NULL)
                        perc_f_params$caption <-
                            if (is.null(cm)) clean(involved_prefix) else cm
                        ## perc_f_params$caption <- "clean(involvedprefix)"
                        
                        ## rm these variables from the cycle
                        x <- x[, names(x) %without% involved_mr, drop = FALSE]
                    }
                }
                ## TODOHERE
                params <- c(list(x = as.data.frame(data),
                                 mr_prefixes = involved_prefix),
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


## make a sensible latex label given different scenarios
latex_label_maker <- function(label,
                              label_prefix,
                              varnames) # nomi variabili di x e y, underscored
{
    ## LABEL
    if (label != '') {
        ## do nothing: if label is specified it's assumed to be correct
    } else {
        ## otherwise substitute with the variable name, prefixed by
        ## label_prefix
        label <- paste0('tab:', label_prefix, varnames)
    }
    label
}
    
## make a sensible latex caption given different scenarios
latex_caption_maker <- function(caption = NULL,
                                caption_prefix = NULL,
                                x_comment = NULL,
                                y_comment = NULL)
{
    if (caption != '') {
        ## add caption prefix (useful for a common bivariate_tables,
        ## for example
        caption <- paste0(caption_prefix, caption)
    } else {
        ## substitute caption with comment if available
        if (!is.null(y_comment)){
            caption <- paste0(caption_prefix, x_comment,
                              ' by ', tolower(y_comment))
        } else {
            caption <- paste0(caption_prefix, x_comment)
        }
    }
    caption
}
