#' tableone::CreateTableOne wrapper with latex and excel export
#'
#' tableone::CreateTableOne wrapper with latex and excel export
#'
#' @param vars same as in tableone::CreateTableOne
#' @param strata same as in tableone::CreateTableOne
#' @param data same as in tableone::CreateTableOne
#' @param factorVars same as in tableone::CreateTableOne
#' @param includeNA same as in tableone::CreateTableOne
#' @param test same as in tableone::CreateTableOne
#' @param testApprox same as in tableone::CreateTableOne
#' @param argsApprox same as in tableone::CreateTableOne
#' @param testExact same as in tableone::CreateTableOne
#' @param argsExact same as in tableone::CreateTableOne
#' @param testNormal same as in tableone::CreateTableOne
#' @param argsNormal same as in tableone::CreateTableOne
#' @param testNonNormal same as in tableone::CreateTableOne
#' @param argsNonNormal same as in tableone::CreateTableOne
#' @param smd same as in tableone::CreateTableOne
#' @param addOverall same as in tableone::CreateTableOne (but TRUE as default)
#' @param showAllLevels same as in tableone::print.TableOne (but TRUE as default)
#' @param exact same as in tableone::print.TableOne (if NULL
#'     fisher.test is used if lbmisc::fisher_needed)
#' @param nonnormal same as in tableone::print.TableOne (if NULL, all
#'     is considered nonnormal)
#' @param catDigits same as in tableone::print.TableOne
#' @param contDigits same as in tableone::print.TableOne
#' @param pDigits same as in tableone::print.TableOne
#' @param wb if an openxlsx Workbook is given, Excel exporting to that
#'     one will occurr
#' @param sheet sheet name for openxlsx WorkBook
#' @param print_latex print to latex
#' @param label label for the created latex table
#' @param caption caption for the created latex table
#' @export
tableone <- function(vars,
                     strata,
                     data,
                     factorVars,
                     includeNA = FALSE,
                     test = TRUE,
                     testApprox = chisq.test,
                     argsApprox = list(correct = TRUE),
                     testExact = fisher.test,
                     argsExact = list(workspace = 2 * 10^5),
                     testNormal = oneway.test,
                     argsNormal = list(var.equal = TRUE),
                     testNonNormal = kruskal.test,
                     argsNonNormal = list(NULL),
                     smd = TRUE,
                     addOverall = TRUE,
                     ## general settings (print.TableOne) 
                     showAllLevels = TRUE,
                     exact = NULL,
                     nonnormal = NULL,
                     catDigits = 1,
                     contDigits = 2,
                     pDigits = 3,
                     ## excel exporting
                     wb = NULL, # export in excel if this is a Workbook
                     sheet = 'tab1',
                     ## latex printing
                     print_latex = TRUE,
                     label = 'table1',
                     caption = 'Descrittive del campione')
{
    tab1 <- tableone::CreateTableOne(    
                          vars = vars,
                          strata = strata,
                          data = data,
                          factorVars = factorVars,
                          includeNA = includeNA,
                          test = test,
                          testApprox = testApprox,
                          argsApprox = argsApprox,
                          testExact = testExact,
                          argsExact = argsExact,
                          testNormal = testNormal,
                          argsNormal = argsNormal,
                          testNonNormal = testNonNormal,
                          argsNonNormal = argsNonNormal,
                          smd = smd,
                          addOverall = addOverall)

    ## modifica a fisher laddove ce n'è bisogno
    categoriche <- vars[sapply(data[, vars], lbmisc::is.qualitative)]
    numeriche <- vars[sapply(data[, vars], lbmisc::is.quantitative)]
    rimanenti <- vars %without% c(categoriche, numeriche)
    if (length(rimanenti) > 0 && test){
        r <- paste0(rimanenti, collapse = ',')
        message(rimanenti, ' non considerati nelle procedure automatiche (per test esatti o non normali, ocio)')
    }
    
    ## test di fisher laddove necessario
    if (test && length(categoriche) > 0 && is.null(exact)){
        fn <- sapply(
            categoriche,
            function(v) lbstat::fisher_needed(data[, v], data[, strata])
        )
        exact <- categoriche[fn]
    }

    ## test di kruskal.wallis di default
    if (length(numeriche) > 0 && is.null(nonnormal)){
        nonnormal <- numeriche
    }
        
    if (print_latex){
        # latex exporting
        p <- print(tab1,
                   printToggle = FALSE,
                   noSpaces = TRUE,
                   showAllLevels = showAllLevels,
                   smd = smd,
                   exact = exact,
                   nonnormal = nonnormal,
                   catDigits = catDigits,
                   contDigits = contDigits,
                   pDigits = pDigits)
        k <- knitr::kable(p, 
                          format = "latex", 
                          caption = caption, 
                          label = label,
                          vline = "",
                          toprule = "\\hline", 
                          midrule = "\\hline",
                          linesep = "",
                          bottomrule = "\\hline")
        print(k)
    }
    if (methods::is(wb, "Workbook")){
        ## excel exporting
        tab1mat <- print(tab1,
                         quote = FALSE, 
                         noSpaces = TRUE,
                         printToggle = FALSE,
                         showAllLevels = showAllLevels,
                         smd = smd,
                         exact = exact,
                         nonnormal = nonnormal,
                         catDigits = catDigits,
                         contDigits = contDigits,
                         pDigits = pDigits)
        ## trasforma a data.frame se no viene fatto in esportazione ed
        ## aggiunge monnezza
        Variables <- rownames(tab1mat)
        tab1mat <- cbind(data.frame(Variables), tab1mat)
        xlsx_table(tab = tab1mat,
                   test_df = NULL, 
                   wb = wb,
                   sheet = sheet,
                   caption = caption,
                   rowNames = FALSE,  # aggiunte come colonna pulita
                   varname = 'notneeded')
    }
    invisible(tab1)
}
