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
#' @param addOverall same as in tableone::CreateTableOne
#' @param wb openxlsx WorkBook
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
                     addOverall = FALSE,
                     # export
                     wb = NULL, # export if this is a Workbook
                     sheet = 'tab1',
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
    if (print_latex){
        # latex exporting
        p <- print(tab1, printToggle = FALSE, noSpaces = TRUE)
        print(knitr::kable(p, 
                           format = "latex", 
                           caption = caption, 
                           label = label))
    }
    if (methods::is(wb, "Workbook")){
        ## excel exporting
        tab1mat <- print(tab1,
                         quote = FALSE, 
                         noSpaces = TRUE,
                         printToggle = FALSE)
        xlsx_table(tab = tab1mat, test_df = NULL, 
                   wb = wb, sheet = sheet, caption = caption,
                   rowNames = TRUE, varname = 'notneeded')
    }
    invisible(tab1)
}
