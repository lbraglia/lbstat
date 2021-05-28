#'F test to compare two linear models (a little utility)
#' 
#' @param mod_base if missing, the null model of mod_full is considered
#' @param mod_full the model with all the covariates
#' @param output str for a string, df for a dataframe
#' @export
ftest <- function(mod_base = NULL, mod_full = NULL, output = c('str', 'df')){
    output <- match.arg(output)
    if (is.null(mod_full)) stop("specifica un modello")
    if (is.null(mod_base)) {
        sm <- summary(mod_full)
        stat <- sm$fstatistic
        rval <- as.data.frame(as.list(stat))
        rval$p <- pf(q = rval$value, 
                     df1 = rval$numdf, df2 = rval$dendf, 
                     lower.tail = F)
    } else {
        test <- anova(mod_base, mod_full)
        rval <- test[2, c("F", "Df", "Res.Df", "Pr(>F)") ]
    }
    names(rval) <- c('statistics', 'numdf', 'dendf', 'p')
    rownames(rval) <- 'F-test'
    if (output == 'df') rval 
    else sprintf("F(%.0f, %.0f) = %.3f, p = %.3f", 
                 rval$numdf, 
                 rval$dendf,
                 rval$statistics,
                 rval$p)
}
