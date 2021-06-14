#' check the 10 n per covariate rules as per harrel
#' @param x a lm, glm(binomial) or coxph object
#' @param rule which treshold to check
#' @export
n_params_ratio <- function(x, rule = c('<10', '<15')){
    rule <- match.arg(rule)
    # implement limits of number of predictors for harrel rules: pag 72-73 of
    # rergession modelling strategies
    LM  <- inherits(x, 'lm') &(! inherits(x, 'glm'))
    GLM <- inherits(x, 'glm')
    COX <- inherits(x, 'coxph')
    if (LM){
        evs <- nrow(model.frame(x))
        covs <- length(x$coefficients) - 1
    } else if (GLM) {
        outc <- model.frame(x)[, 1]
        if (length(unique(outc)) > 2) stop('non logistiche da implementare')
        evs <- min(table(model.frame(x)[1]))
        covs <- length(x$coefficients) - 1
    } else if (COX) {
        evs <- sum(model.frame(x)[, 1][, 'status'])
        covs <- length(x$coefficients)
    } else stop("Something wrong here")
    ratio <- evs/covs
    rul <- if (rule == '<10') 10 else 15
    data.frame(m = evs, 
               params = covs, 
               ratio = ratio, 
               rule = factor(ratio >= rul, 
                             levels = c(FALSE, TRUE),
                             labels = c('Not good', 'Ok')))
}
