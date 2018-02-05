#' pretty print a statistical model
#' 
#' export the model as a table with confidence interval and p-values
#' 
#' @param mod a lm, glm or coxph object
#'
#' @examples
#'
#' ## lm
#' pretty_model(lm(dist ~ speed,  data = cars))
#' 
#' @export
pretty_model <- function(mod, ...){
    est <- stats::coef(mod)
    ci <- stats::confint(mod, ...)
    smod <- summary(mod)

    if (inherits(mod, "glm")){
        f <- exp
        p <- stats::coef(smod)[, 4]
        fam <- stats::family(mod)$family
        link <-  stats::family(mod)$link
        estname <- if (fam == 'binomial' && link == 'logit') 'OR'
                   else if (fam == 'poisson') 'IRR'
                   else stop('Family of glm not handled')
    } else if (inherits(mod, "coxph")) {
        f <- exp
        p <- stats::coef(smod)[, 5]
        estname <- 'HR'
    } else if (inherits(mod, 'lm')) {
        f <- identity
        p <- stats::coef(smod)[, 4]
        estname <- 'Est'
    } else {
        stop('Only lm, glm, and coxph are currently supported')
    }
    rval <- f(cbind(est, ci))
    rval <- data.frame(rval, pretty_pval(p))
    colnames(rval) <- c(estname, 'Lower CI', 'Upper CI', 'p-value')
    rval
}
