#' pretty print a statistical model
#' 
#' export the model as a table with confidence interval and p-values
#' 
#' @param mod a lm, glm or coxph object
#' @param ... other options passed to \code{confint}
#' 
#' @examples
#' \dontrun{
#' ## lm
#' mod_lm <- lm(dist ~ speed,  data = cars)
#' pretty_model(mod_lm)
#' 
#' ## binomial glm
#' cars$distgt36 <- cars$dist > 36
#' mod_binomial_glm <- glm(distgt36 ~ speed,  data = cars, family = binomial)
#' pretty_model(mod_binomial_glm)
#' 
#' ## poisson glm
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome <- gl(3, 1, 9)
#' treatment <- gl(3, 3)
#' mod_poisson_glm <- glm(counts ~ outcome + treatment, family = poisson)
#' pretty_model(mod_poisson_glm)
#' 
#' ## coxph
#' library(survival)
#' test1 <- list(time = c(4, 3, 1, 1, 2, 2, 3),
#'               status = c(1, 1, 1, 0, 1, 1, 0),
#'               x = c(0, 2, 1, 1, 1, 0, 0),
#'               sex = c(0, 0, 0, 0, 1, 1, 1))
#' mod_coxph <- coxph(Surv(time, status) ~ x + sex, data = test1)
#' pretty_model(mod_coxph)
#' }
#' @export
pretty_model <- function(mod, add_stars = FALSE, ...){
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
    if (add_stars)
        rval[, "p_star"] <- pval_stars(p)
    ci_levels <- sprintf("(%s)", colnames(ci))
    ci_colnames <- paste(c('Lower CI', 'Upper CI'), ci_levels)
    p_colnames <- if (add_stars) c('p-value', 'stars') else 'p-value'
    colnames(rval) <- c(estname, ci_colnames, p_colnames)
    rval
}
