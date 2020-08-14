#' Select predictors
#'
#' Select best predictors using alternatively stepwise AIC, ridge
#' regression, Lasso (with optimal lambda determined according to
#' crossvalidation for the latters) or simplified purposeful selection
#' (aka start from univariate of each covariate specified in formula
#' and select what to put in a final multivariate analysis)
#' 
#' @param formula the full model specification
#' @param data dataset for search
#' @param family type of variable predicted
#' @param method which method to use ('lasso', 'ridge', 'step',
#'     'purposeful' or abbreviations)
#' @param cv_nfolds crossvalidation subsamples for lasso and ridge
#' @param lambdas lambdas for lasso and ridge
#' @param seed seed for pseudo-random number generation
#' @param purposeful_p p-value in univariate analysis below which a
#'     variable is included in multivariate analysis
#' @export
var_select <- function(formula,
                       data,
                       family = c("gaussian", "binomial", "poisson", 
                                  "multinomial", "cox", "mgaussian"),
                       method = c('lasso', 'ridge', 'step', 'purposeful'),
                       cv_nfolds = 10,  # for ridge and lasso
                       lambdas = NULL,  # for ridge and lasso
                       seed = 1,        # for ridge and lasso
                       purposeful_p = 0.2)
{
    method <- match.arg(method)
    family <- match.arg(family)
    formula <- as.formula(formula)
    if (method %in% c('lasso', 'ridge')){
        ## --------------------------
        ## lasso and ridge regression
        ## --------------------------
        alpha <- if ('lasso' == method) 1 else 0 # 0 for ridge, 1 for lasso
        set.seed(seed)
        y <- data[, all.vars(formula)[1]]
        x <- model.matrix(formula, data = data)[, -1]
        ## remove variables in x with 0 variance
        x <- x[, apply(x, 2, var) > 0]
        ## compute estimates
        estimate <- glmnet::glmnet(x = x, y = y, 
                                   alpha = alpha,
                                   lambda = lambdas,
                                   family = family)
        ## find best lambda and predictions
        cv_out <- glmnet::cv.glmnet(x = x, y = y, 
                                    alpha = alpha, 
                                    family = family,
                                    lambda = lambdas,
                                    nfolds = cv_nfolds)
        best_l <- cv_out$lambda.min
        coefs <- {
            tmp <- predict(estimate, type = 'coefficients', s = best_l)
            ## if lasso return only the non zero coefficients, otherwise 
            ## (ridge) return them all
            if ('lasso' == method) tmp[tmp[, 1] != 0, ] else tmp[, 1]
        }

        ## selected variables: obtain covariates from matching starting name
        ## again if 'ridge', all are selected
        covariates <- get_covariates(formula)
        names(covariates) <- covariates
        ncoefs <- names(coefs)
        matches <- lapply(covariates, function(x)
            any(grepl(sprintf("^%s", x), ncoefs)))
        selected <- names(which(unlist(matches)))
        
        ## return
        list('cv_out'      = cv_out,
             'best_l'      = best_l,
             'est'         = estimate,
             'coefs'       = coefs,
             'selected'    = selected)
    } else if ('step' == method) {
        ## --------------------------
        ## stepwise AIC method
        ## --------------------------
        full_f <- formula
        outcome <- as.character(full_f[[2]])
        null_f <- as.formula(sprintf('%s ~ 1', outcome))
        scope <- list(upper = full_f[-2], lower = null_f[-2])
        mod_null <- glm(null_f, family = family, data = data)
        mod_full <- glm(full_f, family = family, data = data)
        ## starting from the null model or the full model
        step_null <- step(mod_null, scope = scope)
        step_full <- step(mod_full, scope = scope)
        ## extracted variables
        from_null_selected <- get_covariates(step_null)
        from_full_selected <- get_covariates(step_full)
        ## return values
        list('from_null' = step_null,
             'from_null_selected' = from_null_selected,
             'from_full' = step_full,
             'from_full_selected' = from_full_selected,
             'from_both_selected' =
                 intersect(from_null_selected, from_full_selected))
    } else if ('purposeful' == method) {
        full_f <- formula
        outcome <- as.character(full_f[[2]])
        null_f <- as.formula(sprintf('%s ~ 1', outcome))
        covariates <- get_covariates(formula)
        uni_fs <- lapply(covariates, function(x){
            (sprintf('%s ~ %s', outcome, x))
        })
        ## null estimate
        null_estimate <-
            estimates_worker(formula = null_f, family = family, data = data)
        ## univariate estimates
        uni_estimates <- lapply(uni_fs, function(f){
            estimates_worker(formula = f, family = family, data = data)
        })
        names(uni_estimates) <- covariates
        ## univariate tests
        uni_tests <- lapply(uni_estimates, function(est){
            uni_test_worker(estimate = est,
                            null = null_estimate,
                            family = family)
        })
        ## Multivariate variables and estimate
        ## browser()
        multi_vars <- names(
            unlist(Filter(function(x) x < purposeful_p, uni_tests))
        )
        if (length(multi_vars) > 0) {
            multi_f <- as.formula(sprintf('%s ~ %s',
                                          outcome,
                                          paste(multi_vars, collapse = '+')))
            multi_estimate <- estimates_worker(
                formula = multi_f, family = family, data = data
            )
        } else {
            multi_estimate <- NA
        }
        ## final results table
        final_table <- purposeful_table(uni_estimates, multi_estimate)

        ## output
        list('uni_estimates' = uni_estimates,
             'uni_tests'     = uni_tests,
             'multi_vars'    = multi_vars,
             'multi_estimate' = multi_estimate,
             'table' = final_table)
        
    }
}

purposeful_not_implemented <- c("multinomial", "mgaussian")

## helpers
estimates_worker <- function(formula, family, data){
    if (family %in% purposeful_not_implemented)
        stop("This family is still to be implemented")

    if ('gaussian' == family) {
        lm(formula = formula, data = data)
    } else if (family %in% c('binomial', 'poisson')) {
        glm(formula = formula, family = family, data = data)
    } else if ('cox' == family) {
        coxph(formula = formula, data = data)
    }
}

uni_test_worker <- function(estimate, null, family){
    if (family %in% purposeful_not_implemented)
        stop("This family is still to be implemented")
    
    if ('gaussian' == family) {
        test <- anova(null, estimate)
        test[["Pr(>F)"]][2]
    } else if (family %in% c('binomial', 'poisson')) {
        test <- anova(null, estimate, test = 'Chisq')
        test[["Pr(>Chi)"]][2]
    } else if ('cox' == family) {
        test <- anova(null, estimate)
        test[["P(>|Chi|)"]][2]
    }

}

purposeful_table <- function(unis, multi){

    ## univariate preprocessing
    uni_pm <- lapply(unis, function(x) {
        tmp <- pretty_model(x)
        tmp <- tmp[rownames(tmp) %nin% '(Intercept)', ]
        tmp$variable <- gsub("^.+\\.", "", rownames(tmp))
        tmp
    })
    uni_pm <- do.call(rbind, uni_pm)[, c(5, 1:4)]
    rownames(uni_pm) <- NULL
    ## order of variables in the table
    uni_order <- uni_pm$variable

    ## multivariate preprocessing
    multi_pm <- pretty_model(multi)
    multi_pm <- multi_pm[rownames(multi_pm) %nin% '(Intercept)', ]
    multi_pm <- cbind(data.frame('variable' = rownames(multi_pm)), multi_pm)
    rownames(multi_pm) <- NULL
    
    ## table composition
    if (! all(multi_pm$variable %in% uni_pm$variable))
        stop("something went wrong")
    tab <- lbmisc::merge(uni_pm, multi_pm, by = 'variable', all.x = TRUE,
                         suffixes = c(" - univ", " - multiv"))
    tab <- tab[match(uni_order, tab$variable), ]
    rownames(tab) <- NULL
    tab
}

get_covariates <- function(x) labels(terms(formula(x)))
