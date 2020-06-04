#' Select predictors of a given
#'
#' Select best predictors using stepwise AIC, ridge regression or Lasso (with
#' optimal lambda  determined according to crossvalidation for the latters)
#' 
#' @param formula the full model specification
#' @param data dataset for search
#' @param family type of variable predicted
#' @param method which method to use
#' @param cv_nfolds crossvalidation subsamples for lasso and ridge
#' @param lambdas lambdas for lasso and ridge
#' @param seed seed for pseudo-random number generation
#' @export
var_select <- function(formula,
                       data,
                       family = c("gaussian", "binomial", "poisson", 
                                  "multinomial", "cox", "mgaussian"),
                       method = c('lasso', 'ridge', 'step'),
                       cv_nfolds = 10,  # for ridge and lasso
                       lambdas = NULL,  # for ridge and lasso
                       seed = 1)
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
        null_f <- as.formula(sprintf('%s ~ 1', as.character(full_f[[2]])))
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
    }
}

## helpers
get_covariates <- function(x) labels(terms(formula(x)))
