#' Plot kernel density estimates for quantitative variable of a
#' list/data.frame
#'
#' Plot kernel density estimates for not 0-1 only quantitative
#' variable of a list/data.frame.
#' 
#' @param x a data.frame
#' @param na.rm remove NA values from density calculation
#' @param quiet if FALSE tells number and names of variables plotted
#' @param ... further arguments passed to \code{density}
#' @examples
#' par(mfrow = c(2, 3))
#' quant_density(airquality)
#' @export
quant_density <- function(x, na.rm = TRUE, quiet = TRUE, ...){
    stopifnot(is.list(x))
    ## plotted density: not 0-1 only numeric variable
    x <- Filter(f = lbmisc::is.quantitative, x = x)
    var_names <- if (is.character(names(x))) names(x) else ''
    if (!quiet){
        msg <- sprintf('plotting densities for %d variables:\n',
                       sum(var_names %nin% ''))
        cat(msg)
        cat(paste(var_names[var_names %nin% ''], col = '\n'))
    }
    
    plot_fun <- function(data, varname){
        
        graphics::plot(stats::density(x = data, na.rm = na.rm, ...),
                       main = varname)
        graphics::abline(v = mean(x = data, na.rm = na.rm),
                         col = lbmisc::col2hex('red', alpha = 0.5),
                         lty = 'dotted')
        graphics::abline(v = stats::median(x = data, na.rm = na.rm),
                         col = lbmisc::col2hex('blue', alpha = 0.5),
                         lty = 'dashed')
    }
    Map(plot_fun, x, var_names)
    invisible(NULL)
}

#' Draw a boxplot with Kruskal-Wallis or Anova test too
#'
#' Draw a boxplot with Kruskal-Wallis or Anova test too
#' @param x quantitative variable
#' @param group group/qualitative variable (coerced to factor)
#' @param test which test to perform
#' @param add_p_height add a line with p of the test performed at the
#'   specified height (on the y scale) if this value is not NA
#' @param ... further params passed to boxplot
#' @export
boxplot_test <- function(x, group, test = c('kruskal.test', 'anova'), 
                         add_p_height = NA, ...) 
{
    db <- data.frame(x = x, group = factor(group))
    n_groups <- nlevels(db$group)
    f <- x ~ group
    test <- match.arg(test)
    ## TODO: make a unique thing with code from biv_quant
    if ("anova" == test) {
        test <- stats::oneway.test(formula = f, data = db, var.equal = FALSE)
        test_name <- "Anova"
        test_p <- lbmisc::pretty_pval(test$p.value, equal = TRUE)
    }
    else if ("kruskal.test" == test) {
        test <- stats::kruskal.test(formula = f, data = db)
        test_name <- "Kruskal-Wallis"
        test_p <- lbmisc::pretty_pval(test$p.value, equal = TRUE)
    }
    bp <- boxplot(x ~ group, data = db, ...)
    if (!is.na(add_p_height)) {
        bp_text <- sprintf('p%s', test_p)
        lbmisc::segments_text(text = bp_text, 
                              x0 = 1, x1 = n_groups,
                              y = add_p_height)
    }
    invisible(list("bp" = bp, 'test_name' = test_name, 
                   'test' = test, 'test_p' = test_p))
}


#' Draw a scatterplot with correlation coefficient
#' 
#' Draw a scatterplot with correlation coefficient
#' @param x quantitative variable
#' @param y quantitative variable
#' @param rho correlation method, either 'spearman' (default) or 'pearson'
#' @param plot_rho_x if not NA where to put the correlation estimate string
#' @param plot_rho_y if not NA where to put the correlation estimate string
#' @param ... further parameters passed to plot
#' @export
scatter_cor <- function (x, y, 
                         rho = c("spearman", "pearson"),
                         plot_rho_x = NA,
                         plot_rho_y = NA,
                         ...)
{
    rho <- match.arg(rho)
    plot(x = x, y = y, ...)
    cor_test <- cor.test(x = x, y = y, method = rho)
    if (!anyNA(c(plot_rho_x, plot_rho_y))){
        ## spearman non ha CI
        cor_string <- sprintf('Corr = %.2f', cor_test$estimate)
        text(x = plot_rho_x, y = plot_rho_y, labels = cor_string)
    }
    invisible(list('cor_test' = cor_test))
}
