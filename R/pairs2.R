#' upper panel for stats::pairs: 
#'
#' points with lowess estimator
#' 
#' @export
pairs_upper <- function(x, y, 
                        points_col = lbmisc::col2hex('black', 0.3),
                        low_col = 'red', ...){
    ## NA rompono le scatole a lowess (a parte non poter esser plottati
    df <- na.omit(data.frame(x = x, y = y))
    points(x = df$x, y = df$y, pch = 20, col = points_col, ...)
    lines(stats::lowess(df$x, df$y), col = low_col)
}

#' lower panel for stats::pairs
#'
#' correlation coefficients
#' 
#' @export
pairs_lower <- function(x, y){
    # http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    use <- 'complete.obs'
    dgts = 2
    pearson <- round(cor(x, y, use = use, method = 'pearson'), digits = dgts)
    spearman <- round(cor(x, y, use = use, method = 'spearman'), digits = dgts)
    txt_pearson <- paste0("Pearson = ", pearson)
    txt_spearman <- paste0("Spearman = ", spearman)
    text(0.5, 0.75, txt_pearson)
    text(0.5, 0.25, txt_spearman)
    # cex.cor <- 0.8/strwidth(txt)
    ## text(0.5, 0.5, txt, cex = cex.cor * r)
}

#' diagonal panel for stats::pairs
#'
#' histogram
#' 
#' @export
pairs_diagonal <- function(x, ...){
    # from ?pairs
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, ...)
}

#' Battery included scatterplot matrix
#' 
#' @param x data.frame
#' @param ... other parameters passed to (but panel, lower.panel,
#'     upper.panel, diag.panel)
#' @return
#' @export
pairs2 <- function(x, ...){
    pairs(x = x
          upper.panel = pairs_upper,
          lower.panel = pairs_lower,
          diag.panel  = pairs_diagonal,
          ...)
}
