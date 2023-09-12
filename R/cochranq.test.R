#' Cochran q test for dependent proportions
#'
#' Tipo mc nemar ma per più di due tempi o più di due trattamenti,
#' nulla che la proporzione sia la stessa.
#' 
#' @source \url{https://statisticaconr.blogspot.com/2011/10/cochran-q-test-in-r-nonparametric-test.html} and \url{https://stat.ethz.ch/pipermail/r-help/2006-September/113156.html}
#' @examples
#' mydata <- matrix(c(
#'                 1, 1, 0,
#'                 0, 1, 0,
#'                 1, 1, 1,
#'                 0, 1, 0,
#'                 0, 1, 0,
#'                 0, 1, 1,
#'                 0, 0, 0,
#'                 0, 1, 0,
#'                 1, 1, 0,
#'                 0, 1, 0,
#'                 0, 0, 0,
#'                 0, 0, 1),
#'              nrow = 12,
#'              byrow = T,
#'              dimnames = list(1 : 12,
#'                c("MarcaA", "MarcaB", "MarcaC")))
#' cochranq.test(mydata)
#' @export
cochranq.test <- function(mat)
{
  k <- ncol(mat)

  C <- sum(colSums(mat) ^ 2)
  R <- sum(rowSums(mat) ^ 2)
  T <- sum(rowSums(mat))

  num <- (k - 1) * ((k * C) - (T ^ 2))
  den <- (k * T) - R

  Q <- num / den

  df <- k - 1
  names(df) <- "df"
  names(Q) <- "Cochran's Q"

  p.val <- pchisq(Q, df, lower = FALSE)

  QVAL <- list(statistic = Q, parameter = df, p.value = p.val,
               method = "Cochran's Q Test for Dependent Samples",
               data.name = deparse(substitute(mat)))
  class(QVAL) <- "htest"
  return(QVAL)
}
