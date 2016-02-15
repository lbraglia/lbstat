#' Cross tabulation and table creation
#' 
#' This is a wrapper around table (using \code{useNA="ifany"} by
#' default) and addsmargins. 
#' 
#' @param ... Arguments to be passed to table.
#' @return The function return same results of table with NA (if present) and
#' margins.
#' @export
Table <- function(...) addmargins(base::table(useNA = 'ifany', ...))
