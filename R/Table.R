#' Cross tabulation and table creation
#' 
#' This is a wrapper around table (using \code{useNA="ifany"} by
#' default) and addsmargins. 
#' 
#' @param ... Arguments to be passed to table.
#' @param useNA display NA counts
#' @param f function to be used for summaries
#' @return The function return same results of table with NA (if present) and
#' margins.
#' @examples
#' with(airquality, Table(Month, Day))
#' @export
Table <- function(..., useNA = 'ifany', f = list('Sum' = sum))
    addmargins(base::table(useNA = useNA, ...),
               FUN = f)
