
#' Forestplot for subgroup analysis in a quantative variable for
#' survival analysis
#' 
#' @export
sg_fplotter_quant <- function(sg, ...){ 
    ## sg è il dataset prodotto da subgroups, 
    ## vname è il nome della variabile
    ## fix the shit
    ## browser()
    clean_me <- c('hr', 'ci_lower', 'ci_upper', 'hr_string', 'p')
    sg[sg$ev <= 1L, clean_me] <- NA
    sg[is.na(sg$ci_upper) | is.infinite(sg$ci_upper) | sg$ci_upper > 999, 
      clean_me] <- NA
    table_text_vars <- c("group", "n", "ev", 'hr_string', 'interaction_p') 
    head <- c("Group", "N", "events", 'HR', 'Int. p') 
    labeltext <- sg[, table_text_vars]
    header <- data.frame(as.list(head))
    names(header) <- table_text_vars
    labeltext <- rbind(header, labeltext)
    is_summary <- c(TRUE, rep(FALSE, nrow(labeltext) - 1L))
    lab_font <- fpTxtGp(label = list(gpar(fontfamily = "", cex=0.7)))
    min_ticks <- floor(min(log2(sg$ci_lower), na.rm = TRUE))
    min_ticks <- if (is.infinite(min_ticks)) -1 else min_ticks
    max_ticks <- ceiling(max(log2(sg$ci_upper), na.rm = TRUE))
    max_ticks <- if (is.infinite(max_ticks)) 1 else max_ticks
    xticks <- 2^{seq(min_ticks, max_ticks)}
    boxsize <- sg$n / max(sg$n, na.rm = TRUE)
    tryCatch({forestplot::forestplot(
                              labeltext = labeltext,
                              mean  = c(rep(NA, nrow(header)), sg$hr),
                              lower = c(rep(NA, nrow(header)), sg$ci_lower),
                              upper = c(rep(NA, nrow(header)), sg$ci_upper),
                              graph.pos = ncol(labeltext) - 1L, 
                              align = c('l', rep('c', ncol(labeltext) - 1L)),
                              is.summary = is_summary,
                              xlog = TRUE,
                              txt_gp = lab_font,
                              hrzl_lines = TRUE,
                              boxsize = 0.25,
                              ## boxsize = boxsize,
                              xticks = xticks,
                              colgap = unit(5, 'mm'),
                              mar = unit(rep(0.5, 4), units = 'cm'),
                              ...)
                              invisible(FALSE) # error, c-style
    }, error = function(x) invisible(TRUE) #error c-style
    )
}
