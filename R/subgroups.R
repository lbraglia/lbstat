#' Function to perform subgroups (effect estimate) analysis
#' 
#' function to perform subgroups analysis given minimal amout of
#' infos. It should produce a table suitable for forest plotting.
#' 
#' @param ep end point (quantitative, factor or Surv).  Estimates with
#'     \code{lm} for quantitative, \code{glm} for binomial factor,
#'     \code{coxph} for \code{Surv}
#' @param factor main estimate factor
#' @param subgroups factor for subsetting and interaction test
#' @param factor_lab label for the main factor under study (eg
#'     Treatment in a RCT)
#' @param subgroups_lab label for the subgroup (eg could by Histology,
#'     Age class and so on)
#' 
#'@export
subgroups <- function(ep, factor, subgroups, 
                      factor_lab = 'Treatment',
                      subgroups_lab = 'Subgroup',
                      ...)
{
    if (!is.factor(subgroups)) stop("subgroups must be a factor")
    UseMethod("subgroups")
}

#'@export
subgroups.Surv <- function(ep, factor, subgroups, 
                           factor_lab = 'Treatment',
                           subgroups_lab = 'Subgroup',
                           ...) 
{
    data <- data.frame(ep = ep, factor = factor, subgroups = subgroups)
    main_f   <- ep ~ factor
    subs_f   <- ep ~ subgroups
    int_f    <- ep ~ factor * subgroups
    # stima main, stima nei subset e stima con interazione
    main_cox <- survival::coxph(formula = main_f, data = data)
    subs_cox <- survival::coxph(formula = subs_f, data = data)
    int_cox  <- survival::coxph(formula = int_f, data = data)
    ## test di interazione
    int_test <- anova(int_cox, main_cox)[2, 'P(>|Chi|)']
    ## sfit <- summary(fit)
    ## cox <- coxph(f, data = x)
    ## scox <- summary(cox)
    
    ## ## tabella dei numeri
    ## n <- t(sfit$table[, c('records', 'events')])
    ## dim(n) <- NULL
    ## n <- as.data.frame(setNames(as.list(n), 
    ##                             c('std_n', 'std_ev', 'ht_n', 'ht_ev')))
    ## ## tabella effetti
    ## effetti <- setNames(pretty_model(cox), c('HR', 'low', 'up', 'p'))
    ## effetti[n$ht_ev %in% 0 | n$std_ev %in% 0, ] <- NA
    ## effetti$hr_label <- sprintf("%.2f (%.2f - %.2f)", 
    ##                             effetti$HR,
    ##                             effetti$low,
    ##                             effetti$up)
    ## cbind(gruppo, n, effetti)
    
    ## return
    list(int_cox, int_test)
}

## =========================================================================
## ----------------------------------------------------------     SARCULATOR
## =========================================================================

## ## x <- group13_spl[[1]]
## fp_data <- function(x, ep = c('os', 'pfs')){
##     ep <- match.arg(ep)
##     if (ep == 'os')
##         f <- Surv(time = os_time, event = os_status) ~ treatment
##     else
##         f <- Surv(time = dfs_time, event = dfs_status) ~ treatment
##     fit <- survfit(f, data = x)
##     sfit <- summary(fit)
##     cox <- coxph(f, data = x)
##     scox <- summary(cox)
##     ## gruppo
##     gruppo <- data.frame(
##         'sarc' = gsub('pr-OS: ', '', as.character(unique(x$sarc10os_gr2))),
##         'isto' = isto_renamer(unique(x$inc03)))
##     ## tabella dei numerini
##     n <- t(sfit$table[, c('records', 'events')])
##     dim(n) <- NULL
##     n <- as.data.frame(setNames(as.list(n), 
##                                 c('std_n', 'std_ev', 'ht_n', 'ht_ev')))
##     ## tabella effetti
##     effetti <- setNames(pretty_model(cox), c('HR', 'low', 'up', 'p'))
##     effetti[n$ht_ev %in% 0 | n$std_ev %in% 0, ] <- NA
##     effetti$hr_label <- sprintf("%.2f (%.2f - %.2f)", 
##                                 effetti$HR,
##                                 effetti$low,
##                                 effetti$up)
##     cbind(gruppo, n, effetti)
## }
## os_res <- do.call(rbind, lapply(group13_spl, fp_data, ep = 'os'))

## lab_font <- fpTxtGp(label = list(gpar(fontfamily = "", cex=0.7)))

## forest_table <- as.data.frame(lapply(os_res[, vars], as.character))
## header <- data.frame(c(NA, 'pr-OS'), 
##                      c(NA, 'Histology'), 
##                      c('STD', 'n.'),
##                      c('STD', 'ev.'), 
##                      c('HT', 'n.'), 
##                      c('HT','ev.'),
##                      c(NA, 'HR (95%CI)'))

## names(header) <- names(forest_table)
## labeltext <- rbind(header, forest_table)
## is_summary <- c(TRUE, TRUE, rep(FALSE, nrow(forest_table)))

## forestplot::forestplot(labeltext = labeltext,
##                        hrzl_lines = TRUE,
##                        is.summary = is_summary,
##                        align = 'l',
##                        colgap = unit(3, 'mm'),
##                        txt_gp = lab_font,
##                        mean = c(NA, NA, os_res$HR),
##                        lower = c(NA, NA, os_res$low),
##                        upper = c(NA, NA, os_res$up), 
##                        # clip = c(0.1, 10),
##                        xlog = TRUE)

## =========================================================================
## ---------------------------------------------------------           MRP
## =========================================================================

## subgroup_fp <- function(f,    # Surv() ~ treatment formula
##                         data, # data.frame
##                         subgroups = '' ## name of the factor variable for subgr
##                         )
## {
##     ## TODO: interaction test printing
##     do_subgroups <- subgroups %in% names(data)

##     ## nomi variabili del dataset
##     flist <- as.list(f)
##     event_var <- as.character(flist[[2]]$event)
##     time_var<- as.character(flist[[2]]$time)
##     treatm_var <- as.character(flist[[3]])
##     ## check per sicurezza perché dopo si assume che gli eventi siano 0 o 1
##     if (!all(data[, event_var]) %in% c(0, 1, NA))
##         stop('Events must be 0,1 or NA')
    
##     ## livelli della variabile di trattamento
##     treatm_var_labs <- levels(data[, treatm_var])
##     n_header <- paste(rep(treatm_var_labs, each = 2), c('n', 'ev'), sep = "_")
##     grp1_n  <- n_header[1]
##     grp1_ev <- n_header[2]
##     grp2_n  <- n_header[3]
##     grp2_ev <- n_header[4]

##     ## a row of NA data.frame with structure conforming to results returned
##     void <- res <- data.frame(as.list(rep(NA, 9)))
##     full_header <- c(n_header, c('HR', 'low', 'up', 'p', 'HR_string'))
##     void <- setNames(void, full_header)
    
##     surv_worker <- function(f, data) {
##         all_vars <- c(event_var, time_var, treatm_var, subgroups)
##         data <- NA_remove(data[, all_vars], quiet = TRUE)
        
##         if (nrow(data) > 0L) {
##             ## numbers
##             fit <- survfit(formula = f, data = data)
##             sfit <- summary(fit)
##             ## when the group is only one (n = 0, ev = 0 for the other), 
##             ## a vector is returned
##             n <- if (is.matrix(sfit$table)) 
##                      t(sfit$table[, c('records', 'events')])
##                  else {
##                      ## bisogna ricostruire eventi e n via tabelle 
##                      ## di contingenza
##                      n <- table(data[, treatm_var])
##                      ## qui si assume che tutti gli eventi 
##                      ## siano tra 0 e 1 se no
                         
##                      ev <- table(factor(data[, event_var], levels = 0:1), 
##                                  data[, treatm_var])['1', ]
##                      rbind(n, ev)
##                  } 
                    
##             dim(n) <- NULL
##             n <- as.data.frame(setNames(as.list(n), n_header))

##             ## estimates: do them if there's at least one event per group
##             feasible_estimates <- n[, grp1_ev] > 0L & n[, grp2_ev] > 0L
##             if (feasible_estimates) {
##                 cox <- coxph(formula = f, data = data)
##                 scox <- summary(cox)
##                 HR <- setNames(pretty_model(cox), c('HR', 'low', 'up', 'p'))
##                 HR$HR_string <- sprintf("%.2f (%.2f - %.2f)", 
##                                         HR$HR, HR$low, HR$up)
##             } else {
##                 HR <- data.frame(
##                     'HR' = NA, 'low' = NA, 'up' = NA, 'p' = NA,
##                     'HR_string' = NA
##                     #sprintf("%.2f (%.2f - %.2f)", NA, NA, NA)
##                 )
##             }
            
##             cbind(n, HR)
            
##         } else { ## no elements in this subgroup
##             void ## return a void data.frame (for the moment)
##         }
##     }

##     ## Overall estimates
##     overall_HR <- surv_worker(f = f, data = data)
##     overall_HR <- cbind(data.frame(group = 'All'), overall_HR)
##     rownames(overall_HR) <- NULL
    
##     ## Subgroups estimates
##     if (do_subgroups){
##         data_spl <- split(data, data[, subgroups], drop = FALSE)
##         subgroups_HR <- lapply(data_spl, 
##                                function(x) surv_worker(f = f, data = x))
##         subgroups_HR <- do.call(rbind, subgroups_HR)
##         subgroups_HR$group <- rownames(subgroups_HR)
##     }
    
##     ## Return
##     if (do_subgroups) {
##         ## add the group NA to void
##         ## void2 <- cbind(group = NA, void)
##         ## res <- rbind(overall_HR, void2, subgroups_HR) 
##         res <- rbind(overall_HR, subgroups_HR) 
##         rownames(res) <- NULL
##     } else { 
##         res <- overall_HR
##     }
    
##     ## rm NA(NA - NA)
##     res[res$HR_string %in% 'NA (NA - NA)', 'HR_string'] <- NA
##     res
## }
