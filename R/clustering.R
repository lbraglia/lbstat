#' A k-means which give profiles as well
#' 
#' @export
kmeans2 <- function(data,
                    k,
                    preproc = identity,
                    nstart = 100,
                    seed = 5114365)
{
    set.seed(seed)
    
    raw_db     <- data
    preproc_db <- preproc(raw_db)

    # almost standard k-means
    cl <- kmeans(preproc_db, centers = k, nstart = nstart)
    
    # get clustering main stats
    cluster_stats <- with(
        cl,
        data.frame(
            "n_cluster" = k,
            "totss" = totss,
            "tot.withinss" = tot.withinss,
            "betweenss" = betweenss,
            "expl_perc" = betweenss / totss))

    ## get the unscaled centers for cluster profiling
    rounded_colmeans <- function(x) round(colMeans(x), 2)
    unscaled_centers <- lapply(split(raw_db, cl$cluster), rounded_colmeans)    
    unscaled_centers <- do.call(rbind, unscaled_centers)
    
    cluster_profile <- cbind(
        data.frame("cluster" = rownames(unscaled_centers), "n" = cl$size),
        unscaled_centers
    )
    
    # return results
    list("stats"    = cluster_stats,
         "profiles" = cluster_profile)
}

#' Gap analysis with automatic optimal refitting
#'
#' See exercise2 from hennig
#' 
#' @export
gapnc <- function(data,
                  FUNcluster = kmeans,    # glusGap params ..
                  K.max = 10,             #
                  B = 100,                #
                  d.power = 2,            #
                  spaceH0 = "scaledPCA",  # 
                  method = "globalSEmax", # maxSE params
                  SE.factor = 2,          # 
                  ...) # options passed to FUNcluster for final/optimal clustering
{
    spaceH0 <- match.arg(spaceH0, c("scaledPCA", "original"))
    method <- match.arg(method, c("firstSEmax", "Tibs2001SEmax",
                                  "globalSEmax", "firstmax", "globalmax"))

    ## gap statistics
    gap <- cluster::clusGap(x = data,
                            FUNcluster = FUNcluster,
                            K.max = K.max,
                            B = B,
                            d.power = d.power,
                            spaceH0 = spaceH0,
                            ...)

    ## Find optimal number of clusters
    nc <- cluster::maxSE(gap$Tab[, 3], gap$Tab[, 4],
                         method = method,
                         SE.factor = SE.factor)

    ## Re-run clustering (kmeans) with optimal number of cluster.
    optimal_cl <- FUNcluster(data, nc, ...)

    ## return
    res <- list(
        ## results
        "gap" = gap,
        "n_cl" = nc,
        "optimal_cl" = optimal_cl,
        ## inputs
        "data" = data,
        "FUNcluster" = FUNcluster,
        "FUNcluster_params" = list(...),
        "K.max" = K.max,
        "B" = B,
        "d.power" = d.power,
        "spaceH0" = spaceH0,
        "method" = method,
        "SE.factor" = SE.factor)
    class(res) <- c("gapAnalysis", "list")
    res
}

plot.gapAnalysis <- function(x) {
    ## id for clusters (x on plotting)
    clus <- seq(1, x$K.max)

    ## Actual plotting 
    par(mfrow = c(1, 3))

    ## 1) Values of gap and +/- 1se bands and optimal K
    plot(x$gap, main = "Gap +/- 1*se bands")
    abline(v = x$n_cl, lty = "dotted", col = "blue")

    ## 2) elbow method: values of S_k and optimal K (according to gap analysis)
    plot(clus, exp(x$gap$Tab[, 1]),
         xlab = "k", ylab = "S_k", type = "l",
         main = "Elbow method")
    abline(v = x$n_cl, lty = "dotted", col = "blue")

    ## 3) log S_k and optimal k
    ## determining the ylim first
    tmp <- x$gap$Tab[, 1:2]
    dim(tmp) <- NULL
    logsk_plot_ylim <- range(tmp)
    plot(clus, x$gap$Tab[, 1],  type = "l", # 3a)  in the data
         xlab = "k", ylab = "log(S_k)", main = "log S_k plot",
         ylim = logsk_plot_ylim)
    points(clus, x$gap$Tab[, 2], type = "l", lty = 2) # 3b) under unif
    abline(v = x$n_cl, lty = "dotted", col = "blue")
    legend("topright",
           legend = c("log(S_k) in data", "E(log(S_k)) uniform"),
           lty = 1:2, bg = "white")
}


#' Mahalanobis distance matrix
#'
#' Mahalanobis distance matrix
#' 
#' @export
mahalanobis2 <- function(x, center = NULL, cov, inverted = FALSE, ...){
    ## The mahalanobis command can only compute a vector of Mahalanobis
    ## distances between one vector and one point.  So producing all
    ## distances is more tedious; here's how to make a distance matrix:
    if (is.null(center)){
        ## if no center is provided, return the distance matrix of all
        ## the distances
        x_cov <- stats::cov(x)
        mm <- apply(x, 1, function(single_unit)
            stats::mahalanobis(x,
                               center = single_unit,
                               cov = x_cov,
                               inverted = inverted,
                               ...))
        as.dist(mm)
    } else {
        ## otherwise do the standard mahalanobis
        stats::mahalanobis(x, center = center, cov = cov,
                           inverted = inverted, ...)
    }
}
