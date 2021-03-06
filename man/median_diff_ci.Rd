% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/median_ci.R
\name{median_diff_ci}
\alias{median_diff_ci}
\title{Median bootstrap confidence interval (BCA or parametric for ordered
factors) for numeric and ordered factor}
\usage{
median_diff_ci(x, y, R = 10000, test = c("mood", "none"))
}
\arguments{
\item{x}{data from the first group}

\item{y}{data from the second group}

\item{R}{bootstrap repetition}

\item{test}{add the mood test?}
}
\description{
Median bootstrap confidence interval (BCA or parametric for ordered
factors) for numeric and ordered factor
}
