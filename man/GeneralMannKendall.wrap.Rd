% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Comp_TrendAnalysis.R
\name{GeneralMannKendall.wrap}
\alias{GeneralMannKendall.wrap}
\title{Mann-Kendall trend analysis  commit 1}
\usage{
GeneralMannKendall.wrap(
  X,
  level = 0.1,
  dep.option = "INDE",
  DoDetrending = TRUE
)
}
\arguments{
\item{X}{data (vector). IMPORTANT: it assumes that X is regularly-spaced.}

\item{level}{numeric, between 0 and 1, level of the test (default: 0.1).}

\item{dep.option}{dependency option, option for handling temporal dependence
(default 'INDE' for independence).}

\item{DoDetrending}{logical, only used for when dep.option == 'LTP'
(default: TRUE).}
}
\value{
a dataframe, with the different statistics and values of interest of
the test.
}
\description{
Apply the generalMannKendall function to the serie X.
}
\examples{
GeneralMannKendall.wrap(X=1:100)
GeneralMannKendall.wrap(X=rep(1,100))
}
