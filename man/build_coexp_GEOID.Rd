% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_coexp_GEOID.R
\name{build_coexp_GEOID}
\alias{build_coexp_GEOID}
\title{Builds a coexpression network given a GEO ID}
\usage{
build_coexp_GEOID(gseid, gene.list, method = "spearman", flag = "rank")
}
\arguments{
\item{gseid}{string GEO ID of expression experiment}

\item{gene.list}{array of gene labels}

\item{method}{correlation method to use, default Spearman's rho}

\item{flag}{string to indicate if the network should be ranked}
}
\value{
net Matrix symmetric
}
\description{
The function generates a dense coexpression network from expression data stored in 
GEO. The expression data is downloaded from GEO. Correlation coefficicents are used 
as to weight the edges of the nodes (genes). Calls \code{\link{get_expression_matrix_from_GEO}}
and \code{\link{build_coexp_network}}.
}
\keyword{GEO}
\keyword{GSE}
\keyword{coexpression}
\keyword{dense}
\keyword{network}
