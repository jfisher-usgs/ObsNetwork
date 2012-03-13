\name{WriteGAResults}

\alias{WriteGAResults}

\title{Write Results of Genetic Algorithm}

\description{
Prints returned list components from \code{\link{OptimizeNetwork}} to a file.
}

\usage{
WriteGAResults(ga, file)
}

\arguments{
\item{ga}{list; object returned from \code{\link{OptimizeNetwork}}.}
\item{file}{character; the path of a file for writing (optional).}
}

\details{
Printed components of \code{ga} include \code{rm.obs},
\code{obj.values}, \code{ans.rep}, and \code{elapsed.time}.
}

\value{
None (invisible \code{NULL}).
}

\author{J.C. Fisher}

\seealso{
\code{\link{write.table}}, \code{\link{cat}}
}

\examples{\dontrun{WriteGAResults(ga)}}

\keyword{IO}
