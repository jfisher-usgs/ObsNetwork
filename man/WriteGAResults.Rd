\name{WriteGAResults}

\alias{WriteGAResults}

\title{Write Results of Genetic Algorithm}

\description{
Prints list components returned from the \code{OptimizeNetwork} function 
to a file.
}

\usage{
WriteGAResults(x, file)
}

\arguments{
\item{x}{list; object returned from \code{\link{OptimizeNetwork}}.}
\item{file}{character; path of a file for writing (optional).}
}

\details{
Printed components of \code{ga} include: \code{call}; 
\code{obj.values}; \code{nrep.ans}; \code{elapsed.time}; 
\code{sum(ncalls.penalty)}; and \code{pts.rm}.
}

\value{
None (invisible \code{NULL}).
}

\author{J.C. Fisher}

\seealso{
\code{\link{OptimizeNetwork}}
}

\examples{\dontrun{WriteGAResults(ans)}}

\keyword{IO}
