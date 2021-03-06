\name{WriteGAResults}

\alias{WriteGAResults}

\title{Write Results of Genetic Algorithm}

\description{
Prints list components returned from the \code{OptimizeNetwork} function to a file.
}

\usage{
WriteGAResults(x, file)
}

\arguments{
\item{x}{\code{list}; the object returned from \code{\link{OptimizeNetwork}}.}
\item{file}{\code{character}; the path of a file for writing (optional).}
}

\details{
Printed components of \code{ga} include:
  \code{call};
  \code{obj.values};
  \code{rmsd};
  \code{local.error};
  \code{niters};
  \code{nrep.ans};
  \code{proc.time};
  \code{start.time};
  \code{obj.space};
  \code{ncalls.penalty};
  \code{pts.rm}; and
  \code{ga.ans@suggestion}.
}

\value{
None (invisible \code{NULL}).
}

\author{J.C. Fisher}

\examples{\dontrun{WriteGAResults(ans)}}

\keyword{IO}
