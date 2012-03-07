\name{ProcessObservations}

\alias{ProcessObservations}

\title{Process Observation Data}

\description{
Something.
}

\usage{
ProcessObservations(d, x.var, y.var, projargs, siteno.var, sitenm.var,
                    net.var, var1.var, var2.var, acy.var, dt.var, dt.lim,
                    dt.fmt = "\%Y-\%m-\%d \%H:\%M")
}

\arguments{
\item{d}{data.frame; something}
\item{x.var, y.var}{something}
\item{projargs}{character; projection arguments to be entered exactly as in the
  \href{http://trac.osgeo.org/proj/}{PROJ.4} documentation.}
\item{siteno.var}{something}
\item{sitenm.var}{something}
\item{net.var}{something}
\item{var1.var}{something}
\item{var2.var}{something}
\item{acy.var}{something}
\item{dt.var}{something}
\item{dt.lim}{character; vector of length 2 giving the starting and ending
  date-time limits for ...}
\item{dt.fmt}{character; a date-time format as used by \code{\link{strptime}}.}
}

\details{
Something...
}

\value{
Something
}

\author{J.C. Fisher}

\seealso{
\code{\link{as.POSIXct}}, \code{\link{CRS}}, \code{\link{mean}},
\code{\link{median}}, \code{\link{sd}}
}

\examples{
f <- system.file("extdata/ESRP_WaterLevels.csv.gz", package = "ObsNetwork")
d <- read.table(file = f, sep = ",", header = TRUE, fill = TRUE,
                strip.white = TRUE, blank.lines.skip = TRUE,
                allowEscapes = TRUE, flush = TRUE,
                stringsAsFactors = FALSE)
projargs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
dt.lim <- c("2008-01-01 00:00", "2008-12-31 23:59")
obj <- ProcessObservations(d, x.var = "dec_long_va", y.var = "dec_lat_va",
                           projargs = projargs, siteno.var = "site_no",
                           sitenm.var = "station_nm", net.var = "network",
                           var1.var = "alt_lev_va", var2.var = "alt_va",
                           acy.var = "lev_acy", dt.var = "lev_dt",
                           dt.lim = dt.lim)
summary(obj)
}

\keyword{misc}
