\name{ProcessObservations}

\alias{ProcessObservations}

\title{Process Observation Data}

\description{
This function summarizes data records for each observation site in a table.
}

\usage{
ProcessObservations(d, x.var, y.var, projargs, siteno.var, sitenm.var,
                    net.var, var1.var, var2.var, acy.var, dt.var, dt.lim,
                    dt.fmt = "\%Y-\%m-\%d \%H:\%M")
}

\arguments{
\item{d}{data.frame; data at observation sites.}
\item{x.var, y.var}{character; x- and y-variable column name in \code{d},
  respectively.}
\item{projargs}{character; projection arguments to be entered exactly as in the
  \href{http://trac.osgeo.org/proj/}{PROJ.4} documentation.}
\item{siteno.var}{character; site-number-variable column name in \code{d}.}
\item{sitenm.var}{character; site-name-variable column name in \code{d}.}
\item{net.var}{character; network-variable column name in \code{d}.}
\item{var1.var}{character; dependent-variable column name in \code{d}.}
\item{var2.var}{character; independent-variable column name in \code{d}.}
\item{acy.var}{character; measurement-accuracy-variable column name in
  \code{d}.}
\item{dt.var}{character; date-time-variable column name in \code{d}.}
\item{dt.lim}{character; vector of length 2 giving the starting and ending
  date-time limits. Observations recorded during this time period are used
  to calculate the median independent variable \code{var1}
  and mean measurement accuracy \code{acy}.}
\item{dt.fmt}{character; a date-time format as used by \code{\link{strptime}}.
              Date-time specified in the \code{dt.var} field and
              \code{dt.lim} values must be provided in this format.}
}

\details{
The site-number variable is used to organize data records and must be unique
for each observation site and coordinate location. The output table will
contain one record for each site number.
}

\value{
Returns a data frame with variables:
\item{siteno}{numeric; unique identifier for observation site.}
\item{mapid}{numeric; identifier (row index number) used to locate observation
  site on a figure, see \code{label.pts} in \code{\link{PlotRaster}}.}
\item{var1}{numeric; the median value of the dependent variable
  (such as the water-table elevation) recorded during the time period defined
  by \code{dt.lim}.}
\item{var2}{numeric; independent variable (such as the land-surface elevation).}
\item{acy}{numeric; mean measurement accuracy during the time period defined
  by \code{dt.lim}.}
\item{sd}{numeric; standard deviation of the dependent variable over the entire
  period of record.}
\item{net}{character; identifier for observation network.}
\item{sitenm}{character; local observation site identifier.}
}

\author{J.C. Fisher}

\seealso{
\code{\link{as.POSIXct}}, \code{\link[sp]{CRS}}, \code{\link{median}},
\code{\link{mean}}, \code{\link{sd}}
}

\examples{
# Read data from text file
f <- system.file("extdata/ESRP_WaterLevels.csv.gz", package = "ObsNetwork")
d <- read.table(file = f, sep = ",", header = TRUE, fill = TRUE, flush = TRUE,
                strip.white = TRUE, blank.lines.skip = TRUE,
                allowEscapes = TRUE, stringsAsFactors = FALSE)
summary(d)

# Establish geographic coordinate system; in decimal degrees, WGS 84
projargs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
dt.lim <- c("2008-01-01 00:00", "2008-12-31 23:59") # 2008 calendar year
obj <- ProcessObservations(d, x.var = "dec_long_va", y.var = "dec_lat_va",
                           projargs = projargs, siteno.var = "site_no",
                           sitenm.var = "station_nm", net.var = "network",
                           var1.var = "alt_lev_va", var2.var = "alt_va",
                           acy.var = "lev_acy", dt.var = "lev_dt",
                           dt.lim = dt.lim)
summary(obj)

# Compare with available data set
data(ESRP_WaterLevels_2008)
all.equal(obj, ESRP_WaterLevels_2008)
}

\keyword{manip}
