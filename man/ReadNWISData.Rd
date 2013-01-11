\name{ReadNWISData}

\alias{ReadNWISData}

\title{Read USGS NWIS Data}

\description{
This function reads water-level measurements exported from the USGS
National Water Information System (NWIS) and summarizes this data
for a specified duration of time.
}

\usage{
ReadNWISData(file, dt.lim, dt.fmt = "\%Y-\%m-\%d \%H:\%M", sep = "\t")
}

\arguments{
\item{file}{character; a path to the file to be opened.}
\item{dt.lim}{character; vector of length 2 giving the starting and ending
  date-time limits. Measurements recorded during this time period are used
  to calculate the median water-level elevation \code{var1}
  and mean measurement accuracy \code{acy}. Defaults to the entire period 
  of record.}
\item{dt.fmt}{character; a date-time format as used by \code{\link{strptime}}.
              Date-time values specified in \code{dt.lim} must be provided 
              in this format.}
\item{sep}{character; the field separator string. Values on each line of the
  file are separated by this string, its default is a tab separator.}
}

\format{
The input file contains the names of the variables in its first line.
Each subsequent line corresponds to a single water-level measurement.
Measurement variables (columns in the data table) include: 
\describe{
  \item{SITE_NO}{Site number.}
  \item{DEC_LONG_VA}{Decimal longitude.}
  \item{DEC_LAT_VA}{Decimal latitude.}
  \item{COORD_ACY_CD}{Latitude/longitude coordinate accuracy (optional).}
  \item{COORD_DATUM_CD}{Latitude/longitude (horizontal) coordinate datum 
    (optional).}
  \item{COORD_METH_CD}{Method used to determine horizontal datum (optional).}
  \item{ALT_VA}{Gage or land-surface datum in feet.}
  \item{ALT_ACY_VA}{Accuracy of land-surface datum in feet.}
  \item{ALT_DATUM_CD}{The geodetic or local vertical datum of the elevation 
    datum elevation component (optional). For example, NGVD29 and NAVD88.}
  \item{ALT_METH_CD}{Method used to determined elevation of land-surface 
    datum.}
  \item{HOLE_DEPTH_VA}{Hole depth (optional).}
  \item{WELL_DEPTH_VA}{Well depth (optional).}
  \item{LEV_DT}{Water-level date/time for groundwater sites.}
  \item{LEV_VA}{Water-level measurement referenced to land-surface datum 
    in feet.}
  \item{LEV_ACY_CD}{Water-level measurement accuracy code.}
  \item{LEV_METH_CD}{Code indicating how the water level was measured 
    (optional).}
  \item{STATION_NM}{Site name.}
  \item{NETWORK}{Water-level monitoring network name (optional). Sites belonging
    to multiple networks should be comma separated.} 
}
}

\details{
This function is a pre-processor for \pkg{ObsNetwork}. Groundwater data can 
be downloaded at \href{http://waterdata.usgs.gov/id/nwis}{NWISWeb}.
}

\value{
Returns an object of class \code{\link{SpatialPointsDataFrame}} with components:
\item{siteno}{numeric; unique identifier for site.}
\item{var1}{numeric; the median value of the water-table elevation in meters, 
  for the time period defined by \code{dt.lim}.}
\item{acy}{numeric; mean measurement accuracy of the water-table elevation 
  in meters, for the time period defined by \code{dt.lim}. This variable
  is calculated from the summation of \code{alt.acy.va} and \code{lev.acy.va} 
  values.}
\item{sd}{numeric; standard deviation of the water-table elevation in meters, 
  for the entire period of record.}
\item{mapno}{integer; identifier (row index number) used to locate 
  sites on a map.}
\item{network}{character; water-level monitoring network name.}
\item{nrec.por}{integer; number of records in the entire period of record.}
\item{nrec}{integer; number of records in the time period defined by 
  \code{dt.lim}.}
\item{alt.va}{numeric; land-surface datum in meters.}
\item{alt.acy.va}{numeric; land-surface datum accuracy in meters.}
\item{lev.acy.va}{numeric; water-level measurement accuracy in meters.}
\item{coord.acy.va}{numeric; latitude/longitude coordinate accuracy in 
  arc-seconds.}
\item{coord.meth.cd}{character; method used to determine horizontal datum.}
\item{alt.meth.cd}{character; method used to determine elevation at  
  land-surface datum.}
\item{lev.meth.cd}{character; all methods used to measure water levels, 
  comma separated.}
\item{sitenm}{character; site name.}
}

\author{J.C. Fisher}

\seealso{
\code{\link{read.table}}, \code{\link{as.POSIXct}}, \code{\link[sp]{CRS}}
}

\examples{
# Read data from text file
file <- system.file("extdata/ESRP_WaterLevels.txt.gz", package = "ObsNetwork")
dt.lim <- c("2008-01-01 00:00", "2008-12-31 23:59") # 2008 calendar year
obj <- ReadNWISData(file, dt.lim)
str(obj)

# Compare with available data set
data(ESRP_WaterLevels_2008)
identical(obj, ESRP_WaterLevels_2008)
}

\keyword{manip}