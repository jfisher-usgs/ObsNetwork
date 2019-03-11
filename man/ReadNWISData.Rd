\name{ReadNWISData}

\alias{ReadNWISData}

\title{Read USGS NWIS Data}

\description{
This function reads water-level measurements exported from the U.S. Geological Survey (USGS) National Water Information System (NWIS) database and summarizes this data for a specified duration of time.
}

\usage{
ReadNWISData(file, dt.lim, dt.fmt = "\%Y-\%m-\%d \%H:\%M", sep = "\t")
}

\arguments{
\item{file}{\code{character}; a path to the file to be opened.}
\item{dt.lim}{\code{character}; a vector of length 2 giving the starting and ending date-time limits.
  Measurements recorded during this time period are used to calculate the median water-level elevation \code{var1} and mean measurement accuracy \code{var1.acy}.
  Defaults to the entire period of record.}
\item{dt.fmt}{\code{character}; a date-time format as used by \code{\link{strptime}}.
  Date-time values specified in \code{dt.lim} must be provided in this format.}
\item{sep}{\code{character}; the field separator string.
  Values on each line of the file are separated by this string, its default is the tab separator.}
}

\format{
The input text file contains the names of the variables in its first line.
Each subsequent line corresponds to a single water-level measurement.
Measurement variables (columns in the data table) include:
\describe{
\item{SITE_NO}{is the site number.}
\item{DEC_LONG_VA}{is the decimal longitude.}
\item{DEC_LAT_VA}{is the decimal latitude.}
\item{COORD_ACY_CD}{is the latitude/longitude coordinate accuracy code (optional).}
\item{COORD_DATUM_CD}{is the latitude/longitude (horizontal) coordinate datum code (optional).}
\item{COORD_METH_CD}{is the code indicating the method used to determine horizontal datum (optional).}
\item{ALT_VA}{is the gage or land-surface datum, in feet.}
\item{ALT_ACY_VA}{is the accuracy of land-surface datum, in feet.}
\item{ALT_DATUM_CD}{is a code indicating the geodetic or local vertical datum of the elevation datum elevation component (optional); for example, "NGVD29" or "NAVD88".}
\item{ALT_METH_CD}{is a code indicating the method used to determine the elevation of land-surface datum (optional).}
\item{HOLE_DEPTH_VA}{is the borehole depth, in feet (optional).}
\item{WELL_DEPTH_VA}{is the well depth, in feet (optional).}
\item{LEV_DT}{is the date/time when water-level measurement was recorded.
  The expected conversion specification format is "\%Y-\%m-\%d \%H:\%M:\%S"; see \code{\link{strftime}} for details.}
\item{LEV_VA}{is the water-level measurement referenced to land-surface datum (that is, depth below land surface), in feet.}
\item{LEV_ACY_CD}{is a water-level measurement accuracy code.}
\item{LEV_METH_CD}{is a code indicating how the water level was measured (optional).}
\item{STATION_NM}{is the site name (optional).}
\item{NETWORK_NM}{is the water-level monitoring network name (optional).
  Sites belonging to multiple networks should be comma separated.
  This variable is not included in the NWIS database and it is tasked to the user to populate this field.}
}
}

\details{
This function is a pre-processor for \pkg{ObsNetwork}.
Groundwater data can be downloaded at \href{https://waterdata.usgs.gov/id/nwis/gw}{NWISWeb}.
}

\value{
Returns an object of \code{\link{SpatialPointsDataFrame-class}} with the following components:
\item{site.no}{\code{numeric}; a unique identifier for site.}
\item{var1}{\code{numeric}; the median value of the water-level elevation, in meters, for the time period defined by \code{dt.lim}.}
\item{var1.acy}{\code{numeric}; the mean measurement accuracy of the water-level elevation, in meters, for the time period defined by \code{dt.lim}.
  This variable is calculated from the summation of \code{alt.acy.va} and \code{lev.acy.va} values.}
\item{var1.sd}{\code{numeric}; the standard deviation of the water-level elevation, in meters, for the entire period of record.}
\item{var2}{\code{numeric}; the land-surface datum, in meters.}
\item{map.no}{\code{integer}; an identifier (row index number) used to locate sites on map.}
\item{network.nm}{\code{character}; the monitoring network name.}
\item{nrec.por}{\code{integer}; the number of records in the entire period of record.}
\item{nrec}{\code{integer}; the number of records in the time period defined by \code{dt.lim}.}
\item{alt.acy.va}{\code{numeric}; the accuracy of land-surface datum, in meters.}
\item{lev.acy.va}{\code{numeric}; the mean water-level measurement accuracy, in meters, for the time period defined by \code{dt.lim}.}
\item{coord.acy.va}{\code{numeric}; the latitude/longitude coordinate accuracy, in arc-seconds.}
\item{coord.meth.cd}{\code{character}; the method used to determine horizontal datum.}
\item{alt.meth.cd}{\code{character}; the method used to determine elevation at land-surface datum.}
\item{lev.meth.cd}{\code{character}; all methods used to determine water levels, comma separated.}
\item{site.nm}{\code{character}; the site name.}
}

\author{J.C. Fisher}

\seealso{
\code{\link{read.table}}, \code{\link{as.POSIXct}}
}

\examples{
# Read data from text file
file <- system.file("extdata/ESRP_NWIS.tsv", package = "ObsNetwork")
dt.lim <- c("2008-01-01 00:00", "2008-12-31 23:59") # 2008 calendar year
obj <- ReadNWISData(file, dt.lim)
str(obj)

# Compare with available data set
data(ESRP_NWIS)
identical(obj, ESRP_NWIS)
}

\keyword{manip}
