\name{ESRP_NWIS}

\alias{ESRP_NWIS}

\docType{data}

\title{Water-Level Data Set for the ESRP Aquifer}

\description{
A summary of water-level elevation measurements from wells located in the eastern Snake River Plain (ESRP) aquifer, Idaho.
}

\usage{
ESRP_NWIS
}

\format{
An object of \code{\link{SpatialPointsDataFrame-class}} with 335 points (coordinates).
Geographic coordinates are in units of decimal degrees, and in conformance with the North American Datum of 1983 (NAD 83).
This data set has the following variables:
\describe{
\item{site.no}{is a unique numerical identifier for each well site,
  for example \href{https://waterdata.usgs.gov/id/nwis/inventory/?site_no=435339112444601}{435339112444601}.}
\item{var1}{is the median water-level elevation for calendar year 2008, in meters above the North American Vertical Datum of 1988 (NAVD 88).}
\item{var1.acy}{is the mean measurement accuracy of water-level elevations, in meters, for calendar year 2008.}
\item{var1.sd}{is the standard deviation of water-level elevations, in meters, for entire period of record; duration varies for each well site.}
\item{var2}{is a land-surface reference point elevation, in meters above the NAVD 88.}
\item{map.no}{is a numeric identifier used to locate well sites on map.}
\item{network.nm}{is an identifier for water-level monitoring network.
  For this data set "State" is used to identify wells in the 2008
  Federal-State Cooperative water-level monitoring network (166 wells), and "INL" for wells in the 2008 U.S. Geological Survey-Idaho National Laboratory water-level monitoring network (171 wells).
  Wells coded as "State, INL" belong to both monitoring networks.}
\item{nrec.por}{is the number of records in the entire period of record.}
\item{nrec}{is the number of records in calendar year 2008.}
\item{alt.acy.va}{is the accuracy of land-surface reference point elevation, in meters.}
\item{lev.acy.va}{is the mean water-level measurement accuracy, in meters, for calendar year 2008.}
\item{coord.acy.va}{is the latitude/longitude coordinate accuracy, in arc-seconds.}
\item{coord.meth.cd}{is the method used to determine horizontal datum.}
\item{alt.meth.cd}{is the method used to determine land-surface datum.}
\item{lev.meth.cd}{are all methods used to determine water levels; comma separated.}
\item{site.nm}{is the local well identifier, for example "07N 31E 34BDD1 USGS 25".}
}
}

\source{
The National Water Information System (\href{https://waterdata.usgs.gov/nwis}{NWIS});
accessed January 2013 using \href{https://github.com/jfisher-usgs/RNWIS}{RNWIS} (version 0.1-8).
The software \href{https://www.ngs.noaa.gov/PC_PROD/VERTCON/}{VERTCON} (version 2.1) was used to convert National Geodetic Vertical Datum of 1929 (NGVD 29) heights to NAVD 88.
The final data table was constructed using the \code{\link{ReadNWISData}} pre-processing function.
}

\examples{
data(ESRP_NWIS)
plot(coordinates(ESRP_NWIS))
str(ESRP_NWIS)
}

\keyword{datasets}
