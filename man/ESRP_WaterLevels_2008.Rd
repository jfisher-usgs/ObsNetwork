\name{ESRP_WaterLevels_2008}

\alias{ESRP_WaterLevels_2008}

\docType{data}

\title{Water-Table Data Set for the ESRP Aquifer}

\description{
A summary of water-table elevations recored at wells in 
the eastern Snake River Plain (ESRP) aquifer, Idaho.
}

\usage{
ESRP_WaterLevels_2008
}

\format{
An object of \code{\link{SpatialPointsDataFrame-class}} with 335 points
(coordinates). Geographic coordinates are in units of decimal degrees,
and in conformance with the North American Datum of 1983 (NAD 83).
This data set has the following attributes:
\describe{
  \item{siteno}{unique numerical identifier for each well site, for example
    \href{http://waterdata.usgs.gov/id/nwis/inventory/?site_no=435339112444601}{435339112444601};}
  \item{mapid}{numeric identifier used to locate well site on a figure;}
  \item{var1}{median water-table elevation for calendar year 2008, in meters 
    above the North American Vertical Datum of 1988 (NAVD 88);}
  \item{var2}{land surface elevation, in meters above NAVD 88;}
  \item{acy}{mean measurement accuracy of water-table elevations (\code{var1}), 
    in meters;}
  \item{sd}{standard deviation of water-table elevations for
    entire period of record, duration varies for each well site, in meters;}
  \item{net}{unique identifier for observation network. For this data set 
    "State" is used to identify wells in the 2008 state-run water-table 
    monitoring network (166 wells), and "INL" for wells in the 2008 U.S. 
    Geological Survey, Idaho National Laboratory, water-table monitoring 
    network (169 wells);}
  \item{sitenm}{local well identifier, for example
    "07N 31E 34BDD1 USGS 25".}
}
}

\source{
The National Water Information System
(\href{http://waterdata.usgs.gov/nwis}{NWIS}); accessed December 2011
using \href{https://github.com/jfisher-usgs/RNWIS}{RNWIS}.
\href{http://www.ngs.noaa.gov/PC_PROD/VERTCON/}{VERTCON} (version 2.1) was
used to convert National Geodetic Vertical Datum of 1929 (NGVD 29) heights
to NAVD 88. Data attributes \sQuote{var1}, \sQuote{acy}, and \sQuote{sd} were
calculated using the \code{\link{ProcessObservations}} function.
}

\examples{
data(ESRP_WaterLevels_2008)
plot(coordinates(ESRP_WaterLevels_2008))
summary(ESRP_WaterLevels_2008)
}

\keyword{datasets}
