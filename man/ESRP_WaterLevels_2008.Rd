\name{ESRP_WaterLevels_2008}

\alias{ESRP_WaterLevels_2008}

\docType{data}

\title{Water-Level Data Set for the ESRP Aquifer}

\description{
This data set gives locations and 2008 water-level measurements,
along with a number of well site variables at the observed locations,
collected in the Eastern Snake River Plain (ESRP) aquifer, Idaho.
}

\usage{
ESRP_WaterLevels_2008
}

\format{
An object of \code{\link{SpatialPointsDataFrame-class}} with 335 points
(coordinates) in units of decimal degrees, in conformance with the North
American Datum of 1983 (NAD 83), and the following attributes:
  \describe{
    \item{siteno}{unique numerical identifier for well site;}
    \item{mapid}{numeric identifier used to locate well sites on a figure;}
    \item{var1}{mean water level for calendar year 2008 in meters above the
                North American Vertical Datum of 1988 (NAVD 88);}
    \item{var2}{land surface elevation in meters above NAVD 88;}
    \item{acy}{measurement accuracy of water-level elevation (\code{var1})
               in meters;}
    \item{sd}{standard deviation of water level elevations (\code{var1}) for
              entire period of record, varies for each well site;}
    \item{net}{identifier for observation network, "State" identifies
               wells in the state-run 2008 synoptic sampling network (n = 166)
               and "INL" identifies wells in the U.S. Geological Survey,
               Idaho National Laboratory monitoring network (n = 169);}
    \item{sitenm}{local well identifier.}
  }
}

\source{
The National Water Information System
(\href{http://waterdata.usgs.gov/nwis}{NWIS}); accessed December 2011.
\href{http://www.ngs.noaa.gov/PC_PROD/VERTCON/}{VERTCON} (version 2.1) was
used to convert National Geodetic Vertical Datum of 1929 (NGVD 29) heights
to NAVD 88. Data attributes \sQuote{acy} and \sQuote{sd} were
calculated using \code{\link{ProcessObservations}}.
}

\examples{
data(ESRP_WaterLevels_2008)
plot(coordinates(ESRP_WaterLevels_2008))
summary(ESRP_WaterLevels_2008)

# Commands used to construct this data object:
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

all.equal(ESRP_WaterLevels_2008, obj)
}

\keyword{datasets}
