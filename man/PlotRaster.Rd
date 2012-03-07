\name{PlotRaster}

\alias{PlotRaster}

\title{Create a Filled Contour Plot of Spatial Data}

\description{
Something.
}

\usage{
PlotRaster(grd, zcol, pts, ply, rm.idxs, xlim, ylim, at,
           pal = terrain.colors, contour = TRUE, label.pts = FALSE,
           main = "", gr.type = "windows", gr.file = NULL)
}

\arguments{
\item{grd}{SpatialGridDataFrame; raster data.}
\item{zcol}{character; z-variable column name.}
\item{pts}{SpatialPointsDataFrame; point coordinates to include in layout.}
\item{ply}{SpatialPolygonsDataFrame; polygon to include in layout.}
\item{rm.idxs}{integer; }
\item{xlim}{numeric; vector of length 2 giving left and right limits for
  the x-axis.}
\item{ylim}{numeric; vector of length 2 giving lower and upper limits for
  the y-axis.}
\item{at}{something}
\item{pal}{something}
\item{contour}{logical; }
\item{label.pts}{logical; }
\item{main}{character; main title to be placed on top.}
\item{gr.type}{character; the name of a graphics driver, either
  "pdf", "postscript", "png", or "windows".}
\item{gr.file}{character; the name of a file for writing the graphics,
               only used if \code{gr.type} is not "windows".}
}

\value{
Returns (or plots) the raster plot.
}

\author{J.C. Fisher}

\seealso{
\code{\link{spplot}}
}

\examples{
data(ESRP_NED500m)
data(ESRP_WaterLevels_2008)
data(ESRP_SpatialDomain)

PlotRaster(ESRP_NED500m, "var2", ESRP_WaterLevels_2008, ESRP_SpatialDomain,
           contour = FALSE, pal = heat.colors)

obs <- ESRP_WaterLevels_2008[ESRP_WaterLevels_2008$net == "State", ]
PlotRaster(ESRP_NED500m, "var2", obs, ESRP_SpatialDomain,
           xlim = c(-115.25, -111.5), ylim = c(42.25, 44.5),
           contour = FALSE, label.pts = "mapid")
}

\keyword{hplot}
