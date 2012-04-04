\name{PlotRaster}

\alias{PlotRaster}

\title{Create a Filled Contour Plot of Spatial Data}

\description{
Raster plot for spatial data with attributes.
}

\usage{
PlotRaster(grd, zcol, pts, ply, rm.idxs, xlim, ylim, at,
           pal = heat.colors, contour = FALSE, label.pts = FALSE,
           main = "", gr.type = "windows", gr.file = NULL,
           width = 7, height = NA, lo = list(), ll.lines = FALSE)
}

\arguments{
\item{grd}{SpatialGridDataFrame; raster data.}
\item{zcol}{character; z-variable column name or number in attribute table
  of \code{grd}.}
\item{pts}{SpatialPointsDataFrame; point coordinates to include in layout.}
\item{ply}{SpatialPolygonsDataFrame; a polygon defining the spatial domain
  of the raster data; data outside this domain is excluded. Polygon is
  included in the layout.}
\item{rm.idxs}{integer; vector of row indexes in \code{pts}, point symbols at
  these coordinates are drawn as crosses (\code{pch=4}) rather than the
  default, an open circle symbol (\code{pch=21}).}
\item{xlim}{numeric; vector of length 2 giving left and right limits for
  the x-axis.}
\item{ylim}{numeric; vector of length 2 giving lower and upper limits for
  the y-axis.}
\item{at}{numeric; vector giving breakpoints along the range of z.}
\item{pal}{function; a color palette to be used to assign colors in the plot.}
\item{contour}{logical; add contour lines to plot, default is \code{FALSE}.}
\item{label.pts}{character; column name in \code{pts}, specifies the labels to
  place at point coordinates (optional).}
\item{main}{character; main plot title to be placed on top.}
\item{gr.type}{character; the name of a graphics driver, either
  "postscript", "pdf", "png", or the default "windows".}
\item{gr.file}{character; the path of a file for writing the graphics,
               only used if \code{gr.type} is not "windows".}
\item{width, height}{numeric; the (nominal) width and height of the canvas
                     of the plotting window in inches.}
\item{lo}{list; a list with more layout items, see \code{sp.layout} argument
  in \code{\link[sp]{spplot}}.}
\item{ll.lines}{logical; plot long-lat grid over projected data, default is
  \code{FALSE}.}
}

\details{
Spatial data is transformed to the map projection and datum of the
raster data set, \code{grd}.
}

\value{
Returns (or plots) the raster plot.
}

\author{J.C. Fisher}

\seealso{
\code{\link[sp]{spplot}}
}

\examples{
data(ESRP_NED)
data(ESRP_WaterLevels_2008)
data(ESRP_Boundary)
data(ESRP_Lakes)
data(ESRP_Rivers)
data(INL_Boundary)

# Plot topography
PlotRaster(ESRP_NED, "var2")

# Set axis limits, add points and long-lat grid to plot layout
pts <- ESRP_WaterLevels_2008[ESRP_WaterLevels_2008$net == "State", ]
xlim <- c(10000, 328000)
ylim <- c(81200, 335700)
PlotRaster(ESRP_NED, "var2", pts, ESRP_Boundary, xlim = xlim, ylim = ylim,
           pal = terrain.colors, label.pts = "mapid", ll.lines = TRUE)

# Plot hill shade, add lakes, river, and INL boundary to the plot layout
slp <- terrain(raster(ESRP_NED), opt = "slope")
asp <- terrain(raster(ESRP_NED), opt = "aspect")
grd <- as(hillShade(slp, asp, 40, 270), "SpatialGridDataFrame")
zlim <- range(grd[[1]], na.rm = TRUE)
at <- seq(zlim[1], zlim[2], length.out = 50)
pal <- function(n) grey(0:50 / 50)[1:n]
lo <- list()
lo[[1]] <- list("sp.polygons", ESRP_Lakes, col = "#1B70E0", fill = "#BAE4E5",
                first = FALSE)
lo[[2]] <- list("sp.lines", ESRP_Rivers, col = "#1B70E0", first = FALSE)
lo[[3]] <- list("sp.polygons", INL_Boundary, col = "#000000", first = FALSE)
PlotRaster(grd, 1, xlim = xlim, ylim = ylim, at = at, pal = pal, lo = lo)

# Set new axis limits
pts <- ESRP_WaterLevels_2008[ESRP_WaterLevels_2008$net == "INL", ]
xlim <- c(178000, 257500)
ylim <- c(202000, 272000)
PlotRaster(ESRP_NED, "var2", pts, ESRP_Boundary, xlim = xlim, ylim = ylim,
           pal = terrain.colors, ll.lines = TRUE)
}

\keyword{hplot}
