\name{PlotRaster}

\alias{PlotRaster}

\title{Create a Filled Contour Plot of Spatial Data}

\description{
Raster plot for spatial data with attributes.
}

\usage{
PlotRaster(grd, zcol, pts, ply, net.idxs, rm.idxs, xlim, ylim, at,
           pal = heat.colors, contour = FALSE, label.contours = FALSE,
           label.pts = FALSE, main = "", gr.type = "windows",
           gr.file = NULL, width = 7, height = NA, lo = list(),
           ll.lines = FALSE)
}

\arguments{
\item{grd}{\code{SpatialGridDataFrame}; the raster data.}
\item{zcol}{\code{character}; the z-variable column name or number in attribute table of \code{grd}.}
\item{pts}{\code{SpatialPointsDataFrame}; the point coordinates to include in layout.}
\item{ply}{\code{SpatialPolygonsDataFrame}; a polygon defining the spatial domain of the raster data; data outside this domain is excluded.
  Polygon is included in the layout.}
\item{net.idxs}{\code{integer}; a vector of row indexes in \code{pts}, point coordinates at these sites are drawn as circle symbols with white backgrounds.
  All other circle symbols are drawn with gray backgrounds.}
\item{rm.idxs}{\code{integer}; a vector of row indexes in \code{pts}, point symbols at these sites are drawn as crosses.}
\item{xlim}{\code{numeric}; a vector of length 2 giving left and right limits for the x-axis.}
\item{ylim}{\code{numeric}; a vector of length 2 giving lower and upper limits for the y-axis.}
\item{at}{\code{numeric}; a vector giving breakpoints along the range of z (including upper and lower limits).}
\item{pal}{\code{function}; a color palette to be used to assign colors in the plot.}
\item{contour}{\code{logical}; whether to add contour lines to plot, default is \code{FALSE}.}
\item{label.contours}{\code{logical}; whether to label contour lines, default is \code{FALSE}.}
\item{label.pts}{\code{character} or \code{logical}; the attribute in \code{pts} specifying the labels to place at point coordinates.
  If \code{TRUE}, points are labeled with index numbers.}
\item{main}{\code{character}; the main plot title to be placed on top.}
\item{gr.type}{\code{character}; the name of a graphics driver, either "postscript", "pdf", "png", or the default "windows".}
\item{gr.file}{\code{character}; the path of a file for writing the graphics, only used if \code{gr.type} is not "windows".}
\item{width, height}{\code{numeric}; the (nominal) width and height of the canvas of the plotting window in inches.}
\item{lo}{\code{list}; a list with more layout items, see \code{sp.layout} argument in \code{\link[sp]{spplot}}.}
\item{ll.lines}{\code{logical}; whether to plot longitude-latitude grid over projected data, default is \code{FALSE}.}
}

\details{
Spatial data is transformed to the map projection and datum of the raster data set, \code{grd}.
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
data(ESRP_NWIS)
data(ESRP_Boundary)
data(ESRP_Lakes)
data(ESRP_Rivers)
data(INL_Boundary)

# Plot topography
PlotRaster(ESRP_NED, "var2")

# Set axis limits, add points and long-lat grid to plot layout
pts <- ESRP_NWIS[ESRP_NWIS$network.nm == "State", ]
xlim <- c(10000, 328000)
ylim <- c(81200, 335700)
PlotRaster(ESRP_NED, "var2", pts, ESRP_Boundary, xlim = xlim, ylim = ylim,
           pal = terrain.colors, label.pts = "map.no", ll.lines = TRUE)

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
pts <- ESRP_NWIS[ESRP_NWIS$network.nm == "INL", ]
xlim <- c(178000, 257500)
ylim <- c(202000, 272000)
PlotRaster(ESRP_NED, "var2", pts, ESRP_Boundary, xlim = xlim, ylim = ylim,
           pal = terrain.colors, ll.lines = TRUE, contour = TRUE)
}

\keyword{hplot}
