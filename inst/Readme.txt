
library(colorspace)
library(gstat)
library(genalg)
library(rgdal)
library(raster)
library(RSurvey)

setwd("K:/Software/ObsNetwork")
# setwd("D:/WORK/JFisher/Software/ObsNetwork")
RestoreSession(file.path(getwd(), "R"))

###





network <- "INL"
f.ply <- "INL_Polygon.gz"
xlim <- c(-113.3, -112.2)
ylim <- c(43.3, 44.0)
dem.fact <- 1


network <- "State"
f.ply <- "ESRP_Polygons.gz"
xlim <- c(-115.25, -111.5)
ylim <- c(42.25, 44.5)
dem.fact <- 2



krige.technique <- "OK"
vg.model <- vgm(model="Lin", nugget=0)
fit.vg <- TRUE



krige.technique <- "RK"
vg.model <- vgm(psill=4200, model="Sph", range=80, nugget=0)
fit.vg <- FALSE




###


f.dem <- "USGS_NED_500m.gz"
path <- file.path(getwd(), "inst", "extdata")
f.obs <- "ESRP_Observations.gz"
yr <- 2008
dt.lim <- c("2008-01-01 00:00", "2008-12-31 23:59")
nmax <- 50 # default is Inf


###


# Polygon(s)
f <- file.path(path, f.ply)
d <- read.table(f, header=TRUE, sep="\t", fill=TRUE, strip.white=TRUE,
                blank.lines.skip=TRUE, allowEscapes=TRUE, flush=TRUE)

if (ncol(d) > 2) {
  lst <- list()
  cd <- unique(d[, 3])
  hole <- FALSE
  for (i in seq(along=cd)) {
    lst[[i]] <- Polygon(d[d[, 3] == cd[i], 1:2], hole=hole)
    hole <- TRUE
  }
} else {
  lst <- list(Polygon(d[, 1:2], hole=FALSE))
}
ply <- SpatialPolygons(list(Polygons(lst, "sd")),
                       proj4string=CRS("+proj=longlat +datum=NAD83"))


# Grid map
f <- file.path(path, f.dem)
dem <- read.asciigrid(f, as.image=FALSE, plot.image=FALSE, colname="alt",
                      proj4string=CRS("+proj=longlat +datum=NAD83"))
dem <- as(crop(raster(dem), extent(c(xlim, ylim))), 'SpatialGridDataFrame')

if (dem.fact > 1) {
  dem <- as(aggregate(raster(dem), fact=dem.fact, fun=mean, expand=TRUE,
                      na.rm=TRUE), 'SpatialGridDataFrame')
}


# Observations
f <- file.path(path, f.obs)
obs <- ReadObservations(file=f, x.var="dec_long_va", y.var="dec_lat_va",
                        site.var="site_no", net.var="network", alt.var="alt_va",
                        hole.var="hole_depth_va", lev.var="lev_va",
                        acy.var="lev_acy", dt.var="lev_dt", dt.lim=dt.lim)
idxs <- zerodist(obs, zero=0.0, unique.ID=FALSE)
if (nrow(idxs) > 0)
  stop()
obs$alt.lev <- obs$alt - obs$lev


PlotMap(dem, "alt", obs[obs$net == network, ], ply,
        xlim=xlim, ylim=ylim, pal=1L, contour=FALSE)


# Drift
lm.drift <- lm(alt.lev ~ x + y, data=obs)
summary(lm.drift)
## plot3d(x=cbind(coordinates(obs), drift(coordinates(obs))),
##        col="red", xlab="x", ylab="y", zlab="z")
## plot3d(x=cbind(coordinates(obs), obs$alt.lev), col="blue", add=TRUE)


# Variogram model (Ordinary-kriging and Regression-kriging)
if (krige.technique == "OK") {
  fo <- alt.lev~1
} else {
  fo <- alt.lev~alt
}
vg <- variogram(fo, obs)
if (fit.vg)
  vg.model <- fit.variogram(vg, vg.model)
plot(vg, vg.model)


# Reduce size
is.in.ply <- !is.na(over(obs, ply, fn=mean))
if (sum(as.integer(is.in.ply)) < nrow(obs))
  warning("")
obs <- obs[is.in.ply, ]
dem$alt <- dem$alt * over(dem, ply, fn=mean)


# Kriging interpolation
kr <- krige(formula=fo, locations=obs, newdata=dem, model=vg.model, nmax=nmax)
kr$var1.se <- sqrt(kr$var1.var)

PlotMap(kr, "var1.pred", obs, ply, xlim=xlim, ylim=ylim, pal=2L)
PlotMap(kr, "var1.se",   obs, ply, xlim=xlim, ylim=ylim, pal=3L, contour=FALSE)











