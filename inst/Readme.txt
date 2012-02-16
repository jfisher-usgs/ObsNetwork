
library(colorspace)
library(gstat)
library(genalg)
library(rgdal)
library(raster)
library(RSurvey)

# setwd("K:/Software/ObsNetwork")
setwd("D:/WORK/JFisher/Software/ObsNetwork")
RestoreSession(file.path(getwd(), "R"))

###





network <- "INL"
f.ply <- "INL_Polygon.gz"
xlim <- c(-113.3, -112.2)
ylim <- c(43.3, 44.0)



network <- "State"
f.ply <- "ESRP_Polygon.gz"
xlim <- c(-115.25, -111.5)
ylim <- c(42.25, 44.5)




krige.technique <- "OK"
vg.model <- vgm(model="Lin", nugget=0)
fit.vg <- TRUE


krige.technique <- "RK"
vg.model <- vgm(psill=4200, model="Sph", range=80, nugget=0)
fit.vg <- FALSE






###


path <- file.path(getwd(), "inst", "extdata")
f.obs <- "ESRP_Observations.gz"
f.dem <- "USGS_NED_1km.gz"
yr <- 2008
dx <- NULL
dt.lim <- c("2008-01-01 00:00", "2008-12-31 23:59")
nmax <- 50 # default is Inf


###


# Polygon
f <- file.path(path, f.ply)
ply <- read.table(f, header=TRUE, sep="\t", fill=TRUE, strip.white=TRUE,
                  blank.lines.skip=TRUE, allowEscapes=TRUE, flush=TRUE)
ply <- ply[, c("dec_long_va", "dec_lat_va")]
names(ply) <- c("x", "y")
ply <- Polygons(list(Polygon(ply, hole=FALSE)), "sp")
ply <- SpatialPolygons(list(ply), proj4string=CRS("+proj=longlat +datum=NAD83"))


# DEM
f <- file.path(path, f.dem)
dem <- read.asciigrid(f, as.image=FALSE, plot.image=FALSE, colname="alt",
                      proj4string=CRS("+proj=longlat +datum=NAD83"))
dem <- as(crop(raster(dem), extent(c(xlim, ylim))), 'SpatialGridDataFrame')

if (!is.null(dx)) {
  grd.par <- gridparameters(dem)
  if (dx > min(grd.par$cellsize)) {
    cellcentre.offset <- grd.par$cellcentre.offset +
                         dx / 2 - grd.par$cellsize[1] / 2
    cellsize <- c(dx, dx)
    cells.dim <- c(diff(bbox(dem)[1, ]) / dx + 1,
                   diff(bbox(dem)[2, ]) / dx + 1)
    gt <- GridTopology(cellcentre.offset, cellsize, cells.dim)
    sg <- SpatialGrid(gt, proj4string=CRS("+proj=longlat +datum=NAD83"))
    dem <- aggregate(dem, sg)
  } else {
    warning("")
  }
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







































#### STOP ######################################################################


f <- file.path(path, paste("Map", map.id, "_SpatialDomain.gz", sep=""))
grd <- BuildGrid(file=f, x.var="Longitude", y.var="Latitude", dx=dx)


lm.drift <- lm(observation ~ x + y, data=obs)
coeff <- as.numeric(coefficients(lm.drift))
drift <- function(x) coeff[1] + coeff[2] * x[, 1] + coeff[3] * x[, 2]
obs$residual <- obs$observation - drift(coordinates(obs))

# plot3d(x=cbind(coordinates(obs), drift(coordinates(obs))),
#        col="red", xlab="x", ylab="y", zlab="z")
# plot3d(x=cbind(coordinates(obs), obs$observation), col="blue", add=TRUE)


v <- gstat::variogram(gstat(id="residual", formula=residual~1, data=obs))

# model <- fit.variogram(object=v, model=vgm(model="Lin", nugget=0))

# model <- vgm(model="Sph", nugget=0, range=35, psill=130)

model <- vgm(model="Sph", nugget=0, range=80, psill=28000)

plot(v, model=model)



# RunCrossvalidation(obs, model)

PlotKriging(obs, model, grd, drift)



#### EXAMPLE ###################################################################

## Local universal kriging, using one continuous variable
## the variogram should be that of the residual:

data(meuse)
data(meuse.grid)

coordinates(meuse) <- ~x+y
gridded(meuse.grid) <- ~x+y

x <- krige(log(zinc) ~ sqrt(dist), meuse, meuse.grid,
           model = vgm(.149, "Sph", 700, .0674), nmax = 40)

spplot(x, zcol="var1.pred")

#### Regression Kriging ####

## http://spatial-analyst.net/wiki/index.php?title=Regression-kriging_guide

f <- file.path(path, paste("Map", map.id, "_NED1km.gz", sep=""))
elev <- read.asciigrid(f, as.image=FALSE, plot.image=FALSE, colname="elev",
                       proj4string=CRS("+proj=longlat +datum=NAD83"))

spplot(elev, scales=list(draw=TRUE))











########################### OLD README ########################################

file.obs <- file.path(dir.path, "inst", "extdata", "ObservationData.txt")
obs <- ReadObservations(file=file.obs, x.var="Longitude", y.var="Latitude",
                        site.var="Site_ID", obs.var="WL_elev",
                        acc.var="Accuracy")

###

file.ply <- file.path(dir.path, "inst", "extdata", "SpatialDomain.txt")
grd.gr <- BuildGrid(file=file.ply, x.var="Longitude", y.var="Latitude", dx=0.01)
grd.ga <- BuildGrid(file=file.ply, x.var="Longitude", y.var="Latitude", dx=0.03)

###

#for (i in c(0, 45, 90, 135)) {
#  model <- vgm(psill=190000, model="Gau", range=112, nugget=0, anis=c(i, 0.5))
#  FitVariogram(obs, model)
#}

v.fit <- FitVariogram(obs, model=vgm(model="Lin", nugget=0))

#v.fit <- FitVariogram(obs, model=vgm(psill=190000, model="Gau",
#                                     range=112, nugget=0))

###

RunCrossvalidation(obs, v.fit)

###

#PlotKriging(obs, v.fit, grd.gr, at.pred=seq(2600, 6000, by=200),
#            at.se=seq(30, 70, by=2))

PlotKriging(obs, v.fit, grd.gr)

graphics.off()

#PlotKriging(obs, v.fit, grd.gr, rm.idxs=c(1, 20))

#PlotKriging(obs, v.fit, grd.gr, rm.idxs=50:100)

###

#ga <- RunGA(obs, v.fit, grd.ga, nsites=10, niters=10)

ga <- RunGA(obs, v.fit, grd.ga, nsites=10)

summary.rbga(ga$ans, echo=TRUE)
PlotKriging(obs, v.fit, grd.gr, rm.idxs=ga$rm.idxs)

