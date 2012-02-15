
library(colorspace)
library(gstat)
library(genalg)
library(rgdal)
library(RSurvey)

# setwd("K:/Software/ObsNetwork")
setwd("D:/WORK/JFisher/Software/ObsNetwork")
RestoreSession(file.path(getwd(), "R"))

###


f.obs <- "INL_Data.gz"
f.ply <- "INL_SpatialDomain.gz"

f.obs <- "ESRP_Data.gz"
f.ply <- "ESRP_SpatialDomain.gz"



f.dem <- "ESRP_NED1km.gz"
path <- file.path(getwd(), "inst", "extdata")


yr <- 2008
dt.lim <- paste(yr, c("-01-01 00:00", "-12-31 23:59"), sep="")
f <- file.path(path, f.obs)
obs <- ReadObservations(file=f, x.var="dec_long_va", y.var="dec_lat_va",
                        site.var="site_no", alt.var="alt_va",
                        hole.var="hole_depth_va", lev.var="lev_va",
                        acy.var="lev_acy", dt.var="lev_dt", dt.lim=dt.lim)

# idxs <- zerodist(obs, zero=0.0, unique.ID=FALSE)
# site1 <- obs$site[idxs[, 1]]
# site2 <- obs$site[idxs[, 2]]
# hole1 <- obs$hole[idxs[, 1]]
# hole2 <- obs$hole[idxs[, 2]]
# rm.sites <- rep(NA, nrow(idxs))
# rm.sites[hole1 > hole2] <- site1[hole1 > hole2]
# rm.sites[hole1 < hole2] <- site2[hole1 < hole2]
# coord <- coordinates(obs)[idxs[, 1], ]
# dup.sites <- cbind(site1, site2, coord, hole1, hole2, rm.sites)
# write.table(dup.sites, "clipboard", quote=FALSE, sep="\t",
#             row.names=FALSE, col.names=FALSE)


obs <- remove.duplicates(obs, zero=0.0, remove.second=TRUE)





# All elevation values are in meters and are referenced to the NAVD 88.
f <- file.path(path, f.dem)
dem <- read.asciigrid(f, as.image=FALSE, plot.image=FALSE, colname="alt",
                      proj4string=CRS("+proj=longlat +datum=NAD83"))

f <- file.path(path, f.ply)
ply <- read.table(f, header=TRUE, sep="\t", fill=TRUE, strip.white=TRUE,
                  blank.lines.skip=TRUE, allowEscapes=TRUE, flush=TRUE)
ply <- ply[, c("dec_long_va", "dec_lat_va")]
names(ply) <- c("x", "y")
ply <- Polygons(list(Polygon(ply, hole=FALSE)), "sp")
ply <- SpatialPolygons(list(ply), proj4string=CRS("+proj=longlat +datum=NAD83"))

lay1 <- list("sp.points", obs, pch=21, cex=0.5, col="black", fill="white", first=FALSE)
lay2 <- list("sp.polygons", ply, col="black", first=FALSE)
spplot(dem, scales=list(draw=TRUE), sp.layout=list(lay1, lay2))


# dx <- 0.02
# nx <- diff(bbox(dem)[1, ]) / dx
# ny <- diff(bbox(dem)[2, ]) / dx
# off <- gridparameters(dem)$cellcentre.offset
# gt <- GridTopology(off, cellsize=c(dx, dx), cells.dim=c(nx, ny))
# sg <- SpatialGrid(gt, proj4string=CRS("+proj=longlat +datum=NAD83"))
# dem <- aggregate(dem, sg)


obs$alt <- overlay(dem, obs)$alt


lm.obs <- lm(lev~alt, obs)
plot(lev~alt, as.data.frame(obs))
abline(lm(lev~alt, as.data.frame(obs)))



null.vgm <- vgm(var(obs$lev), "Sph", 100, nugget=0)
vgm.lev.r <- fit.variogram(variogram(lev~alt, obs), model=null.vgm)
plot(variogram(lev~alt, obs), vgm.lev.r, main="Fitted by gstat")


dem$alt <- dem$alt * over(dem, ply, fn=mean)



lev.uk <- krige(lev~alt, locations=obs, newdata=dem, model=vgm.lev.r, nmax=50)
lev.uk$pred <- dem$alt - lev.uk$var1.pred
lev.uk$se <- sqrt(lev.uk$var1.var)


at <- pretty(lev.uk$pred, 50)
col <- terrain.colors(length(at))
colorkey <- list(width=1, space="right", labels=list(rot=90))
scales <- list(draw=TRUE, y=list(rot=90, tck=-1), x=list(tck=-1))

spplot(lev.uk, "pred", main="UK predictions", at=at, col.regions=col,
       scales=scales, colorkey=colorkey)





spplot(lev.uk, "se",  main="UK standard error", scales=scales, colorkey=colorkey)























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



#### EXAMPLE ####

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











########################### OLD README ###########################

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

