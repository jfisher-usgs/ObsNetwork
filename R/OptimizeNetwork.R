OptimizeNetwork <- function() {

  # Additional functions (subroutines)


  # Main program

  library(colorspace)
  library(sp)
  library(gstat)
  library(genalg)
  library(rgdal)
  library(raster)

  # setwd("C:/Users/jfisher/Documents/ObsNetwork")
  setwd("D:/Software/ObsNetwork")
  # setwd("D:/WORK/JFisher/Software/ObsNetwork")


### library(RSurvey)
### RestoreSession(file.path(getwd(), "R"))

  # ESRP_NED500m <- grd
  # f <- file.path(getwd(), "data", "ESRP_NED500m.rda")
  # save(ESRP_NED500m, file=f, compress=TRUE)
  # load(file=f)

  # ESRP_WaterLevels_2008 <- obs
  # f <- file.path(getwd(), "data", "ESRP_WaterLevels_2008.rda")
  # save(ESRP_WaterLevels_2008, file=f, compress=TRUE)








  ###


  network <- "INL"
  xlim <- c(-113.3, -112.2)
  ylim <- c(43.3, 44.0)


  network <- "State"
  xlim <- c(-115.25, -111.5)
  ylim <- c(42.25, 44.5)


  formula <- var1~1
  vg.model <- vgm(model="Lin", nugget=0)
  vg.fit <- TRUE


  formula <- var1~var2
  vg.model <- vgm(psill=4200, model="Sph", range=80, nugget=0)
  vg.fit <- FALSE


  ###


  grd.fact <- 5

  yr <- 2008
  nmax <- 50
  nsites <- 20

  path <- file.path(getwd(), "inst", "extdata")
  f.grd <- file.path(path, "ESRP_NED500m.tif")
  f.obs <- file.path(path, "ESRP_WaterLevels.csv.gz")
  dsn.ply <- file.path(path, "ESRP_SpatialDomain")

  pal.var2 <- function(n) {
                rev(diverge_hcl(n, h=c(260, 0), c=100, l=c(50, 90), power=1))
              }
  pal.var1 <- function(n) {
                rev(heat_hcl(n, h=c(265, 80), c=c(60, 10), l=c(25, 95),
                             power=c(0.7, 2)))
              }
  pal.se   <- function(n) {
                heat_hcl(n, h=c(130, 30), c=c(65, 6), l=c(45, 100),
                         power=c(0.3, 1.8))
              }


  ###


  # Raster grid

  grd <- readGDAL(f.grd, band=1)
  names(grd) <- "var2"

  grd.crs <- grd@proj4string


  # Crop grid to axis limits
  grd <- as(crop(raster(grd), extent(c(xlim, ylim))), 'SpatialGridDataFrame')


  # Polygon
  ply <- readOGR(dsn=dsn.ply, layer=basename(dsn.ply))

  ply <- rgdal::spTransform(ply, grd.crs)


  # Observations
  d <- read.table(file=f.obs, header=TRUE, sep=",", fill=TRUE, strip.white=TRUE,
                  blank.lines.skip=TRUE, allowEscapes=TRUE, flush=TRUE,
                  stringsAsFactors=FALSE)
  obs.projargs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  dt.lim <- c("2008-01-01 00:00", "2008-12-31 23:59")
  obs <- ProcessObservations(d, x.var="dec_long_va", y.var="dec_lat_va",
                             projargs=obs.projargs, siteno.var="site_no",
                             sitenm.var="station_nm", net.var="network",
                             var1.var="alt_lev_va", var2.var="alt_va",
                             acy.var="lev_acy", dt.var="lev_dt", dt.lim=dt.lim)

  obs <- rgdal::spTransform(obs, grd.crs)


  # Plot accuracy and standard deviation (man page only)
  PlotBubble(obs, "acy", main="Measurment error",
             ply=ply, xlim=xlim, ylim=ylim)
  PlotBubble(obs, "sd", main="Standard deviation",
             ply=ply, xlim=xlim, ylim=ylim)


  # Identify drift (man page only)
  lm.drift <- lm(var1 ~ x + y, data=obs)
  summary(lm.drift)

  # Construct variogram model
  vg <- variogram(formula, obs)
  if (vg.fit)
    vg.model <- fit.variogram(vg, vg.model)

  # Plot variogram model (man page only)
  print(plot(vg, vg.model))


  # Plot DEM (man page only)
  PlotRaster(grd, "var2", obs[obs$net == network, ], ply,
             xlim=xlim, ylim=ylim, pal=pal.var2, contour=FALSE,
             label.pts="mapid")


  # Crop grid to polygon
  grd$var2 <- grd$var2 * overlay(grd, ply)


  # Cross-validation
  cross.validation <- RunCrossValidation(formula, obs, grd, vg.model, nmax)
  PlotBubble(cross.validation$cv, "residual", main="Residuals",
             ply=ply, xlim=xlim, ylim=ylim)


  # Kriging interpolation (man page only)
  kr <- krige(formula=formula, locations=obs, newdata=grd, model=vg.model,
              nmax=nmax)
  kr$var1.se <- sqrt(kr$var1.var)
  PlotRaster(kr, "var1.pred", obs, ply, xlim=xlim, ylim=ylim, pal=pal.var1)
  PlotRaster(kr, "var1.se",   obs, ply, xlim=xlim, ylim=ylim, pal=pal.se)


  # Reduce grid resolution
  if (grd.fact > 1) {
    grd.mod <- as(aggregate(raster(grd), fact=grd.fact, fun=mean, expand=TRUE,
                            na.rm=TRUE), 'SpatialGridDataFrame')
  }

  # Plot updated grid (man page only)
  PlotRaster(grd.mod, "var2", ply=ply, xlim=xlim, ylim=ylim, pal=pal.var2,
             contour=FALSE)


  # Run GA
  ga <- RunGA(obs, network, grd.mod, nsites=nsites,
              vg.model=vg.model, formula=formula, nmax=nmax,
              niters=10, pop.size=300, obj.weights=c(10, 1, 1, 1))

  WriteGAResults(ga)

  is.rm.site <- obs$siteno %in% ga$rm.obs$siteno
  kr <- krige(formula=formula, locations=obs[!is.rm.site, ], newdata=grd,
              model=vg.model, nmax=nmax)
  kr$var1.se <- sqrt(kr$var1.var)
  PlotRaster(kr, "var1.pred", obs, ply, xlim=xlim, ylim=ylim, pal=pal.var1,
             rm.idxs=which(is.rm.site))
  PlotRaster(kr, "var1.se",   obs, ply, xlim=xlim, ylim=ylim, pal=pal.se,
             rm.idxs=which(is.rm.site))
}
