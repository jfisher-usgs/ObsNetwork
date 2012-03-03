OptimizeNetwork <- function() {

  # Additional functions (subroutines)


  # Main program


  library(colorspace)
  library(gstat)
  library(genalg)
  library(rgdal)
  library(raster)
  library(RSurvey)

  # setwd("C:/Users/jfisher/Documents/ObsNetwork")
  setwd("D:/Software/ObsNetwork")
  # setwd("D:/WORK/JFisher/Software/ObsNetwork")
  RestoreSession(file.path(getwd(), "R"))

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


  vg.formula <- var1~1
  vg.model <- vgm(model="Lin", nugget=0)
  vg.fit <- TRUE


  vg.formula <- var1~var2
  vg.model <- vgm(psill=4200, model="Sph", range=80, nugget=0)
  vg.fit <- FALSE


  ###


  grd.fact <- 5
  ply.dsn <- "ESRP_SpatialDomain"
  grd.file <- "ESRP_NED500m.tif"
  path <- file.path(getwd(), "inst", "extdata")
  obs.file <- "ESRP_WaterLevels.csv.gz"
  yr <- 2008
  nmax <- 50
  nsites <- 20





  pal.var2 <- function(n) {
                rev(diverge_hcl(n, h=c(260, 0), c=100, l=c(50, 90), power=1))
              }
  pal.var1 <- function(n) {
                rev(heat_hcl(n, h=c(265, 80), c=c(60, 10), l=c(25, 95),
                             power=c(0.7, 2)))
              }
  pal.err  <- function(n) {
                heat_hcl(n, h=c(130, 30), c=c(65, 6), l=c(45, 100),
                         power=c(0.3, 1.8))
              }


  ###


  # Raster grid
  f <- file.path(path, grd.file)
  grd <- readGDAL(f, band=1)
  names(grd) <- "var2"

  grd.crs <- grd@proj4string


  # Crop grid to axis limits
  grd <- as(crop(raster(grd), extent(c(xlim, ylim))), 'SpatialGridDataFrame')


  # Polygon
  f <- file.path(path, ply.dsn)
  ply <- readOGR(dsn=f, layer=ply.dsn)

  ply <- rgdal::spTransform(ply, grd.crs)


  # Observations
  f <- file.path(path, obs.file)
  d <- read.table(file=f, header=TRUE, sep=",", fill=TRUE, strip.white=TRUE,
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
  vg <- variogram(vg.formula, obs)
  if (vg.fit)
    vg.model <- fit.variogram(vg, vg.model)

  # Plot variogram model (man page only)
  print(plot(vg, vg.model))


  # Plot DEM (man page only)
  PlotGrid(grd, "var2", obs[obs$net == network, ], ply,
           xlim=xlim, ylim=ylim, pal=pal.var2, contour=FALSE, label.pts="mapid")


  # Crop grid to polygon
  grd$var2 <- grd$var2 * overlay(grd, ply)


  # Cross-validation
  cross.validation <- RunCrossValidation(vg.formula, obs, grd, vg.model, nmax,
                                         ply)
  PlotBubble(cross.validation$cv, "residual", main="Residuals",
             ply=ply, xlim=xlim, ylim=ylim)


  # Kriging interpolation (man page only)
  kr <- krige(formula=vg.formula, locations=obs, newdata=grd, model=vg.model,
              nmax=nmax)
  kr$var1.se <- sqrt(kr$var1.var)
  PlotGrid(kr, "var1.pred", obs, ply, xlim=xlim, ylim=ylim, pal=pal.var1)
  PlotGrid(kr, "var1.se",   obs, ply, xlim=xlim, ylim=ylim, pal=pal.err)


  # Reduce grid resolution
  if (grd.fact > 1) {
    grd <- as(aggregate(raster(grd), fact=grd.fact, fun=mean, expand=TRUE,
                        na.rm=TRUE), 'SpatialGridDataFrame')
  }

  # Plot updated grid (man page only)
  PlotGrid(grd, "var2", ply=ply, xlim=xlim, ylim=ylim, pal=pal.var2,
           contour=FALSE)


  # Run GA

  graphics.off()

  ga <- RunGA(obs, network, grd, nsites=nsites,
              vg.model=vg.model, formula=vg.formula, nmax=nmax,
              niters=10, pop.size=300)


  # TODO: ADD FUNCTION: WriteGAResults()

  PlotGrid(ga$kr, "var1.pred", obs, ply, , xlim=xlim, ylim=ylim, pal=pal.var1,
           rm.idxs=which(obs$siteno %in% ga$rm.obs$siteno))







}
