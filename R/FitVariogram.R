FitVariogram <- function(obs, model, ...) {

  # Create gstat object based on ordinary kriging
  obj <- gstat(id="observation", formula=observation~1, data=obs)

  # Calculate sample variogram from gstat object
  v <- gstat::variogram(obj)

  # Fit variogram model to sample variogram
  v.fit <- do.call(fit.variogram, append(list(object=v, model=model),
                                         list(...)))
  print(v.fit)

  # Axis limits
  xlim <- range(pretty(extendrange(v$dist)))
  ylim <- range(pretty(extendrange(v$gamma)))
  if (xlim[1] < 0)
    xlim[1] <- 0
  if (ylim[1] < 0)
    ylim[1] <- 0

  # Plot variogram
  x11()
  print(plot(v, model=v.fit, col="black", xlim=xlim, ylim=ylim,
             xlab="Lag distance in kilometers",
             ylab="Semivariance in square kilometers"))

  invisible(v.fit)
}
