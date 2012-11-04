WriteGAResults <- function(ga, file) {

  if (missing(file)) {
    file <- as.character(tcltk::tcl("tk_getSaveFile",
                                    title="Save GA Results As",
                                    defaultextension="txt",
                                    initialfile="*.txt",
                                    initialdir=file.path(getwd(), "..")))
    if (length(file) == 0 | !nzchar(file[1]))
      return(NULL)
    file <- paste(file, collapse=" ")
  }

  # Removed observations
  x <- ga$rm.pts
  write.table(x, file=file, append=FALSE, quote=FALSE, sep="\t",
              row.names=FALSE)
  cat("\n", file=file, append=TRUE)

  # Objective values
  x <- ga$obj.values[nrow(ga$obj.values), ]
  write.table(x, file=file, append=TRUE, col.names=FALSE, row.names=TRUE,
              quote=FALSE, sep="\t")

  # Answer repeated
  x <- ga$ans.rep
  cat("\nNumber of times final solution was repeated:", x, "\n", file=file,
      append=TRUE)

  # Elapsed time
  x <- format(ga$elapsed.time)
  cat("\nElapsed time:", x, "hours\n", file=file, append=TRUE)

  # Penalty calls
  x <- format(ga$ncalls.penalty)
  cat("\nNumber of calls to penalty function:", x, "\n", file=file, append=TRUE)
  
  # Best solution
  x <- paste(deparse((as.vector(ga$best.solution))), collapse="\n")
  cat("\nBest solution:\n", x, file=file, append=TRUE)
  
  invisible(NULL)
}
