WriteGAResults <- function(x, file) {

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
  
  # Fucntion call
  cat(paste(format(x$call), collapse="\n"), "\n\n", file=file, append=FALSE)
  
  # Objective values
  tbl <- x$obj.values[nrow(x$obj.values), ]
  write.table(tbl, file=file, col.names=FALSE, row.names=TRUE, quote=FALSE, 
              sep="\t", append=TRUE)
  
  # Answer repeated
  cat("\nNumber of times final solution was repeated:\t", x$nrep.ans, "\n", 
      file=file, append=TRUE)

  # Elapsed time
  cat("\nElapsed time, in hours:\t", format(x$elapsed.time), "\n", file=file, 
      append=TRUE)

  # Penalty calls
  cat("\nNumber of calls to penalty function:\t", sum(x$ncalls.penalty), "\n", 
      file=file, append=TRUE)
  
  # Removed observations
  tbl <- x$pts.rm
  cat("\nRemoved sites:\n", file=file, append=TRUE) 
  cat(paste(names(tbl), collapse="\t"), "\n", file=file, append=TRUE)
  write.table(tbl, file=file, append=TRUE, quote=FALSE, sep="\t", 
              col.names=FALSE, row.names=FALSE)
  
  invisible(NULL)
}
