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
  
  # Call
  cat(paste(format(x$call), collapse="\n"), "\n\n", file=file, append=FALSE)
  
  # Objectives
  tbl <- x$obj.values[nrow(x$obj.values), ]
  write.table(tbl, file=file, col.names=FALSE, row.names=TRUE, quote=FALSE, 
              sep="\t", append=TRUE)
  
  # Iterations
  cat("\nNumber of completed iterations:\t", x$niter, "\n", 
      file=file, append=TRUE)
  
  # Repeats
  cat("\nNumber of times final solution was repeated:\t", x$nrep.ans, "\n", 
      file=file, append=TRUE)

  # Time
  cat("\nElapsed time, in hours:\t", format(x$elapsed.time), "\n", 
      file=file, append=TRUE)

  # Penalty
  cat("\nNumber of calls to penalty function:\t", sum(x$ncalls.penalty), "\n", 
      file=file, append=TRUE)
  cat(paste(deparse(x$ncalls.penalty), "\n"), file=file, append=TRUE)
  
  # Remove
  cat("\nRemoved sites:\n", file=file, append=TRUE) 
  cat(paste(names(x$pts.rm), collapse="\t"), "\n", file=file, append=TRUE)
  write.table(x$pts.rm, file=file, append=TRUE, quote=FALSE, sep="\t", 
              col.names=FALSE, row.names=FALSE)
  
  # Suggestion
  cat("\nSuggestion for initial population:\n", file=file, append=TRUE)
  obj <- paste(deparse(x$ga.ans@population), collapse="\n")
  cat(paste("suggestion <- ", obj, sep="\n"), file=file, append=TRUE)
  
  invisible(NULL)
}
