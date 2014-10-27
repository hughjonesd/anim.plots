
#' @import animation

# TODO:
# barplot, curve, hist, density, boxplot?, stripchart?
# smoothing?
# formula interface
# generic plot interface? - e.g. plot.density
# docs
# convert to package

.setup.anim <- function (interval) {
  dev.control('enable')
  if (! is.null(interval)) .old.ani.options <<- ani.options(interval=interval)
}

.teardown.anim <- function() {
  if (exists(".old.ani.options")) ani.options(.old.ani.options)
}


#' Create an animated plot.
#' 
#' \code{anim.plot} 
#' 
#' @param x a matrix of x values. Each column is plotted in a single frame.
#' @param y a matrix of y values.
#' 
#' @examples
#' 
#' x <- matrix(rep(-200:200/100, 10), nrow=401, ncol=10)
#' y <- sin(outer(-200:200/100, 1:10))
#' anim.plot(x, y, type="l", interval=0.5)
#' anim.plot(x, y, type="l", interval=0.5, smooth=3)
#' 
#' @export
anim.plot <- function (x, y, interval=NULL, xlim=NULL, ylim=NULL, col=NULL, 
      pch=NULL, cex=NULL, lty=NULL, lwd=NULL, asp=NULL, smooth=NULL, ...) {
  
  interp <- function (obj, smooth) {
    size <- if(is.matrix(obj)) ncol(obj) else length(obj)
    xout <- seq(1, size, 1/smooth) 
    if (is.matrix(obj)) return(t(apply(obj, 1, function (y)
      approx(1:size, y, xout)$y)
    ))
    approx(1:size, obj, xout)$y
  }
  
  realintvl <- if (is.null(interval)) 1 else interval
  if (! is.null(smooth)) realintvl <- realintvl/smooth
  .setup.anim(interval=realintvl)
  ani.record(reset=TRUE)
  res <- list()
  if (is.null(xlim)) xlim <- range(x)
  if (is.null(ylim)) ylim <- range(y)
  
  # args that could be a matrix: xlim, ylim, col, pch, cex, lty, lwd, 
  # args that could be a vector: main?, sub?, type?, log?, ann?, axes?, frame.plot?, asp
  oth.args <- list(xlim=xlim, ylim=ylim, col=col, pch=pch, cex=cex, lty=lty, 
        lwd=lwd, asp=asp,...) 
  matrix.argnames <- c("xlim", "ylim", "col", "pch", "cex")
  vector.argnames <- c("asp", "lwd", "lty")
  if (! is.null(smooth)) {
    x <- interp(x, smooth)
    y <- interp(y, smooth)
    for (ma in matrix.argnames) 
          if (is.matrix(oth.args[[ma]])) oth.args[[ma]] <- 
          interp(oth.args[[ma]], smooth)
    for (va in vector.argnames) 
      if (is.vector(oth.args[[va]])) oth.args[[va]] <- 
      interp(oth.args[[va]], smooth) 
  }
  for (i in 1:ncol(x)) { 
    args.t <- list()
    for (ma in matrix.argnames) args.t[[ma]] <- if (is.matrix(oth.args[[ma]])) 
          oth.args[[ma]][,i] else oth.args[[ma]]
    for (la in vector.argnames) args.t[[ma]] <- if (is.vector(oth.args[[la]])) 
          oth.args[[la]][[i]] else oth.args[[la]]
    plot(x[,i], y[,i], xlim=args.t$xlim, ylim=args.t$ylim, col=args.t$col, 
          pch=args.t$pch, cex=args.t$cex, lty=args.t$lty, lwd=args.t$lwd, 
          asp=args.t$asp, ...)
    ani.record()
    ani.pause()
  } 
  .teardown.anim()
  return(invisible(animation:::.ani.env$.images))
}

