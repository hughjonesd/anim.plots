
#' @import animation

# TODO:
# barplot, curve, hist, density, boxplot?, stripchart?
# formula interface
# generic plot interface? - e.g. plot.density
# generic function interface?
# extend x and y to allow vectors?

# idea:
# barplot, curve, hist, stripchart, sunflowerplot, matplot..., symbols, arrows, 
# segments, points, lines, rug, contour, filled.contour, image, rect, polygon
# text, mtext
# ... all have basic structure same, just need extensions...
# * take data and parameters.
# * interpolate it if necessary.
# * call a sequence of functions, pausing between each one.
# * return a list of the calls [or just do this if do.plot=FALSE]
# each interface can just do the same procedure, specifying which arguments
# get interpolated etc. 
# you can compose stuff by calling the procedure with the "function"
# being something that does the appropriate calls (which need to be 
# setup right first re timing...)


.setup.anim <- function (interval) {
  if (dev.cur()==1) dev.new()
  dev.control('enable')
  if (! is.null(interval)) .old.ani.options <<- ani.options(interval=interval)
}

.teardown.anim <- function() {
  if (exists(".old.ani.options")) ani.options(.old.ani.options)
}

.do.loop <- function(fn, nframes, show=TRUE, mat.args=list(), vec.args=list(),
       oth.args=NULL) {
  # all arguments must be in the right form already, no guessing done
  mycalls <- list()
  for (i in 1:nframes) { 
    args.t <- list()
    for (aa in names(mat.args)) {
      an <- aa
      aa <- mat.args[[aa]]
      dl <- length(dim(aa))
      args.t[[an]] <- if (dl==3) aa[,,i] else if (dl==2) aa[,i] else aa
    }
    for (aa in names(vec.args)) {
      an <- aa
      aa <- vec.args[[aa]]
      args.t[[an]] <- if (length(aa)>1) aa[i] else aa
    }
    cl <- as.call(c(fn, args.t, oth.args)) # or match.call?
    if (show) {
      eval(cl)
      ani.record()
      ani.pause()
    }
    mycalls <- c(mycalls, cl)
  } 
  return(invisible(mycalls))
}

.col.interp <- function(colmat, smooth) {
  ncolours <- (ncol(colmat)-1)*smooth + 1
  colmat <- t(apply(colmat, 1, function(cl) 
    colorRampPalette(cl, alpha=TRUE)(ncolours)
  ))
  return(colmat) 
}


.interp <- function (obj, smooth) {
  size <- if(is.matrix(obj)) ncol(obj) else length(obj)
  xout <- seq(1, size, 1/smooth) 
  if (is.matrix(obj)) return(t(apply(obj, 1, function (y)
    approx(1:size, y, xout)$y)
  ))
  approx(1:size, obj, xout)$y
}



#' Create an animated plot.
#' 
#' \code{anim.plot} 
#' 
#' @param x a matrix or vector. If a matrix, each column is plotted in 
#'        a single frame of the animation. If a vector, the same values are plotted
#'        in each frame. Either \code{x} or \code{y} must be a matrix.
#' @param y a matrix or vector.
#' @param interval how many seconds to wait between each frame.
#' @param xlim These and subsequent arguments are the same as in \code{\link{plot}}.
#' @param ylim 
#' @param col 
#' @param pch
#' @param cex
#' @param lty
#' @param lwd
#' @param asp
#' @param smooth Interpolate data by linear smoothing? If NULL, no smoothing is 
#'        done. If e.g. \code{smooth=2}, the number of plots will be doubled.
#' @param ... Other arguments passed to \code{plot}.
#'       
#' @details
#' Parameters \code{xlim, ylim, col, pch} and \code{cex} can be matrices. Each
#' column will apply to a single frame of the animation. 
#' 
#' Parameters \code{asp, lwd}
#' and \code{lty} can be vectors of length > 1, in which case each value will 
#' apply to a single frame.
#' 
#' When \code{smooth > 1}, parameters are interpolated where appropriate (including
#' colours).
#' 
#' @examples
#' 
#' x <- matrix(rep(-200:200/100, 10), nrow=401, ncol=10)
#' y <- sin(outer(-200:200/100, 1:10))
#' anim.plot(x, y, type="l", interval=0.5)
#' anim.plot(x, y, type="l", interval=0.5, smooth=3)
#' ## changing colours
#' anim.plot(x, y, type="l", interval=0.5, col=matrix(1:10, nrow=1))
#' anim.plot(x, y, type="l", interval=0.5, col=matrix(1:10, nrow=1), smooth=5)
#' ## changing line width
#' anim.plot(x, y, lwd=matrix(1:10, ncol=10), type="l")
#' 
#' sizes <- matrix(c(1:6,5:1), ncol=11, nrow=5, byrow=T)
#' anim.plot(1:5, matrix(1:5, ncol=11, nrow=5), pch=19, col="orange", 
#'      cex=sizes, interval=.2)
#'      
#' ## discoveries 1860-1959
#' d.tmp <- sapply(1:91, function(x) discoveries[x:(x+9)])
#' suppressWarnings( # problems with 'labels'...
#' anim.plot(1:10, d.tmp, interval=.1, xlab="Year", ylab="Discoveries", type="h",
#'      labels=t(outer(1860:1951, 0:9, "+")), at=1:10, col="blue", yaxt="n")
#'  )
#' @export
anim.plot <- function (x, y, interval=NULL, xlim=NULL, ylim=NULL, col=par("col"), 
      pch=par("pch"), cex=1, labels=NULL, asp=NULL, lty=par("lty"), lwd=par("lwd"), 
    smooth=NULL, ...) {  
  realintvl <- if (is.null(interval)) 1 else interval
  if (! is.null(smooth)) realintvl <- realintvl/smooth
  .setup.anim(interval=realintvl)
  ani.record(reset=TRUE)

  oth.args <- list(...)
  if (is.null(xlim)) xlim <- range(x)
  if (is.null(ylim)) ylim <- range(y)
  if (! "xlab" %in% names(oth.args)) oth.args$xlab <- deparse(substitute(x))
  if (! "ylab" %in% names(oth.args)) oth.args$ylab <- deparse(substitute(y))
  mat.args <- list(x=x, y=y, xlim=xlim, ylim=ylim, col=col, pch=pch, cex=cex,
    labels=labels)
  vec.args <- list(asp=asp, lty=lty, lwd=lwd)

  if (! is.null(smooth)) {
    for (ma in setdiff(names(mat.args), "col")) if (is.matrix(mat.args[[ma]])) 
          mat.args[[ma]] <- .interp(mat.args[[ma]], smooth)
    for (va in names(vec.args)) if (length(vec.args[[va]]) > 1) vec.args[[va]] <- 
          .interp(vec.args[[va]], smooth) 
    if (is.matrix(mat.args$col)) mat.args$col <- .col.interp(mat.args$col, smooth)
  } 
  
  nframes <- if (is.matrix(mat.args$x)) ncol(mat.args$x) else ncol(mat.args$y)
  .do.loop(plot, nframes, mat.args=mat.args, vec.args=vec.args, oth.args=oth.args)
  .teardown.anim()
  return(invisible(animation:::.ani.env$.images))
}

anim.barplot <- function(height, width=1, space=NULL, col=NULL, smooth=NULL, ...) {
  # if nec, interpolate
  # plot data
}



