
#' @import animation
#' @import Formula

# TODO:
# - why do you require a matrix? why not just
# x,y,t and the different values of t define the different plots?
# this gets rid of all your bullshit impedance mismatch, and allows
# easy definition of intervals directly from t. It's also what e.g.
# plot3d does. And interpolation would also be straightforward.
# presumably "moving window" would also become rather easy.
#
# - easy way to annotate an existing plot with points, legend, axes, etc.

# barplot, curve, hist, density, boxplot?, stripchart?
# generic plot interface? - e.g. plot.density (maybe use xy.coords in .default)
# "plot moving window" function?
# generic function interface?
# interval breaks in the following:
#    anim.plot(weight ~ chn | Time, data=ChickWeight, subset=chn<5, interval=1, 
#     col=as.numeric(Diet), smooth=3)
# 
# idea:
# barplot, curve, hist, stripchart, sunflowerplot, matplot..., symbols, arrows, 
# segments, points, lines, rug, contour, filled.contour, image, rect, polygon
# text, mtext
# ... all have basic structure same, just need extensions...
# * take data and parameters.
# * interpolate it if necessary.
# * call a sequence of functions, pausing between each one.
# * return a list of the calls [or just do this if do.plot=FALSE]


.setup.anim <- function () {
  if (dev.cur()==1) dev.new()
  dev.control('enable')
  ani.record(reset=TRUE)
  # if (! is.null(interval)) .old.ani.options <<- ani.options(interval=interval)
}

.teardown.anim <- function() {
  #if (exists(".old.ani.options")) ani.options(.old.ani.options)
}

.do.loop <- function(fn, times, show=TRUE, speed=1, slice.args=list(), chop.args=list(),
  oth.args=list(), arg.dims=list()) {
  # slice.args we take a slice and drop a dimension
  # chop.args we cut without dropping
  # oth.args we leave alone
  # individual functions must put things in right boxes
  # examples: matplot() wants matrices so makes sense to pass in 3d arrays
  # arrows() wants vectors x0,y0,x1,y1: all pass in as vectors
  # many functions have xlim=c(a,b) and should usually pass this as oth.args
  mydiml <- function(obj) {
    if (is.null(dim(obj))) {
      if (length(obj)==1) 0 else 1
    } else {
      length(dim(obj))
    }
  }
  
  for (ar in names(c(slice.args, chop.args))) if (! ar %in% names(arg.dims)) 
        arg.dims[[ar]] <- 0
  times <- sort(times)
  utimes <- unique(times)
  nframes <- length(utimes)
  intervals <- c(diff(utimes), 0)
  adn <- names(arg.dims)

  mycalls <- list()
  .setup.anim()
  for (i in 1:nframes) { 
    args.t <- list()
    for (an in names(slice.args)) {
      aa <- slice.args[[an]]
      dl <- mydiml(aa)
      if (dl <= arg.dims[[an]]) next
      args.t[[an]] <- switch(dl+1, aa, aa[i], aa[,i], aa[,,i])
    }
    idx <- times==utimes[i]
    for (cn in names(chop.args)) {
      ca <- chop.args[[cn]]
      dl <- mydiml(ca)
      args.t[[cn]] <- switch(dl+1, ca, ca[idx], ca[,idx, drop=FALSE])
    }

    cl <- as.call(c(fn, args.t, oth.args)) # or match.call?
    if (show) {
      eval(cl)
      ani.record()
      ani.pause(intervals[i]/speed)
      attr(cl, "interval") <- intervals[i]
    }
    mycalls <- c(mycalls, cl)
  } 
  .teardown.anim()
  class(mycalls) <- "anim.frames"
  attr(mycalls, "speed") <- speed
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

#' @export 
anim.plot.default <- function (x, y, times, speed=1, use.times=TRUE, xlim=NULL, ylim=NULL, col=par("col"), 
      pch=par("pch"), cex=1, labels=NULL, asp=NULL, lty=par("lty"), lwd=par("lwd"), 
    smooth=NULL, ...) {  
  realintvl <- interval
  if (! is.null(smooth)) realintvl <- rep(realintvl/smooth, each=smooth)

  args <- list(...)
  args$xlim <- if (is.null(xlim)) range(x[is.finite(x)]) else xlim
  args$ylim <- if (is.null(ylim)) range(y[is.finite(y)]) else ylim
  if (! "xlab" %in% names(args)) args$xlab <- deparse(substitute(x))
  if (! "ylab" %in% names(args)) args$ylab <- deparse(substitute(y))
  
  # x, y should be chopped. xlim and ylim should be chopped if dim==2.
  # xlab and ylab should be sliced or other: not one value per point
  # same for asp, lty, lwd. NB: these can be "sliced" by taking one value.
  # col, pch, cex, should be chopped.
  # you could say, if col is a matrix, we apply it once for each frame?
  # labels' length could vary?
#  mat.args <- list(x=x, y=y, xlim=xlim, ylim=ylim, col=col, pch=pch, cex=cex,
#  vec.args <- list(asp=asp, lty=lty, lwd=lwd)
#   labels=labels)

#   if (! is.null(smooth)) {
#     for (ma in setdiff(names(mat.args), "col")) if (is.matrix(mat.args[[ma]])) 
#           mat.args[[ma]] <- .interp(mat.args[[ma]], smooth)
#     for (va in names(vec.args)) if (length(vec.args[[va]]) > 1) vec.args[[va]] <- 
#           .interp(vec.args[[va]], smooth) 
#     if (is.matrix(mat.args$col)) mat.args$col <- .col.interp(mat.args$col, smooth)
#   } 
  
  
  chop.args <- list(x=x, y=y, col=col, pch=pch, cex=cex)
  slice.args <- c(list(asp=asp, lty=lty, lwd=lwd), args)
  
  .do.loop(plot, times=times, use.times=use.times, speed=speed, 
        chop.args=chop.args, slice.args=slice.args, arg.dims=list(
          xlab=0, ylab=0, xlim=1, ylim=1, lwd=0, lty=0, asp=0,
          x=1, y=1, col=1, pch=1, cex=1))
}

#' @export 
anim.plot.formula <- function(x, data=parent.frame(), subset=NULL, na.action=NULL, ...) {
  if (missing(x) || !inherits(x, "formula")) 
    stop("'x' missing or invalid")
  fml <- as.Formula(x)
  if (any(length(fml) != c(1,2))) stop("Formula must be like: y ~ x | t")
  
  # cargo-culted from plot.formula
  m <- match.call(expand.dots=FALSE)
  eframe <- parent.frame()
  md <- eval(m$data, eframe)
  dots <- lapply(m$..., eval, md, eframe)
  mf <- model.frame(fml, data=md, na.action=na.action, lhs=1, rhs=1:2)
  subset.expr <- m$subset
  if (!missing(subset)) {
    s <- eval(subset.expr, data, eframe)
    l <- nrow(mf)
    dosub <- function(x) if (length(x) == l) x[s] else x
    dots <- lapply(dots, dosub)
    mf <- mf[s, ]
  }
  
  # get levels of t. 
  tm <- model.part(fml, data=mf, rhs=2, drop=TRUE)
  x <- model.part(fml, data=mf, rhs=1, drop=TRUE)
  y <- model.part(fml, data=mf, lhs=1, drop=TRUE)
  # we are basically praying here:
  dots <- lapply(dots, function(z) if (length(z)==length(tm)) z[order(tm)] else z) 
  x <- x[order(tm)]
  y <- y[order(tm)]
  tm <- tm[order(tm)]
  # now for each individual value of tm put x and y into matrices
  colsize <- length(unique(tm))
  nr <- max(table(tm))
  X <- Y <- matrix(NA, ncol=colsize, nrow=nr)
  dotmats <- list()
  margs <- c("col", "pch", "cex", "labels")
  for (dotarg in margs) if (dotarg %in% names(dots) && length(dots[[dotarg]]) == 
        tm) dotmats[[dotarg]] <- matrix(NA, ncol=colsize, nrow=nr)
  for (i in 1:colsize) {
    tmi <- unique(tm)[i]
    l <- 1:length(tm[tm==tmi])
    X[l,i] <- x[tm==tmi]
    Y[l,i] <- y[tm==tmi]
    for (dotarg in margs) if (dotarg %in% names(dotmats)) 
          dotmats[[dotarg]][l,i] <- dots[[dotarg]][tm==tmi]
    # how to handle these? It has to be same for each value of tm
    # vec.args <- list(asp=asp, lty=lty, lwd=lwd)
  }
  for (dotarg in margs) if (dotarg %in% names(dotmats)) dots[[dotarg]] <- 
        dotmats[[dotarg]][l,i] 
  if (! "xlab" %in% names(dots)) dots$xlab <- all.vars(fml)[2] 
  if (! "ylab" %in% names(dots)) dots$ylab <- all.vars(fml)[1]
  if (! "interval" %in% names(dots)) dots$interval <- diff(tm)
  do.call("anim.plot", c(list(x=X, y=Y), dots))
  # work out matrices for each value of the second part in order
  # do other values come from within data?
}

anim.barplot <- function(height, width=1, space=NULL, col=NULL, smooth=NULL, ...) {
  # if nec, interpolate
  # plot data
}


#' Create an animated plot.
#' 
#' \code{anim.plot}
#' 
#' @param x a matrix or vector. If a matrix, each column is plotted in a single
#'   frame of the animation. If a vector, the same values are plotted in each
#'   frame. Either \code{x} or \code{y} must be a matrix.
#' @param y a matrix or vector.
#' @param interval how many seconds to wait between each frame.
#' @param xlim These and subsequent arguments are the same as in
#'   \code{\link{plot}}.
#' @param ylim
#' @param col
#' @param pch
#' @param labels
#' @param cex
#' @param lty
#' @param lwd
#' @param asp
#' @param smooth Interpolate data by linear smoothing? If NULL, no smoothing is 
#'   done. If e.g. \code{smooth=2}, the number of plots will be doubled.
#' @param ... Other arguments passed to \code{plot}.
#'   
#' @details Parameters \code{xlim, ylim, col, pch, labels} and \code{cex} can be
#' matrices. Each column will apply to a single frame of the animation.
#' 
#' Parameters \code{asp, lwd} and \code{lty} can be vectors of length > 1, in
#' which case each value will apply to a single frame.
#' 
#' When \code{smooth > 1}, parameters are interpolated where appropriate
#' (including colours).
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
#' ## different intervals
#' anim.plot(x, y, interval=c(1,1,1,1,10,1,1,1,1)/10, type="l")
#' sizes <- matrix(c(1:6,5:1), ncol=11, nrow=5, byrow=TRUE)
#' anim.plot(1:5, matrix(1:5, ncol=11, nrow=5), pch=19, col="orange", 
#'      cex=sizes, interval=.2)
#'      
#' ## discoveries 1860-1959
#' d.tmp <- sapply(1:91, function(x) discoveries[x:(x+9)])
#' labs.tmp <- outer(0:9, 1860:1951,"+")
#' suppressWarnings( # problems with 'labels'...
#' anim.plot(1:10, d.tmp, interval=.1, xlab="Year", ylab="Discoveries", type="h",
#'      labels=labs.tmp, at=1:10, col="blue", yaxt="n")
#'  )
#' @export
anim.plot <- function(...) UseMethod("anim.plot")



