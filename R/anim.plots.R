
#' @import animation

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

.do.loop <- function(fn, times, show=TRUE, speed=1, use.times=TRUE, slice.args=list(), chop.args=list(),
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
      if (length(obj)==1 || is.null(obj)) 0 else 1
    } else {
      length(dim(obj))
    }
  }
  
  for (ar in names(c(slice.args, chop.args))) if (! ar %in% names(arg.dims)) 
        arg.dims[[ar]] <- 0
  times <- sort(times)
  utimes <- unique(times)
  nframes <- length(utimes)
  intervals <- if (use.times) c(diff(utimes), 0) else c(rep(1, nframes-1), 0)
  adn <- names(arg.dims)

  mycalls <- list()
  .setup.anim()
  for (i in 1:nframes) { 
    args.t <- list()
    for (an in names(slice.args)) {
      aa <- slice.args[[an]]
      dl <- mydiml(aa)
      args.t[[an]] <- if (dl <= arg.dims[[an]]) aa else switch(dl+1, aa, aa[i], 
            aa[,i], aa[,,i])
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

  x <- as.vector(x)
  y <- as.vector(y) # consider doing sthg like xy.coords?
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
          x=1, y=1, col=1, pch=1, cex=1, type=0))
}

#' @export 
anim.plot.formula <- function(x, data=parent.frame(), subset=NULL, na.action=NULL, ...) {
  if (missing(x) || !inherits(x, "formula")) 
    stop("'x' missing or invalid")
  
  # cargo-culted from plot.formula
  m <- match.call(expand.dots=FALSE)
  eframe <- parent.frame()
  md <- eval(m$data, eframe)
  dots <- lapply(m$..., eval, md, eframe)
  mf <- model.frame(x, data=md, na.action=na.action)
  subset.expr <- m$subset
  if (!missing(subset)) {
    s <- eval(subset.expr, data, eframe)
    l <- nrow(mf)
    dosub <- function(x) if (length(x) == l) x[s] else x
    dots <- lapply(dots, dosub)
    mf <- mf[s, ]
  }
  
  # get levels of t. 
  tm <- mf[,3]
  x <- mf[,2]
  y <- mf[,1]
  # we are basically praying here:
  dots <- lapply(dots, function(z) if (length(z)==length(tm)) z[order(tm)] else z) 
  x <- x[order(tm)]
  y <- y[order(tm)]
  tm <- tm[order(tm)]
  if (! "xlab" %in% names(dots)) dots$xlab <- all.vars(x)[2] 
  if (! "ylab" %in% names(dots)) dots$ylab <- all.vars(x)[1]
  do.call("anim.plot", c(list(x=x, y=y, times=tm), dots))
  # work out matrices for each value of the second part in order
  # do other values come from within data?
}


#' Create an animated barplot.
#' 
#' @param height a vector, matrix or array. If a vector it is divided up by 
#'   \code{times} and \code{\link{barplot}} is called on each chunk. If a
#'   matrix, \code{\link{barplot}} is called on each column. If an array, 
#'   \code{\link{barplot}} is called on each matrix of form \code{height[,,i]}.
#'   
#' @examples
#' anim.barplot(1:100, times=rep(1:10, each=10), ylim=c(0,100))
#' ## barplot with a matrix
#' ChickWeight$wq <- cut(ChickWeight$weight, 5)
#' tbl <- as.array(xtabs(~ wq + Diet + Time, data=ChickWeight))
#' anim.barplot(tbl, xlab="Diet", ylab="N", legend.text=paste("Quintile", 1:5))
#' anim.barplot(tbl, xlab="Diet", ylab="N", beside=TRUE, ylim=c(0,20),
#'    legend.text=paste("Quintile", 1:5))
#'    
#' @export
anim.barplot <- function(...) UseMethod("anim.barplot")

#' @export
anim.barplot.default <- function(height, times=NULL, 
      show=TRUE, speed=1, use.times=TRUE, width=1, space=NULL, names.arg=NULL, 
      density=NULL, angle=NULL, col=NULL, border=NULL, horiz=FALSE, xlim=NULL, 
      ylim=NULL, xlab=NULL, ylab=NULL, main=NULL, sub=NULL, offset=NULL, 
      legend.text=NULL, ...) {
  # plot data
  slice.args <- list(height=height, space=space, xlim=xlim, ylim=ylim, main=main, 
        sub=sub, xlab=xlab, ylab=ylab, legend.text=legend.text)
  
  # in barplot:
  # height is matrix or vector; space is 1, 2 or length(height); width is length(height)
  # so is names.arg, density, angle, col, border; legend.text is TRUE or nrow(height)
  # if height is a matrix and beside = FALSE then we want ncol(height)
  args <- list(...)
  chop.args <- list(width=width, names.arg=names.arg, density=density, 
        angle=angle, col=col, border=border, offset=offset)
  
  ltdim <- if (is.logical(legend.text)) 0 else 1
  
  oth.args <- args
  if (is.vector(height)) chop.args$height=height else slice.args$height=height
  hdim <- if(is.matrix(height)) 1 else 2
  if (is.null(times)) {
    if (is.array(height)) times <- 1:tail(dim(height), 1) else stop("'times' not specified")
  }

  arg.dims <- list(height=hdim, space=1, xlim=1, ylim=1, main=0, sub=0, xlab=0, 
        ylab=0, space=1, legend.text=ltdim)
  .do.loop(barplot, times=times, use.times=use.times, show=show, speed=speed, 
        slice.args=slice.args, chop.args=chop.args, oth.args=oth.args, 
        arg.dims=arg.dims)
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
#' x <- rep(1:100/10, 10)
#' times <- rep(1:10, each=100)
#' y <- sin(x*times/4)
#' anim.plot(x,y,times, ylab="Sine wave", type="l")
#' anim.plot(x,y,times, ylab="Sine wave", type="l", fg="red", col="blue")
#' ## changing colours - a per-point parameter
#' cols <- (x+9*times)/100 # length 1000
#' anim.plot(x,y,times, ylab="Sine wave", type="l", col=rgb(cols, 0, 1-cols), lwd=2)
#' anim.plot(x,y,times, ylab="Sine wave", type="p", col=rainbow(100)[x *10])
#' 
#' ## changing line width - a whole-plot parameter
#' anim.plot(x, y, lwd=matrix(1:10, ncol=10), type="l")
#'      
#' ## discoveries 1860-1959: moving window
#' dis <- as.vector(sapply(1:91, function(x) discoveries[x:(x+9)]))
#' years <- outer(0:9, 1860:1951,"+")
#' anim.plot(years, dis, times=rep(1:100, each=10), xlab="Year", ylab="Discoveries", type="h",
#'      col="blue", xlim=years[c(1,10),], lwd=8, lab=c(10,5,7))
#'      
#' ## Formula interface
#' ChickWeight$chn <- as.numeric(as.factor(ChickWeight$Chick))
#' anim.plot(weight ~ chn + Time, data=ChickWeight, col=as.numeric(Diet), 
#'      pch=as.numeric(Diet), speed=3)
#' @export
anim.plot <- function(...) UseMethod("anim.plot")



