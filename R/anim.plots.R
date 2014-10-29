
#' @import animation

# TODO:
#
# - easy way to annotate an existing plot with points, legend, axes, etc.
# curve, hist, density, boxplot?, stripchart?, heatmap, mosaic
# maps with colours?
# plot3d - is this possible?
# generic function interface?
# smoothing function?
# make barplot respect matrix parameters; also vectors should go in time order
# (obscure bug with ncol??)
# make sure panel.first and panel.last work?


.setup.anim <- function () {
  if (dev.cur()==1) dev.new()
  dev.control('enable')
  ani.record(reset=TRUE)
  # if (! is.null(interval)) .old.ani.options <<- ani.options(interval=interval)
}

.teardown.anim <- function() {
  #if (exists(".old.ani.options")) ani.options(.old.ani.options)
}

.do.loop <- function(fn, times, show=TRUE, speed=1, use.times=TRUE, slice.args=list(), chunk.args=list(), window=t,
  oth.args=list(), arg.dims=list(), chunkargs.ref.length=NULL) {
  # slice.args we take a slice and drop a dimension
  # chunk.args we cut without dropping
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
  
  times <- sort(times)
  utimes <- unique(times)
  nframes <- length(utimes)
  intervals <- if (use.times) c(diff(utimes), 0) else c(rep(1, nframes-1), 0)
  
  for (ar in names(slice.args)) {
    if (! ar %in% names(arg.dims)) arg.dims[[ar]] <- 0
    if (arg.dims[[ar]]==0) suppressWarnings(slice.args[[ar]] <- 
          rep(slice.args[[ar]], length=nframes))
  }
  if (! is.null(chunkargs.ref.length)) for (ar in names(chunk.args)) 
        suppressWarnings(chunk.args[[ar]] <- rep(chunk.args[[ar]], 
        length=chunkargs.ref.length))

  mycalls <- list()
  .setup.anim()
  for (t in 1:nframes) {
    win.t <- eval(window)
    win.t <- win.t[win.t %in% 1:nframes]
    args.t <- list()
    for (an in names(slice.args)) {
      aa <- slice.args[[an]]
      dl <- mydiml(aa)
      args.t[[an]] <- if (dl <= arg.dims[[an]]) aa else switch(dl+1, aa, aa[t], 
            aa[,t], aa[,,t])
    }
    idx <- times %in% utimes[win.t]
    for (cn in names(chunk.args)) {
      ca <- chunk.args[[cn]]
      dl <- mydiml(ca)
      args.t[[cn]] <- switch(dl+1, ca, ca[idx], ca[,idx, drop=FALSE])
    }

    cl <- as.call(c(fn, args.t, oth.args)) # or match.call?
    if (show) {
      eval(cl)
      ani.record()
      ani.pause(intervals[t]/speed)
      attr(cl, "interval") <- intervals[t]
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



#' Create an animated barplot.
#' 
#' @param height a vector, matrix or array. If a vector it is divided up by 
#'   \code{times} and \code{\link{barplot}} is called on each chunk. If a
#'   matrix, \code{\link{barplot}} is called on each column. If an array, 
#'   \code{\link{barplot}} is called on each matrix of form \code{height[,,i]}.
#' @param times a vector of times. If NULL and \code{height} is a matrix,
#'   the last dimension of \code{height} will be used.
#' @param show if false, do not show plot; just return calls.
#' @param speed higher is faster.
#' @param use.times if \code{TRUE}, animation speed is determined by the
#' \code{times} argument. If \code{FALSE}, animation speed is constant.
#' @param width arguments passed to \code{\link{barplot}}. Arguments \code{width, 
#' names.arg, density, angle, col, border} and \code{offset} may be either vectors
#' of length \code{length(tbl)} or matrices with one column for each unique value of times.
#' Other arguments should be length 1 or vectors.
#' @param space
#' @param names.arg
#' @param density
#' @param angle
#' @param col
#' @param border
#' @param horiz
#' @param xlim
#' @param ylim
#' @param xlab
#' @param ylab
#' @param main
#' @param sub
#' @param offset
#' @param legend.text
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
      show=TRUE, speed=1, use.times=TRUE, window=t, width=1, space=NULL, names.arg=NULL, 
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
  chunk.args <- list(width=width, names.arg=names.arg, density=density, 
        angle=angle, col=col, border=border, offset=offset)
  
  ltdim <- if (is.logical(legend.text)) 0 else 1
  
  oth.args <- args
  if (is.vector(height)) chunk.args$height=height else slice.args$height=height
  hdim <- if(is.matrix(height)) 1 else 2
  if (is.null(times)) {
    if (is.array(height)) times <- 1:tail(dim(height), 1) else stop("'times' not specified")
  }

  arg.dims <- list(height=hdim, space=1, xlim=1, ylim=1, main=0, sub=0, xlab=0, 
        ylab=0, space=1, legend.text=ltdim)
  .do.loop(barplot, times=times, use.times=use.times, window=substitute(window),
        show=show, speed=speed, slice.args=slice.args, chunk.args=chunk.args, 
        oth.args=oth.args, arg.dims=arg.dims)
}

#' Create an animated plot.
#' 
#' \code{anim.plot}
#' 
#' @param x,y vectors of x and y coordinates. These can be passed in any way 
#'   accepted by \code{\link{xy.coords}}.
#' @param times a vector of times. If NULL and \code{x} is a matrix, a sequence
#'   from 1 to the last dimension of \code{x} will be used.
#' @param show if false, do not show plot; just return calls.
#' @param speed animation speed.
#' @param window what window of times to show in each animation. The default,
#'   \code{t}, shows just plots from time t. To draw a plot incrementally,
#'   use \code{window=1:t}. 
#' @param use.times if \code{TRUE}, animation speed is determined by the 
#'   \code{times} argument. If \code{FALSE}, animation speed is constant.
#' @param xlim,ylim,col,pch,labels,cex,lty,lwd,asp arguments passed to 
#'   \code{\link{plot}}.
#' @param ... Other arguments passed to \code{plot}.
#'   
#' @details
#' 
#' Each unique level of \code{times} will generate a single frame of animation. 
#' The frames will be ordered by \code{times}.
#' 
#' In general:
#' 
#' \itemize{ 
#' \item Parameters that apply to each point of the plot, such as
#' \code{xlim, ylim, col, pch, labels} and \code{cex}, can be passed as vectors
#' which will be recycled to \code{length(times)}. 
#' \item Parameters that apply
#' to the plot as a whole, and can have length > 1, such as \code{xlim} and \code{ylim}, can be passed as vectors or matrices. If vectors, the
#' same vector will be passed to every frame. If matrices, column \code{i} will
#' be passed to the \code{i}'th frame. 
#' \item Parameters that apply to the plot
#' as a whole, and always have length 1, such as \code{xlab} and \code{main},
#' can be passed as vectors and will be
#' recycled to the number of frames. 
#' }
#' 
#' @examples
#' 
#' x <- rep(1:100/10, 10)
#' times <- rep(1:10, each=100)
#' y <- sin(x*times/4)
#' anim.plot(x,y,times, type="l")
#' anim.plot(x,y,times, type="l", fg="red", col="blue")
#' ## changing colours - a per-point parameter
#' cols <- (x+9*times)/100 # length 1000
#' anim.plot(x,y,times, ylab="Sine wave", type="l", col=rgb(cols, 0, 1-cols), lwd=2)
#' anim.plot(x,y,times, ylab="Sine wave", type="p", col=rainbow(100)[x *10])
#' 
#' ## changing line width - a whole-plot parameter
#' anim.plot(x, y, times, lwd=matrix(1:10, ncol=10), type="l")
#'           
#' ## incremental plot using window
#' anim.plot(1:10, 1:10, times=1:10, window=1:t)
#' 
#' ## moving window
#' anim.plot(1:10, 1:10, times=1:10, window=(t-2):t)
#' 
#' ## discoveries 1860-1959: moving window
#' xlim <- rbind(1860:1959,1870:1969)
#' anim.plot(1860:1959, discoveries, times=1:100, xlim=xlim,  
#'      xaxp=rbind(xlim, 10), window=t:(t+10), type="h", lwd=8, speed=5)
#'      
#' ## Formula interface
#' ChickWeight$chn <- as.numeric(as.factor(ChickWeight$Chick))
#' anim.plot(weight ~ chn + Time, data=ChickWeight, col=as.numeric(Diet), 
#'      pch=as.numeric(Diet), speed=3)
#' @export
anim.plot <- function(...) UseMethod("anim.plot")


#' @export 
anim.plot.default <- function (x, y=NULL, times, speed=1, use.times=TRUE, window=t, 
  xlim=NULL, ylim=NULL, col=par("col"), xaxp=NULL, yaxp=NULL,
  pch=par("pch"), cex=1, labels=NULL, asp=NULL, lty=par("lty"), lwd=par("lwd"), 
  smooth=NULL, ...) {  
  
  args <- list(...)
  if (! "xlab" %in% names(args)) args$xlab <- deparse(substitute(x))
  if (! "ylab" %in% names(args)) args$ylab <- deparse(substitute(y))
  xy <- xy.coords(x, y, recycle=TRUE)
  x <- xy$x
  y <- xy$y
  args$xlim <- if (is.null(xlim)) range(x[is.finite(x)]) else xlim
  args$ylim <- if (is.null(ylim)) range(y[is.finite(y)]) else ylim
  #   if (! is.null(smooth)) {
  #     for (ma in setdiff(names(mat.args), "col")) if (is.matrix(mat.args[[ma]])) 
  #           mat.args[[ma]] <- .interp(mat.args[[ma]], smooth)
  #     for (va in names(vec.args)) if (length(vec.args[[va]]) > 1) vec.args[[va]] <- 
  #           .interp(vec.args[[va]], smooth) 
  #     if (is.matrix(mat.args$col)) mat.args$col <- .col.interp(mat.args$col, smooth)
  #   } 
  
  
  chunk.args <- list(x=x, y=y, col=col, pch=pch, cex=cex)
  slice.args <- c(list(asp=asp, lty=lty, lwd=lwd, xaxp=xaxp, yaxp=yaxp), args)
  
  .do.loop(plot, times=times, use.times=use.times, speed=speed, window=substitute(window),
        chunk.args=chunk.args, slice.args=slice.args, arg.dims=list(
        xlab=0, ylab=0, xlim=1, ylim=1, xaxp=1, yaxp=1, lwd=0, lty=0, asp=0, panel.first=1, panel.last=1,
        x=1, y=1, col=1, pch=1, cex=1, type=0), chunkargs.ref.length=max(length(x), 
        length(y)))
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

#' Create an animated contour plot
#' 
#' @param x,y,z,... parameters passed to \code{\link{contour}}
#' @param times,speed,use.times,window,show see \code{\link{anim.plot}} for details.
#' 
#' @examples
#' tmp <- volcano
#' tmp[] <- 200 - ((row(tmp) - 43)^2 + (col(tmp) - 30)^2)/20
#' cplot <- array(NA, dim=c(87,61,20))
#' cplot[,,1] <- tmp
#' cplot[,,20] <- volcano
#' cplot <- apply(cplot, 1:2, function(x) seq(x[1], x[20], length.out=20))
#' cplot <- aperm(cplot, c(2,3,1))
#' anim.contour(z=cplot, times=1:20, speed=3, levels=80 + 1:12*10, lty=c(1,2,2))
#' anim.filled.contour(z=cplot, times=1:20, speed=3, levels=80 + 1:12*10, 
#'    color.palette=terrain.colors)
#' @export
anim.contour <- function(...) UseMethod("anim.contour")

#' @export
anim.filled.contour <- function(...) UseMethod("anim.filled.contour")

#' @export
anim.filled.contour.default <- function(...) anim.contour.default(..., fn=filled.contour)

#' @export
anim.contour.default <- function(x, y, z, times, speed=1, use.times=TRUE, window=t, 
      show=TRUE, fn=contour, ...) {
  if (missing(z)) {
    z <- x 
    x <- seq(0,1, length.out=dim(z)[1])
    y <- seq(0,1, length.out=dim(z)[2])
  }
  if (missing(x)) x <- seq(0,1, length.out=dim(z)[1])
  if (missing(y)) y <- seq(0,1, length.out=dim(z)[2])
  dots <- list(...)
  slice.args <- list(z=z)
  slice.args$x <- x
  slice.args$y <- y
  if (! "zlim" %in% names(dots)) dots$zlim <- range(z, finite=TRUE)
  if (! "xlim" %in% names(dots)) dots$xlim <- range(x, finite=TRUE)
  if (! "ylim" %in% names(dots)) dots$ylim <- range(y, finite=TRUE)
  
  .do.loop(fn, times=times, show=show, use.times=use.times,
        slice.args=c(slice.args, dots), 
        arg.dims=list(z=2, x=1, y=1, nlevels=0, levels=1, 
        labels=1, labcex=0, drawlabels=0, xlim=1, ylim=1, zlim=1, vfont=1,
        axes=0, frame.plot=0, col=1, lty=1, lwd=1, color.palette=1))
}
