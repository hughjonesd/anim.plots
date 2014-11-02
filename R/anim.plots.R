
#' @import animation

# TODO:
# density, segments, arrows, stars, polygons, etc.
# plot3d - is this possible? persp is done...
# compose two plots together? 
# how to save while respecting intervals: interpolate data for frames
# in .do.loop:  
# EXAMPLE:
# ht <- 40
# x <- rep(1, ht)
# y <- 1:ht
# intervals <- jitter(ht:1/20) # in seconds
# times <- cumsum(intervals)
# ani.record(reset=T)
# 
# framerate <- 50 # per second
# intervals <- round(intervals*framerate)
# 
# # interpolate data? But, can I do this in general?
# # if so, the whole .do.loop would have to be rewritten to, indeed, smooth stuff.
# # a set of "smoothable" parameters.
# # if it were told to smooth, the intervals would be replaced by 1 and smoothed
# # versions of everything would be created. The resulting plot would be recorded.
# 
# # e.g. if intervals are 5,4,3,2,1. in [ x[1] , x[2] ) we want 5 intervals, etc.
# # in general for every variable: take x as sequence along it. it as y. then create xout
# spaced <- seq(min(times), max(times), length.out=max(times))
# x <- approx(times, x, xout=spaced)$y
# y <- approx(times, y, xout=spaced)$y

.setup.anim <- function (reset=TRUE, dev.control.enable=TRUE) {
  if (dev.cur()==1) dev.new()
  if (dev.control.enable) dev.control('enable')
  ani.record(reset=reset)
  # if (! is.null(interval)) .old.ani.options <<- ani.options(interval=interval)
}

.teardown.anim <- function() {
  #if (exists(".old.ani.options")) ani.options(.old.ani.options)
}

.do.loop <- function(fn, times, show=TRUE, speed=1, use.times=TRUE, window=t,
      window.process=NULL, slice.args=list(), chunk.args=list(), oth.args=list(), 
      arg.dims=list(), chunkargs.ref.length=NULL) {
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

  for (ca in names(chunk.args)) {
    chunk.args[[ca]] <- chunk.args[[ca]][order(times)]
  }
  times <- sort(times)
  
  mycalls <- list()
  class(mycalls) <- "anim.frames"
  attr(mycalls, "speed") <- speed
  for (t in 1:nframes) {
    # hack for anim.lines.formula
    win.t <- if (is.character(window)) eval(parse(text=window)) else eval(window)
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

    if (! is.null(window.process)) args.t <- window.process(args.t)
    cl <- as.call(c(fn, args.t, oth.args)) # or match.call?
    attr(cl, "interval") <- intervals[t]
    mycalls[[t]] <- cl
  } 
  attr(mycalls, "dev.control.enable") <- ! any(sapply(list(points, lines, text, symbols), 
        identical, fn))
  if (show) replay(mycalls)
  
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

#' Replay an \code{anim.frames} object
#' 
#' Replay all or some of the frames of an object.
#' 
#' @param fr an \code{anim.frames} object
#' @param speed a new speed
#' @param frames numeric vector specifying which frames to replay
#' @param before an expression to evaluate before each frame is plotted
#' @param after an expression to evaluate after each frame is plotted
#' @param ... other arguments (not currently used)
#' 
#' @details
#' \code{before} and \code{after} will have the arguments from the
#' frame's call available in their environment - see the example below.
#' 
#' @examples
#' 
#' myplot <- anim.plot(1:10, 1:10, speed=3)
#' replay(myplot, speed=5)
#' replay(myplot, frames=c(1,5,6,10))
#' 
#' myplot <- anim.plot(x<-rnorm(100), x+rnorm(100,0,3), 20, window=1:t, 
#'      show=FALSE, main="Regressions as sample size increases")
#' replay(myplot, after=abline(lm(y~x), col="red"))
#'  
#' @export
replay <- function(...) UseMethod("replay")

#' @export
#' @rdname replay
replay.anim.frames <- function(fr, frames=1:length(fr), speed=attr(fr, "speed"),
  after=NULL, before=NULL, ...) {
  before2 <- substitute(before)
  after2 <- substitute(after)
  .setup.anim(dev.control.enable=attr(fr, "dev.control.enable"))
  for (t in frames) {
    argl <- as.list(fr[[t]])
    if (! missing(before)) eval(before2, argl)
    eval(fr[[t]])
    if (! missing(after)) eval(after2, argl)
    ani.record()
    ani.pause(attr(fr[[t]], "interval")/speed)
  }
  .teardown.anim()
} 

#' Create an animated barplot.
#' 
#' @param height a vector, matrix or array. If a vector it is divided up by 
#'   \code{times} and \code{\link{barplot}} is called on each chunk. If a
#'   matrix, \code{\link{barplot}} is called on each column. If an array, 
#'   \code{\link{barplot}} is called on each matrix of form \code{height[,,i]}.
#' @param times a vector of times. If NULL and \code{height} is a matrix,
#'   the last dimension of \code{height} will be used
#' @param show,speed,use.times,window see \code{\link{anim.plot}} 
#' @param width,space,beside,names.arg,density,angle,col,border,horiz,xlim,ylim,xlab,ylab,main,sub,offset,legend.text,... arguments passed to \code{\link{barplot}}. 
#'  
#' @details
#' 
#' Arguments \code{width, names.arg, density, angle, col, border} 
#' and \code{offset} may be either vectors
#' of length \code{length(tbl)} or matrices with one column for each unique 
#' value of \code{times}. Other arguments should be length 1 or vectors.
#' 
#' @examples
#' anim.barplot(1:100, times=rep(1:10, each=10), ylim=c(0,100))
#' ## barplot with a matrix
#' ChickWeight$wq <- cut(ChickWeight$weight, 5)
#' tbl <- as.array(xtabs(~ wq + Diet + Time, data=ChickWeight))
#' ptbl <- prop.table(tbl, 2:3)
#' anim.barplot(ptbl, xlab="Diet", ylab="N", xlim=c(0,8), legend.text=paste(
#'      "Quintile", 1:5), col=1:5)
#' anim.barplot(tbl, xlab="Diet", ylab="N", beside=TRUE, ylim=c(0,20),
#'    legend.text=paste("Quintile", 1:5), col=1:5)
#'    
#' @export
anim.barplot <- function(...) UseMethod("anim.barplot")

#' @export
#' @rdname anim.barplot
anim.barplot.default <- function(height, times=NULL, 
      show=TRUE, speed=1, use.times=TRUE, window=t, window.process=NULL, 
      width=1, space=NULL, names.arg=NULL, beside=FALSE, density=NULL, 
      angle=NULL, col=NULL, border=NULL, horiz=FALSE, xlim=NULL, 
      ylim=NULL, xlab=NULL, ylab=NULL, main=NULL, sub=NULL, offset=NULL, 
      legend.text=NULL, ...) {
  # plot data
  slice.args <- list(height=height, space=space, xlim=xlim, ylim=ylim, main=main, 
        sub=sub, xlab=xlab, ylab=ylab, legend.text=legend.text, width=width, 
        names.arg=names.arg, density=density, angle=angle, border=border, 
        offset=offset, col=col)

  args <- list(...)  
  ltdim <- if (is.logical(legend.text)) 0 else 1
  oth.args <- args
  oth.args$beside <- beside
  chunk.args <- list()
  if (is.vector(height)) chunk.args$height=height else slice.args$height=height

  hdim <- if(is.matrix(height)) 1 else 2
  if (is.null(times)) {
    if (is.array(height)) times <- 1:tail(dim(height), 1) else 
          stop("'times' not specified")
  } else if (length(times)==1) {
    lng <- if (is.array(height)) tail(dim(height), 1) else length(height)
    if (lng %% times != 0) warning("'height' length is not an exact multiple of 'times'")
    times <- rep(1:times, each=lng/times)
  }
  crl <- if(is.vector(height)) max(length(height), length(times))

  arg.dims <- list(height=hdim, space=1, xlim=1, ylim=1, main=0, sub=0, xlab=0, 
        ylab=0, space=1, legend.text=ltdim, col=1, density=1, angle=1, names.arg=1,
        border=1, offset=1, width=1)
  .do.loop(barplot, times=times, use.times=use.times, window=substitute(window),
        window.process=window.process, show=show, speed=speed, 
        slice.args=slice.args, chunk.args=chunk.args, 
        oth.args=oth.args, arg.dims=arg.dims, chunkargs.ref.length=crl)
}

#' Create an animated plot.
#' 
#' \code{anim.plot}
#' 
#' @param x,y vectors of x and y coordinates. These can be passed in any way 
#'   accepted by \code{\link{xy.coords}}.
#' @param times a vector of times. If \code{times} is length one, there will
#'   be that many frames, equally divided over the length of \code{x} and \code{y}.
#' @param show if false, do not show plot; just return calls.
#' @param speed animation speed.
#' @param window what window of times to show in each animation. The default,
#'   \code{t}, shows just plots from time t. To draw a plot incrementally,
#'   use \code{window=1:t}. 
#' @param use.times if \code{TRUE}, animation speed is determined by the 
#'   \code{times} argument. If \code{FALSE}, animation speed is constant.
#' @param xlim,ylim,col,pch,labels,cex,lty,lwd,asp,xaxp,yaxp,... arguments passed to 
#'   \code{\link{plot}}.
#' @param fn function called to create each frame
#' @param data a data frame from where the values in \code{formula} should be 
#'    taken
#' @param formula a \code{\link{formula}} such as \code{y ~ x + time}
#' @param subset a vector specifying which rows of \code{data} to use
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
#' to the plot as a whole, and always have length 1, such as \code{xlab} and
#' \code{main}, can be passed as vectors and will be recycled to the number of
#' frames. 
#' \item Parameters that apply to the plot as a whole, and can have
#' length > 1, such as \code{xlim} and \code{ylim}, can be passed as vectors or
#' matrices. If vectors, the same vector will be passed to every frame. If
#' matrices, column \code{i} will be passed to the \code{i}'th frame. 
#' }
#' 
#' @examples
#' x <- rep(1:100/10, 10)
#' times <- rep(1:10, each=100)
#' y <- sin(x*times/4)
#' anim.plot(x,y,times, type="l", col="orange", lwd=2)
#' 
#' ## changing colours - a per-point parameter
#' cols <- (x+9*times)/100 # length 1000
#' anim.plot(x,y,times, ylab="Sine wave", type="l", col=rgb(cols, 0, 1-cols), lwd=2)
#' anim.plot(x,y,times, ylab="Sine wave", type="p", col=rainbow(100)[x *10])
#' 
#' ## changing line width - a whole-plot parameter
#' anim.plot(x, y, times, lwd=matrix(1:10, ncol=10), type="l")
#'           
#' ## times as a single number
#' anim.plot(1:10, 1:10, times=5)
#'            
#' ## incremental plot using window
#' anim.plot(1:10, 1:10, window=1:t)
#' 
#' ## moving window
#' anim.plot(1:10, 1:10, window=(t-2):t)
#' 
#' ## discoveries 1860-1959
#' xlim <- rbind(1860:1959,1870:1969)
#' anim.plot(1860:1959, discoveries, times=1:100, xlim=xlim,  
#'      xaxp=rbind(xlim, 10), window=t:(t+10), type="h", lwd=8, speed=5)
#'      
#' ## Formula interface
#' ChickWeight$chn <- as.numeric(as.factor(ChickWeight$Chick))
#' tmp <- anim.plot(weight ~ chn + Time, data=ChickWeight, col=as.numeric(Diet), 
#'      pch=as.numeric(Diet), speed=3)
#' 
#' # adding extra arguments:
#' replay(tmp, after=legend("topleft", legend=paste("Diet", 1:4), pch=1:4, col=1:4))
#'  
#'  ## Zooming in:
#'  x <- rnorm(4000); y<- rnorm(4000)
#'  x <- rep(x, 40); y <- rep(y, 40)
#'  xlims <- 4*2^(-(1:40/10))
#'  # matrices w
#'  ylims <- xlims <- rbind(xlims, -xlims) 
#'  anim.plot(x, y, times=40, speed=5, xlim=xlims, ylim=ylims, 
#'        col=rgb(0,0,0,.3), pch=19)
#'  
#'  ## Earthquakes this week
#'  if (require("maps")) {
#'    eq = read.table(
#'        file="http://earthquake.usgs.gov/earthquakes/catalogs/eqs7day-M1.txt", 
#'        fill=TRUE, sep=",", header=TRUE)
#'    eq$time <- as.numeric(strptime(eq$Datetime, "%A, %B %d, %Y %X UTC"))
#'  eq <- eq[-1,]
#'    map('world')
#'    tmp <- anim.points(Lat ~ Lon + time, data=eq, cex=Magnitude, col=rgb(
#'          1-Depth/200, 0, Depth/200,.7), pch=19, speed=3600*12, show=FALSE)   
#'    replay(tmp, before=map('world', fill=TRUE, col="wheat"))
#'  }
#' @export
anim.plot <- function(...) UseMethod("anim.plot")

#' @export
#' @rdname anim.plot
anim.points <- function(...) UseMethod("anim.points")

#' @export
#' @rdname anim.plot
anim.lines <-function(...) UseMethod("anim.lines")

#' @export
#' @rdname anim.plot
anim.text <-function(...) UseMethod("anim.text")

#' @export 
#' @rdname anim.plot
anim.plot.default <- function (x, y=NULL, times=1:length(x), speed=1, show=TRUE,
      use.times=TRUE, window=t, window.process=NULL, xlim=NULL, ylim=NULL, 
      col=par("col"), xaxp=NULL, yaxp=NULL, pch=par("pch"), cex=1, labels=NULL, 
      asp=NULL, lty=par("lty"), lwd=par("lwd"), fn=plot, ...) {  
  
  args <- list(...)
  if (! "xlab" %in% names(args)) args$xlab <- deparse(substitute(x))
  if (! "ylab" %in% names(args)) args$ylab <- deparse(substitute(y))
  xy <- xy.coords(x, y, recycle=TRUE)
  x <- xy$x
  y <- xy$y
  args$xlim <- if (is.null(xlim)) range(x[is.finite(x)]) else xlim
  args$ylim <- if (is.null(ylim)) range(y[is.finite(y)]) else ylim
  
  if (length(times)==1) {
    lng <- length(x)
    if (lng %% times != 0) warning("'height' length is not an exact multiple of 'times'")
    times <- rep(1:times, each=lng/times)
  }
  chunk.args <- list(x=x, y=y, col=col, pch=pch, cex=cex)
  slice.args <- c(list(asp=asp, lty=lty, lwd=lwd, xaxp=xaxp, yaxp=yaxp), args)
  if (identical(fn, text)) {
    chunk.args$labels <- labels
    slice.args$labels <- NULL
  }
  .do.loop(fn, times=times, speed=speed, show=show, use.times=use.times, 
        window=substitute(window), window.process=window.process, 
        chunk.args=chunk.args, slice.args=slice.args, 
        arg.dims=list(xlab=0, ylab=0, xlim=1, ylim=1, xaxp=1, yaxp=1, lwd=0, 
        lty=0, asp=0, panel.first=1, panel.last=1, x=1, y=1, col=1, pch=1, cex=1, 
        type=0), chunkargs.ref.length=max(length(x), length(y)))
}

#' @export 
#' @rdname anim.plot
anim.plot.formula <- function(formula, data=parent.frame(), subset=NULL, 
      fn=plot, ...) {
  if (missing(formula) || ! inherits(formula, "formula")) 
    stop("'formula' missing or invalid")
  
  # cargo-culted from plot.formula
  m <- match.call(expand.dots=FALSE)
  eframe <- parent.frame() 
  md <- eval(m$data, eframe)
  dots <- lapply(m$..., eval, md, eframe) 
  mf <- model.frame(formula, data=md)
  subset.expr <- m$subset
  if (!missing(subset)) {
    s <- eval(subset.expr, data, eframe)
    l <- nrow(mf)
    dosub <- function(x) if (length(x) == l) x[s] else x
    dots <- lapply(dots, dosub)
    mf <- mf[s, ]
  }
  
  # get levels of t. 
  x <- mf[,2]
  y <- mf[,1]
  tm <- if (ncol(mf) >= 3) mf[,3] else 1:length(x)
  
  # why doesn't ordering happen happen OK in .do.loop?
  ot <- order(tm)
  # we are basically praying here:
  dots <- lapply(dots, function(z) if (length(z)==length(tm)) z[ot] else z) 
  
  x <- x[ot]
  y <- y[ot]
  tm <- tm[ot]
  if (! "xlab" %in% names(dots)) dots$xlab <- all.vars(mf)[2] 
  if (! "ylab" %in% names(dots)) dots$ylab <- all.vars(mf)[1]
  do.call("anim.plot", c(list(x=x, y=y, times=tm, fn=fn), dots))
}

#' @export 
#' @rdname anim.plot
anim.points.default <- function(...) anim.plot.default(..., fn=points)

#' @export 
#' @rdname anim.plot
anim.lines.default <- function(...) anim.plot.default(..., fn=lines)

#' @export 
#' @rdname anim.plot
anim.text.default <- function(...) anim.plot.default(..., fn=text)

#' @export 
#' @rdname anim.plot
anim.symbols <- function(...) anim.plot.default(..., fn=symbols)


#' @export 
#' @rdname anim.plot
anim.points.formula <- function(formula, ...) {
  m <- match.call(expand.dots=TRUE)
  fn <- as.character(m[[1]])
  fn <- sub("anim.([a-z]+).formula", "\\1", fn)
  if (fn=="lines") m[["window"]] <- "t:(t+1)" # e.g. lines(1,1) won't work...
  fn <- eval(as.name(fn))
  m[[1]] <- quote(anim.plot.formula)
  m[["fn"]] <- fn
  eval(m)
}

#' @export 
#' @rdname anim.plot
anim.lines.formula <- anim.points.formula

#' @export 
#' @rdname anim.plot
anim.text.formula <- anim.points.formula


#' Create an animated contour plot or perspective plot
#' 
#' Create an animated contour plot or perspective plot of 3D data.
#' 
#' @param x,y,z,... parameters passed to \code{\link{contour}} or \code{\link{persp}}
#' @param times,speed,use.times,window,show see \code{\link{anim.plot}} for details.
#' @param fn underlying function to use.
#' 
#' @examples
#' 
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
#'    
#' cplot2 <- apply(cplot, 1:2, function(x) seq(0, x[20], length.out=20))
#' dim(cplot2)
#' cplot2 <- aperm(cplot2, c(2,3,1))
#' par(bg="white")
#' anim.persp(z=cplot2, times=1:20, xlab="", ylab="", zlab="Height", phi=45,
#' theta=30, speed=5, border=NA, r=3, col="yellowgreen", shade=.5, box=FALSE)
#'  
#' if (require("plot3D")) {
#'  xlim <- -4:0*10
#'  ylim <- (xlim + 40)/2
#'  xlim <- rbind(xlim, xlim+60)
#'  ylim <- rbind(ylim, ylim+60)
#'  anim.contour(Hypsometry$x, Hypsometry$y, Hypsometry$z, times=5, 
#'        levels=c(seq(0,500, 50), seq(600,1500,100)), xlim=xlim, ylim=ylim,
#'        col=c("black", rev(topo.colors(15))))
#' }
#' @export
anim.contour <- function(...) UseMethod("anim.contour")

#' @export
#' @rdname anim.contour
anim.filled.contour <- function(...) UseMethod("anim.filled.contour")

#' @export
#' @rdname anim.contour
anim.filled.contour.default <- function(...) anim.contour.default(..., fn=filled.contour)

#' @export
#' @rdname anim.contour
anim.persp <- function(...) {
  m <- match.call(expand.dots=TRUE)
  m[[1]] <- quote(anim.contour)
  m$fn <- quote(persp)
  eval(m)
}

#' @export
#' @rdname anim.contour
anim.contour.default <- function(x, y, z, times, speed=1, use.times=TRUE, window=t, 
      window.process=NULL, show=TRUE, fn=contour, ...) {
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
  if (length(times)==1) times <- 1:times
  .do.loop(fn, times=times, show=show, use.times=use.times,
        window=window, window.process=window.process,
        slice.args=c(slice.args, dots), 
        arg.dims=list(z=2, x=1, y=1, nlevels=0, levels=1, 
        labels=1, labcex=0, drawlabels=0, xlim=1, ylim=1, zlim=1, vfont=1,
        axes=0, frame.plot=0, col=1, lty=1, lwd=1, color.palette=1))
}

#' Draw an animated histogram.
#' 
#' @param x,density,angle,col,border,... parameters passed to \code{\link{hist}}.
#' @param times,show,speed,use.times,window see \code{\link{anim.plot}}.
#' 
#' @details
#' Parameters \code{x, density, angle, col} and \code{border} are all
#' "chunked", i.e. first recycled to the length of \code{times} or \code{x}
#' (whichever is longer), then split according to the unique values of \code{times}.
#' See \code{\link{anim.plot}} for more details.
#' 
#' @examples 
#' anim.hist(rep(rnorm(5000), 7), times=rep(1:7, each=5000), 
#'      breaks=c(5,10,20,50,100,200, 500, 1000))
#' @export
anim.hist <- function(x, times, speed=1, show=TRUE, use.times=TRUE, window=t,
      window.process=NULL, density=NULL, angle=NULL, col=NULL, border=NULL, ...) {
  
  dots <- list(...)
  if (! "breaks" %in% names(dots)) dots$breaks = "Sturges"
  if (! "xlab" %in% names(dots)) dots$xlab <- ""
  if (! "main" %in% names(dots)) dots$main <- "Histogram"
    
  dbr <- if (is.matrix(dots$breaks)) 1 else 0
  .do.loop(hist, times=times, show=show, speed=speed, use.times=use.times, 
        window=substitute(window), window.process=window.process, 
        chunk.args=list(x=x, density=density, angle=angle, col=col, 
        border=border), slice.args=dots, arg.dims=list(breaks=dbr, xlim=1, 
        ylim=1, xlab=1, x=1), chunkargs.ref.length=max(length(x), length(times)))
}

#' Draw an animated curve.
#' 
#' This function is the animated version of \code{\link{curve}}.
#' 
#' @param expr a function which takes two arguments, or an expression involving
#'    \code{x} and \code{t}.
#' @param x values of \code{x} at which the function will be evaluated in each frame.
#'    Alternatively, you may specify \code{from, to} and \code{n}.
#' @param from,to endpoints of \code{x}
#' @param n number of values of \code{x} at which the function will be evaluated
#'   for each frame
#' @param times vector of values of \code{t} at which the function will be 
#'   evaluated. Each unique value creates a single animation frame.
#' @param type,... parameters passed to \code{\link{anim.plot.default}}
#' 
#' @details
#' Note that \code{times} is interpreted differently here than elsewhere. In
#' particular, it cannot be a length-1 vector.
#' 
#' @examples
#' anim.curve(x^t, times=10:50/10, n=20)
#' anim.curve(sin(x*t), times=1:30, n=100, speed=12, col="darkgreen", from=-1, to=1)
#' 
#' ## curve is constant in t, but window moves. 
#' ## NB: 'from' and 'to' control where the expression is evaluated. 
#' ## 'xlim' just controls the window.
#' anim.curve(sin(cos(-x)*exp(x/2)), times=0:100/10, from=-5, to=10, n=500, 
#'      col="red", lwd=2, xlim=rbind(top <- seq(-5, 10, 1/10), top+5))
#' @export
anim.curve <- function(expr, x=NULL, from=0, to=1, n=255, times, type="l", ...) {
  sexpr <- substitute(expr)
  if (is.name(sexpr)) {
    expr <- call(as.character(sexpr), as.name("x"), as.name("t"))
  } else {
    expr <- sexpr
  }
  if (is.null(x)) x <- seq.int(from, to, length.out=n)
  
  y <- outer(x, times, function (x,t) {
    ll <- list(x=x, t=t)
    eval(expr, envir=ll, enclos=parent.frame())
  })
  y <- as.vector(y)
  times <- rep(times, each=length(x))
  anim.plot(x=x, y=y, times=times, type=type, ...)
}

#' Save an anim.frames object in various formats.
#' 
#' This function simply calls replay on the object and then calls
#' \code{\link{animation::saveGIF}} and friends on the result.
#' 
#' @param obj an \code{anim.frames} object
#' @param type one of 'GIF', 'Video', 'SWF', 'HTML', or 'Latex'
#' @param filename file to save to
#' @param ... arguments passed to e.g. \code{\link{saveGIF}}
#' 
#' @details
#' 
#' For most of the underlying functions, the \code{times} parameter
#' will be ignored as if you had set \code{use.times=FALSE}.
#' 
#' @examples
#' 
#' \dontrun{
#' tmp <- anim.plot(1:10, 1:10, pch=1:10, show=FALSE)
#' anim.save(tmp, "GIF", "filename.gif")
#' 
#' ## for anything more complex. Note the curlies:
#' saveGIF({replay(tmp, after=legend("topleft", legend="My legend"))},
#'  "filename.gif")
#' }
#' @export
anim.save <- function(obj, type, filename, ...) {
  stopifnot(type %in% c("GIF", "Video", "SWF", "HTML", "Latex"))
  fn <- as.name(paste("save", type, sep=""))
  mf <- match.call(expand.dots=FALSE)
  mf[[1]] <- fn
  mf$obj <- NULL
  mf$expr <- substitute(replay(obj))
  mf$type <- NULL
  switch(type, 
    "GIF"=mf$movie.name <- filename, 
    "Video"=mf$video.name <- filename,
    "SWF"=mf$swf.name <- filename, 
    "HTML"=mf$htmlfile <- filename, 
    "Latex"=mf$latex.filename <- filename
  )
  eval(mf)
}

