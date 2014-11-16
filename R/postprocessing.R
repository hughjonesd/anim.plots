
#' @import tools
NULL

#' Replay an \code{anim.frames} object
#' 
#' Replay all or some of the frames of an object.
#' 
#' @param x an \code{anim.frames} object
#' @param speed a new speed
#' @param frames numeric vector specifying which frames to replay
#' @param before an expression to evaluate before each frame is plotted
#' @param after an expression to evaluate after each frame is plotted
#' @param ... other arguments (not currently used)
#' 
#' @details
#' \code{before} and \code{after} will have the arguments from the
#' frame's call available in their environment - see the example.
#' 
#' The \code{plot} method simply calls \code{replay}.
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
replay.anim.frames <- function(x, frames=1:length(x), speed=attr(x, "speed"),
  after=NULL, before=NULL, ...) {
  speed # force eval
  before2 <- substitute(before)
  after2 <- substitute(after)
  .setup.anim(dev.control.enable=attr(x, "dev.control.enable"))
  times <- attr(x, "times")/speed
  intervals <- c(diff(times), 0)
  times <- times[frames]
  intervals <- intervals[frames]
  x <- x[frames]
  
  for (t in 1:length(x)) {
    argl <- as.list(x[[t]])
    if (! missing(before)) eval(before2, argl)
    eval(x[[t]])
    if (! missing(after)) eval(after2, argl)
    ani.record()
    ani.pause(intervals[t])
  }
  invisible()
} 

#' Smooth an \code{anim.frames} object
#' 
#' Some export formats ignore information in the \code{times}
#' attribute and plot frames at constant speed. \code{anim.smooth} creates
#' a smoothed version of the \code{anim.frames} object with frames at 
#' constant intervals, suitable for export.
#' 
#' @param x an \code{anim.frames} object
#' @param fps how many frames per second to smooth to
#' 
#' @return
#' A smoothed \code{anim.frames} object, with the \code{speed} attribute equal
#' to \code{fps}.
#' 
#' @details
#' Note that plot parameters such as x and y positions are not interpolated.
#' If you want your whole animation to look smoother, you have to do
#' the work yourself using e.g.\code{\link{approx}}.
#' 
#' If you smooth to a large value of \code{fps}, the animations may look bad
#' in R because they overtax the graphics engine. They should still look good
#' when saved, though.
#' 
#' @examples
#' accel <- anim.plot(1, 1:30, times=sqrt(1:30))
#' \dontrun{
#' anim.save(accel, "GIF", "wrong.gif")
#' }
#' accel <- anim.smooth(accel, fps=20)
#' \dontrun{
#' anim.save(accel, "GIF", "better.gif")
#' }
#' @export
anim.smooth <- function(x, fps=10) {
  times <- attr(x, "times")/attr(x, "speed")
  rts <- times
  # times 1,4,5. Plots 1,2,3. fps 2. new times 1, 1.5...5. new plots 1,1,1,1,1,1,2,2,3 
  times <- seq(min(times), max(times), by=1/fps)
  intervals <- c(diff(times), 0)
  idx <- sapply(times, function(x) which.max(x <=rts))
  dce <- attr(x, "dev.control.enable")
  x <- x[idx]
  class(x) <- "anim.frames"
  attr(x, "times") <- times
  attr(x, "speed") <- fps
  attr(x, "dev.control.enable") <- dce
  x
}

#' @export
#' @rdname replay
plot.anim.frames <- function(x, ...) replay(x, ...)


#' Save an anim.frames object in various formats.
#' 
#' This function simply calls replay on the object and then calls
#' \code{\link{saveGIF}} and friends on the result.
#' 
#' @param obj an \code{anim.frames} object, or an expression to evaluate
#' @param type one of 'GIF', 'Video', 'SWF', 'HTML', or 'Latex'
#' @param filename file to save to
#' @param ... arguments passed to e.g. \code{\link{saveGIF}}
#' 
#' @examples
#' 
#' \dontrun{
#' tmp <- anim.plot(1:10, 1:10, pch=1:10, show=FALSE)
#' anim.save(tmp, "mygif.gif")
#' 
#' anim.save(replay(tmp, after=legend("topleft", legend="My legend")), 
#'    "mygif2.gif")
#' }
#' 
#' @export
anim.save <- function(obj, filename,  type=switch(file_ext(filename), 
      "gif"="GIF", "mp4"="Video", "swf"="SWF", "html"="HTML", "tex"="Latex"), ...) {
  stopifnot(type %in% c("GIF", "Video", "SWF", "HTML", "Latex"))
  fn <- as.name(paste("save", type, sep=""))
  mf <- match.call(expand.dots=FALSE)
  mf[[1]] <- fn
  mf$obj <- NULL
  mf$expr <- if (inherits(obj, "anim.frames")) substitute(replay(obj)) else 
    substitute(obj)
  mf$type <- NULL
  speed <- if (! is.null(attr(obj, "speed"))) attr(obj, "speed") else 1
  ani.options(interval=1/speed)
  switch(type, 
    "GIF"=mf$movie.name <- filename, 
    "Video"=mf$video.name <- filename,
    "SWF"=mf$swf.name <- filename, 
    "HTML"=mf$htmlfile <- filename, 
    "Latex"=mf$latex.filename <- filename
  )
  eval(mf)
}

#' Merge anim.frames objects
#' 
#' Merge two or more anim.frames objects to create a new anim.frames object
#' 
#' @param ... anim.frames objects returned from, e.g. \code{\link{anim.plot}}
#' @param speed speed for the merged object. This may be left unspecified only
#'    if all objects have the same speed.
#'
#' @details
#' If two or more calls in the merged animation are at the same time, calls
#' from the earlier object in \code{...} will be run first. 
#' 
#' If you merge two animations from \code{\link{anim.plot}}, plot.window will be
#' called before each frame of the merged animation. This may not be what
#' you want. Instead, use \code{anim.points} or similar for all but the first
#' animation.
#' 
#' 
#' @examples
#' tmp <- anim.plot(1:5, 1:5, speed=2)
#' tmp2 <- anim.plot(1:5, 5:1, col="red", speed=2)
#' ## Not what you want:
#' replay(merge(tmp, tmp2))
#' 
#' ## better:
#' tmp3 <- anim.points(1:5, 5:1,col="red", speed=2)
#' newf <- merge(tmp, tmp3)
#' replay(newf)
#' ## NB: result of the merge looks different from the two
#' ## individual animations
#' 
#' ## not the same:
#' newf2 <- merge(tmp2, tmp) 
#' ## points will be called before plot!
#' replay(newf2)
#' @export
merge.anim.frames <- function(..., speed=NULL) {
  frs <- list(...)
  speeds <- sapply(frs, attr, "speed")
  if (is.null(speed)) if (max(speeds) > min(speeds)) {
    stop("'speed' not specified but some objects have different speeds")
  } else {
    speed <- max(speeds)
  }
  times <- c(sapply(frs, attr, "times"))
  tiebreaks <- sapply(1:length(frs), function(x) rep(x, length(frs[[x]])))
  newfr <- unlist(frs, recursive=FALSE)
  newfr <- newfr[order(times, tiebreaks)]
  times <- sort(times)
  class(newfr) <- "anim.frames"
  attr(newfr, "times") <- times
  attr(newfr, "speed") <- speed
  # maybe this is the wrong way to think about it?
  attr(newfr, "dev.control.enable") <- any(sapply(frs, attr, "dev.control.enable"))
  newfr
}


