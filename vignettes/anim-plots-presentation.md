Simple animation with anim.plots
========================================================
author: David Hugh-Jones
date: 30 October 2014

Animation in R
========================================================


- Creating animations in R was hard. 
- Then Yihui Xie wrote the `animation` package. 
- Plot, record, loop, save....
- `anim.plots` adds some syntactic sugar to this.
- Code is available at [http://github.com/hughjonesd/anim.plots]






Simple example
========================================================


```r
library(anim.plots)
anim.plot(1:5, 1:5, col="green")
```

<video   controls="controls" loop="loop"><source src="anim-plots-presentation-figure/unnamed-chunk-2-.webm" />video of chunk unnamed-chunk-2</video>

`times` controls the frames
========================================================


```r
x <- rep(1:100/10, 20)
times <- rep(1:20, each=100) # twenty frames with 100 points each
y <- sin(x*times/4)
waves <- anim.plot(x,y,times, type="l", col="orange", lwd=2, speed=2)
```

<video   controls="controls" loop="loop"><source src="anim-plots-presentation-figure/unnamed-chunk-3-.webm" />video of chunk unnamed-chunk-3</video>

Incremental animations
========================================================

- Use `window`:


```r
anim.plot(rep(1:10,2), rep(2:1, each=10), window=1:t, pch=1:20, ylim=c(0,3), cex=2,
      col=1:5, xlab=paste("Plot symbols", 1:20))
```

<video   controls="controls" loop="loop"><source src="anim-plots-presentation-figure/unnamed-chunk-4-.webm" />video of chunk unnamed-chunk-4</video>


Parameters
========================================================

- Per-point parameters get recycled to the number of points
- Per-plot parameters get recycled to the number of plot frames
- For more complex parameters, use a matrix

Example - zooming in
========================================================


```r
 x <- rnorm(4000); y<- rnorm(4000)
 x <- rep(x, 40); y <- rep(y, 40)
 xlims <- 4*2^(-(1:40/10))
 ylims <- xlims <- rbind(xlims, -xlims) 
 anim.plot(x, y, times=40, speed=5, xlim=xlims, ylim=ylims, 
       col=rgb(0,1,.5,.3), pch=19)
```

<video   controls="controls" loop="loop"><source src="anim-plots-presentation-figure/unnamed-chunk-5-.webm" />video of chunk unnamed-chunk-5</video>

`window` again
========================================================



```r
## discoveries 1860-1959
xlim <- rbind(1860:1959,1870:1969)
anim.plot(1860:1959, discoveries, times=1:100, xlim=xlim, col="red",
     xaxp=rbind(xlim, 10), window=t:(t+10), type="h", lwd=8, speed=5)
```

<video   controls="controls" loop="loop"><source src="anim-plots-presentation-figure/unnamed-chunk-6-.webm" />video of chunk unnamed-chunk-6</video>

Formula interface
========================================================


```r
data(ChickWeight)
ChickWeight$chn <- as.numeric(as.factor(ChickWeight$Chick))

tmp <- anim.plot(weight ~ chn + Time, data=ChickWeight, col=as.numeric(Diet), 
     pch=as.numeric(Diet), speed=3)
```

<video   controls="controls" loop="loop"><source src="anim-plots-presentation-figure/unnamed-chunk-7-.webm" />video of chunk unnamed-chunk-7</video>



Extra plotting commands
========================================================

- Use `replay`:


```r
replay(tmp, after=legend("topleft", legend=paste("Diet", 1:4), pch=1:4, col=1:4))
```

<video   controls="controls" loop="loop"><source src="anim-plots-presentation-figure/unnamed-chunk-8-.webm" />video of chunk unnamed-chunk-8</video>

```
NULL
```

Histogram
========================================================



```r
anim.hist(rep(rnorm(5000), 7), times=rep(1:7, each=5000), 
     breaks=c(5,10,20,50,100,200, 500, 1000))
```

<video   controls="controls" loop="loop"><source src="anim-plots-presentation-figure/unnamed-chunk-9-.webm" />video of chunk unnamed-chunk-9</video>



Animated curve
========================================================



```r
anim.curve(x^t, times=10:50/10, n=20)
```

<video   controls="controls" loop="loop"><source src="anim-plots-presentation-figure/unnamed-chunk-10-.webm" />video of chunk unnamed-chunk-10</video>

Contour plot
========================================================



```r
data(volcano)
tmp <- volcano
tmp[] <- 200 - ((row(tmp) - 43)^2 + (col(tmp) - 30)^2)/20
cplot <- array(NA, dim=c(87,61,20))
cplot[,,1] <- tmp
cplot[,,20] <- volcano
cplot <- apply(cplot, 1:2, function(x) seq(x[1], x[20], length.out=20))
cplot <- aperm(cplot, c(2,3,1))

anim.contour(z=cplot, times=1:20, speed=3, levels=80 + 1:12*10, lty=c(1,2,2))
```

<video   controls="controls" loop="loop"><source src="anim-plots-presentation-figure/unnamed-chunk-11-.webm" />video of chunk unnamed-chunk-11</video>

Earthquakes last week
=============================

- Needs internet access

<video   controls="controls" loop="loop"><source src="anim-plots-presentation-figure/unnamed-chunk-12-.webm" />video of chunk unnamed-chunk-12</video>

```
NULL
```

Thanks!
=============================

- http://github.com/hughjonesd/anim.plots


