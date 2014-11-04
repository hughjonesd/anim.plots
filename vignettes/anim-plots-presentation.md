<link rel="stylesheet" href="http://vis.supstat.com/assets/themes/dinky/css/scianimator.css">
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"></script>
<script src="http://vis.supstat.com/assets/themes/dinky/js/jquery.scianimator.min.js"></script>
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



Simple example
========================================================


```r
library(anim.plots)
ap <- anim.plot(1:5, 1:5, col="green")
```

```r
anim.save(ap, "ap.gif", type="GIF")
```


<div class="scianimator">
<div id="unnamed_chunk_3" style="display: inline-block;">
</div>
</div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      var imgs = Array(10);
      for (i=0; ; i++) {
        if (i == imgs.length) break;
        imgs[i] = "anim-plots-presentation-figure/unnamed-chunk-3-" + (i + 1) + ".png";
      }
      $("#unnamed_chunk_3").scianimator({
          "images": imgs,
          "delay": 250,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("#unnamed_chunk_3").scianimator("play");
    });
  })(jQuery);
</script>

```
[1] TRUE
```

`times` controls the frames
========================================================

```r
x <- rep(1:100/10, 20) 
times <- rep(1:20, each=100) # twenty frames with 100 points each
y <- sin(x*times/4)
waves <- anim.plot(x,y,times, type="l", col="orange", lwd=2)
```


<div class="scianimator">
<div id="unnamed_chunk_4" style="display: inline-block;">
</div>
</div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      var imgs = Array(20);
      for (i=0; ; i++) {
        if (i == imgs.length) break;
        imgs[i] = "anim-plots-presentation-figure/unnamed-chunk-4-" + (i + 1) + ".png";
      }
      $("#unnamed_chunk_4").scianimator({
          "images": imgs,
          "delay": 250,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("#unnamed_chunk_4").scianimator("play");
    });
  })(jQuery);
</script>

Incremental animations
========================================================
- Use `window`:

```r
symbs <- anim.plot(rep(1:10,2), rep(2:1, each=10), window=1:t, pch=1:20, ylim=c(0,3), cex=2,
      col=1:5, xlab=paste("Plot symbols", 1:20))
```


<div class="scianimator">
<div id="unnamed_chunk_5" style="display: inline-block;">
</div>
</div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      var imgs = Array(20);
      for (i=0; ; i++) {
        if (i == imgs.length) break;
        imgs[i] = "anim-plots-presentation-figure/unnamed-chunk-5-" + (i + 1) + ".png";
      }
      $("#unnamed_chunk_5").scianimator({
          "images": imgs,
          "delay": 250,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("#unnamed_chunk_5").scianimator("play");
    });
  })(jQuery);
</script>


Parameters
========================================================
- Per-point parameters get recycled to the number of points
- Per-plot parameters get recycled to the number of plot frames
- For more complex parameters, use a matrix

Example - zooming in
========================================================
<small>

```r
 x <- rnorm(4000); y<- rnorm(4000)
 x <- rep(x, 40); y <- rep(y, 40)
 xlims <- 4*2^(-(1:40/10))
 ylims <- xlims <- rbind(xlims, -xlims) 
 anim.plot(x, y, times=40, xlim=xlims, ylim=ylims, col=rgb(0,1,.5,.3), pch=19)
```


<div class="scianimator">
<div id="unnamed_chunk_6" style="display: inline-block;">
</div>
</div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      var imgs = Array(40);
      for (i=0; ; i++) {
        if (i == imgs.length) break;
        imgs[i] = "anim-plots-presentation-figure/unnamed-chunk-6-" + (i + 1) + ".png";
      }
      $("#unnamed_chunk_6").scianimator({
          "images": imgs,
          "delay": 250,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("#unnamed_chunk_6").scianimator("play");
    });
  })(jQuery);
</script>
</small>

`window` again
========================================================

```r
## discoveries 1860-1959
xlim <- rbind(1860:1959,1870:1969)
anim.plot(1860:1959, discoveries, times=1:100, xlim=xlim, col="red",
     xaxp=rbind(xlim, 10), window=t:(t+10), type="h", lwd=8)
```


<div class="scianimator">
<div id="unnamed_chunk_7" style="display: inline-block;">
</div>
</div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      var imgs = Array(100);
      for (i=0; ; i++) {
        if (i == imgs.length) break;
        imgs[i] = "anim-plots-presentation-figure/unnamed-chunk-7-" + (i + 1) + ".png";
      }
      $("#unnamed_chunk_7").scianimator({
          "images": imgs,
          "delay": 250,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("#unnamed_chunk_7").scianimator("play");
    });
  })(jQuery);
</script>

Formula interface
========================================================

```r
data(ChickWeight)
ChickWeight$chn <- as.numeric(as.factor(ChickWeight$Chick))

tmp <- anim.plot(weight ~ chn + Time, data=ChickWeight, col=as.numeric(Diet), 
     pch=as.numeric(Diet))
```


<div class="scianimator">
<div id="unnamed_chunk_8" style="display: inline-block;">
</div>
</div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      var imgs = Array(12);
      for (i=0; ; i++) {
        if (i == imgs.length) break;
        imgs[i] = "anim-plots-presentation-figure/unnamed-chunk-8-" + (i + 1) + ".png";
      }
      $("#unnamed_chunk_8").scianimator({
          "images": imgs,
          "delay": 250,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("#unnamed_chunk_8").scianimator("play");
    });
  })(jQuery);
</script>


Need to run extra commands?
========================================================
- Use `replay` with `before` and `after`:

```r
replay(tmp, after=legend("topleft", legend=paste("Diet", 1:4), pch=1:4, col=1:4))
```


<div class="scianimator">
<div id="unnamed_chunk_9" style="display: inline-block;">
</div>
</div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      var imgs = Array(12);
      for (i=0; ; i++) {
        if (i == imgs.length) break;
        imgs[i] = "anim-plots-presentation-figure/unnamed-chunk-9-" + (i + 1) + ".png";
      }
      $("#unnamed_chunk_9").scianimator({
          "images": imgs,
          "delay": 250,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("#unnamed_chunk_9").scianimator("play");
    });
  })(jQuery);
</script>

Histogram
========================================================

```r
anim.hist(rep(rnorm(5000), 7), times=rep(1:7, each=5000), 
     breaks=c(5,10,20,50,100,200, 500, 1000))
```


<div class="scianimator">
<div id="unnamed_chunk_10" style="display: inline-block;">
</div>
</div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      var imgs = Array(5);
      for (i=0; ; i++) {
        if (i == imgs.length) break;
        imgs[i] = "anim-plots-presentation-figure/unnamed-chunk-10-" + (i + 1) + ".png";
      }
      $("#unnamed_chunk_10").scianimator({
          "images": imgs,
          "delay": 250,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("#unnamed_chunk_10").scianimator("play");
    });
  })(jQuery);
</script>

Animated curve
========================================================

```r
anim.curve(x^t, times=10:50/10, n=20)
```


<div class="scianimator">
<div id="unnamed_chunk_11" style="display: inline-block;">
</div>
</div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      var imgs = Array(41);
      for (i=0; ; i++) {
        if (i == imgs.length) break;
        imgs[i] = "anim-plots-presentation-figure/unnamed-chunk-11-" + (i + 1) + ".png";
      }
      $("#unnamed_chunk_11").scianimator({
          "images": imgs,
          "delay": 250,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("#unnamed_chunk_11").scianimator("play");
    });
  })(jQuery);
</script>

Contour plot
========================================================

- One I made earlier
- `cplot` comes from manipulating the `volcano` dataset
<small>

<div class="scianimator">
<div id="unnamed_chunk_12" style="display: inline-block;">
</div>
</div>
<script type="text/javascript">
  (function($) {
    $(document).ready(function() {
      var imgs = Array(20);
      for (i=0; ; i++) {
        if (i == imgs.length) break;
        imgs[i] = "anim-plots-presentation-figure/unnamed-chunk-12-" + (i + 1) + ".png";
      }
      $("#unnamed_chunk_12").scianimator({
          "images": imgs,
          "delay": 250,
          "controls": ["first", "previous", "play", "next", "last", "loop", "speed"],
      });
      $("#unnamed_chunk_12").scianimator("play");
    });
  })(jQuery);
</script>
</small>

Earthquakes last week
=============================



```
Error in file(file, "rt") : cannot open the connection
```
