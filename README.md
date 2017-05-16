# anim.plots

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/anim.plots)](https://cran.r-project.org/package=anim.plots)
[![Travis-CI Build Status](https://travis-ci.org/hughjonesd/anim.plots.svg?branch=master)](https://travis-ci.org/hughjonesd/anim.plots)

`anim.plots` provides simple animated versions of basic R plots, using the `animation`
package. It includes animated versions of `plot`, `barplot`, `persp`, `contour`,
`filled.contour`, `hist`, `curve`, `points`, `lines`, `text`, `symbols`, `segments`, and
`arrows`.

## Installation

Stable version from CRAN:

```R
install.packages("anim.plots")
```

Latest version from github:

```R
# install.packages("devtools")
devtools::install_github("anim.plots/hughjonesd")
```

## Example

Zooming into a bivariate normal distribution:

```R
x <- rep(rnorm(400), 10)
y <- rep(rnorm(400), 10)
xlims <- 4 * 2^-(1:10/10)
ylims <- xlims <- rbind(xlims, -xlims)
anim.plot(x, y, times = 10, speed = 2, xlim = xlims, ylim = ylims, col = rgb(0,1,0,.3), pch = 19,
      show = FALSE)
```

![anim.plots example](https://hughjonesd.github.io/anim.plots/readme-example.gif)

## More information

* [Vignette](https://hughjonesd.github.io/anim.plots/anim.plots.html)
* [Web presentation](https://hughjonesd.github.io/anim.plots/anim-plots-presentation.html)

## Feedback


Got a nice example? <a class="twitter-share-button"
  href="https://twitter.com/intent/tweet?text=@davidhughjones&hashtags=anim.plots,Rstats">
Tweet it</a>.