
# build script, builds vignette manually and updates github.io website


# first, create vignette with animation files
rmarkdown::render('vignettes/anim.plots.Rmd', clean = FALSE)
# now, vignettes has what we want including the anim.plots_files folder
# copy them to inst/doc and to docs for website
# perhaps vignettes products could be moved not copied?
for (dest in c('inst/doc', 'docs')) {
  file.copy('vignettes/anim.plots.html',  dest, overwrite = TRUE)
  file.copy('vignettes/anim.plots.Rmd',   dest, overwrite = TRUE)
  file.copy('vignettes/anim.plots_files', dest, recursive = TRUE)
}
# create presentation on website
rmarkdown::render('docs/anim-plots-presentation.Rmd')

# create presentation
# run build with --no-build-vignettes
devtools::build(vignettes = FALSE)

# create example for README.md
setwd('docs')
x <- rep(rnorm(400), 10)
y <- rep(rnorm(400), 10)
xlims <- 4 * 2^-(1:10/10)
ylims <- xlims <- rbind(xlims, -xlims)
readme_example <- anim.plot(x, y, times = 10, speed = 2, xlim = xlims, ylim = ylims, col = rgb(0,1,0,.3), pch = 19,
      show = FALSE)
anim.save(readme_example, filename = 'readme-example.gif')
setwd('..')

