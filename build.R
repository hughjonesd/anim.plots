
# build script, builds vignette manually and updates github.io website


# first, create vignette with animation files
rmarkdown::render('vignettes/anim.plots.Rmd', clean = FALSE)
# now, vignettes has what we want including the anim.plots_files folder
# copy them to inst/doc and to docs for website
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
