
# build script, builds vignette manually

# first, create vignette with animation files
rmarkdown::render('vignettes/anim.plots.Rmd', clean = FALSE)
# now, vignettes has what we want including the anim.plots_files folder
# copy them to inst/doc
file.copy('vignettes/anim.plots.html',  'inst/doc', overwrite = TRUE)
file.copy('vignettes/anim.plots.Rmd',   'inst/doc', overwrite = TRUE)
file.copy('vignettes/anim.plots_files', 'inst/doc', recursive = TRUE)
# run build with --no-build-vignettes
devtools::build(vignettes = FALSE)
