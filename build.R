
# build script

# first, create vignette with animation files
rmarkdown::render('vignettes/anim.plots.Rmd', clean = FALSE)
# now, vignettes has what we want including the anim.plots_files folder
# copy them to inst/doc
# run build with --no-build-vignettes
file.copy('vignettes/anim.plots.Rmd', 'inst/doc', overwrite = TRUE)
file.copy('vignettes/anim.plots_files', 'inst/doc', recursive = TRUE)
devtools::build(vignettes = FALSE)
