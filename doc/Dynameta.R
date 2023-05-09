## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("devtools")
#  library(devtools)

## ---- eval = FALSE------------------------------------------------------------
#  devtools::install_github("gls21/Dynameta", build_vignettes = TRUE)

## ----setup--------------------------------------------------------------------
library(Dynameta)
library(tibble) # for previewing sample dataset 

## ---- eval = FALSE------------------------------------------------------------
#  help(package = "Dynameta")

## -----------------------------------------------------------------------------
dim(Dynameta::sample_data)

# Print dataset as a tibble (nice way to preview dataset)
as_tibble(sample_data)

## ---- eval = FALSE------------------------------------------------------------
#  Dynameta::launch_Dynameta() # The function takes no arguments

