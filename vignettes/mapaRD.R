## -----------------------------------------------------------------------------
devtools::install_github("gcaff/mapaRD")

## -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(mapaRD)

## -----------------------------------------------------------------------------
d <- read.csv("http://gcaff.github.io/data/RD/pob_region_rd.csv")
head(d)

