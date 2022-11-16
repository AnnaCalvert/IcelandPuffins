library(RMark)
library(tidyverse)
library(here)
library(rnaturalearth)
library(sf)
library(R2ucare)
library(knitr)
library(kableExtra)
library(grid)
library(patchwork)
library(png)
#library(R2jags)

options(scipen = 999, digits=4)

# Set up folders

(proj.dir <- here())
(data.dir <- file.path(proj.dir, "data"))
(Rdata.dir <- file.path(data.dir, "Rdata"))
(CMR.dir <- file.path(data.dir, "CMR"))
(R.dir <- file.path(proj.dir, "R"))

#source the functions that we've created
source(file.path(R.dir, "functions.R"))
source(file.path(R.dir, "CMRfunctions.R"))

sessionInfo()
