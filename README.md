
# README- IDEMtools
================

A multimetric index calculator for algae IBIs developed for the Indiana Department of Environmental Management.

## Badges

[![Maintenance](https://img.shields.io/badge/Maintained%3F-yes-green.svg)](https://github.com/Blocktt/IDEMtools/graphs/commit-activity)
[![GitHub
license](https://img.shields.io/github/license/Blocktt/IDEMtools)](https://github.com/Blocktt/MassIBItools/blob/master/LICENSE)
[![GitHub
issues](https://img.shields.io/github/issues-raw/Blocktt/IDEMtools)](https://github.com/Blocktt/MassIBItools/issues)
[![Github all
releases](https://img.shields.io/github/downloads/Blocktt/IDEMtools/total)](https://github.com/Blocktt/MassIBItools/releases)

## Installation

``` r
library(devtools)  #install if needed
Sys.setenv("TAR" = "internal")  # needed for R v3.6.0
install_github("Blocktt/IDEMtools", force=TRUE, build_vignettes=TRUE)
```

## Purpose

Functions to aid the Indiana DEM in bioassessment and IBI scoring using their updated index. Program uses Erik Leppo's BioMonTools for calculations (https://github.com/leppott/BioMonTools). 

## Status

In development.

## Usage

A Shiny app purpose built for IDEM to run IBI metric scoring. 

## Documentation

Vignette and install guide updates are planned for the future.

## Issues

<https://github.com/Blocktt/IDEMtools/issues>

