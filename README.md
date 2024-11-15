
# Sitka Herring Unfished Biomass

## Overview

This repository contains R programs which attempt to update Alaska Department of 
and Game (ADF&G) calculations of the unfished biomass of the Sitka Sound herring 
stock. The unfished biomass of this stock - denoted $B_0$ in current references 
referred to as average unfished biomass (AUB) in older literature - is a reference
point used to set a trigger threshold for fishery harvest in this stock. ADF&G 
estimated $B_0$ to be 67k tons in the late 1990's, which resulted in a trigger 
threshold of 20k tons which was later raised to 25k tons. 

Since that estimation, Sitka Sound herring stock biomass appears to more productive
reached historic highs in the late 2010's/early 2020's. This repository provides 
reproducible code for a new estimate of unfished biomass, as well as other related
projects. The updated estimate for $B_0$ was 85,576 tons. This codebase may be used
for similar calculations in the future; alternatively, structural changes to the 
stock assessment model may facilitate other estimation procedures.

## Subdirectories

### `estimate_B0`

This folder contains the R code performing a simulation that estimates Sitka Sound
unfished biomass. The scripts in the directory rely heavily on functions in the 
package `herringSim`. See a full writeup of methodology and results in *Roberts, Miller, and Dressel (2024)*

### `herringSim`

Data and R functions used throughout this repository, as well as relevant code 
documentation, reside in the `herringSim` R package. Once you have cloned the
repository, you can install the package by running `R CMD INSTALL herringSim_0.2.0.tar.gz`
in a `cmd` or `bash` shell. Make sure your working directory is at the root of the repo.


### `recruitment_comparisons`

This project attempts to identify a method to simulate recruitment in the Sitka Sound
herring unfished biomass simulations performed in `estimate_B0`. See appendix d
of *Roberts, Miller, and Dressel (2024)* for a writeup of this analysis.


## To run Sitka Sound unfished biomass calculation

0. Obtain all software dependencies 
1. Clone `sitka-sound-unfished-biomass` repository
2. Install `herringSim`. Verify that you can attach its contents like any other R package
3. Execute the R script `estimate_B0/r-code/estimate_B0.r`. Runtime approximately 8 hours.
4. View results by opening `estimate_B0/plots/combined_histogram.png`

It is recommended to read *Roberts, Miller, and Dressel (2024)* for a full understanding of the
methods in this simulation. See the `herringSim` functions `B_sim_init()` and 
`B_sim_empirical()` for more details on the implementation of the methods.

## Dependencies

This repository was created with R 4.2.0 and the following packages are used. In 
addition, you will need Quarto 1.4 to render `.qmd` files. 

|package      |version |
|:------------|:-------|
|dplyr        |1.1.0   |
|flextable    |0.9.3   |
|ggplot2      |3.4.1   |
|officer      |0.6.2   |
|data.table   |1.14.2  |
|ggrepel      |0.9.1   |
|scales       |1.2.1   |
|rmarkdown    |2.14    |
|magrittr     |2.0.3   |
|shiny        |1.7.2   |
|shinyWidgets |0.7.2   |
|stats4       |4.2.0   |
|reshape2     |1.4.4   |
|stats        |4.2.0   |
|RColorBrewer |1.1.3   |
|cowplot      |1.1.1   |
|gganimate    |1.0.8   |
|ggforce      |0.4.1   |
|gifski       |1.6.6.1 |
|here         |1.0.1   |
|magick       |2.7.3   |
|patchwork    |1.1.2   |
|png          |0.1.7   |
|ggridges     |0.5.4   |
|knitr        |1.39    |
|chron        |2.3.58  |
|mapdata      |2.3.1   |
|maps         |3.4.1   |
|marmap       |1.0.9   |
|ncdf4        |1.21    |
|PBSmapping   |2.73.2  |
|tidyverse    |2.0.0   |

## References

Roberts, C. L., S. E. Miller, and S. C. Dressel. 2024. A simulation study to estimate the unfished biomass of Sitka Sound Pacific herring. Alaska Department of Fish and Game, Division of Commercial Fisheries, Regional Information Report No. 1J24-01, Juneau.
