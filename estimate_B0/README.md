# Estimate Sitka Sound Unfished Biomass

## Overview

Allowable harvest levels  of Pacific herring in Sitka Sound are prescribed using a 
threshold management strategy, wherein the fishery is closed if the estimated biomass 
falls below a certain threshold level. The department last calculated a threshold 
level for Sitka Sound herring (16,759 tons) in 1997 based on 25% of the estimated 
unfished biomass (B_0; 67,036 tons) of the stock; although in response to subsistence 
concerns the threshold level in regulation was set at 20,000 tons (1997) and later 
elevated to the current 25,000 tons (2010 to present). The analysis contained in this 
folder updated ADF&G’s estimate for the unfished spawning biomass of herring in Sitka 
Sound based on parameter estimates from the integrated statistical catch-at-age 
2023-forecast model, more rigorous statistical methods, and a longer time series of 
data. In this analysis the inputs to, and outputs from, the 2023-forecast model were 
used to simulate the average unfished biomass of the Sitka Sound Pacific herring stock 
under a no-fishing scenario in a statistically rigorous simulation. Simulations were 
repeated 1,000 times and combined into a single nonparametric distribution of spawning 
biomasses. See a full writeup of methodology and results in **final paper goes here**.

## Contents

The R script `r-code/estimate_B0.r` executes the simulation and produces the results,
which can be succinctly viewed and interpreted by the plot `combined_histogram.png`
that it generates. This script relies heavily on functions in `herringSim`. The 
raw simulation results are written to a file called
`data/raw_results_many_simulations_asa2023.txt`. In addition, a sensitivity analysis
is performed in the similarly named `r-code/estimate_B0_sensitivity.r`, in which 
the simulation is repeated but omitting the abnormally large 2019 recruitment (2016
year-class). A full writeup of this project is found at **final paper goes here**.

## Conclusions

The final estimate for unfished biomass of Sitka Sound herring based on the 
median simulated value was 85,576 tons.
