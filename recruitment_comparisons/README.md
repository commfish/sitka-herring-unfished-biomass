# Comparison of recruitment forecasting methods

## Overview

Recruitment in fish stocks is a notoriously variable vital rate that drives population 
dynamics and presents a particular challenge to estimate and to forecast within stock 
assessments. Despite the inherent variability in recruitment and/or lack of a clear 
relationship to spawning biomass or other driving factors, the practical application 
of stock assessment and management generally includes recruitment forecasts as part of 
forecasted biomass and age compositions from which allowable harvest and fisheries 
opening and closures are often determined. There are many different methods used by 
fisheries researchers to forecast recruitment. Recruitment forecasting methods include 
classical spawner-recruit models such as Ricker and Beverton-Holt, 
environmentally-conditioned stock recruit models, sampling algorithms, time series 
methods, and modern machine learning algorithms. 

In a recent paper, *Van Beveren et al. (2021)* published a framework for comparing 
different recruitment forecasting methods used in stock assessments. The general idea 
is to use an age-structured model to represent the true state of a fish stock (i.e., 
the “operating model”), truncate the final years of the operating model, produce 
recruitment forecasts using the multiple methods under evaluation, and then use the 
forecasts to calculate spawning biomass. Recruitment is defined as the abundance of 
immature and mature herring.  The forecast methods are then evaluated by their ability 
to produce accurate predictions (least biased, and most precise) of spawning biomass 
in the operating model. This approach was adapted to choose a recruitment forecasting 
method for application in biomass simulations for the Sitka Sound Pacific herring 
stock. In this subproject, nine methods were evaluated by their ability to predict 
spawning biomass of Sitka Sound Pacific herring based on the performance metrics mean 
percent error (MPE) and mean absolute percent error (MAPE). The goal was to identify
an appropriate method for simulating recruitment in the Sitka Sound herring unfished 
biomass simulations. The least biased, and most precise, recruitment forecasting 
method was used to simulate recruitment to help determine the unfished biomass of 
Sitka Sound herring.

## Contents

The most important files are `r-code/forecast_comparisons.r`, which executes the 
program described loosely above, and `r-code/forecast_comparisons_analysis.r` which
creates plots and runs analyses interpreting the results. The `r-code/sitka_ersst.r` 
helper script extracts sea-surface temperature data that gets used to fit 
sst-conditioned ricker models. The `data` directory contains inputs that are fed to 
`r-code/forecast_comparisons.r`. The `flowchart-anim` directory creates an animation
that illustrates the process implemented by `r-code/forecast_comparisons.r`. 

## Conclusion

Forecasting recruitment by sampling past recruits, stratified by their respective 
spawning biomasses, was the least biased and most precise method for predicting 
future spawning biomass in the operating model. Hence, "three-strata sampling" was
used to simulate recruitment in the Sitka Sound herring unfished biomass simulations.
