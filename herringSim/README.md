# HerringSim Package

This R package houses functions, data, and documentation used in the `commfish-private/sitka-herring-unfished-biomass` git repository. 

## Key functions

**`B_sim_init`**: Initializes a numbers-at-age matrix for simulation procedure.

**`B_sim_empirical`**: Simulates herring biomass trajectory by generating age-3     
    recruits via stratified sampling.

## Key data objects

**`*_asa1996`**: Data and outputs from 1996-forecast age-structured stock assessment
    model for Sitka Sound herring. There are different data objects for spawning stock biomass and recruitment estimates (`sr`), age-specific maturity (`maturity`), 
    weight-at-age (`mean_wt`), and average survival (`survival`).

**`*_asa2023_forecast`**: Data and outputs from 2023-forecast age-structured stock 
    assessment model for Sitka Sound herring. There are different data objects for 
    spawning stock biomass and recruitment estimates (`sr`), age-specific maturity 
    (`maturity`), weight-at-age (`mean_wt`), average survival (`survival`), 
    numbers-at-age (`naa`), and catch-at-age (`caa`).
