# Introduction

This is a new implementation of the methodology described in Scientia Consulting
report [Electricity allocation factor estimates for 2016/17](https://www.mfe.govt.nz/publications/climate-change/electricity-allocation-factor-estimates-201617).
This modifies the offers of certain generating units depending on inferred ETS costs
for these generating units. This generates a counterfactual that attempts to model
what the cost of electricity would have been in the absence of the ETS.

# Implementation

The original code used in the Scientia report was written entirely in GAMS as a
branch of vSPD. This implementation instead uses R code to generate offer overrides
for vSPD. 

# Installation

```
install.packages('renv')
renv::restore()
```

# Generating EAFs

## Generating counterfactual offers

TODO: description of modifying offers

## Running counterfactual scenarios

TODO: description of using overrides with  vSPD

## Generating EAFs

TODO: description of comparing to final prices