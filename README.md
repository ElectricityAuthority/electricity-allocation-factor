# Introduction

This is a new implementation of the methodology described in the Scientia Consulting
report [Electricity allocation factor estimates for 2016/17](https://environment.govt.nz/publications/electricity-allocation-factor-estimates-for-201617/).
This modifies the offers of certain generating units depending on inferred ETS costs
for those generating units. This generates a counterfactual experiment configuration that attempts to model
what the cost of electricity would have been in the absence of the ETS. This experiment can be run with the Authority's vSPD model to 
generate counterfactual prices. This repo also contains code to calculate EAF values from these counterfactual results and the factual
electricity prices and other input data sources available from EMI.

# Implementation

The original code used in the Scientia report was written entirely in GAMS as a fork of vSPD. 
This implementation instead uses R code to generate offer overrides for vSPD. 
These offer overrides can then be used with SPD case files available via EMI to run the counterfactual.

# Installation

This uses R 3.6.3. Install these versions of [R](https://cran.r-project.org/bin/windows/base/old/3.6.3/) and [Rtools](https://cran.r-project.org/bin/windows/Rtools/history.html). 
Download [version 1.0.5 of GDXRRW](https://support.gams.com/doku.php?id=gdxrrw:interfacing_gams_and_r). 
Then run the following to install dependencies.

```
install.packages('renv')
renv::activate()
install.packages('{path to gdxrrw_1.0.5.zip}', repos = NULL)
renv::restore()
```

# Setup

Configure the environment variables

* `EMI_SAS` - SAS for EMI datasets. See the [EMI forum](https://forum.emi.ea.govt.nz/thread/accessing-emi-datasets-with-azure-storage-explorer/) for the latest SAS to use.
* `GAMS_PATH` - Path to your GAMS installation. Only required for running tests.

For more information on running the EAF process, see the [vignette provided](https://github.com/ElectricityAuthority/electricity-allocation-factor/blob/master/vignettes/EAF%20calculation%20process.Rmd).

# Data

There are four input datasets used in this calculation. Wholesale market data, such as offers, is available from the wholesale datasets section on EMI.
The data specific to this project is available in a [project section](https://www.emi.ea.govt.nz/Wholesale/Datasets/_AdditionalInformation/SupportingInformationAndAnalysis/2021/20210419_EAF_Data) on EMI.

## ETS data

This includes the history of NZ ETS prices. 

ETS factors are included as another file. This represents the changes to surrender obligations over time.
See this [EPA factsheet](https://www.epa.govt.nz/assets/Uploads/Documents/Emissions-Trading-Scheme/Guidance/ETS-Surrender-Obligations-One-for-two-phase-out-factsheet.pdf) for an explanation.

## EIF factors

These include our best estimates of emission intensity factors.

## Offers

Generation offers in the wholesale market include offers to generate different quantities of electricity at different price points for a given future trading period.
For more information on this data see the [EMI dataset](https://www.emi.ea.govt.nz/Wholesale/Datasets/BidsAndOffers) and the [SPD functional spec](https://www.transpower.co.nz/system-operator/key-documents/software-specifications) relating to generation offers.

## Prices

Prices for the factual case come from the solutions of final pricing cases used by the clearing manager.
These are used to generate the load-weighted average prices which are compared to the counterfactual load-weighted average prices in the EAF calculation.

# Generating EAFs

## Generating counterfactual offers

The main file contains the function `generate_offer_adjustments`, which takes assumption and offer files as inputs and produces a vSPD override file.

This has two `scenario_type` options (2, 3) corresponding to the second and third simulations in the original Scientia report. 
Running the type-3 results additionally requires a counterfactual price file from the output of the type-2 simulation.

## Running counterfactual scenarios

Simulations are run via vSPD, which requires a GAMS license. 

After generating offer adjustments, additional functions in the vSPD file can be used to download the relevant final pricing case files and generate the file list for the simulation.
vSPD can then be configured with the override and file list. Once the experiment is configured and inputs are placed in the correct directories, vSPD can then be run to generate the counterfactual results.

## Generating EAFs

This repo also provides code to generate EAFs from the simulation results. This downloads nodal pricing outcomes from EMI datasets and compares these to the vSPD counterfactual outputs.
This produces a output file, which contains the load-averaged factual and counterfactual prices, the average adjusted NZU price for the period, and the final EAF value.

# Methodology changes

There are a number of changes that have been made where the original Scientia report was not sufficiently detailed for us to reproduce the same results.
While former Scientia staff were able to provide us with the original code used in the scenario 2 simulations, they were unable to locate some of the code used in the scenario 3 simulation.
Accordingly, our low-priced thermal adjustment (LPTA) will differ from that used in the original report. We have attempted to replicate this as closely as possible given the detail provided in the original report.

## Low-priced thermal adjustment

The original LPTA was made using generator behaviour during the entire period (2016-17), as with the EAF calculation we have produced, we instead calculate by calendar year.

The function `low_priced_thermal_adjustment` produces this adjustment. 

For each unit to be adjusted, we take the observed prices at the point of connection where the unit injects and join them to the type-2 counterfactual prices. We then take the factual and counterfactual prices and assign them to $2 bands, e.g. \$0 - \$2. We then take the factual prices and calculate load-weighted average daily spot prices at each relevant point of connection. Using these daily prices and factual offers, we calculate the curves of the sensitivity of low-priced thermal offers to spot price, as in figure 3 of the original Scientia report. These sensitivity curves are then smoothed via moving average.

Using the observed differences between factual and counterfactual prices and the sensitivity curves, we calculate a daily adjustment factor for the proportion of low-priced offers which are reassigned to other tranches. We only allow LPTA factors in the range [0, 1]. So, the proportion of low-priced thermal generation offered cannot increase, even if historical observations of some price band $p_1$ tended to have more low-priced generation offered than some price band $p_2$ for $p_1 < p_2$. For example, in figure 3 of the original Scientia report TCC is a pronounced example where the proportion offered at low-prices starts to decreases as the average spot prices increases above $100.

