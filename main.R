source("libraries.R")
walk(list.files(here::here("R"), full.names = TRUE), source)

#' Create adjusted offers for use in counterfactual simulation.
#'
#' @param offers_from_trade_date Start of the period to generate offer overrides for
#' @param offers_to_trade_date End of the period to generate offer overrides for
#' @param ets_price_file Path to the file of ETS unit prices
#' @param ets_factors_file Path to the file of ETS unit price discount factors for electricity generation
#' @param eif_file Path to the file of emissions intensity factors for generation plants
#' @param output_path Path to the folder for outputs
#' @param df_cf_prices_file Path to the nodal price output from the scenario 2 counterfactual for the scenario 3 adjustment.
#' @param dl_offers Indicator of whether to download offers files from EMI.
#' @param scenario_type These are numbered for the scenarios in the Scientia report.
#' @param low_price_threshold Threshold in $/MWh below which offers are not adjusted for price or quantity in the scenario 2 adjustment, 
#' and which are subject to the low-priced thermal adjustment for scenario 3
#' @param price_band_interval Size of the buckets used in $ for the spot prices 
#' into which offers are grouped for generating the low-priced thermal offer curves in the scenario 3 adjustment.
#' For example, with the default value of 2 we measure the average low-priced MW offered for each unit in 
#' $2 increments of the spot price at that unit's node
#' @param ma_length The number of intervals in units of price_band_interval that should be smoothed over
#'  used in scenario 3 thermal curve
#' @param save_thermal_curve Save the thermal curve output
#' @param dl_assumptions Retrieves the latest version of assumptions used from blob storage
#' @param parallel Whether to parallelise reading offers files
#' @param df_lp_thermal_path Path to file describing which units to apply the low-priced thermal offer adjustment to
#'
#' @return Outputs adjusted offers and corresponding vSPD override to file
generate_offer_adjustments = function(offers_from_trade_date = "2016-01-01", offers_to_trade_date = "2016-01-01", ets_price_file = "data/input/ETS_unit_prices/nz_ets_prices.csv", 
    ets_factors_file = "data/input/ETS_unit_prices/ets_factors.csv", eif_file = "data/input/EIF/EIF.csv", output_path = "data/output", df_cf_prices_file = "data/input/prices/Counterfactual/EAF_2016_counterfactual_NodeResults_TP.csv", 
    df_lp_thermal_path = "data/input/low_priced_thermal_adjustment_units/low_priced_thermal_adjustment_units.csv", output_gdx_path = file.path(output_path, "GDX", glue("{offers_from_trade_date}-{offers_to_trade_date}-scenario{scenario_type}-{run_name}-ets-offer-overides.gdx")), 
    output_offers_path = file.path(output_path, "offers", glue("{offers_from_trade_date}-{offers_to_trade_date}-scenario{scenario_type}-{run_name}-adjusted-offers.csv")), 
    run_name = "", dl_offers = TRUE, dl_assumptions = TRUE, scenario_type = c("2", "3"), low_price_threshold = 1, price_band_interval = 2, ma_length = 10, save_thermal_curve = TRUE, 
    parallel = TRUE) {
    scenario_type = match.arg(scenario_type)
    setup_data_directories()

    if (dl_assumptions) {
        download_assumption_files()
    }

    df_ets = read_csv(ets_price_file)
    df_eif = read_csv(eif_file) %>%
        select(PoCUnit = Offer, EIF = `emission_intensity_kgCO2/MWh`)
    df_ets_factors = read_csv(ets_factors_file)

    if (dl_offers) {
        download_emi_offers(offers_from_trade_date, offers_to_trade_date)
    }

    df_offers = read_offers_files(offers_from_trade_date = offers_from_trade_date, offers_to_trade_date = offers_to_trade_date, parallel = parallel)
    df_adjusted_offers = ets_price_adjustments(df_offers, df_ets, df_ets_factors, df_eif, offer_min_price = low_price_threshold)

    if (scenario_type == 3) {
        # Adjust offers to reduce the quantity of low priced thermal offers conditional on the spot price reductions observed in scenario 2
        df_adjusted_offers = low_priced_thermal_adjustment(offers_from_trade_date, offers_to_trade_date, df_adjusted_offers, df_offers, df_cf_prices_file, df_ets, df_ets_factors, 
            df_lp_thermal_path, low_price_threshold, price_band_interval, ma_length, save_thermal_curve, run_name)
    }

    write_csv(df_adjusted_offers, file = output_offers_path)

    # create overrides GDX
    create_offer_override(df_adjusted_offers, output_gdx_path, scenario_type)
}
