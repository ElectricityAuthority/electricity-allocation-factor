#' Calculate load-weighted average prices over given time dimension.
#'
#' @param df_in data frame containing columns as below
#' @param time_col Column to average prices over
#' @param load_col Load for each period
#' @param price_col Price for each period
#'
#' @return
calculate_load_weighted_price = function(df_in, time_col, load_col, price_col) {
    time_col = enquo(time_col)
    load_col = enquo(load_col)
    price_col = enquo(price_col)

    df_in %>%
        group_by(!!time_col) %>%
        summarise(load_weighted_price = weighted.mean(!!price_col, !!load_col))

}

#' Calculates an EAF from given factual and counterfactual results
#'
#' @param from_trade_date Start date of factual prices for comparison
#' @param to_trade_date End date of factual prices for comparison
#' @param cf_prices_file Location of counterfactual nodal prices
#' @param out_path Path to save output to
#' @param ets_price_file Location of ETS prices
#' @param ets_factors_file Location of ETS price factors for the phase-in of electricity generation
#' @param scenario_type The adjustment type used
#' @param run_name Name for the EAF simulation
#' @param dl_fp Whether to download factual prices
#' @param dl_assumptions Whether to download input assumption files
#'
#' @return
calculate_annual_eaf = function(from_trade_date, to_trade_date, cf_prices_file, out_path = glue("data/output/eaf/{from_trade_date}-{to_trade_date}-scenario-{scenario_type}-{run_name}-eaf.csv"), 
    ets_price_file = "data/input/ETS_unit_prices/nz_ets_prices.csv", ets_factors_file = "data/input/ETS_unit_prices/ets_factors.csv", scenario_type = c("2", 
        "3"), run_name = "", dl_fp = TRUE, dl_assumptions = TRUE) {
    scenario_type = match.arg(scenario_type)

    if (dl_assumptions) {
        download_assumption_files()
    }

    if (dl_fp) {
        download_emi_load_prices(from_trade_date, to_trade_date)
    }

    df_load_prices = read_load_prices(from_trade_date, to_trade_date) %>%
        mutate(year = year(Date))

    df_cf_prices = read_csv(cf_prices_file) %>%
        mutate(year = as.numeric(str_extract(DateTime, "(?<=[^0-9])([0-9]+)")))

    df_annual_load_prices = calculate_load_weighted_price(df_load_prices, year, `Load (MW)`, `Price ($/MWh)`)
    df_cf_load_prices = calculate_load_weighted_price(df_cf_prices, year, `Load (MW)`, `Price ($/MWh)`)

    df_ets = read_csv(ets_price_file)
    df_ets_factors = read_csv(ets_factors_file)

    df_eaf = calculate_eaf_values(df_annual_load_prices, df_cf_load_prices, df_ets, df_ets_factors)
    df_eaf = df_eaf %>%
        mutate(scenario_type = scenario_type)
    write_csv(df_eaf, out_path)

}

#' Join factual, counterfactual and ETS prices to calculate EAF values
#'
#' @param df_factual_load_prices Factual load weighted prices
#' @param df_cf_load_prices Counterfactual load weighted prices
#' @param df_ets ETS prices
#' @param df_ets_factors ETS price factors for the phase-in of electricity generation
#'
#' @return A data frame containing the EAF value for the year of interest
calculate_eaf_values = function(df_factual_load_prices, df_cf_load_prices, df_ets, df_ets_factors) {
    df_eaf = df_factual_load_prices %>%
        rename(factual_load_weighted_price = load_weighted_price) %>%
        inner_join(df_cf_load_prices %>%
            rename(cf_load_weighted_price = load_weighted_price), by = "year") %>%
        # Join ETF factors where start date <= trading date <= end date
    inner_join(df_ets %>%
        fuzzy_inner_join(df_ets_factors, by = c(date = "start_date", date = "end_date"), match_fun = list(function(x, y) {
            x >= y
        }, function(x, y) {
            x <= y
        })) %>%
        mutate(nz_ets_spot_price_adjusted_nzd = nz_ets_spot_price_nzd * ets_factor, year = year(date)) %>%
        group_by(year) %>%
        summarise(mean_nz_ets_spot_price_adjusted_nzd = mean(nz_ets_spot_price_adjusted_nzd))) %>%
        mutate(eaf = (factual_load_weighted_price - cf_load_weighted_price)/mean_nz_ets_spot_price_adjusted_nzd)

    return(df_eaf)

}
