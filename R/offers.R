#' Update offers based on ETS costs
#'
#' @param df_offers Offer data
#' @param df_ets ETS spot price series
#' @param df_ets_factors Discount factors for electricity generation in the ETS
#' @param df_eif Emission intensity factors for generating plant
#' @param offer_min_price The minimum price above which offers are subject to the adjustment
#' @param offer_max_price The maximum price above which the adjustment is not applied
#'
#' @return data.frame of adjusted offers 
ets_price_adjustments = function(df_offers, df_ets, df_ets_factors, df_eif, offer_min_price = 1, offer_max_price = 9999) {
    df_offers_adjusted = df_offers %>%
        left_join(df_eif, by = c("PoCUnit")) %>%
        left_join(df_ets, by = c(TradingDate = "date")) %>%
        # Join ETF factors where start date <= trading date <= end date
    fuzzy_inner_join(df_ets_factors, by = c(TradingDate = "start_date", TradingDate = "end_date"), match_fun = list(function(x, y) {
        x >= y
    }, function(x, y) {
        x <= y
    })) %>%
        mutate(nz_ets_spot_price_nzd_adjusted = nz_ets_spot_price_nzd * ets_factor) %>%
        mutate(OfferAdjustment = (EIF/1000) * nz_ets_spot_price_nzd_adjusted) %>%
        mutate(AdjustedOfferPrice = case_when(!is.na(OfferAdjustment) & DollarsPerMegawattHour > offer_min_price & DollarsPerMegawattHour < offer_max_price & 
            DollarsPerMegawattHour - OfferAdjustment > 0.01 ~ DollarsPerMegawattHour - OfferAdjustment, !is.na(OfferAdjustment) & DollarsPerMegawattHour > 
            offer_min_price & DollarsPerMegawattHour < offer_max_price ~ 0.01, TRUE ~ DollarsPerMegawattHour))

    df_offers_adjusted = df_offers_adjusted %>%
        select(PoCUnit, PointOfConnection, Unit, TradingDate, TradingPeriod, Band, DollarsPerMegawattHour, Megawatt, AdjustedOfferPrice, OfferAdjustment) %>%
        mutate(Adjusted = DollarsPerMegawattHour != AdjustedOfferPrice) %>%
        tp_to_time(TradingDate, TradingPeriod)

    return(df_offers_adjusted)
}

#' Transform date / trading period to GDX coding of datetime
#' Extra periods in daylight savings transition are represented by a ';'
#' i.e. 2;00 instead of 2:00
#'
#' @param df_in input data with trading date and period columns
#' @param date_col trading date column
#' @param tp_col trading period column
#'
#' @return data.frame with additional DatetimeSpd column, coded in GDX format.
tp_to_time = function(df_in, date_col, tp_col) {
    date_col = enquo(date_col)
    tp_col = enquo(tp_col)
    trading_period_seconds = 30 * 60

    df_in %>%
        group_by(!!date_col) %>%
        mutate(n_periods = n_distinct(!!tp_col)) %>%
        ungroup %>%
        mutate(time_spd = case_when(n_periods == 46 & !!tp_col >= 5 ~ seconds_to_period((TradingPeriod + 1) * trading_period_seconds), n_periods == 50 & !!tp_col >= 
            7 ~ seconds_to_period((TradingPeriod - 3) * trading_period_seconds), TRUE ~ seconds_to_period((TradingPeriod - 1) * trading_period_seconds))) %>%
        mutate(time_spd = case_when(n_periods == 50 & !!tp_col %in% 7:8 ~ glue("{str_pad(hour(time_spd), width = 2, pad = '0')};{str_pad(minute(time_spd), width = 2, pad = '0')}"), 
            TRUE ~ glue("{str_pad(hour(time_spd), width = 2, pad = '0')}:{str_pad(minute(time_spd), width = 2, pad = '0')}"))) %>%
        mutate(DatetimeSpd = paste(toupper(format(!!date_col, "%d-%b-%Y")), time_spd)) %>%
        select(-time_spd, -n_periods)
}

#' Create override file for use with vSPD
#'
#' @param df_adjusted_offers data.frame of adjusted offers to be placed into override
#' @param gdx_override_template template of parameters to override
#' @param override_parameters list of overrides from override template
#'
#' @return
create_offer_override = function(df_adjusted_offers, output_overrides_path, scenario_type, gdx_override_template = "data/input/overrides/parameters.gdx", override_parameters = c("energyOfferOverrides", 
    "demandOverrides", "dispatchableEnergyBidOverrides", "energyBidOverrides", "offerParameterOverrides", "fastILROfferOverrides", "fastPLSROfferOverrides", 
    "fastTWDROfferOverrides", "sustainedILROfferOverrides", "sustainedPLSROfferOverrides", "sustainedTWDROfferOverrides")) {
    # Load list of overrides from override template
    overrides_list = map(override_parameters, function(x) {
        rgdx(gdx_override_template, list(name = x))
    })

    # Load adjusted offers and transform to GDX format
    energy_offers_overrides = df_adjusted_offers %>%
        filter(Adjusted, Megawatt != 0) %>%
        mutate(IndexVariable = "i_GenerationMWOfferPrice") %>%
        mutate(Band = paste0("t", Band)) %>%
        mutate(DatetimeSpd = toupper(DatetimeSpd)) %>%
        select(DatetimeSpd, PoCUnit, Band, IndexVariable, value = AdjustedOfferPrice) %>%
        as.data.frame()

    if (scenario_type == 3) {
        # Add thermal adjusted offers
        energy_offers_overrides = bind_rows(energy_offers_overrides, df_adjusted_offers %>%
            filter(ThermalAdjusted) %>%
            mutate(IndexVariable = "i_GenerationMWOffer") %>%
            mutate(Band = paste0("t", Band)) %>%
            mutate(DatetimeSpd = toupper(DatetimeSpd)) %>%
            select(DatetimeSpd, PoCUnit, Band, IndexVariable, value = AdjustedMegawatt)) %>%
            as.data.frame()

    }

    # Get energy offers overrides in GDX form
    names(energy_offers_overrides)[1:(ncol(energy_offers_overrides) - 1)] = rep("*", ncol(energy_offers_overrides) - 1)
    write.gdx(file = output_overrides_path, params = list(energyOfferOverrides = energy_offers_overrides))
    energy_offers_overrides = rgdx(output_overrides_path, list(name = "energyOfferOverrides"))

    # Combine override template and energy offer overrides into final override file.
    overrides_list[[which(override_parameters == "energyOfferOverrides")]] = energy_offers_overrides
    gdxrrw::wgdx(output_overrides_path, overrides_list)

}

#' See https://stackoverflow.com/a/6468532
#' For creating price buckets
#'
#' @param x 
#' @param to 
#'
#' @return
round_up <- function(x, to = 10) {
    to * (x%/%to + as.logical(x%%to))
}

#' Convert numeric values into price bands
#'
#' @param x numeric value
#' @param to width of band to code
#'
#' @return coded price bands
create_price_band = function(x, to = 2) {
    as.character(glue("${round_up(x - to, to)}-{round_up(x, to)}"))
}

#' Perform the low-priced thermal adjustment
#' 
#' TODO: add ability to specify a different period for the thermal curve than the offers/adjustment period
#'
#' @param offers_from_trade_date Start of the period to generate offer overrides for
#' @param offers_to_trade_date End of the period to generate offer overrides for
#' @param df_adjusted_offers Offers with scenario 2 adjustment applied
#' @param df_offers Factual offers
#' @param df_cf_prices_file Nodal pricing results from scenario 2 simulation
#' @param df_ets Path to the file of ETS unit prices
#' @param df_ets_factors Path to the file of ETS unit price discount factors for electricity generation
#' @param low_price_threshold Threshold below which traches' number of megawatts offered is adjusted
#' @param price_band_interval Bands to bucketise observed prices/offers
#' @param save_thermal_curve Whether to save an output file of the thermal curves calculated
#' @param df_lp_thermal_path file detailing which units to adjust
#' @param ma_length The number of intervals in units of price_band_interval
#' that are smoothed over in creation of the thermal curves
#' @param run_name label applied to output files
#'
#' @return
low_priced_thermal_adjustment = function(offers_from_trade_date, offers_to_trade_date, df_adjusted_offers, df_offers, df_cf_prices_file, df_ets, df_ets_factors, 
    df_lp_thermal_path, low_price_threshold = 1, price_band_interval = 2, ma_length, save_thermal_curve = TRUE, run_name = "") {
    df_cf_prices = read_csv(df_cf_prices_file)
    df_load_prices = read_load_prices(offers_from_trade_date, offers_to_trade_date) %>%
        tp_to_time(Date, TradingPeriod)

    df_daily_price_diff = get_daily_price_diffs(df_load_prices, df_cf_prices, price_band_interval)

    # Find units to which the adjustment is applied For a discussion of the selection criteria see the original Scientia report
    df_lp_thermal = read_csv(df_lp_thermal_path)
    poc_unit_oi = df_lp_thermal$Offer

    df_factual_price_daily_bands = get_factual_nodal_price_averages(df_load_prices, poc_unit_oi, price_band_interval)

    thermal_offer_price_curves = create_thermal_curves(df_offers, df_factual_price_daily_bands, poc_unit_oi, price_band_interval, low_price_threshold, ma_length)

    if (save_thermal_curve) {
        write_csv(thermal_offer_price_curves, glue("data/output/thermal_curves/{offers_from_trade_date}-{offers_to_trade_date}-{run_name}-thermal-curve.csv"))
    }

    daily_thermal_adjustment_factors = get_daily_low_priced_adjustment_factors(df_daily_price_diff, thermal_offer_price_curves, poc_unit_oi)

    df_adjusted_offers = adjust_thermal_offers(df_adjusted_offers, daily_thermal_adjustment_factors, low_price_threshold)

    return(df_adjusted_offers)

}

#' Calculate average daily price differences between factual and counterfactual cases.
#' This is used to find the difference in prices between the scenario 2 and factual cases.
#'
#' @param df_load_prices factual nodal prices
#' @param df_cf_prices counterfactual nodal prices
#' @param price_band_interval Bands to bucketise observed prices
#'
#' @return data.frame containing price differences each day between factual and counterfactual cases
get_daily_price_diffs = function(df_load_prices, df_cf_prices, price_band_interval) {
    df_daily_price_diff = df_cf_prices %>%
        select(DatetimeSpd = DateTime, PointOfConnection = Node, CounterfactualPrice = `Price ($/MWh)`) %>%
        mutate(Date = dmy(str_sub(DatetimeSpd, 1, 11))) %>%
        inner_join(df_load_prices %>%
            select(DatetimeSpd, PointOfConnection, FactualPrice = `Price ($/MWh)`) %>%
            mutate(Date = dmy(str_sub(DatetimeSpd, 1, 11))), by = c("DatetimeSpd", "Date", "PointOfConnection")) %>%
        mutate(PriceDiff = FactualPrice - CounterfactualPrice) %>%
        group_by(Date, PointOfConnection) %>%
        summarise(across(c(PriceDiff, FactualPrice, CounterfactualPrice), mean)) %>%
        ungroup() %>%
        mutate(across(contains("Price"), ~create_price_band(., price_band_interval), .names = "{.col}Band")) %>%
        mutate(across(contains("PriceBand"), ~as.numeric(str_extract(., "[0-9]+")), .names = "{.col}Start"))


    return(df_daily_price_diff)
}

#' Calculate the average daily spot price at nodes of interest
#'
#' @param df_load_prices factual price data
#' @param poc_unit_oi generating units of interest
#' @param price_band_interval Bands to bucketise observed prices
#'
#' @return Mean daily spot price at units of interest
get_factual_nodal_price_averages = function(df_load_prices, poc_unit_oi, price_band_interval) {
    df_factual_price_daily_bands = df_load_prices %>%
        filter(PointOfConnection %in% poc_unit_oi) %>%
        group_by(Date, PointOfConnection) %>%
        summarise(`Price ($/MWh)` = mean(`Price ($/MWh)`, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(DailyPriceBand = create_price_band(`Price ($/MWh)`, price_band_interval)) %>%
        arrange(`Price ($/MWh)`) %>%
        mutate(DailyPriceBand = factor(DailyPriceBand, levels = unique(.$DailyPriceBand)))

    return(df_factual_price_daily_bands)
}


#' Calculate generating units' offer behaviour of low-priced tranches conditional on spot prices
#'
#' @param df_offers Actual offers of thermal generators
#' @param df_factual_price_daily_bands Average actual daily spot prices for generating units
#' @param poc_unit_oi Units to create curves for
#' @param price_band_interval Bands to bucketise observed offer prices
#' @param low_price_threshold Threshold below which offers are subject to adjustment
#' @param ma_length The number of intervals in units of price_band_interval that are smoothed over
#'
#' @return data.frame detailing average megawatts offered below a given low-price threshold 
#' conditional on spot price by specified generating units
create_thermal_curves = function(df_offers, df_factual_price_daily_bands, poc_unit_oi, price_band_interval, low_price_threshold, ma_length) {

    df_thermal_curves = df_offers %>%
        filter(PoCUnit %in% poc_unit_oi) %>%
        filter(IsLatest == "Y", ProductClass == "Injection") %>%
        mutate(LowPrice = DollarsPerMegawattHour <= low_price_threshold) %>%
        inner_join(df_factual_price_daily_bands, by = c(TradingDate = "Date")) %>%
        mutate(price_band_start = as.numeric(str_extract(DailyPriceBand, "[0-9]+"))) %>%
        group_by(PoCUnit, price_band_start, TradingDate, TradingPeriod, LowPrice) %>%
        summarise(Megawatt = sum(Megawatt)) %>%
        group_by(PoCUnit, price_band_start, LowPrice) %>%
        summarise(Megawatt = mean(Megawatt)) %>%
        ungroup %>%
        filter(LowPrice) %>%
        select(-LowPrice)

    price_band_starts = seq(from = 0, to = max(df_thermal_curves$price_band_start), by = price_band_interval)


    df_thermal_curves = crossing(price_band_start = price_band_starts, PoCUnit = unique(poc_unit_oi)) %>%
        left_join(df_thermal_curves) %>%
        arrange(PoCUnit, price_band_start) %>%
        group_by(PoCUnit) %>%
        mutate(Megawatt = na.locf0(Megawatt)) %>%
        mutate(Megawatt = na.locf0(Megawatt, fromLast = TRUE)) %>%
        ungroup

    # Apply moving average to smooth thermal curves
    df_thermal_curves = df_thermal_curves %>%
        group_by(PoCUnit) %>%
        mutate(Megawatt = frollmean(Megawatt, ma_length, align = "left")) %>%
        mutate(Megawatt = na.locf0(Megawatt)) %>%
        ungroup

    return(df_thermal_curves)

}

#' Generate a daily factor for the reduction in low-priced offer tranches
#'
#' @param df_daily_price_diff output from get_daily_price_diffs
#' @param thermal_offer_price_curves historic behaviour of generating units at different price levels, output from create_thermal_curves
#' @param poc_unit_oi Units to generate adjustment factors for
#'
#' @return
get_daily_low_priced_adjustment_factors = function(df_daily_price_diff, thermal_offer_price_curves, poc_unit_oi) {

    df_thermal_adjustment_factors = df_daily_price_diff %>%
        filter(PointOfConnection %in% poc_unit_oi) %>%
        left_join(thermal_offer_price_curves %>%
            rename(FactualMegawatt = Megawatt), by = c(PointOfConnection = "PoCUnit", FactualPriceBandStart = "price_band_start")) %>%
        left_join(thermal_offer_price_curves %>%
            rename(CoutnerfactualMegawatt = Megawatt), by = c(PointOfConnection = "PoCUnit", CounterfactualPriceBandStart = "price_band_start")) %>%
        mutate(ThermalAdjustmentFactor = (FactualMegawatt - CoutnerfactualMegawatt)/FactualMegawatt) %>%
        # Don't allow increases in low-priced offers.
    mutate(ThermalAdjustmentFactor = ifelse(ThermalAdjustmentFactor < 0, 0, ThermalAdjustmentFactor)) %>%
        select(Date, PointOfConnection, ThermalAdjustmentFactor)

    return(df_thermal_adjustment_factors)

}

#' Apply the low-priced thermal offer adjustment to certain units below a threshold
#' 
#' @param df_adjusted_offers input offers to the adjustment
#' @param daily_thermal_adjustment_factors data.frame containing adjustment factor per unit per day. 
#' As output from get_daily_low_priced_adjustment_factors
#' @param low_price_threshold The threshold price below which offers are adjusted.
#'
#' @return data.frame with of offers with adjusted megawatt offers and whether the thermal adjustment was applied.
adjust_thermal_offers = function(df_adjusted_offers, daily_thermal_adjustment_factors, low_price_threshold) {

    df_adjusted_offers = df_adjusted_offers %>%
        left_join(daily_thermal_adjustment_factors %>%
            mutate(ThermalAdjustment = TRUE), by = c(TradingDate = "Date", PoCUnit = "PointOfConnection")) %>%
        mutate(IsLowPriced = (DollarsPerMegawattHour <= low_price_threshold) & (DollarsPerMegawattHour > 0), MustRun = DollarsPerMegawattHour == 0) %>%
        group_by(PoCUnit, TradingDate, TradingPeriod) %>%
        mutate(HasLowPricedComponent = sum(IsLowPriced) > 0) %>%
        mutate(LowPricedReductionBand = ifelse(IsLowPriced, Megawatt * ThermalAdjustmentFactor, 0)) %>%
        mutate(LowPricedReductionTotal = sum(LowPricedReductionBand)) %>%
        group_by(PoCUnit, TradingDate, TradingPeriod, IsLowPriced) %>%
        mutate(OfferMegawattAdjustment = case_when(IsLowPriced & ThermalAdjustment ~ -LowPricedReductionBand, MustRun & ThermalAdjustment ~ 0, !IsLowPriced & 
            !MustRun & ThermalAdjustment ~ (LowPricedReductionTotal * Megawatt/sum(Megawatt)), is.na(ThermalAdjustment) ~ 0)) %>%
        ungroup() %>%
        mutate(AdjustedMegawatt = Megawatt + OfferMegawattAdjustment) %>%
        mutate(ThermalAdjusted = Megawatt != AdjustedMegawatt)

    return(df_adjusted_offers)

}
