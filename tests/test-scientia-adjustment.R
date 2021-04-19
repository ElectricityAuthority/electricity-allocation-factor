test_that("Scientia scenario 2 offer adjustment is reproduced with same input data", {
    df_offers_adjusted = ets_price_adjustments(df_offers_gdx, df_ets, df_ets_factors, df_eif)

    df_offers_adjusted = df_offers_adjusted %>%
        select(TradingPeriod, PoCUnit, Band, TradingDate, Megawatt, DollarsPerMegawattHour = AdjustedOfferPrice, Unit, PointOfConnection)

    expect_equal(df_offers_adjusted, df_offer_results_scientia)
})
