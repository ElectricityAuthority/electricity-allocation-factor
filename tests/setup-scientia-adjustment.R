#Download fixtures provided by Scientia staff
fixture_path = here::here("tests/fixtures")
dir.create(fixture_path)

emi_sas = Sys.getenv("EMI_SAS") 
emi_endpoint = "https://emidatasets.blob.core.windows.net/publicdata"
emi_datasets_container = blob_container(endpoint = emi_endpoint, sas = emi_sas)
test_fixtures = list_blobs(emi_datasets_container, 'Datasets/Wholesale/_AdditionalInformation/SupportingInformationAndAnalysis/2021/20210419_EAF_Data/test_fixtures/')

walk(test_fixtures$name,
     function(x){
         download_blob(emi_datasets_container,
                       src = x, 
                       dest = file.path(fixture_path, basename(x)),
                       overwrite = TRUE)
     })

#Transform these into comparable format to our outputs
gdxrrw::igdx(Sys.getenv("GAMS_PATH"))
df_offers_gdx = gdxrrw::rgdx.param(file.path(fixture_path, "FP_20160601_F.gdx"), "i_TradePeriodEnergyOffer") %>%
    as_tibble() %>%
    mutate(trading_date = as.Date("2016-06-01"))

df_offers_gdx = df_offers_gdx %>%
    pivot_wider(names_from = i4, values_from = value) %>%
    rename(TradingPeriod = i1, PoCUnit = i2, Band = i3, TradingDate = trading_date, Megawatt = i_GenerationMWOffer, DollarsPerMegawattHour = i_GenerationMWOfferPrice) %>%
    mutate(poc_split = str_extract_all(PoCUnit, "[A-z0-9]+")) %>%
    mutate(across(c(TradingPeriod, Band), ~str_extract(., "[0-9]+") %>%
        as.numeric)) %>%
    mutate(Unit = map(poc_split, ~.[[2]]), PointOfConnection = map(poc_split, ~.[[1]])) %>%
    unnest(PointOfConnection, Unit) %>%
    select(-poc_split) %>%
    mutate(across(where(is.factor), as.character))

df_ets = read_excel(file.path(fixture_path, "co2_spot_daily.xlsx")) %>%
    select(date = Date, nz_ets_spot_price_nzd = `price_$_per_tCO2e`) %>%
    mutate(across(where(is.factor), as.character))

df_ets_factors = read_csv(file.path(fixture_path, "ets_factors.csv")) %>%
    mutate(across(where(is.factor), as.character))

df_eif = read_excel(file.path(fixture_path, "emn_ofradj_thml_hydro.xlsx")) %>%
    select(PoCUnit = Offer, EIF = `emission_intensity_kgCO2/MWh`)


df_offer_results_scientia = gdxrrw::rgdx.param(file.path(fixture_path, "FP_20160601_F_AllData.gdx"), "i_TradePeriodEnergyOffer") %>%
    as_tibble() %>%
    mutate(trading_date = as.Date("2016-06-01"))

df_offer_results_scientia = df_offer_results_scientia %>%
    pivot_wider(names_from = "NRGofrCmpnt", values_from = i_tradePeriodEnergyOffer) %>%
    rename(TradingPeriod = tp, PoCUnit = o, Band = trdBlk, TradingDate = trading_date, Megawatt = i_generationMWoffer, DollarsPerMegawattHour = i_generationMWofferPrice) %>%
    mutate(poc_split = str_extract_all(PoCUnit, "[A-z0-9]+")) %>%
    mutate(across(c(TradingPeriod, Band), ~str_extract(., "[0-9]+") %>%
        as.numeric)) %>%
    mutate(Unit = map(poc_split, ~.[[2]]), PointOfConnection = map(poc_split, ~.[[1]])) %>%
    unnest(PointOfConnection, Unit) %>%
    select(-poc_split) %>%
    mutate(across(where(is.factor), as.character))
