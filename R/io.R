#' Takes offer files from EMI Datasets and combines them into one data frame.
#'
#' @param input_dir Location of offer files
#' @param offers_from_trade_date Start date for files to combine
#' @param offers_to_trade_date End date for files to combine
#' @param parallel Whether to parallelise the file read
#' @param n_cores Number of cores to use when parallel is TRUE
#'
#' @return data frame of combined offer files
read_offers_files = function(input_dir = "data/input/Offers", offers_from_trade_date = "2016-01-01", offers_to_trade_date = "2016-01-01", parallel = T, 
                             n_cores = detectCores() - 1) {
    dates_to_read = seq.Date(as.Date(offers_from_trade_date), as.Date(offers_to_trade_date), by = "1 days")
    dates_to_read = str_replace_all(dates_to_read, "-", "")

    if (parallel) {
        cl = makeCluster(n_cores)
        clusterEvalQ(cl, {
            source("main.R")
        })
        clusterExport(cl, "input_dir", envir = environment())
        offers = parLapply(cl, dates_to_read, fun = function(x) {
            read_offer_file(file.path(input_dir, glue("{x}_Offers.csv")))
        })
        stopCluster(cl)
    } else {
        offers = map(dates_to_read, function(x) {
            read_offer_file(file.path(input_dir, glue("{x}_Offers.csv")))
        })
    }

    offers = bind_rows(offers)

    return(offers)
}

#' Read energy offers from an offers file from EMI Datasets.
#'
#' @param offer_file_path Path to offer file to Load
#'
#' @return Energy offers
read_offer_file = function(offer_file_path) {
    read_csv(offer_file_path, col_types = cols_only(PointOfConnection = col_character(), Unit = col_character(), Trader = col_character(), ProductType = col_character(), 
        ProductClass = col_character(), IsLatest = col_character(), TradingDate = col_date(), TradingPeriod = col_integer(), Band = col_integer(), Megawatt = col_double(), 
        DollarsPerMegawattHour = col_double())) %>%
        filter(ProductType == "Energy", ProductClass == "Injection", IsLatest == "Y") %>%
        mutate(PoCUnit = paste(PointOfConnection, Unit))
}

#' Takes nodal pricing files from EMI Datasets and combines them into one data frame.
#'
#' @param from_trade_date Start date for files to combine
#' @param to_trade_date End date for files to combine
#' @param input_dir Location of nodal price files
#'
#' @return data frame of combined nodal price files
read_load_prices = function(from_trade_date = "2016-01-01", to_trade_date = "2016-01-01", input_dir = "data/input/Prices/FinalPrices") {
    dates_to_read = seq.Date(as.Date(from_trade_date), as.Date(to_trade_date), by = "1 days")

    load_prices = map(dates_to_read, function(x) {
        file_date_str = str_replace_all(x, "-", "")
        file_year = year(x)
        input_files = list.files(file.path(input_dir, file_year), full.names = T)
        input_file = input_files[which(str_detect(input_files, file_date_str))]

        read_load_price_file(input_file)
    })

    load_prices = bind_rows(load_prices)

    return(load_prices)
}

#' Read nodal prices from a file from EMI Datasets
#'
#' @param load_price_file Path to nodal price file to load
#'
#' @return 
read_load_price_file = function(load_price_file) {
    read_csv(load_price_file, col_types = cols(Date = col_date(), TradingPeriod = col_integer(), Island = col_character(), PointOfConnection = col_character(), 
        `Load (MW)` = col_double(), `Generation (MW)` = col_double(), `Price ($/MWh)` = col_double()))
}

#' Download files from an EMI Dataset for a given date range
#'
#' @param from_trade_date Start date of files to download
#' @param to_trade_date End data of files to download
#' @param emi_sas Shared access signature for the EMI Datasets container. 
#' See https://forum.emi.ea.govt.nz/thread/accessing-emi-datasets-with-azure-storage-explorer/ for more detail.
#' @param emi_endpoint EMI Datasets endpoint 
#' @param download_dir Directory to download files to
#' @param download_function function to be used for specific dataset format.
download_emi_files = function(from_trade_date, to_trade_date, emi_sas = Sys.getenv("EMI_SAS"), emi_endpoint = "https://emidatasets.blob.core.windows.net/publicdata", 
    download_dir = "data/input/Offers", download_function) {
    dates_to_read = seq.Date(as.Date(from_trade_date), as.Date(to_trade_date), by = "1 days")
    dates_to_read = str_replace_all(dates_to_read, "-", "")

    emi_datasets_container = blob_container(endpoint = emi_endpoint, sas = emi_sas)

    walk(dates_to_read, function(x) {
        download_function(x, emi_datasets_container, download_dir)
    })
}

#' Download function for offers files
#'
#' @param date_str Date of the offers file
#' @param emi_datasets_container the blob_container
#' @param download_dir Directory for the download file
download_emi_offer_file = function(date_str, emi_datasets_container, download_dir) {
    year = str_sub(date_str, 1, 4)

    download_blob(emi_datasets_container, src = glue("Datasets/Wholesale/BidsAndOffers/Offers/{year}/{date_str}_Offers.csv"), dest = file.path(download_dir, 
        glue("{date_str}_Offers.csv")), overwrite = TRUE)
}

#' Download function for nodal price files
#'
#' @param date_str Date of the nodal price file
#' @param emi_datasets_container the blob_container
#' @param download_dir Directory for the download file
download_emi_load_price_file = function(date_str, emi_datasets_container, download_dir) {
    year = str_sub(date_str, 1, 4)
    lgp_files = list_blobs(emi_datasets_container, glue("Datasets/Wholesale/Final_pricing/Load_Generation_Price/{year}"))
    lgp_file = lgp_files %>%
        filter(str_detect(name, date_str)) %>%
        pull(name)

    download_blob(emi_datasets_container, src = glue("{lgp_file}"), dest = file.path(download_dir, glue("{date_str}_Load_Generation_Price.csv")), overwrite = TRUE)
}

#' Download function for final pricing cases
#'
#' @param date_str Date of the case file
#' @param emi_datasets_container the blob_container
#' @param download_dir Directory for the download file
download_emi_final_pricing_case_file = function(date_str, emi_datasets_container, download_dir) {
    year = str_sub(date_str, 1, 4)
    gdx_files = list_blobs(emi_datasets_container, glue("Datasets/Wholesale/FinalPricing/GDX/{year}"))
    gdx_file = gdx_files %>%
        filter(str_detect(name, date_str)) %>%
        pull(name)

    download_blob(emi_datasets_container, src = glue("{gdx_file}"), dest = file.path(download_dir, glue("{basename(gdx_file)}")), overwrite = TRUE)
}

download_emi_offers = partial(download_emi_files, download_function = download_emi_offer_file)

download_emi_load_prices = partial(download_emi_files, download_function = download_emi_load_price_file)

download_emi_final_pricing_case_files = partial(download_emi_files, download_function = download_emi_final_pricing_case_file)

#' Downloads assumption files for use in EAF simulations
#' These include:
#' Emission intensity factors for different generation units
#' NZ Emissions Trading Scheme Unit prices
#' Discounts applied to ETS pricing for electricity generation
#' Thermal units to apply the Scenario 3 low-priced offer adjustment to
#'
#' @param emi_sas Shared access signature for the EMI Datasets container. 
#' @param endpoint blob service endpoint for files.
#' @param download_dir directory to save files to
download_assumption_files = function(emi_sas = Sys.getenv("EMI_SAS"), endpoint = "https://emidatasets.blob.core.windows.net/publicdata", download_dir = "data/input") {

    container = blob_container(endpoint = endpoint, sas = emi_sas)

    dataset_location = "Datasets/Wholesale/_AdditionalInformation/SupportingInformationAndAnalysis/2021/20210419_EAF_Data/"
    files_to_dl = c("EIF/EIF.csv", "ETS_unit_prices/nz_ets_prices.csv", "ETS_unit_prices/ets_factors.csv", "low_priced_thermal_adjustment_units/low_priced_thermal_adjustment_units.csv")

    walk(files_to_dl, function(x) {
        download_blob(container, src = paste0(dataset_location, x), dest = file.path(download_dir, x), overwrite = TRUE)
    })
}

#' Setup data directory structure
setup_data_directories = function() {
    data_dirs = c("input/ETS_unit_prices", "input/GDX", "input/GDX/Counterfactual", "input/low_priced_thermal_adjustment_units", "input/Offers", "input/overrides", 
        "input/Prices", "input/Prices/Counterfactual", "input/Prices/FinalPrices", "output/eaf", "output/GDX", "output/inc", "output/offers", "output/thermal_curves")

    walk(data_dirs, function(x) {
        dir.create(here::here("data", x), recursive = TRUE)
    })

}
