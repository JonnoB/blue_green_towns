#' Load and Prepare Geographic Data from ONSPD and Lookup Tables
#'
#' This function loads geographic data from an ONSPD CSV file and enriches it with information 
#' from various lookup tables. It processes the data by selecting relevant columns, joining with 
#' lookup tables, and filtering and modifying the data as necessary.
#'
#' @param ONSPD_path The file path to the ONSPD (Office for National Statistics Postcode Directory) CSV file.
#' @param documents_path The path to the directory containing lookup table files.
#'
#' @return A data frame containing processed geographic data. The data frame includes columns 
#' for lower layer super output area codes (LSOA), middle layer super output area codes (MSOA), 
#' administrative district codes and names, region codes and names, and country codes. 
#' The function also handles regional naming for Scotland and Wales and removes any NA values 
#' in the region names and local authority district names (LADNM).
#'
#' @details
#' The function uses `return_lookup_data` to load lookup tables for ward names, regions, and 
#' local authority districts. These tables are then joined with the ONSPD data to enrich it with 
#' additional geographic information. The function includes optional lines to filter out specific 
#' areas (e.g., Scotland) or include ward names, which can be activated as needed.
#'
#' The function performs several transformations on the ONSPD data:
#' - Selects and renames key columns for easier interpretation.
#' - Removes duplicate rows to simplify the dataset.
#' - Joins with lookup tables to add region and local authority district information.
#' - Mutates the `RGNCD` and `RGNNM` columns to ensure proper naming for Wales and Scotland.
#' - Filters out rows with missing region names and local authority district names.
#'
#' @examples
#' # Example usage
#' geo_data <- load_and_prepare_geo_data("/path/to/ONSPD.csv", "/path/to/documents")
#'
#' @importFrom readr read_csv
#' @importFrom dplyr select left_join mutate filter distinct case_when
#' @export

load_and_prepare_geo_data <- function(ONSPD_path, documents_path) {
  
  # Load lookup data using the return_lookup_data function
  wardnames_lookup <- return_lookup_data(documents_path, "^ward names.*(csv)")
  
  region_lookup <- return_lookup_data(documents_path, "^region.*(csv)") %>%
    select(1, 3)
  
  LADNM_lookup <- return_lookup_data(documents_path, "^la_ua.*(csv)") %>%
    select(1:2)
  
  # Read and process ONSPD data
  ONSPD_df <- read_csv(ONSPD_path) %>%
    select(lsoa11cd = lsoa11,
           MSOACD = msoa11, oslaua,
           OA11CD = oa11, Country_code = ctry,
           RGNCD = rgn,
           WDCD = osward) %>%
    distinct() %>%
    left_join(., region_lookup, by = "RGNCD") %>%
    # Uncomment the following line if ward names are needed
    # left_join(wardnames_lookup, by = "WDCD") %>%
    left_join(LADNM_lookup, by = c("oslaua" = "LADCD")) %>%
    select(Country_code, Admin_district_code = oslaua,
           lsoa11cd, MSOACD, LADNM, 
           RGNCD, RGNNM
           # Uncomment the following line if ward names are needed
           # , WD16NM = WDNM
    ) %>%
    mutate(
      # Make sure Scotland and Wales have region names
      RGNCD = case_when(
        grepl("W", Country_code) ~ "Wales",
        grepl("S", Country_code) ~ "Scotland",
        TRUE ~ RGNCD),
      RGNNM = case_when(
        grepl("W", Country_code) ~ "Wales",
        grepl("S", Country_code) ~ "Scotland",
        TRUE ~ RGNNM
      )) %>%
    filter(!is.na(RGNNM),
           LADNM != "NA"
           # Uncomment the following line if filtering out Scotland is needed
           # RGNCD != "Scotland"
    ) %>%
    distinct()
  
  # Return the processed data frame
  return(ONSPD_df)
}