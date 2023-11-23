#' Process Low Use Data and Compute Ratios
#'
#' This function processes data to identify low use properties by LSOA (Lower Layer Super Output Area),
#' calculates the ratio of low use properties, and associates them with geographic data.
#'
#' @param all_codes A data frame containing codes that need to be matched with low use properties.
#' @param low_use_dict A dictionary (data frame) mapping codes to their low use status and type.
#' @param properties_in_each_lsoa A data frame with the count of all properties in each LSOA.
#'
#' @return A data frame with LSOA codes, the number and type of low use properties, the total number of properties, 
#'         and the ratio of low use properties to total properties. This data frame also includes additional 
#'         geographic data joined from the ONSPD dataset.
#'
#' @details
#' The function joins `all_codes` with `low_use_dict` to filter out low use properties. It then groups by LSOA code and 
#' summarizes the data to count low use properties and identify their type. The function also joins with 
#' `properties_in_each_lsoa` to calculate the ratio of low use properties to total properties in each LSOA, and the 
#' number of homes that are not low use. Additionally, it joins with the ONSPD dataset to add geographic information. 
#' The final data frame contains complete cases only.
#'
#' @examples
#' # Assuming all_codes, low_use_dict, and properties_in_each_lsoa are defined
#' processed_data <- process_low_use_data(all_codes, low_use_dict, properties_in_each_lsoa)
#'
#' @importFrom dplyr left_join filter group_by summarise mutate distinct
#' @importFrom purrr complete_cases
#' @export

process_low_use_data <- function(all_codes, low_use_dict, properties_in_each_lsoa) {
  
  # Process the low use data
  all_low_use_data <- all_codes %>%
    left_join(low_use_dict, by = 'code') %>%
    filter(low_use) %>%
    group_by(lsoa11cd) %>%
    summarise(low_use = n(),
              type = first(type)) %>%
    # Load the council tax household counts
    left_join(properties_in_each_lsoa, by = 'lsoa11cd' ) %>%
    mutate(low_use_ratio = low_use/all_properties,
           homes = all_properties - low_use) %>%
    left_join(., ONSPD_df %>%
                distinct(), by = 'lsoa11cd') %>%
    filter(complete.cases(.))
  
  # Return the processed data
  return(all_low_use_data)
}

