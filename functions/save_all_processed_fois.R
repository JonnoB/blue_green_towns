#' Save Processed Data to CSV Files Based on Administrative District Codes
#'
#' This function saves subsets of a given data frame into separate CSV files, 
#' each corresponding to a unique administrative district code. The files are saved 
#' in different folders based on a specific column value ('type') within the data.
#'
#' @param all_low_use_data A data frame containing the processed data with 
#'        columns 'Admin_district_code', 'type', and 'LADNM'.
#' @param data_folder The base directory path where the output CSV files will be saved.
#'
#' @details
#' The function iterates over unique values in the 'Admin_district_code' column of 
#' `all_low_use_data`. For each unique code, it filters the data and determines the 
#' appropriate subfolder ('low_use_LAD_files' or 'low_use_LAD_files_bourne') based on 
#' the first value of the 'type' column. It then constructs a file name using the 
#' district name and code, and saves the filtered data into a CSV file in the 
#' specified subfolder within `data_folder`.
#'
#' The function assumes the presence of 'Admin_district_code', 'type', and 'LADNM' 
#' columns in `all_low_use_data`. The output CSV files are named using the format 
#' "[District Name][District Code].csv".
#'
#' @examples
#' # Assuming all_low_use_data is a data frame with the necessary columns and 
#' # data_folder is a valid directory path
#' save_all_processed_fois(all_low_use_data, "/path/to/data_folder")
#'
#' @importFrom dplyr filter
#' @importFrom purrr walk
#' @importFrom readr write_csv
#' @export

save_all_processed_fois <- function(all_low_use_data, data_folder) {

  
  # Iterate over unique administrative district codes
  unique(all_low_use_data$Admin_district_code) %>%
    walk(~{
      # Filter data for each district code
      temp = all_low_use_data %>%
        filter(Admin_district_code == .x)
      
      # Determine folder name based on 'type' column value
      type_folder = ifelse(temp$type[1] == "BG", "low_use_LAD_files", "low_use_LAD_files_bourne")
      
      # Create file name
      file_name = paste(temp$LADNM[1], paste0(.x, ".csv"))
      
      # Save the filtered data to a CSV file
      write_csv(temp, file = file.path(data_folder, type_folder, file_name))
    })
}
