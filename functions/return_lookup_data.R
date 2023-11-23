#' Return Lookup Data from CSV File
#'
#' This function reads a CSV file from a specified folder based on a regular expression pattern 
#' and returns a data frame with modified column names. It is specifically designed to work with 
#' lookup tables where the column names include year digits. It removes these digits to ensure 
#' compatibility across different years' tables. However, it may encounter errors if there are 
#' changes in the naming conventions used in the files.
#'
#' @param folder_path The path to the folder containing the CSV file.
#' @param file_regex_pattern A regular expression pattern to match the desired file.
#'
#' @return A data frame with the contents of the CSV file, but with the year digits removed 
#' from the column names.
#'
#' @details
#' The function searches for a file in the specified `folder_path` that matches the given 
#' `file_regex_pattern`. It reads the first matched file into a data frame, assuming the file is 
#' in CSV format. The function then modifies the column names of this data frame by removing 
#' any digits, which are assumed to represent years in the context of lookup tables. This 
#' standardization makes the process robust against changes in the year parts of column names 
#' in different versions of the lookup tables. However, it is important to note that significant 
#' deviations from the expected naming convention (e.g., changes made by the Office for 
#' National Statistics) may lead to errors.
#'
#' @examples
#' # Example usage
#' df <- return_lookup_data("path/to/lookup/files", "ward_names_.*\\.csv")
#'
#' @note
#' This function relies on the naming convention where the year is included in the column names.
#' If this convention is changed, the function may not work as intended.
#'
#' @importFrom readr read_csv
#' @export
return_lookup_data <- function(folder_path, file_regex_pattern ){

  
  
  #get the path to the ward names lookup
  lookup_path <-list.files(folder_path, 
                           pattern = file_regex_pattern, 
                           ignore.case = TRUE, 
                           full.names = TRUE)
  
  lookup_df <- read_csv(lookup_path) 
  
  
  
  #Removes the years digits from the column names.
  #The lookup tables have the year that the table codes were generated the name
  #i.e. on the wardnames lookup from after the 2020 update 
  #the column names will be "WD20CD" and "WD20NM",
  #This Regex removes the numbers meaning that changes in years will not affect the process
  #This makes the process more robust.
  #However, if the ONS change the naming convention could cause an error
  names(lookup_df) = gsub("\\d","",names(lookup_df))
  
  return(lookup_df)
}
