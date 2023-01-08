
process_foi_templates <- function(file_path, 
                                  council_tax_type = "Discount_Class", 
                                  keep_classes = NULL,
                                  full = TRUE ){
  #This function provides a standardised way of cleaning the data
  #df: data frame of the laoded excel template with only the first 4 columns
  #council_tax_type the name of the colummn that contains classes of interest
  #keep_classes which classes to keep if NULL keeps nothing
  #full: do you want the whole process or partial. 
  #a partial process can be useful to find out what you want to keep


  

  
  if(full){
    df <- file_path %>%
      #load excel file and take first 4 columns
      read_xlsx(., sheet=1) %>%
      select(1:4) %>%
      #filter to keep only the necessary classes
      filter(.data[[council_tax_type]] %in% keep_classes) %>%
      #get the counts per low use class
      group_by(LSOA_CODE) %>%
      summarise(low_use = n())
  } else {
    
    df <- file_path %>%
      read_xlsx(., sheet=1) %>%
      select(1:4)  
    
  }
  return(df)
}

