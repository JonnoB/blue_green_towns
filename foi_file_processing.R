

#plu stands for processed low use dataframe

#This script processes all the files then combines them into a single dataframe and wipes the original dataframes from
#the workspace

test_valley_plu_df<- process_foi_templates(file_path = file.path(foi_files, "Test ValleyDiscountsLSOA.xlsx" ) , 
                               council_tax_type = "Discount_Class", 
                               keep_classes = c("Empty & Unfurnished  < 3 Months", "Empty & Unfurnished  > 3 Months", 
                                                "2nd Home Discount", "300% Surcharge for Empty Properties", "Major Repairs / Structural Alt > 12 mnth", 
                                                "Major Repairs / Structural Alt < 12 mnth", "200% Surcharge for Empty Properties", 
                                                "100% Surcharge for Empty Properties", "Second Home discount", 
                                                "Empty & Unfurn>6 Months - Owned by Chari", "2nd Home - Occupation Restricted", 
                                                "0% surcharge for Empty Properties") )
