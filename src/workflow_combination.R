# Combine datasets to one and save ----------------------------------------
source("src/helper_functions.R")
dir_name <- "output"

bank_names <-
  names(readRDS(file.path(
    dir_name, "list_of_monitored_accounts.rds"
  )))

combined_dataset <- combine_merged_datasets(dir_name, bank_names)

save_combined_dataset(combined_dataset, dir_name)
