# 0. Source helper functions and define constants -------------------------
source("src/helper_functions.R")

dir_name <- "output"
start_date <- "2022-07-01"
end_date <- as.character(Sys.Date())
sub_dir_name <- "summary_tables"
datasets <- c("total_spending", "time_spending", "total_income", "time_income")

# 1. Prepare output directories -------------------------------------------
prepare_output_directories(dir_name, sub_dir_name, individual_subfolders = FALSE)

# 2. Combine datasets of individual banks ---------------------------------
combined_dataset <- load_combined_dataset(dir_name)

# 3. Create four summarized datasets --------------------------------------
total_spending <- create_total_summary(combined_dataset, start_date, end_date, type = "spend")
time_spending <- create_time_series(combined_dataset, start_date, end_date, type = "spend")

total_income <- create_total_summary(combined_dataset, start_date, end_date, type = "income")
time_income <- create_time_series(combined_dataset, start_date, end_date, type = "income")

# 4. Write summarized datasets locally ------------------------------------
save_summarized_datasets(datasets, dir_name, sub_dir_name)
