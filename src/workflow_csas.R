# Get Ceska Sporitelna transactions --------------------------------------

# Source main and helper functions
source("src/extract_csas_accounts.R")
source("src/helper_functions.R")

# Set function-specific variables
dir_name <- "output"
bank_name <- "csas"
bank_accounts <-
  readRDS(file.path(dir_name, "list_of_monitored_accounts.rds"))[[bank_name]]
page_rows <- 1000
api_key <- Sys.getenv("CS_TOKEN")
sort <- "processingDate" # Which column is used for sorting?
sort_direction <- "DESC" # Which direction (DESC/ASC) for the sort.
start_date <- as.character(Sys.Date() - 8)
end_date <- as.character(Sys.Date())
user_agent <- readRDS(file.path(dir_name, "user_agents.rds"))

# Only execute if there are specified bank accounts for this bank
if (!is.null(bank_accounts)) {
  # Verify inputs specified above
  verify_csas_inputs(dir_name, bank_name, bank_accounts, page_rows, start_date, end_date, api_key, user_agent, sort, sort_direction)
  
  # Prepare output directories
  prepare_output_directories(dir_name, bank_name)
  
  # Verification of API key
  verify_csas_api_key(api_key, user_agent)
  
  # Get the most recent transactions
  transactions_df <- get_csas_transactions(
    bank_accounts,
    dir_name,
    page_rows,
    start_date,
    end_date,
    sort,
    sort_direction,
    api_key,
    user_agent
  )
  
  # Append new transactions to (potentially) existing dataset
  transactions_df_appended <- append_new_data(transactions_df, dir_name, bank_name, start_date, end_date)
  
  # Save as a merged dataset and a set of individual ones
  save_merged_and_individual(transactions_df_appended, dir_name, bank_name)
}
