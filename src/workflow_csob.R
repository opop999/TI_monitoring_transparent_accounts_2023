#  Get CSOB's transactions ------------------------------------------------

# Source main and helper functions
source("src/extract_csob_accounts.R")
source("src/helper_functions.R")

# Set function-specific variables
dir_name <- "output"
bank_name <- "csob"
bank_accounts <- readRDS(file.path(dir_name, "list_of_monitored_accounts.rds"))[[bank_name]]
page_rows <- 10000
temporary_cookie_csob <- Sys.getenv("CSOB_COOKIE")
sort <- "AccountingDate" # Which column is used for sorting? We keep this consistent.
sort_direction <- "DESC" # Which direction (DESC/ASC) for the sort. We keep this consistent.
start_date <- as.character(Sys.Date() - 8)
end_date <- as.character(Sys.Date())
user_agent <- readRDS(file.path(dir_name, "user_agents.rds"))

# Only execute if there are specified bank accounts for this bank
if (!is.null(bank_accounts)) {
  # Verify inputs specified above
  verify_csob_inputs(dir_name, bank_name, bank_accounts, page_rows, start_date, end_date, temporary_cookie_csob, user_agent, sort, sort_direction)
  
  # Prepare output directories
  prepare_output_directories(dir_name, bank_name)
  
  # Get the most recent transactions
  transactions_df <- get_csob_transactions(
    bank_accounts,
    dir_name,
    page_rows,
    start_date,
    end_date,
    sort,
    sort_direction,
    user_agent,
    temporary_cookie_csob
  )
  
  # Append new transactions to (potentially) existing dataset
  transactions_df_appended <- append_new_data(transactions_df, dir_name, bank_name, start_date, end_date)
  
  # Save as a merged dataset and a set of individual ones
  save_merged_and_individual(transactions_df_appended, dir_name, bank_name)
}