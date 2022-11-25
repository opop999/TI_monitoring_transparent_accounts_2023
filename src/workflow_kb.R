# Get KB's transactions ---------------------------------------------------

# Source main and helper functions
source("src/extract_kb_accounts.R")
source("src/helper_functions.R")

# Set function-specific variables
dir_name <- "output"
bank_name <- "kb"
bank_accounts <- readRDS(file.path(dir_name, "list_of_monitored_accounts.rds"))[[bank_name]]
skip_param <- 5000 # The total number of records equals skip_param + 50
start_date <- as.character(Sys.Date() - 8)
end_date <- as.character(Sys.Date())
user_agent <- readRDS(file.path(dir_name, "user_agents.rds"))

# Only execute if there are specified bank accounts for this bank
if (!is.null(bank_accounts)) {
  # Get salt token number, used to generate one-time API token
  salt_kb <- get_kb_salt(user_agent)
  
  # Verify inputs specified above
  verify_kb_inputs(dir_name, bank_name, start_date, end_date, bank_accounts, skip_param, salt_kb, user_agent)
  
  # Prepare output directories
  prepare_output_directories(dir_name, bank_name)
  
  # Get the most recent transactions
  transactions_df <- get_kb_transactions(
    bank_accounts,
    dir_name,
    start_date, 
    end_date,
    skip_param,
    salt_kb,
    user_agent
  )
  
  # Append new transactions to (potentially) existing dataset
  transactions_df_appended <- append_new_data(transactions_df, dir_name, bank_name, start_date, 
                                              end_date)
  
  # Save as a merged dataset and a set of individual ones
  save_merged_and_individual(transactions_df_appended, dir_name, bank_name)
}
