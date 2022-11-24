# 0. Source helper functions and define constants -------------------------
source("src/helper_functions.R")
dir_name <- "data"
start_date <- as.character(Sys.Date() - 8)
end_date <- as.character(Sys.Date())

# 1. Specify bank accounts to be extracted --------------------------------
initialize_bank_accounts(
  dir_name = dir_name,
  csas = c(
    "danuse_nerudova" = "000000-4776908073",
    "tomas_zima" = "000000-4758343013",
    "pavel_fischer" = "000000-4805949043"
  ),
  csob = c(
    "libor_michalek" = "309564349",
    "karel_divis" = "310031267"
  ),
  fio = c(
    "petr_pavel" = "2902252345",
    "tomas_brezina" = "2902245970",
    "marek_hilser" = "20182024",
    "josef_stredula" = "2602250487",
    "jaroslav_basta" = "2702310803"
  ),
  kb = c(
    "karel_janecek" = "721721721",
    "andrej_babis" = "123-8794230217",
    "denisa_rohanova" = "123-7843850267"
  ),
  rb = c("jiri_kotab" = "28334472") # ID of account different from account nr.
)


# 2. Specify set of user agents ----------------------------------------------
initialize_user_agents(
  dir_name = dir_name,
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36 Edg/107.0.1418.42",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/106.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/106.0.0.0 Safari/537.36",
  "Mozilla/5.0 (X11; Linux x86_64; rv:106.0) Gecko/20100101 Firefox/106.0",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:105.0) Gecko/20100101 Firefox/105.0",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:106.0) Gecko/20100101 Firefox/106.0",
  "Mozilla/5.0 (Windows NT 10.0; rv:106.0) Gecko/20100101 Firefox/106.0"
)

user_agent <- readRDS(file.path(dir_name, "user_agents.rds"))

# 3. Get FIO transactions -------------------------------------------------

# Source main function
source("src/extract_fio_accounts.R")

# Set function-specific variables
bank_name <- "fio"
bank_accounts <- readRDS(file.path(dir_name, "list_of_monitored_accounts.rds"))[[bank_name]]

# Only execute if there are specified bank accounts for this bank
if (!is.null(bank_accounts)) {
  # Verify inputs specified above
  verify_fio_inputs(dir_name, bank_name, bank_accounts, start_date, end_date, user_agent)

  # Prepare output directories
  prepare_output_directories(dir_name, bank_name)

  # Get the most recent transactions
  transactions_df <- get_fio_transactions(
    bank_accounts,
    start_date,
    end_date,
    user_agent
  )

  # Append new transactions to (potentially) existing dataset
  transactions_df_appended <- append_new_data(transactions_df, dir_name, bank_name, start_date, end_date)

  # Save as a merged dataset and a set of individual ones
  save_merged_and_individual(transactions_df_appended, dir_name, bank_name)
}
# 4. Get Ceska Sporitelna transactions ------------------------------------

# Source main function
source("src/extract_csas_accounts.R")

# Set function-specific variables
bank_name <- "csas"
bank_accounts <-
  readRDS(file.path(dir_name, "list_of_monitored_accounts.rds"))[[bank_name]]
page_rows <- 1000
api_key <- Sys.getenv("CS_TOKEN")
sort <- "processingDate" # Which column is used for sorting?
sort_direction <- "DESC" # Which direction (DESC/ASC) for the sort.

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

# 5. Get CSOB's transactions ----------------------------------------------

# Source main function
source("src/extract_csob_accounts.R")

# Set function-specific variables
bank_name <- "csob"
bank_accounts <- readRDS(file.path(dir_name, "list_of_monitored_accounts.rds"))[[bank_name]]
page_rows <- 10000
temporary_cookie_csob <- Sys.getenv("CSOB_COOKIE")
sort <- "AccountingDate" # Which column is used for sorting? We keep this consistent.
sort_direction <- "DESC" # Which direction (DESC/ASC) for the sort. We keep this consistent.

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

# 6. Get KB's transactions ------------------------------------------------

# Source main function
source("src/extract_kb_accounts.R")

# Set function-specific variables
bank_name <- "kb"
bank_accounts <- readRDS(file.path(dir_name, "list_of_monitored_accounts.rds"))[[bank_name]]
skip_param <- 5000 # The total number of records equals skip_param + 50

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

# 7. Get RB's transactions ------------------------------------------------

# Source main function
source("src/extract_rb_accounts.R")

# Set function-specific variables
bank_name <- "rb"
bank_accounts <- readRDS(file.path(dir_name, "list_of_monitored_accounts.rds"))[[bank_name]]
page_rows <- 100

# Only execute if there are specified bank accounts for this bank
if (!is.null(bank_accounts)) {
  # Verify inputs specified above
  verify_rb_inputs(dir_name, bank_name, bank_accounts, page_rows, start_date, end_date, user_agent)

  # Prepare output directories
  prepare_output_directories(dir_name, bank_name)

  # Get the most recent transactions
  transactions_df <- get_rb_transactions(bank_accounts,
                                         dir_name,
                                         page_rows,
                                         start_date,
                                         end_date,
                                         user_agent)

  # Append new transactions to (potentially) existing dataset
  transactions_df_appended <- append_new_data(transactions_df, dir_name, bank_name, start_date, end_date)

  # Save as a merged dataset and a set of individual ones
  save_merged_and_individual(transactions_df_appended, dir_name, bank_name)
}

# 8. Combine datasets to one and save -------------------------------------
bank_names <-
  names(readRDS(file.path(
    dir_name, "list_of_monitored_accounts.rds"
  )))

combined_dataset <- combine_merged_datasets(dir_name, bank_names)

save_combined_dataset(combined_dataset, dir_name)
