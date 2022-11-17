# Loading the required R libraries ----------------------------------------

# Package names
packages <- c("dplyr", "data.table", "stringr", "jsonlite", "httr", "tidyr", "rvest")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Turn off scientific notation of numbers
options(scipen = 999)


# SHARED HELPER FUNCTIONS -------------------------------------------------


# Validate date format ----------------------------------------------------

validate_date <- function(mydate, date_format = "%Y-%m-%d") {
  tryCatch(!is.na(as.Date(mydate, date_format)),  
           error = function(err) {FALSE})  
}

# Prepare output directories -------------------------------------------

prepare_output_directories <- function(dir_name, entity_name = NULL) {
  stopifnot(
    is.character(dir_name),
    is.character(entity_name) | is.null(entity_name)
  )

  # We have to create a desired directory, if one does not yet exist
  if (!dir.exists(file.path(dir_name))) {
    dir.create(file.path(dir_name))
  } else {
    print("Main output directory already exists.")
  }

  if (is.null(entity_name)) {
    return("Main output directory prepared.")
  }

  # We have to create a desired directory, if one does not yet exist
  if (!dir.exists(file.path(dir_name, entity_name))) {
    dir.create(file.path(dir_name, entity_name))
  } else {
    print("Entity output directory already exists")
  }

  # Repeat the check for subdirectory for individual account datasets
  if (!dir.exists(file.path(dir_name, entity_name, "individual_accounts"))) {
    dir.create(file.path(dir_name, entity_name, "individual_accounts"))
  } else if (dir.exists(file.path(dir_name, entity_name, "individual_accounts"))) {
    print("Entity output subdirectory already exists")
  }

  print("Output directories prepared.")
}


# Initialize bank account numbers -----------------------------------------

initialize_bank_accounts <- function(dir_name, ...) {
  
  stopifnot(
    is.character(dir_name),
    all(sapply(list(...), is.character))
  )

  prepare_output_directories(dir_name)


  # Initiate an empty list, serving as a list of all of the lists
  all_accounts_list <- list(
    ...
  ) 

  # Save the list of lists for all of the accounts ----------------------------
  saveRDS(all_accounts_list, file.path(dir_name, "list_of_monitored_accounts.rds"))
}

# Append newly extracted data to the previous dataset (if it exists)
append_new_data <- function(transactions_df, dir_name, entity_name, start_date, end_date) {
  
  # Only append the full dataset if there are valid records the selected period
  if (dim(transactions_df)[1] == 0) {
    stop(paste("No transactions on any of the selected bank accounts between", start_date, "and", end_date, "- no need to append."))
  }
  
  # How many transactions were gathered?
  print(paste(dim(transactions_df)[1], "transactions on some of the selected bank accounts between", start_date, "and", end_date, "."))
  
  
  # Load in the existing full dataset and merge with yesterday's new data
  if (file.exists(file.path(dir_name, entity_name, paste0(entity_name, "_merged_data.rds")))) {
    print("Older transactions dataframe already exists - it will be appended with new transactions.")
    transactions_df %>%
      bind_rows(readRDS(file.path(dir_name, entity_name, paste0(entity_name, "_merged_data.rds")))) %>% # Append the existing dataset with new rows & delete duplicates
      distinct()
  }
  
  return(transactions_df)
  
}


# Save finished dataset in the merged form and as well as in individual form
save_merged_and_individual <- function(transactions_df_appended, dir_name, entity_name) {
  
  # Save full dataset again both in CSV and RDS
  saveRDS(object = transactions_df_appended, file = file.path(dir_name, entity_name, paste0(entity_name, "_merged_data.rds")))
  fwrite(x = transactions_df_appended, file = file.path(dir_name, entity_name, paste0(entity_name, "_merged_data.csv")))
  
  # Split dataset to individual accounts
  split_transactions_df <- split(transactions_df_appended, transactions_df_appended$entity)
  
  # Write the dataset to chunks
  invisible(lapply(names(split_transactions_df), function(entity_df) {
    fwrite(split_transactions_df[[entity_df]], file = file.path(dir_name, entity_name, "individual_accounts", paste0(names(split_transactions_df[entity_df]), ".csv")))
  }))
}

# FIO HELPER FUNCTIONS ----------------------------------------------------


# Verify arguments for FIO inputs -----------------------------------------

verify_fio_inputs <- function(dir_name, entity_name, accounts_fio, start_date, end_date, user_agent) {
  # Verify function's inputs
  stopifnot(
    is.character(dir_name),
    is.character(entity_name),
    !is.null(accounts_fio) & length(accounts_fio) >= 1,
    is.character(start_date) & validate_date(start_date, "%d.%m.%Y"),
    is.character(end_date) & validate_date(end_date, "%d.%m.%Y"),
    is.character(user_agent) & nchar(user_agent) >= 1
  )
  
  print("All FIO inputs should be OK.")
}

# CSAS HELPER FUNCTIONS ---------------------------------------------------

# Verify that Ceska Sporitelna API key valid --------------------------------
verify_csas_api_key <- function(api_key) {
  test_request <-
    GET(
      "https://api.csas.cz/webapi/api/v3/transparentAccounts/",
      add_headers(`WEB-API-key` = api_key)
    )
  
  if (status_code(test_request) == 200) {
    print("API returned 200, provided API key should be valid.")
  } else if (status_code(test_request) == 412) {
    stop("API returned error 412, provided API key might be invalid.")
  } else {
    warning(
      paste(
        "API returned error",
        status_code(test_request),
        "with the following message:",
        http_status(test_request)[["message"]]
      )
    )
  }
}