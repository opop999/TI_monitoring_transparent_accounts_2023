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
    error = function(err) {
      FALSE
    }
  )
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
    print("OK: Main output directory prepared.")
  }

  if (is.null(entity_name)) {
    return("Entity not specified - will not create specific directory.")
  }

  # We have to create a desired directory, if one does not yet exist
  if (!dir.exists(file.path(dir_name, entity_name))) {
    dir.create(file.path(dir_name, entity_name))
  } else {
    print("Entity output directory already exists.")
    print("OK: Entity output directory prepared.")
    
  }

  # Repeat the check for subdirectory for individual account datasets
  if (!dir.exists(file.path(dir_name, entity_name, "individual_accounts"))) {
    dir.create(file.path(dir_name, entity_name, "individual_accounts"))
  } else if (dir.exists(file.path(dir_name, entity_name, "individual_accounts"))) {
    print("Entity output subdirectory already exists.")
    print("OK: Entity output subdirectory prepared.")
    
  }

  print("OK: All output directories prepared.")
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

  # Save the list of lists for all of the accounts 
  saveRDS(all_accounts_list, file.path(dir_name, "list_of_monitored_accounts.rds"))
  
  print("All bank accounts initialized.")
}

# Initialize user agents -------------------------------------------------
initialize_user_agents <- function(dir_name, ...) {
  stopifnot(
    is.character(dir_name),
    all(sapply(list(...), is.character))
  )
  
  prepare_output_directories(dir_name)
  
  
  # Initiate an empty list, serving as a list of all of the lists
  all_user_agents <- c(
    ...
  )
  
  # Save the user agents as an .rds file
  saveRDS(all_user_agents, file.path(dir_name, "user_agents.rds"))
  
  print("All user agent profiles initialized.")
  
}

# Append newly extracted data to the previous dataset (if it exists)
append_new_data <- function(transactions_df, dir_name, entity_name, start_date, end_date) {
  # Only append the full dataset if there are valid records the selected period
  if (dim(transactions_df)[1] == 0) {
    stop(paste("No transactions on any of the accounts at", toupper(entity_name), "between", format(as.Date(start_date), "%d.%m.%Y"), "and", format(as.Date(end_date), "%d.%m.%Y"), "- no need to append."))
  }

  # How many transactions were gathered?
  print(paste0(dim(transactions_df)[1], " transactions on the selected bank accounts at ", toupper(entity_name), " between ", format(as.Date(start_date), "%d.%m.%Y"), " and ", format(as.Date(end_date), "%d.%m.%Y"), "."))

  # Load in the existing full dataset and merge with yesterday's new data
  if (file.exists(file.path(dir_name, entity_name, paste0(entity_name, "_merged_data.rds")))) {
    print(paste("Older", toupper(entity_name), "combined dataset already exists - it will be appended with new transactions."))
    transactions_df %>%
      bind_rows(readRDS(file.path(dir_name, entity_name, paste0(entity_name, "_merged_data.rds")))) %>% # Append the existing dataset with new rows & delete duplicates
      distinct()
  }

  print(paste("Combined dataset of", toupper(entity_name), "has", nrow(transactions_df), "records."))

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
    is.character(user_agent) & length(user_agent) >= 1
  )

  print("All FIO inputs should be OK.")
}

# CSAS HELPER FUNCTIONS ---------------------------------------------------

# Verify arguments for CSAS inputs -----------------------------------------

verify_csas_inputs <- function(dir_name, entity_name, accounts_csas, page_rows, start_date, end_date, api_key, user_agent, sort, sort_direction) {
  stopifnot(
    !is.null(accounts_csas) & length(accounts_csas) >= 1,
    is.character(dir_name),
    is.character(entity_name),
    is.numeric(page_rows) & page_rows > 0,
    is.character(start_date) & validate_date(start_date),
    is.character(end_date) & validate_date(end_date),
    is.character(api_key) & nchar(api_key) >= 1,
    is.character(user_agent) & length(user_agent) >= 1,
    is.character(sort) & nchar(sort) >= 1,
    is.character(sort_direction) & sort_direction %in% c("ASC", "DESC")
  )

  print("All CSAS inputs should be OK.")
}

# Verify that Ceska Sporitelna API key valid --------------------------------
verify_csas_api_key <- function(api_key, user_agent) {
  test_request <-
    GET(
      "https://api.csas.cz/webapi/api/v3/transparentAccounts/",
      add_headers(
        'Accept' = 'application/json, text/plain, */*',
        'Accept-Language' = 'en;q=0.9',
        'Cache-Control' = 'no-cache',
        'Connection' = 'keep-alive',
        'Origin' = 'https://www.csas.cz',
        'Pragma' = 'no-cache',
        'Referer' = 'https://www.google.com/',
        'Sec-Fetch-Dest' = 'empty',
        'Sec-Fetch-Mode' = 'cors',
        'Sec-Fetch-Site' = 'same-site',
        'Sec-GPC' = '1',
        'User-Agent' = sample(user_agent, 1),
        'Web-Api-Key' = api_key
      )
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

# CSOB HELPER FUNCTIONS -------------------------------------------------

verify_csob_inputs <- function(dir_name, entity_name, accounts_csob, page_rows, start_date, end_date, temporary_cookie_csob, user_agent, sort, sort_direction) {
  stopifnot(
    !is.null(accounts_csob) & length(accounts_csob) >= 1,
    is.character(dir_name),
    is.character(entity_name),
    is.numeric(page_rows) & page_rows > 0,
    is.character(start_date) & validate_date(start_date),
    is.character(end_date) & validate_date(end_date),
    is.character(temporary_cookie_csob) & nchar(temporary_cookie_csob) >= 10,
    is.character(user_agent) & length(user_agent) >= 1,
    is.character(sort) & nchar(sort) >= 1,
    is.character(sort_direction) & sort_direction %in% c("ASC", "DESC")
  )
  
  print("All CSOB inputs should be OK.")
}
