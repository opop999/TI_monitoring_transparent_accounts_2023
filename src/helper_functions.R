# Loading the required R libraries ----------------------------------------

# Package names
packages <- c("dplyr", "data.table", "stringr", "jsonlite", "httr", "tidyr", "rvest", "digest")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, function(package) suppressMessages(library(package, character.only = TRUE))))

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
prepare_output_directories <- function(dir_name, bank_name = NULL, individual_subfolders = TRUE) {
  stopifnot(
    is.character(dir_name),
    is.character(bank_name) | is.null(bank_name)
  )

  # We have to create a desired directory, if one does not yet exist
  if (!dir.exists(file.path(dir_name))) {
    dir.create(file.path(dir_name))
    print("OK: Main directory created.")
  } else {
    print("Main directory already exists.")
    print("OK: Main directory prepared.")
  }

  if (is.null(bank_name)) {
    return("Entity not specified - will not create specific directory.")
  }

  # We have to create a desired directory, if one does not yet exist
  if (!dir.exists(file.path(dir_name, bank_name))) {
    dir.create(file.path(dir_name, bank_name))
    print("OK: Entity directory created.")
  } else {
    print("Entity directory already exists.")
    print("OK: Entity directory prepared.")
    
  }

  # Repeat the check for subdirectory for individual account datasets
  if (individual_subfolders &
      !dir.exists(file.path(dir_name, bank_name, "individual_accounts"))) {
    dir.create(file.path(dir_name, bank_name, "individual_accounts"))
    print("Entity subdirectory created.")
  } else if (individual_subfolders &
             dir.exists(file.path(dir_name, bank_name, "individual_accounts"))) {
    print("Entity subdirectory already exists.")
    print("OK: Entity subdirectory prepared.")
    
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
append_new_data <- function(transactions_df, dir_name, bank_name, start_date=NULL, end_date=NULL) {
  
  if (dim(transactions_df)[1] != 0 & (is.null(start_date) | is.null(end_date))) {
    start_date <- min(transactions_df$date)
    end_date <- max(transactions_df$date)
    print(paste("IMPORTANT: MIN and MAX date argument was not specified. Their value is taken from the combined values of the combined", toupper(bank_name), "bank dataset."))
  }
  
  # Only append the full dataset if there are valid records the selected period
  if (dim(transactions_df)[1] == 0) {
    stop(paste("No transactions on any of the accounts at", toupper(bank_name), "between", format(as.Date(start_date), "%d.%m.%Y"), "and", format(as.Date(end_date), "%d.%m.%Y"), "- no need to append."))
  }

  # How many transactions were gathered?
  print(paste0(dim(transactions_df)[1], " transactions on the selected bank accounts at ", toupper(bank_name), " between ", format(as.Date(start_date), "%d.%m.%Y"), " and ", format(as.Date(end_date), "%d.%m.%Y"), "."))

  # Load in the existing full dataset and merge with yesterday's new data
  if (file.exists(file.path(dir_name, bank_name, paste0(bank_name, "_merged_data.rds")))) {
    print(paste("Older", toupper(bank_name), "combined dataset already exists - it will be appended with new transactions."))
    transactions_df %>%
      bind_rows(readRDS(file.path(dir_name, bank_name, paste0(bank_name, "_merged_data.rds")))) %>% # Append the existing dataset with new rows & delete duplicates
      distinct()
  }

  print(paste("Combined dataset of", toupper(bank_name), "has", nrow(transactions_df), "records."))

  return(transactions_df)
}


# Save finished dataset in the merged form and as well as in individual form
save_merged_and_individual <- function(transactions_df_appended, dir_name, bank_name) {
  # Save full dataset again both in CSV and RDS
  saveRDS(object = transactions_df_appended, file = file.path(dir_name, bank_name, paste0(bank_name, "_merged_data.rds")))
  fwrite(x = transactions_df_appended, file = file.path(dir_name, bank_name, paste0(bank_name, "_merged_data.csv")))

  # Split dataset to individual accounts
  split_transactions_df <- split(transactions_df_appended, transactions_df_appended$entity)

  # Write the dataset to chunks
  invisible(lapply(names(split_transactions_df), function(entity_df) {
    fwrite(split_transactions_df[[entity_df]], file = file.path(dir_name, bank_name, "individual_accounts", paste0(names(split_transactions_df[entity_df]), ".csv")))
  }))
  
  print(paste("Combined dataset and individual datasets of ", toupper(bank_name), "has been saved locally."))
  
}

# Combine merged datasets of all the banks
combine_merged_datasets <- function(dir_name, bank_names) {
  
  combined_dfs_list <- list.files(path = dir_name, pattern = paste0(bank_names, "_merged_data.rds", collapse = "|"), recursive = TRUE, full.names = TRUE)  
  
  lapply(combined_dfs_list, readRDS) %>% bind_rows()
  
}

# Combine merged datasets of all the banks
save_combined_dataset <- function(combined_dataset, dir_name, file_name = "all_banks_combined_dataset") {
  
  stopifnot(
    is.data.frame(combined_dataset) & nrow(combined_dataset) >= 1,
    is.character(dir_name) & length(dir_name) >= 1,
    is.character(file_name) & length(file_name) >= 1
  )
  
  saveRDS(object = combined_dataset, file = file.path(dir_name, paste0(file_name, ".rds")))

  print("Combined dataset of bank transparent account transactions sucessfully saved.")
  
}

load_combined_dataset <- function(dir_name, file_name = "all_banks_combined_dataset") {
  
  stopifnot(
    is.character(dir_name) & length(dir_name) >= 1,
    is.character(file_name) & length(file_name) >= 1
  )
  
  df <- readRDS(file.path(dir_name, paste0(file_name, ".rds")))
  
  print("Combined dataset of bank transparent account transactions sucessfully loaded.")
  
  return(df)
  
}


# FIO HELPER FUNCTIONS ----------------------------------------------------


# Verify arguments for FIO inputs -----------------------------------------

verify_fio_inputs <- function(dir_name, bank_name, bank_accounts, start_date, end_date, user_agent) {
  # Verify function's inputs
  stopifnot(
    is.character(dir_name),
    is.character(bank_name),
    !is.null(bank_accounts) & length(bank_accounts) >= 1,
    is.character(start_date) & validate_date(start_date),
    is.character(end_date) & validate_date(end_date),
    is.character(user_agent) & length(user_agent) >= 1
  )

  print("All FIO inputs should be OK.")
}

# CSAS HELPER FUNCTIONS ---------------------------------------------------

# Verify arguments for CSAS inputs -----------------------------------------

verify_csas_inputs <- function(dir_name, bank_name, bank_accounts, page_rows, start_date, end_date, api_key, user_agent, sort, sort_direction) {
  stopifnot(
    !is.null(bank_accounts) & length(bank_accounts) >= 1,
    is.character(dir_name),
    is.character(bank_name),
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

verify_csob_inputs <- function(dir_name, bank_name, bank_accounts, page_rows, start_date, end_date, temporary_cookie_csob, user_agent, sort, sort_direction) {
  stopifnot(
    !is.null(bank_accounts) & length(bank_accounts) >= 1,
    is.character(dir_name),
    is.character(bank_name),
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


# KB HELPER FUNCTIONS -----------------------------------------------------

get_kb_salt <- function(user_agent) {

  chosen_user_agent <- sample(user_agent, 1)
  
  GET("https://www.kb.cz/js/app.min.js?d=20190626",
                user_agent(chosen_user_agent),
                add_headers(
                  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
                  "Accept-Encoding" = "gzip, deflate, br",
                  "Accept-Language" = "en;q=0.6",
                  "Cache-Control" = "no-cache",
                  "Connection" = "keep-alive",
                  "Host" = "www.kb.cz",
                  "Pragma" = "no-cache",
                  "Sec-Fetch-Dest" = "document",
                  "Sec-Fetch-Mode" = "navigate",
                  "Sec-Fetch-Site" = "none",
                  "Sec-Fetch-User" = "?1",
                  "Upgrade-Insecure-Requests" = "1",
                  "User-Agent" = chosen_user_agent
                )) %>% 
    content(as = "text", encoding = "UTF-8") %>% 
    str_extract(pattern = '(?<=n.salt=\")[a-zA-Z0-9-]+')
  
}


verify_kb_inputs <- function(dir_name, bank_name, bank_accounts, skip_param, salt_kb, user_agent) {
  stopifnot(
    !is.null(bank_accounts) & length(bank_accounts) >= 1,
    is.character(dir_name),
    is.character(bank_name),
    is.numeric(skip_param) & !(skip_param %% 50),
    is.character(salt_kb) & nchar(salt_kb) >= 30,
    is.character(user_agent) & length(user_agent) >= 1
  )
  
  print("All KB inputs should be OK.")
}
