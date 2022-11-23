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
    is.character(bank_name) || is.null(bank_name)
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
  if (individual_subfolders &&
    !dir.exists(file.path(dir_name, bank_name, "individual_accounts"))) {
    dir.create(file.path(dir_name, bank_name, "individual_accounts"))
    print("Entity subdirectory created.")
  } else if (individual_subfolders &&
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


# Repeat call if error ----------------------------------------------------
retry_call <- function(expr,
                       isError = function(x) {
                         inherits(x, "try-error")
                       },
                       maxErrors = 5,
                       sleep = 30) {
  attempts <- 0
  retval <- try(eval.parent(substitute(expr)), silent = TRUE)
  while (isError(retval)) {
    attempts <- attempts + 1
    if (attempts >= maxErrors) {
      msg <- "END: Too many retries."
      cat(msg, "\n")
      stop()
    } else {
      msg <- sprintf(
        "Attempt %i/%i failed.",
        attempts,
        maxErrors
      )
      cat(msg, "\n")
    }
    if (sleep > 0) {
      Sys.sleep(sleep)
    }
    retval <- try(eval.parent(substitute(expr)), silent = TRUE)
  }
  cat(sprintf(
    "SUCCESS on attempt %i/%i",
    attempts,
    maxErrors
  ), "\n")
  return(retval)
}

# Append newly extracted data to the previous dataset (if it exists)
append_new_data <- function(transactions_df, dir_name, bank_name, start_date = NULL, end_date = NULL) {
  if (dim(transactions_df)[1] != 0 && (is.null(start_date) || is.null(end_date))) {
    start_date <- min(transactions_df$date)
    end_date <- max(transactions_df$date)
    print(paste("IMPORTANT: MIN and MAX date argument was not specified. Their value is taken from the combined values of the combined", toupper(bank_name), "bank dataset."))
  }

  # Only append the full dataset if there are valid records the selected period
  if (dim(transactions_df)[1] == 0) {
    print(paste("No transactions on any of the accounts at", toupper(bank_name), "between", format(as.Date(start_date), "%d.%m.%Y"), "and", format(as.Date(end_date), "%d.%m.%Y"), "- no need to append."))
    return()
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
  if (is.null(transactions_df_appended)) {
    return(paste("No transactions on any of the accounts at", toupper(bank_name), "- no need to save"))
  }

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

  lapply(combined_dfs_list, readRDS) %>%
    bind_rows() %>%
    unite(
      col = notes,
      c(
        "message_for_recipient",
        "note"
      ),
      sep = " ",
      na.rm = TRUE
    ) %>%
    select(any_of(c("entity", "date", "type", "amount", "currency", "notes", "contra_account_name", "contra_account_number", "contra_account_bank_code", "vs", "ks", "ss"))) %>%
    na_if("")
}

# Combine merged datasets of all the banks
save_combined_dataset <- function(combined_dataset, dir_name, file_name = "all_banks_combined_dataset") {
  stopifnot(
    is.data.frame(combined_dataset) && nrow(combined_dataset) >= 1,
    is.character(dir_name) && length(dir_name) >= 1,
    is.character(file_name) && length(file_name) >= 1
  )

  saveRDS(object = combined_dataset, file = file.path(dir_name, paste0(file_name, ".rds")))

  print("Combined dataset of bank transparent account transactions sucessfully saved.")
}

load_combined_dataset <- function(dir_name, file_name = "all_banks_combined_dataset") {
  stopifnot(
    is.character(dir_name) && length(dir_name) >= 1,
    is.character(file_name) && length(file_name) >= 1
  )

  df <- readRDS(file.path(dir_name, paste0(file_name, ".rds")))

  print("Combined dataset of bank transparent account transactions sucessfully loaded.")

  return(df)
}

# Create a time series of cumulative spending/income
create_time_series <- function(combined_dataset, start_date, end_date, type = c("income", "spend")) {
  if (type == "income") {
    operator <- ">"
  } else if (type == "spend") {
    operator <- "<"
  }

  combined_dataset %>%
    filter(date >= as.Date(start_date) &
      date <= as.Date(end_date) & amount < 0) %>%
    transmute(
      entity = as.factor(entity),
      date,
      "{type}_million" := abs(amount) / 1000000
    ) %>%
    group_by(entity) %>%
    arrange(date) %>%
    mutate("cumulative_{type}_million" := cumsum(.data[[paste0(type, "_million")]])) %>%
    ungroup()
}

# Create a summary table of a total income
create_total_summary <-
  function(combined_dataset, start_date, end_date, type = c("income", "spend")) {
    if (type == "income") {
      operator <- ">"
    } else if (type == "spend") {
      operator <- "<"
    }

    combined_dataset %>%
      filter(date >= as.Date(start_date) &
        date <= as.Date(end_date) & get(operator)(amount, 0)) %>%
      transmute(
        date,
        "{type}_million" := abs(amount) / 1000000,
        entity = as.factor(entity)
      ) %>%
      group_by(entity) %>%
      summarise(
        "total_transactions_{type}" := n(),
        "total_{type}_million" := round(sum(.data[[paste0(type, "_million")]]), digits = 3),
        "per_transaction_avg_{type}_thousand" := round(mean(.data[[paste0(type, "_million")]]) * 1000, digits = 3)
      ) %>%
      ungroup() %>%
      arrange(desc(.data[[paste0("total_", type, "_million")]]))
  }

# Combine merged datasets of all the banks
save_summarized_datasets <- function(total_spending, time_spending, total_income, time_income, dir_name, sub_dir_name) {
  stopifnot(
    is.data.frame(total_spending),
    is.data.frame(time_spending),
    is.data.frame(total_income),
    is.data.frame(time_income)
  )

  saveRDS(total_spending, file.path(dir_name, sub_dir_name, "total_spending.rds"))
  saveRDS(time_spending, file.path(dir_name, sub_dir_name, "time_spending.rds"))
  saveRDS(total_income, file.path(dir_name, sub_dir_name, "total_income.rds"))
  saveRDS(time_income, file.path(dir_name, sub_dir_name, "time_income.rds"))

  print("Summarized datasets of bank transactions sucessfully saved.")
}
