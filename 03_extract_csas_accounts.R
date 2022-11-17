# Extraction of transaction data on Ceska Sporitelna's transparent bank accounts

# Source helper functions
source("helper_functions.R")

# Temporally disable ssl verification due to certificate issues on the side of the bank
# httr::set_config(config(ssl_verifypeer = 0L))

# 2. Function for extraction of the expense transparent bank accounts based in CS bank
scrape_csas_accounts <- function(accounts_csas, dir_name, sub_dir_name, page_rows, start_date, end_date, sort, sort_direction, api_key, user_agent) {
  
  # Verify function's inputs
  stopifnot(
    !is.null(accounts_csas$numbers) & length(accounts_csas$numbers) >= 1,
    is.character(dir_name),
    is.character(sub_dir_name),
    is.numeric(page_rows) & page_rows > 0,
    is.character(start_date) & nchar(start_date) == 10,
    is.character(end_date) & nchar(end_date) == 10,
    is.character(api_key) & nchar(api_key) >= 1,
    is.character(user_agent) & nchar(user_agent) >= 1,
    is.character(sort) & nchar(sort) >= 1,
    is.character(sort_direction) & sort_direction %in% c("ASC", "DESC")
  )
  
  # Verification of API key
  print("Verifying the validity of provided API key")
  verify_csas_api_key(api_key)
  
  # Prepare output directories
  prepare_output_directories(dir_name, sub_dir_name)

  # How many bank accounts to be extracted?
  print(paste(length(accounts_csas$numbers), "bank account(s) selected, will run the function."))

  # Create list which will be appended
  transactions_list <-
    vector(mode = "list", length = length(accounts_csas$numbers)) %>%
    setNames(accounts_csas$names)

  # Loop to deal with more than one accounts
  for (i in seq_along(accounts_csas[["numbers"]])) {
    
    transactions_list[[accounts_csas[["names"]][i]]] <- GET(
      url = paste0("https://api.csas.cz/webapi/api/v3/transparentAccounts/", accounts_csas[["numbers"]][i], "/transactions"),
      add_headers(
        "WEB-API-key" = api_key,
        "User-Agent" = user_agent
      ),
      query = list(
        order = sort_direction,
        page = "0",
        size = page_rows,
        sort = sort,
        dateFrom = start_date,
        dateTo = end_date
      )
    ) %>%
      content(as = "text") %>%
      fromJSON(flatten = TRUE) %>%
      .[["transactions"]]

    # Skip to next loop iteration if there are no returned transactions
    if (!length(transactions_list[[accounts_csas[["names"]][i]]])) {
      print(paste("No transactions on the account of entity", accounts_csas[["names"]][i], "between", start_date, "and", end_date))
      next
    }


    full_list_clean[[accounts_csas[[1]][i]]] <- full_list[[accounts_csas[[1]][i]]] %>%
      transmute(
        date = as.Date(processingDate),
        amount = as.numeric(amount.value),
        type = as.character(typeDescription),
        ks = as.numeric(sender.constantSymbol),
        vs = as.numeric(sender.variableSymbol),
        ss = as.numeric(sender.specificSymbol),
        contra_account_name = as.character(sender.name),
        message_for_recipient = as.character(sender.description),
        entity_name = accounts_csas[[1]][i]
      )
  }

  yesterday_data <- bind_rows(full_list_clean) %>% distinct()

  # Only append the full dataset if there are valid records from yesterday
  if (!dim(yesterday_data)[1] == 0) {
    print(paste(dim(yesterday_data)[1], "recent transactions on some of the selected bank accounts - will append"))

    # Load in the existing full dataset merge with yesterday's new data
    all_data <- readRDS(paste0(dir_name, "/expense_accounts/csas_expense_merged_data.rds"))

    # Append the existing dataset with new rows from yesterday and delete duplicates
    all_data <- bind_rows(yesterday_data, all_data) %>% distinct()

    # Save full dataset again both in CSV and RDS
    saveRDS(object = all_data, file = paste0(dir_name, "/expense_accounts/csas_expense_merged_data.rds"), compress = FALSE)
    fwrite(x = all_data, file = paste0(dir_name, "/expense_accounts/csas_expense_merged_data.csv"))

    # Split dataset to individual accounts
    split_dataset <- split(all_data, all_data$entity_name)

    for (i in seq_len(length(split_dataset))) {
      fwrite(x = split_dataset[[i]], file = paste0(dir_name, "/expense_accounts/individual_expense_accounts/", names(split_dataset[i]), ".csv"))
    }
  } else if (dim(yesterday_data)[1] == 0) {
    print(paste("No recent transactions on any of the selected bank accounts - no need to append"))
  }
}

# 3. Running the function
scrape_csas_accounts(
  accounts_csas = readRDS("data/list_of_monitored_accounts.rds")[["csas"]],
  dir_name = "data",
  sub_dir_name = "csas",
  page_rows = 1000,
  start_date = as.character(Sys.Date() - 8),
  end_date = as.character(Sys.Date()),
  sort = "processingDate", # Which column is used for sorting? We keep this consistent.
  sort_direction = "DESC", # Which direction (DESC/ASC) for the sort. We keep this consistent.
  api_key = Sys.getenv("CS_TOKEN"),
  user_agent = Sys.getenv("USER_AGENT")
)

# Reset global httr configuration
# httr::reset_config()
