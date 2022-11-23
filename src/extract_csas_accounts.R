# EXTRACTION OF TRANSACTION DATA ON CESKA SPORITELNA'S TRANSPARENT BANK ACCOUNTS

# Verify arguments for function inputs ------------------------------------
verify_csas_inputs <- function(dir_name, bank_name, bank_accounts, page_rows, start_date, end_date, api_key, user_agent, sort, sort_direction = c("DESC", "ASC")) {
  stopifnot(
    !is.null(bank_accounts) && length(bank_accounts) >= 1,
    is.character(dir_name),
    is.character(bank_name),
    is.numeric(page_rows) && page_rows > 0,
    is.character(start_date) && validate_date(start_date),
    is.character(end_date) && validate_date(end_date),
    is.character(api_key) && nchar(api_key) >= 1,
    is.character(user_agent) && length(user_agent) >= 1,
    is.character(sort) && nchar(sort) >= 1,
    is.character(match.arg(sort_direction))
  )

  print("All CSAS inputs should be OK.")
}

# Verify that Ceska Sporitelna API key valid --------------------------------
verify_csas_api_key <- function(api_key, user_agent) {
  test_request <-
    GET(
      "https://api.csas.cz/webapi/api/v3/transparentAccounts/",
      add_headers(
        "Accept" = "application/json, text/plain, */*",
        "Accept-Language" = "en;q=0.9",
        "Cache-Control" = "no-cache",
        "Connection" = "keep-alive",
        "Origin" = "https://www.csas.cz",
        "Pragma" = "no-cache",
        "Referer" = "https://www.google.com/",
        "Sec-Fetch-Dest" = "empty",
        "Sec-Fetch-Mode" = "cors",
        "Sec-Fetch-Site" = "same-site",
        "Sec-GPC" = "1",
        "User-Agent" = sample(user_agent, 1),
        "Web-Api-Key" = api_key
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

# Function for the extraction  --------------------------------------------
get_csas_transactions <- function(bank_accounts, dir_name, page_rows, start_date, end_date, sort, sort_direction = c("DESC", "ASC"), api_key, user_agent) {
  # How many bank accounts to be extracted?
  print(paste(length(bank_accounts), "bank account(s) selected, will run the function."))

  # Create list which will be appended
  transactions_list <-
    vector(mode = "list", length = length(bank_accounts)) %>% setNames(names(bank_accounts))

  # Loop to deal with more than one accounts
  for (i in seq_along(bank_accounts)) {
    chosen_user_agent <- sample(user_agent, 1)

    transactions_list[[names(bank_accounts)[i]]] <-
      GET(
        paste0(
          "https://api.csas.cz/webapi/api/v3/transparentAccounts/",
          bank_accounts[i],
          "/transactions"
        ),
        add_headers(
          "Accept" = "application/json, text/plain, */*",
          "Accept-Language" = "en;q=0.9",
          "Cache-Control" = "no-cache",
          "Connection" = "keep-alive",
          "Origin" = "https://www.csas.cz",
          "Pragma" = "no-cache",
          "Referer" = "https://www.google.com/",
          "Sec-Fetch-Dest" = "empty",
          "Sec-Fetch-Mode" = "cors",
          "Sec-Fetch-Site" = "same-site",
          "Sec-GPC" = "1",
          "User-Agent" = chosen_user_agent,
          "Web-Api-Key" = api_key
        ),
        user_agent(chosen_user_agent),
        query = list(
          order = match.arg(sort_direction),
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
    if (!length(transactions_list[[names(bank_accounts)[i]]])) {
      print(paste("No transactions on the account of entity", names(bank_accounts)[i], "between", format(as.Date(start_date), "%d.%m.%Y"), "and", format(as.Date(end_date), "%d.%m.%Y")))

      # Replace with empty dataset so bind_row at the end is successful
      transactions_list[[names(bank_accounts)[i]]] <- data.frame()

      next
    }

    transactions_list[[names(bank_accounts)[i]]] <- transactions_list[[names(bank_accounts)[i]]] %>%
      transmute(
        date = as.Date(processingDate),
        amount = as.numeric(amount.value),
        currency = as.character(amount.currency),
        type = as.character(typeDescription),
        ks = ifelse("sender.constantSymbol" %in% colnames(.), as.numeric(sender.constantSymbol), NA_real_),
        vs = ifelse("sender.variableSymbol" %in% colnames(.), as.numeric(sender.variableSymbol), NA_real_),
        ss = ifelse("sender.specificSymbol" %in% colnames(.), as.numeric(sender.specificSymbol), NA_real_),
        contra_account_name = as.character(sender.name),
        message_for_recipient = as.character(sender.description),
        entity = names(bank_accounts)[i]
      )

    print(paste(nrow(transactions_list[[names(bank_accounts)[i]]]), "transactions on the account of entity", names(bank_accounts)[i], "between", format(as.Date(start_date), "%d.%m.%Y"), "and", format(as.Date(end_date), "%d.%m.%Y")))

    Sys.sleep(runif(1, 0.1, 1))
  }

  # Bind list to a dataset
  bind_rows(transactions_list)
}
