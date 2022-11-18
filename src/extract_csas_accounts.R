# EXTRACTION OF TRANSACTION DATA ON CESKA SPORITELNA'S TRANSPARENT BANK ACCOUNTS

# Function for the extraction  --------------------------------------------
get_csas_transactions <- function(accounts_csas, dir_name, entity_name, page_rows, start_date, end_date, sort, sort_direction, api_key, user_agent) {
  # How many bank accounts to be extracted?
  print(paste(length(accounts_csas), "bank account(s) selected, will run the function."))

  # Create list which will be appended
  transactions_list <-
    vector(mode = "list", length = length(accounts_csas)) %>% setNames(names(accounts_csas))

  # Loop to deal with more than one accounts
  for (i in seq_along(accounts_csas)) {
    
    chosen_user_agent <- sample(user_agent, 1)
    
    transactions_list[[names(accounts_csas)[i]]] <-
      GET(
        paste0(
          "https://api.csas.cz/webapi/api/v3/transparentAccounts/",
          accounts_csas[i],
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
    if (!length(transactions_list[[names(accounts_csas)[i]]])) {
      print(paste("No transactions on the account of entity", names(accounts_csas)[i], "between", format(as.Date(start_date), "%d.%m.%Y"), "and", format(as.Date(end_date), "%d.%m.%Y")))

      # Replace with empty dataset so bind_row at the end is successful
      transactions_list[[names(accounts_csas)[i]]] <- data.frame()

      next
    }

    transactions_list[[names(accounts_csas)[i]]] <- transactions_list[[names(accounts_csas)[i]]] %>%
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
        entity_name = names(accounts_csas)[i]
      )

    print(paste(nrow(transactions_list[[names(accounts_csas)[i]]]), "transactions on the account of entity", names(accounts_csas)[i], "between", format(as.Date(start_date), "%d.%m.%Y"), "and", format(as.Date(end_date), "%d.%m.%Y")))

    Sys.sleep(runif(1, 0.1, 1))
  }

  # Bind list to a dataset
  bind_rows(transactions_list)
}
