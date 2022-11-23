# EXTRACTION OF TRANSACTION DATA ON RAIFFEISENBANK'S TRANSPARENT BANK ACCOUNTS

# Verify arguments for function inputs ------------------------------------
verify_rb_inputs <- function(dir_name, bank_name, bank_accounts, page_rows, start_date, end_date, user_agent) {
  stopifnot(
    !is.null(bank_accounts) && length(bank_accounts) >= 1,
    is.character(dir_name),
    is.character(bank_name),
    is.numeric(page_rows) && page_rows > 0,
    is.character(start_date) && validate_date(start_date),
    is.character(end_date) && validate_date(end_date),
    is.character(user_agent) && length(user_agent) >= 1
  )
  
  print("All RB inputs should be OK.")
}

# Function for the extraction  --------------------------------------------
get_rb_transactions <- function(bank_accounts, dir_name, page_rows, start_date, end_date, user_agent) {
  # How many bank accounts to be extracted?
  print(paste(length(bank_accounts), "bank account(s) selected, will run the function."))

  # Create list which will be appended
  transactions_list <-
    vector(mode = "list", length = length(bank_accounts)) %>% setNames(names(bank_accounts))

  # Loop to deal with more than one accounts
  for (i in seq_along(bank_accounts)) {
    chosen_user_agent <- sample(user_agent, 1)

    transactions_list[[names(bank_accounts)[i]]] <-
      GET("https://www.rb.cz/frontend-controller/backend-data/transparent-account/transactionsByBankAccount",
        add_headers(
          "Accept" = "application/json, text/plain, */*",
          "Accept-Encoding" = "gzip, deflate, br",
          "Accept-Language" = "en;q=0.7",
          "Cache-Control" = "no-cache",
          "Connection" = "keep-alive",
          "Host" = "www.rb.cz",
          "Pragma" = "no-cache",
          "Referer" = paste0("https://www.rb.cz/povinne-zverejnovane-informace/transparentni-ucty?path=transactions&accountNumber=", bank_accounts[[i]]),
          "Sec-Fetch-Dest" = "empty",
          "Sec-Fetch-Mode" = "cors",
          "Sec-Fetch-Site" = "same-origin",
          "Sec-GPC" = "1",
          "User-Agent" = chosen_user_agent
        ),
        user_agent(chosen_user_agent),
        query = list(
          idBankAccount = bank_accounts[[i]],
          fromIndex = "0",
          lang = "cs",
          maxResults = page_rows,
          dateFrom = start_date,
          dateTo = end_date
        )
      ) %>%
      content(as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE) %>%
      .[["resultList"]]

    # Skip to next loop iteration if there are no returned transactions
    if (!length(transactions_list[[names(bank_accounts)[i]]])) {
      print(paste("No transactions on the account of entity", names(bank_accounts)[i], "between", format(as.Date(start_date), "%d.%m.%Y"), "and", format(as.Date(end_date), "%d.%m.%Y")))

      # Replace with empty dataset so bind_row at the end is successful
      transactions_list[[names(bank_accounts)[i]]] <- data.frame()

      next
    }

    transactions_list[[names(bank_accounts)[i]]] <- transactions_list[[names(bank_accounts)[i]]] %>%
      separate(
        col = accountNumber,
        into = c("contra_account_number", "contra_account_bank_code"),
        sep = "\\/"
      ) %>%
      transmute(
        entity = names(bank_accounts)[i],
        date = as.Date(datum),
        type = as.character(typ),
        amount = as.numeric(amount),
        currency = "CZK",
        note = as.character(info),
        contra_account_name = as.character(accountName),
        contra_account_number = as.numeric(contra_account_number),
        contra_account_bank_code = as.numeric(contra_account_bank_code),
        vs = as.numeric(varSymbol),
        ks = as.numeric(constSymbol),
        ss = as.numeric(specSymbol),
        id = as.character(idTransaction)
      ) %>%
      na_if("")

    print(paste(nrow(transactions_list[[names(bank_accounts)[i]]]), "transactions on the account of entity", names(bank_accounts)[i], "between", format(as.Date(start_date), "%d.%m.%Y"), "and", format(as.Date(end_date), "%d.%m.%Y")))

    Sys.sleep(runif(1, 0.1, 1))
  }

  # Bind list to a dataset
  bind_rows(transactions_list)
}
