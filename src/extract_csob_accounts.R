# EXTRACTION OF TRANSACTION DATA ON CSOB'S TRANSPARENT BANK ACCOUNTS

# Verify arguments for function inputs ------------------------------------
verify_csob_inputs <- function(dir_name, bank_name, bank_accounts, page_rows, start_date, end_date, temporary_cookie_csob, user_agent, sort = "AccountingDate", sort_direction = c("DESC", "ASC")) {
  stopifnot(
    !is.null(bank_accounts) && length(bank_accounts) >= 1,
    is.character(dir_name),
    is.character(bank_name),
    is.numeric(page_rows) && page_rows > 0,
    is.character(start_date) && validate_date(start_date),
    is.character(end_date) && validate_date(end_date),
    is.character(temporary_cookie_csob) && nchar(temporary_cookie_csob) >= 10,
    is.character(user_agent) && length(user_agent) >= 1,
    is.character(match.arg(sort)),
    is.character(match.arg(sort_direction))
  )
  
  print("All CSOB inputs should be OK.")
}

# Function for the extraction  --------------------------------------------
get_csob_transactions <- function(bank_accounts, dir_name, page_rows, start_date, end_date, sort = "AccountingDate", sort_direction = c("DESC", "ASC"), user_agent, temporary_cookie_csob) {
  # How many bank accounts to be extracted?
  print(paste(length(bank_accounts), "bank account(s) selected, will run the function."))

  # Create list which will be appended
  transactions_list <-
    vector(mode = "list", length = length(bank_accounts)) %>% setNames(names(bank_accounts))

  # Loop to deal with more than one accounts
  for (i in seq_along(bank_accounts)) {
    chosen_user_agent <- sample(user_agent, 1)

    transactions_list[[names(bank_accounts)[i]]] <- POST(
      "https://www.csob.cz/et-npw-lta-view/api/detail/transactionList",
      body = sprintf("{
  \"accountList\": [
    {
      \"accountNumberM24\": %s
    }
  ],
  \"filterList\": [
    {
      \"name\": \"AccountingDate\",
      \"operator\": \"ge\",
      \"valueList\": [
        \"%s\"
      ]
    },
    {
      \"name\": \"AccountingDate\",
      \"operator\": \"le\",
      \"valueList\": [
        \"%s\"
      ]
    }
  ],
  \"sortList\": [
    {
      \"name\": \"%s\",
      \"direction\": \"%s\",
      \"order\": 1
    }
  ],
  \"paging\": {
    \"rowsPerPage\": %s,
    \"pageNumber\": 1
  }
}", bank_accounts[i], start_date, end_date, match.arg(sort), match.arg(sort_direction), page_rows),
      add_headers(
        "Accept" = "application/json, text/plain, */*",
        "Content-Type" = "application/json",
        "Cookie" = paste0("TSPD_101=", temporary_cookie_csob),
        "Origin" = "https://www.csob.cz",
        "Host" = "www.csob.cz",
        "Referer" = "https://www.csob.cz/portal/firmy/bezne-ucty/transparentni-ucty/",
        "User-Agent" = chosen_user_agent
      ),
      user_agent(chosen_user_agent), encode = "json"
    ) %>%
      content(as = "text") %>%
      fromJSON(flatten = TRUE) %>% 
      retry_call() # Repeat call several times, if API returned error

    # Extract the number of new records
    nr_new_records <- transactions_list[[names(bank_accounts)[i]]][["paging"]][["recordCount"]]

    # Skip to next loop iteration if there are no returned transactions
    if (nr_new_records < 1) {
      print(paste("No transactions on the account of entity", names(bank_accounts)[i], "between", format(as.Date(start_date), "%d.%m.%Y"), "and", format(as.Date(end_date), "%d.%m.%Y")))

      # Replace with empty dataset so bind_row at the end is successful
      transactions_list[[names(bank_accounts)[i]]] <- data.frame()

      next
    }

    transactions_list[[names(bank_accounts)[i]]] <- transactions_list[[names(bank_accounts)[i]]][["accountedTransaction"]] %>%
      unite(
        col = message_for_recipient,
        c(
          "transactionTypeChoice.domesticPayment.message.message1",
          "transactionTypeChoice.domesticPayment.message.message2",
          "transactionTypeChoice.domesticPayment.message.message3",
          "transactionTypeChoice.domesticPayment.message.message4"
        ),
        sep = "",
        na.rm = TRUE
      ) %>%
      transmute(
        date = as.Date(
          as.POSIXct(baseInfo.accountingDate / 1000, origin = "1970-01-01"),
          tz = "CET"
        ),
        amount = as.numeric(baseInfo.accountAmountData.amount),
        type = as.character(baseInfo.transactionDescription),
        contra_account_name = as.character(transactionTypeChoice.domesticPayment.partyName),
        contra_account_number = as.numeric(
          transactionTypeChoice.domesticPayment.partyAccount.domesticAccount.accountNumber
        ),
        contra_account_bank_code = as.numeric(
          transactionTypeChoice.domesticPayment.partyAccount.domesticAccount.bankCode
        ),
        message_for_recipient = as.character(message_for_recipient),
        ks = as.numeric(
          transactionTypeChoice.domesticPayment.symbols.constantSymbol
        ),
        vs = as.numeric(
          transactionTypeChoice.domesticPayment.symbols.variableSymbol
        ),
        ss = as.numeric(
          transactionTypeChoice.domesticPayment.symbols.specificSymbol
        ),
        currency = as.character(baseInfo.accountAmountData.currencyCode),
        entity = names(bank_accounts)[i]
      ) %>%
      na_if("")

    print(paste(nrow(transactions_list[[names(bank_accounts)[i]]]), "transactions on the account of entity", names(bank_accounts)[i], "between", format(as.Date(start_date), "%d.%m.%Y"), "and", format(as.Date(end_date), "%d.%m.%Y")))

    Sys.sleep(runif(1, 5, 10))
  }

  # Bind list to a dataset
  bind_rows(transactions_list)
}
