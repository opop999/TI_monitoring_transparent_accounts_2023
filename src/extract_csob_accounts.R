# EXTRACTION OF TRANSACTION DATA ON CSOB'S TRANSPARENT BANK ACCOUNTS

# Function for extraction of the expense transparent bank accounts based in CSOB bank
get_csob_transactions <- function(accounts_csob, dir_name, entity_name, page_rows, start_date, end_date, sort, sort_direction, user_agent, temporary_cookie_csob) {
  # How many bank accounts to be extracted?
  print(paste(length(accounts_csob), "bank account(s) selected, will run the function."))

  # Create list which will be appended
  transactions_list <-
    vector(mode = "list", length = length(accounts_csob)) %>% setNames(names(accounts_csob))

  # Loop to deal with more than one accounts
  for (i in seq_along(accounts_csob)) {
    chosen_user_agent <- sample(user_agent, 1)

    transactions_list[[names(accounts_csob)[i]]] <- POST(
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
}", accounts_csob[i], start_date, end_date, sort, sort_direction, page_rows),
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
      fromJSON(flatten = TRUE)

    # Extract the number of new records
    nr_new_records <- transactions_list[[names(accounts_csob)[i]]][["paging"]][["recordCount"]]

    # Skip to next loop iteration if there are no returned transactions
    if (nr_new_records < 1) {
      print(paste("No transactions on the account of entity", names(accounts_csob)[i], "between", format(as.Date(start_date), "%d.%m.%Y"), "and", format(as.Date(end_date), "%d.%m.%Y")))

      # Replace with empty dataset so bind_row at the end is successful
      transactions_list[[names(accounts_csob)[i]]] <- data.frame()

      next
    }

    transactions_list[[names(accounts_csob)[i]]] <- transactions_list[[names(accounts_csob)[i]]][["accountedTransaction"]] %>%
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
        contra_account_bankCode = as.numeric(
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
        entity_name = names(accounts_csob)[i]
      ) %>%
      na_if("")

    print(paste(nrow(transactions_list[[names(accounts_csob)[i]]]), "transactions on the account of entity", names(accounts_csob)[i], "between", format(as.Date(start_date), "%d.%m.%Y"), "and", format(as.Date(end_date), "%d.%m.%Y")))

    Sys.sleep(runif(1, 5, 10))
  }

  # Bind list to a dataset
  bind_rows(transactions_list)
}
