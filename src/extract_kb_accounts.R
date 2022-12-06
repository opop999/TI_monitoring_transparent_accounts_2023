# EXTRACTION OF TRANSACTION DATA ON KOMERCNI BANKA'S TRANSPARENT BANK ACCOUNTS

# Get "Salt" - i.e. short token for full token generation -----------------
get_kb_salt <- function(user_agent, old_salt="KB_SALT") {
  chosen_user_agent <- sample(user_agent, 1)
  # Attempt to retrieve newest salt from script
  tryCatch(
    GET(
      "https://www.kb.cz/js/app.min.js?d=20190626",
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
      )
    ) %>%
      content(as = "text", encoding = "UTF-8") %>%
      str_extract(pattern = '(?<=n.salt=\")[a-zA-Z0-9-]+'),
    # On error, attempt to retrive old salt token possibly set as env variable.
    error = function(err) {
      print("ERROR: HTTP request failed. Attempting to retrieve salt from environmental variable")
      Sys.getenv(old_salt)
    }
  )
}

# Verify arguments for function inputs ------------------------------------
verify_kb_inputs <- function(dir_name, bank_name, start_date, end_date, bank_accounts, skip_param, salt_kb, user_agent) {
  stopifnot(
    !is.null(bank_accounts) && length(bank_accounts) >= 1,
    is.character(dir_name),
    is.character(bank_name),
    is.character(start_date) && validate_date(start_date),
    is.character(end_date) && validate_date(end_date),
    is.numeric(skip_param) && !(skip_param %% 50),
    is.character(salt_kb) && nchar(salt_kb) >= 30,
    is.character(user_agent) && length(user_agent) >= 1
  )
  
  print("All KB inputs should be OK.")
}

# Function for the extraction  --------------------------------------------
get_kb_transactions <- function(bank_accounts, dir_name, start_date, end_date, skip_param, salt_kb, user_agent) {
  # How many bank accounts to be extracted?
  print(paste(length(bank_accounts), "bank account(s) selected, will run the function."))

  # Create list which will be appended
  transactions_list <-
    vector(mode = "list", length = length(bank_accounts)) %>% setNames(names(bank_accounts))

  # Outer loop to deal with more than one accounts
  for (o in seq_along(bank_accounts)) {
    chosen_user_agent <- sample(user_agent, 1)

    temp_token <-
      GET(
        url = paste0("https://www.kb.cz/cs/transparentni-ucty/", bank_accounts[[o]]),
        user_agent(chosen_user_agent),
        add_headers(
          "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
          "Accept-Encoding" = "gzip, deflate, br",
          "Accept-Language" = "en;q=0.6",
          "Cache-Control" = "no-cache",
          "Connection" = "keep-alive",
          "Host" = "www.kb.cz",
          "Pragma" = "no-cache",
          "Referer" = "https://www.kb.cz/cs/transparentni-ucty/",
          "Sec-Fetch-Dest" = "document",
          "Sec-Fetch-Mode" = "navigate",
          "Sec-Fetch-Site" = "same-origin",
          "Sec-Fetch-User" = "?1",
          "Upgrade-Insecure-Requests" = "1",
          "User-Agent" = chosen_user_agent
        )
      ) %>%
      content("text") %>%
      str_extract(pattern = "(?<=token: ').+(?=')")

    hashed_token <- digest(paste0(salt_kb, temp_token), serialize = FALSE, algo = "sha256")


    # Inner loop, which loops over multiple pages of the account
    for (i in seq.int(from = 0, to = skip_param, by = 50)) {
      # Get response
      response <-
        GET(
          paste0(
            "https://www.kb.cz/transparentsapi/transactions/",
            str_remove_all(bank_accounts[[o]], "-"),
            "?skip=",
            i,
            "&token=",
            hashed_token
          ),
          user_agent(chosen_user_agent),
          add_headers(
            "Accept" = "*/*",
            "Accept-Encoding" = "gzip, deflate, br",
            "Accept-Language" = "en;q=0.6",
            "Cache-Control" = "no-cache",
            "Connection" = "keep-alive",
            "Host" = "www.kb.cz",
            "Pragma" = "no-cache",
            "Referer" = paste0("https://www.kb.cz/cs/transparentni-ucty/", bank_accounts[[o]]),
            "Sec-Fetch-Dest" = "empty",
            "Sec-Fetch-Mode" = "cors",
            "Sec-Fetch-Site" = "same-origin",
            "Upgrade-Insecure-Requests" = "1",
            "User-Agent" = chosen_user_agent
          )
        ) %>%
        content("text") %>%
        fromJSON()

      # Save dataframe of transactions to the list
      transactions_list[[names(bank_accounts)[o]]][[as.character(i)]] <- response$items %>%
        separate(symbols, into = c("vs", "ks", "ss"), sep = "/") %>%
        mutate(
          type = case_when(
            str_detect(notes, "Příchozí platba") ~ "Příchozí platba",
            str_detect(notes, "Poplatek") ~ "Poplatek",
            str_detect(notes, "Odchozí platba") ~ "Odchozí platba",
            str_detect(notes, "Příchozí SEPA platba") ~ "Příchozí SEPA platba",
            str_detect(notes, "Trvalý příkaz") ~ "Trvalý příkaz"
          ),
          notes = str_squish(str_replace_all(str_remove_all(notes, "Příchozí platba|Poplatek|Odchozí platba|Příchozí SEPA platba|Trvalý příkaz"), "<br /><br />|<br /> <br />", "<br />"))
        ) %>%
        separate(notes, into = c("contra_account_name", "message_for_recipient"), sep = "<br />", fill = "right") %>%
        transmute(
          id = as.character(id),
          date = as.Date(date, format = "%d.&nbsp;%m.&nbsp;%Y"),
          amount = str_replace_all(string = amount, pattern = "[,]", replacement = "."),
          type = as.character(type),
          contra_account_name = as.character(contra_account_name),
          message_for_recipient = as.character(message_for_recipient),
          currency = str_extract(string = amount, pattern = "[A-Z]{3}$"),
          amount = as.numeric(str_replace_all(string = amount, pattern = "(\\s+|[a-zA-Z])", replacement = "")),
          vs = as.numeric(vs),
          ks = as.numeric(ks),
          ss = as.numeric(ss),
          entity = names(bank_accounts)[o]
        ) %>%
        na_if("")

      if (!response$loadMore) {
        print(paste("Reached premature end of transactions for", names(bank_accounts)[o], "- skipping to next entity."))
        break
      }

      # Create new hashed token for next API call
      hashed_token <- digest(paste0(salt_kb, response$token), serialize = FALSE, algo = "sha256")

      Sys.sleep(runif(1, 0.1, 1))
    }

    transactions_list[[names(bank_accounts)[o]]] <- bind_rows(transactions_list[[names(bank_accounts)[o]]])

    print(paste(nrow(transactions_list[[names(bank_accounts)[o]]]), "transactions extracted for", names(bank_accounts)[o], "."))
  }

  bind_rows(transactions_list) %>% 
    filter(date >= start_date & date <= end_date)
}
