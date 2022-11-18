# EXTRACTION OF TRANSACTION DATA ON FIO'S TRANSPARENT BANK ACCOUNTS

# Function for the extraction  --------------------------------------------
get_fio_transactions <- function(accounts_fio, start_date, end_date, user_agent) {
  # How many bank accounts to be extracted?
  print(paste(length(accounts_fio), "bank account(s) selected, will run the function."))

  # Create list which will be appended
  transactions_list <- vector(mode = "list", length = length(accounts_fio)) %>% setNames(names(accounts_fio))

  for (i in seq_along(accounts_fio)) {
    
    chosen_user_agent <- sample(user_agent, 1)
    
    # Read the source page of the transparent bank account
    fio_tables <- read_html(paste0("https://ib.fio.cz/ib/transparent?a=", accounts_fio[i], "&f=", start_date, "&t=", end_date),
      encoding = "UTF-8",
      add_headers(
        "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
        "Accept-Encoding" = "gzip, deflate, br",
        "Accept-Language" = "en;q=0.9",
        "Cache-Control" = "no-cache",
        "Connection" = "keep-alive",
        "Host" = "ib.fio.cz",
        "Pragma" = "no-cache",
        "Sec-Fetch-Dest" = "document",
        "Sec-Fetch-Mode" = "navigate",
        "Sec-Fetch-Site" = "none",
        "Sec-Fetch-User" = "?1",
        "Upgrade-Insecure-Requests" = "1",
        "User-Agent" = chosen_user_agent
      ),
      user_agent(chosen_user_agent)
    ) %>%
      html_table(header = TRUE, dec = ",")

    if (length(fio_tables) == 1) {
      print(paste("No transactions on the account of entity", names(accounts_fio)[i], "between", start_date, "and", end_date, "- skipping to the next entity."))
      next
    }

    # Rename columns with English equivalents
    transactions_list[[names(accounts_fio)[i]]] <- fio_tables[[2]] %>%
      setNames(c(
        "date",
        "amount",
        "type",
        "contra_account_name",
        "message_for_recipient",
        "ks",
        "vs",
        "ss",
        "note"
      )) %>%
      transmute(
        date = as.Date(date, format = "%d.%m.%Y"),
        amount = str_replace_all(string = amount, pattern = "[,]", replacement = "."),
        amount = as.numeric(str_replace_all(string = amount, pattern = "(\\s+|[a-zA-Z])", replacement = "")),
        type = as.character(type),
        contra_account_name = as.character(contra_account_name),
        message_for_recipient = as.character(message_for_recipient),
        ks = as.numeric(ks),
        vs = as.numeric(vs),
        ss = as.numeric(ss),
        note = as.character(note),
        entity = names(accounts_fio)[i]
      ) %>%
      na_if("")

    print(paste(nrow(transactions_list[[names(accounts_fio)[i]]]), "transactions on the account of entity", names(accounts_fio)[i], "between", start_date, "and", end_date))

    Sys.sleep(runif(1, 0.1, 1))
  }

  # Bind list to a dataset
  bind_rows(transactions_list)
}
