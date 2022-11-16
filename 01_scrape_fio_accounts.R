# SCRAPING OF FIO BANK ACCOUNTS

## 1. Loading the required R libraries

# Package names
packages <- c("rvest", "dplyr", "data.table", "stringr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Turn off scientific notation of numbers
options(scipen = 999)

## 2. Function for scraping of the transparent bank accounts based in FIO bank

scrape_fio_accounts <- function(accounts_fio, dir_name, start_date, end_date, user_agent) {
  # Verify function's inputs
  stopifnot(
    !is.null(accounts_fio$numbers) & length(accounts_fio$numbers) >= 1,
    is.character(dir_name),
    is.character(start_date) & nchar(start_date) == 10,
    is.character(end_date) & nchar(end_date) == 10,
    is.character(user_agent) & nchar(user_agent) >= 1
  )

  # How many bank accounts to be extracted?
  print(paste(length(accounts_fio$numbers), "bank account(s) selected, will run the function."))

  # Create list which will be appended
  transactions_list <- vector(mode = "list", length = length(accounts_fio$numbers)) %>% setNames(accounts_fio$names)

  # We have to create a desired directory, if one does not yet exist
  if (!dir.exists(paste0(dir_name, "/fio"))) {
    dir.create(paste0(dir_name, "/fio"))
  } else {
    print("Output directory already exists")
  }

  # Repeat the check for subdirectory for individual account datasets
  if (!dir.exists(paste0(dir_name, "/fio/individual_accounts"))) {
    dir.create(paste0(dir_name, "/fio/individual_accounts"))
  } else {
    print("Output subdirectory already exists")
  }

  for (i in seq_along(accounts_fio[["numbers"]])) {
    # Read the source page of the transparent bank account
    fio_tables <- read_html(paste0("https://ib.fio.cz/ib/transparent?a=", accounts_fio[["numbers"]][i], "&f=", start_date, "&t=", end_date), user_agent = user_agent) %>%
      html_table(header = TRUE, dec = ",")

    if (length(fio_tables) == 1) {
      print(paste("No transactions on the account of entity", accounts_fio[["names"]][i], "between", start_date, "and", end_date, "- skipping to the next entity."))
      next
    } 
    
      # Rename columns with English equivalents
      transactions_list[[accounts_fio[["names"]][i]]] <- fio_tables[[2]] %>%
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
          entity_name = accounts_fio[["names"]][i]
        ) %>%
        na_if("")

      print(paste(nrow(transactions_list[[accounts_fio[["names"]][i]]]), "transactions on the account of entity", accounts_fio[["names"]][i], "between", start_date, "and", end_date))
    
  }

  # Bind list to a dataset
  transactions_df <- bind_rows(transactions_list)

  # Only append the full dataset if there are valid records from yesterday
  if (dim(transactions_df)[1] == 0) {
    stop(paste("No transactions on any of the selected bank accounts between", start_date, "and", end_date, "no need to append"))
  }

  # How many transactions were gathered?
  print(paste(dim(transactions_df)[1], "transactions on some of the selected bank accounts between", start_date, "and", end_date, "- will append."))

  # Load in the existing full dataset and merge with yesterday's new data
  if (file.exists(paste0(dir_name, "/fio/fio_merged_data.rds"))) {
    print("Older transactions dataframe already exists - it will be appended with new transactions.")

    transactions_df <- transactions_df %>%
      bind_rows(readRDS(paste0(dir_name, "/fio/fio_merged_data.rds"))) %>% # Append the existing dataset with new rows from yesterday and delete duplicates
      distinct()
  }

  # Save full dataset again both in CSV and RDS
  saveRDS(object = transactions_df, file = paste0(dir_name, "/fio/fio_merged_data.rds"))
  fwrite(x = transactions_df, file = paste0(dir_name, "/fio/fio_merged_data.csv"))

  # Split dataset to individual accounts
  split_transactions_df <- split(transactions_df, transactions_df$entity_name)


  # Write the dataset to chunks
  invisible(lapply(names(split_transactions_df), function(df) {
    fwrite(split_transactions_df[[df]], file = paste0(dir_name, "/fio/individual_accounts/", names(split_transactions_df[df]), ".csv"))
  }))
}


## 3. Running the function with inputs

scrape_fio_accounts(
  accounts_fio = readRDS("data/list_of_monitored_accounts.rds")[["fio"]],
  dir_name = "data",
  start_date = format(Sys.Date() - 8, "%d.%m.%Y"),
  end_date = format(Sys.Date(), "%d.%m.%Y"),
  user_agent = Sys.getenv("USER_AGENT")
)
