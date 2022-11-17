# Extraction of transaction data on FIO's transparent bank accounts

# Source helper functions
source("00_helper_functions.R")

# Set constant variables
dir_name <- "data"
entity_name <- "fio"
accounts_fio <- readRDS(file.path(dir_name, "list_of_monitored_accounts.rds"))[[entity_name]]
start_date <- format(Sys.Date() - 8, "%d.%m.%Y")
end_date <- format(Sys.Date(), "%d.%m.%Y")
user_agent <- Sys.getenv("USER_AGENT")

## Function for the extraction of the transparent bank accounts in FIO bank
get_fio_transactions <- function(accounts_fio, start_date, end_date, user_agent) {

  # How many bank accounts to be extracted?
  print(paste(length(accounts_fio), "bank account(s) selected, will run the function."))

  # Create list which will be appended
  transactions_list <- vector(mode = "list", length = length(accounts_fio)) %>% setNames(names(accounts_fio))

  for (i in seq_along(accounts_fio)) {
    # Read the source page of the transparent bank account
    fio_tables <- read_html(paste0("https://ib.fio.cz/ib/transparent?a=", accounts_fio[i], "&f=", start_date, "&t=", end_date), user_agent = user_agent) %>%
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
  }

  # Bind list to a dataset
  bind_rows(transactions_list)
}



# Running the function with inputs ----------------------------------------

# Verify inputs specified above
verify_fio_inputs(dir_name, entity_name, accounts_fio, start_date, end_date, user_agent)

# Prepare output directories
prepare_output_directories(dir_name, entity_name)

# Get the most recent transactions
transactions_df <- get_fio_transactions(accounts_fio,
                                        start_date,
                                        end_date,
                                        user_agent)


transactions_df_appended <- append_new_data(transactions_df, dir_name, entity_name, start_date, end_date)
  
save_merged_and_individual(transactions_df_appended, dir_name, entity_name)
