# 0. Source helper functions and define constants -------------------------
source("src/helper_functions.R")
dir_name <- "output"

# 1. Specify bank accounts to be extracted --------------------------------
initialize_bank_accounts(
  dir_name = dir_name,
  csas = c(
    "danuse_nerudova" = "000000-4776908073",
    "tomas_zima" = "000000-4758343013",
    "pavel_fischer" = "000000-4805949043"
  ),
  csob = c(
    "libor_michalek" = "309564349",
    "karel_divis" = "310031267"
  ),
  fio = c(
    "petr_pavel" = "2902252345",
    "tomas_brezina" = "2902245970",
    "marek_hilser" = "20182024",
    "josef_stredula" = "2602250487",
    "jaroslav_basta" = "2702310803"
  ),
  kb = c(
    "karel_janecek" = "721721721",
    "andrej_babis" = "123-8794230217",
    "denisa_rohanova" = "123-7843850267"
  ),
  rb = c("jiri_kotab" = "28334472") # ID of account different from account nr.
)


# 2. Specify set of user agents ----------------------------------------------
initialize_user_agents(
  dir_name = dir_name,
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36 Edg/107.0.1418.42",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/106.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:106.0) Gecko/20100101 Firefox/106.0",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/106.0.0.0 Safari/537.36",
  "Mozilla/5.0 (X11; Linux x86_64; rv:106.0) Gecko/20100101 Firefox/106.0",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:105.0) Gecko/20100101 Firefox/105.0",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:106.0) Gecko/20100101 Firefox/106.0",
  "Mozilla/5.0 (Windows NT 10.0; rv:106.0) Gecko/20100101 Firefox/106.0"
)