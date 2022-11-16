# We can add and remove the monitored bank accounts pages here and then
# we save the list to a rds file, which is read to the main extraction script.

main_dir <- "data"

if (!dir.exists(main_dir)) {
  dir.create(main_dir)
} else {
  print("Output directory already exists")
}


# Initiate an empty list, serving as a list of all of the lists
all_accounts_list <- vector(mode = "list", length = 4L)
all_accounts_list <- setNames(all_accounts_list, c("fio", "kb", "csob", "csas"))

# 1. This list is for FIO accounts ---------------------------------------------

all_accounts_list[["fio"]] <- list(
  names = c(
    "petr_pavel",
    "josef_brezina"
  ),
  numbers = c(
    "2902252345",
    "2802135053"
  )
)


# # 2. This list is for accounts extracted through Komercni Banka API -----
# all_accounts_list[["kb"]] <- list(
#   names = c(
#     "ano_2011"
#   ),
#   numbers = c(
#     "4090453" # This is formatting for KB API request
#   ),
#   urls = c(
#     "4090453" # This is formatting for direct KB URL
#   )
# )
#
#
#
#
# # 3. This list is for accounts extracted through CSOB API --------------------------
#
# all_accounts_list[["csob"]] <- list(
#   names = c(
#     "kscm"
#   ),
#   numbers = c(
#     "217343303"
#   )
# )
#
#
#

# 4.This list is for accounts extracted through Ceska Sporitelna (CS) API ------

all_accounts_list[["csas"]] <- list(
  names = c(
    "ivo_mares",
    "danuse_nerudova",
    "alena_vitaskova",
    "tomas_zima"
  ),
  numbers = c(
    "000000-4672082043",
    "000000-4776908073",
    "000000-4639079053",
    "000000-4758343013"
  )
)



# 5. Save the list of lists for all of the accounts ----------------------------
saveRDS(all_accounts_list, paste0(main_dir, "/list_of_monitored_accounts.rds"))
