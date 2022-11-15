# To improve readability of the main script and its length, this script is made
# to be modified. We can add and remove the monitored bank accounts pages and then
# we save the list to a rds file, which is read to the main extraction script.

main_dir <- "data"

if (!dir.exists(main_dir)) {
  dir.create(main_dir)
} else {
  print("Output directory already exists")
}


# Initiate an empty list, serving as a list of all of the lists
all_accounts_list <- vector(mode = "list", length = 4L) 
all_accounts_list <- setNames(all_accounts_list, c("fio", "kb", "csob", "cs"))

# 1.1. This list is for FIO accounts --------------------------

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


# 
# # 3.1 This list is for accounts extracted through KB API -----
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
# # 4.1 This list is for accounts extracted through CSOB API --------------------------
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
# # 5.1 This list is for accounts extracted through Ceska Sporitelna (CS) API --------------------------
# 
# all_accounts_list[["cs"]] <- list(
#   names = c(),
#   numbers = c()
# )



# Save the list of lists for all of the accounts --------------------------
saveRDS(all_accounts_list, paste0(main_dir, "/list_of_monitored_accounts.rds")) 
