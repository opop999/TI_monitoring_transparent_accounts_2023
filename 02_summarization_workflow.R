# 0. Source helper functions ----------------------------------------------
source("src/helper_functions.R")

dir_name <- "data"

bank_name <- "summary_tables"

combined_dataset <- load_combined_dataset(dir_name)

prepare_output_directories(dir_name, bank_name, individual_subfolders = FALSE)



# 2. Creating a summary table of a total spending since 1.1.2021
total_spend_summary <- combined_dataset %>%
  filter(amount < 0) %>%
  transmute(date,
            spend_million = round(abs(amount) / 1000000, digits = 3),
            entity = as.factor(entity),
            entity_id = as.numeric(entity)
  ) %>%
  group_by(entity, entity_id) %>%
  summarise(total_spend_million = sum(spend_million)) %>%
  arrange(desc(total_spend_million)) %>%
  ungroup()

# 3. Creating a summary table with cumulative spending per page throughout time
time_summary <- combined_dataset %>%
  filter(amount < 0) %>%
  transmute(date,
            spend_million = abs(amount) / 1000000,
            entity = as.factor(entity),
            entity_id = as.numeric(entity)
  ) %>%
  arrange(date) %>%
  group_by(entity) %>%
  mutate(cumulative_spend_million = cumsum(spend_million)) %>%
  ungroup()




