#### 0. Dependencies ####

library(tidyverse)
library(haven)
library(here)
library(magrittr)
set.seed(89)

rm(list = ls())

source("functions/conjoint_functions.R")
eval(parse("functions/wtp_functions.R", encoding="UTF-8"))
eval(parse("functions/IPUMS_functions.R", encoding="UTF-8"))
eval(parse("functions/labels_functions.R", encoding="UTF-8"))
eval(parse("functions/weights_functions.R", encoding="UTF-8"))


#### 1. Cleaning and Combining ####

data_country <- list.files(path = here("data", "raw"), full.names = TRUE) %>%
  str_subset("RUS.csv", negate = T) %>% 
  map(~ read_csv(.))


names(data_country) <- list.files(path = here("data", "raw")) %>%
  str_remove(".csv$") |>
  str_subset("data_RUS", negate = T) 

list_vars <- list(~age, ~education, ~gender, ~REGION_0)

# Weights
for (i in names(data_country) %>% str_remove("data_")) {
  data_country[[paste0("data_", i)]] <-
    fn.merge(
      data_country[[paste0("data_", i)]],
      list_vars,
      Quotas_list_df %>%
        extract(str_subset(names(Quotas_list_df), paste0("^", i)))
    ) |>
    mutate(id = as.numeric(id))
}


country_data <- list()
country_codes <- c("AUS", "BR", "CAN", "CHL", "CHN", "COL", "FR", "IND",
                   "IT", "RUS", "SP", "UGA", "UK", "US")

for (country in country_codes) {
  country_data[[country]] <- read_csv(paste0("data/raw/data_", country, ".csv")) |>
    mutate(id = as.numeric(id))
}

for (country in country_codes |> str_subset("RUS", negate = T)) {
  country_data[[country]] <- country_data[[country]] %>%
    left_join(data_country[[paste0("data_", country)]], by = c("country", "id"))
}


# Merging weights

country_weights <- list()

for (country in c("AUS", "BR", "CHL", "CHN", "COL", "FR", "IT", "UK", "US")) {
  country_weights[[country]] <- read_csv(paste0("weights/", country, "_w.csv"))[,-1] |>
    mutate(id = as.numeric(id))
  country_data[[country]] <- left_join(country_data[[country]], country_weights[[country]], by = "id")
}

# Recode for reasons to get or not get the vaccine
for (country in c("BR", "FR")) {
  country_data[[country]]$Q14.2 <- gsub(", \t", ",", country_data[[country]]$Q14.2)
}

for (country in country_codes) {
  country_data[[country]]$Q14.2 <- gsub(", ", "|", country_data[[country]]$Q14.2)
  country_data[[country]] <- country_data[[country]] %>% 
    separate(Q14.2, into = c(paste0("int_reason_notget_", seq(1:7))), "\\,")
  for (i in 1:7) {
    country_data[[country]][[paste0("int_reason_notget_", i)]] <- gsub("\\|", ", ", country_data[[country]][[paste0("int_reason_notget_", i)]])
    country_data[[country]][[paste0("int_reason_notget_", i)]] <- gsub(" $", "", country_data[[country]][[paste0("int_reason_notget_", i)]])
  }
}
  
for (country in country_codes) {
  country_data[[country]]$Q14.3 <- gsub(", ", "|", country_data[[country]]$Q14.3)
  country_data[[country]] <- country_data[[country]] %>% 
    separate(Q14.3, into = c(paste0("int_reason_get_", seq(1:12))), "\\,")
  for (i in 1:12) {
    country_data[[country]][[paste0("int_reason_get_", i)]] <- gsub("\\|", ", ", country_data[[country]][[paste0("int_reason_get_", i)]])
    country_data[[country]][[paste0("int_reason_get_", i)]] <- gsub(" $", "", country_data[[country]][[paste0("int_reason_get_", i)]])
  }
}

# Recode for comorbilities

country_data$RUS <- country_data$RUS %>%
  rename(Q19.2 = Q19.1)

for (country in country_codes) {
  country_data[[country]] <- country_data[[country]] %>% 
    separate(Q19.2, into = c(paste0("qol_condition_", seq(1:10))), "\\,")
  for (i in 1:10) {
    country_data[[country]][[paste0("qol_condition_", i)]] <- gsub(" $", "", country_data[[country]][[paste0("qol_condition_", i)]])
  }
}

# Writing separate data #
for (country in country_codes) {
  write_csv(country_data[[country]], paste0("data/country/data_", country, ".csv"))
}

# Creating homogenized demographics variables

for (country in country_codes) {
  country_data[[country]] <- IPUMS_contract(country_data[[country]])
  country_data[[country]] <- IPUMS_employment(country_data[[country]])
  country_data[[country]] <- IPUMS_education(country_data[[country]])
  country_data[[country]] <- IPUMS_work(country_data[[country]])
  country_data[[country]] <- INCOME(country_data[[country]])
}

# Removing redundant questions already recoded

for (country in country_codes) {
  country_data[[country]] <- country_data[[country]] %>%
    select(-c(matches("^[Q]", ignore.case=FALSE)))
}

for (country in country_codes) {
  country_data[[country]]$eq5d_scale_pre <- country_data[[country]]$eq5d_scale_pre %>%
    as.numeric()
}

country_data$RUS$donation_amount <- as.double(country_data$RUS$donation_amount)

# Merging data 
data <- tibble()

for (country in country_data) {
  data <- bind_rows(data, country)
}

# Recode data for wtp format
wtp_data <- recode_for_wtp(data)

# Labeling
data <- labeling(data)

# Removing conjoint and WTP variables
data <- data %>%
  select(-c(starts_with(c("person", "wtp_amount")), 
            ends_with(c("Q5.4_1", "Q5.5_1", "Q5.6_1")), "wtp_access", 
            "wtp_private", "wtpVal", "taxesExtra", "ticketExtra"))

#### 2. Writing data ####

# Save CSV for reference
write_csv(wtp_data, "data/clean_wtp_global.csv")
write_csv(data, "data/data_combined.csv")

# Save RDS to preserve factor coding
write_rds(wtp_data, "data/clean_wtp_global.rds")
write_rds(data, "data/data_combined.rds")

# Save DTA
write_dta(wtp_data, "data/clean_wtp_global.dta")
write_dta(data, "data/data_combined.dta")
