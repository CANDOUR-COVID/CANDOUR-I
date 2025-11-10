#### PACKAGES ------------------------
library(tidyverse)
library(haven)
library(here)
library(magrittr)


# FUNCTIONS --------------------------
functions_to_import <- c(
  "conjoint_functions",
  "wtp_functions",
  "IPUMS_functions",
  "labels_functions",
  "weights_functions"
)

for (.fun in functions_to_import) {
  source(here("wave_I", "scripts", "programs", paste0(.fun, ".R")))
}

# source(here("wave_I", "functions/.R")
# eval(parse("functions/.R", encoding="UTF-8"))
# eval(parse("functions/.R", encoding="UTF-8"))
# eval(parse("functions/.R", encoding="UTF-8"))
# eval(parse("functions/.R", encoding="UTF-8"))

# PRELIMINARIES ------------------------------------
set.seed(89)
country_codes <- paste0("data_", c("AUS", "BR", "CAN", "CHL", "CHN", "COL", "FR", "IND",
                   "IT", "RUS", "SP", "UGA", "UK", "US"))

#### 1. Cleaning and Combining ####

country_data <- list.files(path = here("wave_I", "proc", "build" , "01_data_clean"), full.names = TRUE) %>%
  map(~ read_csv(.) |> mutate(id = as.character(id)))


names(country_data) <- list.files(path = here("wave_I", "proc", "build" , "01_data_clean")) %>%
  str_remove(".csv$") |>
  str_replace("data_chl", "data_CHL")

list_vars <- list(~age, ~education, ~gender, ~REGION_0)

# Weights

weights_list <- country_data[!names(country_data) %in% "data_RUS"] |>
  imap(~ fn.merge(
    .x,
    list_vars,
    Quotas_list_df %>%
      extract(str_subset(names(Quotas_list_df), paste0("^", .y |> str_remove("data_"))))
  ) #|>
    # mutate(id = as.character(id))
  )

country_data <- country_data |>
  imap( ~ {
  if (!is.null(weights_list[[.y]])) {
    left_join(.x, weights_list[[.y]] , c("country", "id")) 
  } else {
    .x
  }
})

# Recode for reasons to get or not get the vaccine
for (country in c("data_BR", "data_FR")) {
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

country_data$data_RUS <- country_data$data_RUS %>%
  rename(Q19.2 = Q19.1)

for (country in country_codes) {
  country_data[[country]] <- country_data[[country]] %>% 
    separate(Q19.2, into = c(paste0("qol_condition_", seq(1:10))), "\\,")
  for (i in 1:10) {
    country_data[[country]][[paste0("qol_condition_", i)]] <- gsub(" $", "", country_data[[country]][[paste0("qol_condition_", i)]])
  }
}

# Writing separate data #
# for (country in country_codes) {
#   write_csv(country_data[[country]], paste0("data/country/data_", country, ".csv"))
# }

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

country_data$data_RUS$donation_amount <- as.double(country_data$data_RUS$donation_amount)

# Merging data 

country_data <- country_data |>
  map(~ .x |>
        mutate(ccode = as.character(ccode)))

data <- bind_rows(country_data)

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
write_csv(wtp_data, here("wave_I", "proc", "build", "02_data_analysis", "clean_wtp_global.csv"))
write_csv(data, here("wave_I", "proc", "build", "02_data_analysis", "data_combined.csv"))

# Save RDS to preserve factor coding
write_rds(wtp_data, here("wave_I", "proc", "build", "02_data_analysis", "clean_wtp_global.rds"))
write_rds(data, here("wave_I", "proc", "build", "02_data_analysis", "data_combined.rds"))

# Save DTA
write_dta(wtp_data, here("wave_I", "proc", "build", "02_data_analysis","clean_wtp_global.dta"))
write_dta(data, here("wave_I", "proc", "build", "02_data_analysis", "data_combined.dta"))
