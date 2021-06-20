setwd("C:/Users/Matias/Dropbox/Predoctoral/Vaccine Survey Data")

rm(list = ls())

library(tidyverse)

#### Paths ####

aus_path <- "Australia/Vaccine Survey - Australia_2 January 2021_19.04.csv"
br_path <- "Brazil/Vaccine Survey - Brazil_2 January 2021_19.06.csv"
can_path <- "Canada/Vaccine Survey - Canada_2 January 2021_19.07.csv"
chl_path <- "Chile/Vaccine Survey - Chile_January 2, 2021_16.18.csv"
chn_path <- "China/Vaccine Survey - China_2 January 2021_19.09.csv"
col_path <- "Colombia/Vaccine Survey - Colombia_2 January 2021_19.10.csv"
fr_path <- "France/Vaccine Survey - France_2 January 2021_19.11.csv"
ind_path <- "India/Vaccine Survey - India_2 January 2021_19.02.csv"
it_path <- "Italy/Vaccine Survey - Italy_2 January 2021_19.12.csv"
sp_path <- "Spain/Vaccine Survey - Spain_2 January 2021_19.13.csv"
uga_path <- "Uganda/Vaccine Survey - Uganda_January 14, 2021_09.21.csv"
uk_path <- "UK/Vaccine Survey - UK_2 January 2021_19.14.csv"
us_path <- "US/Vaccine Survey - US_2 January 2021_19.15.csv"

#### Australia ####

data_AUS <- read_csv(aus_path)

data_AUS <- data_AUS[-c(1:2),]

data_AUS <- data_AUS %>%
  rowid_to_column("id")

data_AUS <- data_AUS %>%
  rename(time_secs = `Duration (in seconds)`,
         age_consent = Q1.2,
         consent = Q1.3)

#### .Demographics and Political Partisanship ####

data_AUS <- data_AUS %>%
  rename(age = Q3.1,
         gender = Q3.2,
         ideology = Q21.1_1,
         INCOME = Q22.10,
         HH_INCOME = Q22.11,
         race = Q22.12,
         ancestry = Q22.13,
         citizen = Q22.14,
         nationality = Q22.15,
         religion = Q22.16,
         marital_status = Q22.17,
         dep_children = Q22.18,
         dep_children_amount = Q22.19,
         HH_size_adults = Q22.20)

data_AUS <- data_AUS %>% 
  mutate(REGION_0 = Q3.3,
         REGION_1 = coalesce(Q3.4_1, Q3.5_1, Q3.6_1, Q3.7_1, Q3.8_1, Q3.9_1, Q3.10_1, Q3.11_1, Q3.12_1)) %>%
  select(-c(Q3.3, Q3.4_1, Q3.5_1, Q3.6_1, Q3.7_1, Q3.8_1, Q3.9_1, Q3.10_1, Q3.11_1, Q3.12_1))

data_AUS <- data_AUS %>%
  rename(willing_risk = Q10.1_1,
         willing_punish = Q10.2_1,
         return_favour = Q10.3_1,
         donation = Q10.4,
         donation_amount = Q10.4_1_TEXT,
         wtp_access = Q11.1,
         wtp_private = Q11.2,
         wtp_amount_1 = Q11.4,
         wtp_amount_2a = Q11.5,
         wtp_amount_2b = Q11.6,
         int_get_vacc = Q14.1,
         int_lost_job = Q14.4,
         int_perc_job = Q14.5_1,
         int_exp_1 = Q14.6_1,
         int_exp_2 = Q14.6_2,
         int_exp_3 = Q14.6_3,
         int_exp_4 = Q14.6_4,
         int_exp_5 = Q14.6_5,
         int_family_vacc_1 = Q14.7_1,
         int_family_vacc_2 = Q14.7_2,
         int_family_vacc_3 = Q14.7_3,
         int_pol_implem_1 = Q14.8_1,
         int_pol_implem_2 = Q14.8_2,
         int_pol_implem_3 = Q14.8_3,
         int_pol_implem_4 = Q14.8_4,
         int_pol_implem_5 = Q14.8_5,
         int_pol_implem_6 = Q14.8_6,
         int_gov_priority_1 = Q14.9_1,
         int_gov_priority_2 = Q14.9_2,
         int_gov_priority_3 = Q14.9_3,
         int_gov_priority_4 = Q14.9_5,
         int_behaviour_1 = Q14.10_1,
         int_behaviour_2 = Q14.10_2,
         int_behaviour_3 = Q14.10_3,
         hes_general_1 = Q16.1_1,
         hes_general_2 = Q16.1_2,
         hes_general_3 = Q16.1_3,
         hes_covid_1 = Q16.2_1,
         hes_covid_2 = Q16.2_2,
         beh_measure_1 = Q17.1_1,
         beh_measure_2 = Q17.1_2,
         beh_measure_3 = Q17.1_3,
         beh_measure_4 = Q17.1_4,
         beh_measure_5 = Q17.1_5,
         beh_measure_6 = Q17.1_6,
         beh_measure_7 = Q17.1_7,
         beh_measure_8 = Q17.1_8,
         beh_measure_9 = Q17.1_9,
         beh_measure_10 = Q17.1_10,
         beh_measure_11 = Q17.1_11,
         beh_measure_12 = Q17.1_12,
         beh_measure_13 = Q17.1_13,
         beh_measure_14 = Q17.1_14,
         beh_measure_15 = Q17.1_15,
         beh_measure_16 = Q17.1_16,
         beh_measure_17 = Q17.1_17,
         beh_risk = Q17.2_1,
         geq_taxes_0 = Q18.1,
         geq_taxes_1 = Q18.3,
         geq_taxes_2a = Q18.4,
         geq_taxes_2b = Q18.5,
         geq_provision_1 = Q18.6_1,
         geq_provision_2 = Q18.6_2,
         geq_provision_3 = Q18.6_4,
         geq_donation = Q18.7,
         geq_ticket_0 = Q18.9,
         geq_ticket_1 = Q18.11,
         geq_ticket_2a = Q18.12,
         geq_ticket_2b = Q18.13,
         geq_current_spending = Q18.14_1,
         geq_future_spending = Q18.15,
         eq5d_mobility_pre = Q20.3_1,
         eq5d_mobility_post = Q20.3_2,
         eq5d_selfcare_pre = Q20.4_1,
         eq5d_selfcare_post = Q20.4_2,
         eq5d_usual_pre = Q20.5_1,
         eq5d_usual_post = Q20.5_2,
         eq5d_pain_pre = Q20.6_1,
         eq5d_pain_post = Q20.6_2,
         eq5d_anxiety_pre = Q20.7_1,
         eq5d_anxiety_post = Q20.7_2,
         eq5d_scale_pre = Q20.9,
         eq5d_scale_post = Q20.10,
         person1_ans = "1_Q5.3",
         person2_ans = "2_Q5.3",
         person3_ans = "3_Q5.3",
         person4_ans = "4_Q5.3",
         person5_ans = "5_Q5.3",
         person6_ans = "6_Q5.3",
         person7_ans = "7_Q5.3",
         person8_ans = "8_Q5.3",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         person1_b = person1b,
         person2_b = person2b,
         person3_b = person3b,
         person4_b = person4b,
         person5_b = person5b,
         person6_b = person6b,
         person7_b = person7b,
         person8_b = person8b)

data_AUS <- data_AUS %>%
  filter(!Q_TerminateFlag %in% c("Screened", "QuotaMet"), Status == "IP Address",
         Progress == "100")

respondi_AUS <- data_AUS %>%
  select(return_tic)

data_AUS <- data_AUS %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, RecordedDate, ResponseId,
            RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage,
            return_tic, Q_TerminateFlag, starts_with("seed"), hideFooter,
            ends_with(c("First Click", "Last Click", "Page Submit", "Click Count"))))

#### .Lottery vignette ####

data_AUS <- data_AUS %>%
  mutate(tvignette = if_else(is.na(Q12.1_1) & !is.na(Q12.2_1), 1,
                             if_else(!is.na(Q12.1_1) & is.na(Q12.2_1), 0, NA_real_)),
         lottery_vignette = as.numeric(coalesce(Q12.1_1, Q12.2_1))) %>%
  select(-c(Q12.1_1, Q12.2_1))

#### .List Experiment ####

data_AUS <- data_AUS %>%
  mutate(tlist = if_else(is.na(Q15.1) & !is.na(Q15.2), 1,
                         if_else(!is.na(Q15.1) & is.na(Q15.2), 0, NA_real_)),
         list_merged = coalesce(Q15.1, Q15.2)) %>%
  select(-c(Q15.1, Q15.2))


data_AUS <- data_AUS %>%
  mutate(list_merged = if_else(list_merged == "Zero", 0,
                               if_else(list_merged == "One", 1, 
                                       if_else(list_merged == "Two", 2,
                                               if_else(list_merged == "Three", 3,
                                                       if_else(list_merged == "Four", 4, NA_real_))))))


#### .Person Trade-off questions ####

data_AUS <- data_AUS %>%
  mutate(group = if_else(!is.na(Q6.1), 1,
                         if_else(!is.na(Q7.1), 2, 
                                 if_else(!is.na(Q8.1), 3, 
                                         if_else(!is.na(Q9.1), 4, NA_real_)))),
         select0 = coalesce(Q6.1, Q7.1, Q8.1, Q9.1),
         select1 = coalesce(Q6.4, Q7.4, Q8.4, Q9.4),
         select2 = coalesce(Q6.5, Q7.5, Q8.5, Q9.5, Q6.6, Q7.6, Q8.6, Q9.6),
         select2a = coalesce(Q6.5, Q7.5, Q8.5, Q9.5),
         select2b = coalesce(Q6.6, Q7.6, Q8.6, Q9.6),
         value_l = if_else(select2a == "Group A" & !is.na(select2a), 6,
                           if_else(select2a == "Group B" & !is.na(select2a), 3,
                                   if_else(select2b ==  "Group A" & !is.na(select2b), 2, 
                                           if_else(select2b == "Group B" & !is.na(select2b), 2, NA_real_)))),
         value1 = 3,
         value2 = if_else(select1 == "Group A", 6, 
                          if_else(select1 == "Group B", 2, NA_real_)),
         response1 = if_else(select1 == "Group A", 1, 
                             if_else(select1 == "Group B", 0, NA_real_)),
         response2 = if_else(select2 == "Group A", 1,
                             if_else(select2 == "Group B", 0, NA_real_)))

write_csv(data_AUS, "Australia/data_AUS.csv")
write_csv(respondi_AUS, "Respondi/respondi_AUS.csv")

#### Brazil ####

data_BR <- read_csv(br_path)

data_BR <- data_BR[-c(1:2),]

data_BR <- data_BR %>%
  rowid_to_column("id")

data_BR <- data_BR %>%
  rename(time_secs = `Duration (in seconds)`,
         age_consent = Q1.2,
         consent = Q1.3)

#### .Demographics and Political Partisanship ####

data_BR <- data_BR %>%
  rename(age = Q3.1,
         gender = Q3.2,
         ideology = Q21.1_1,
         INCOME = Q22.8,
         HH_INCOME = Q22.9,
         race = Q22.10,
         nationality = Q22.12,
         religion = Q22.11,
         marital_status = Q22.13,
         dep_children = Q22.14,
         dep_children_amount = Q22.15,
         HH_size_adults = Q22.16)

data_BR <- data_BR %>% 
  mutate(REGION_0 = Q3.3,
         REGION_1 = coalesce(Q3.4, Q3.5, Q3.6, Q3.7, Q3.8, Q3.9, Q3.10, Q3.11,
                             Q3.12, Q3.13, Q3.14, Q3.15, Q3.16, Q3.17, Q3.18, 
                             Q3.19, Q3.20, Q3.21, Q3.22, Q3.23, Q3.24, Q3.25, 
                             Q3.26, Q3.27, Q3.28, Q3.29, Q3.30),
         religion = if_else(Q22.11 == "Sim - Outra", Q22.11_13_TEXT, Q22.11)) %>%
  select(-c(Q3.3, Q3.4, Q3.5, Q3.6, Q3.7, Q3.8, Q3.9, Q3.10, Q3.11,
            Q3.12, Q3.13, Q3.14, Q3.15, Q3.16, Q3.17, Q3.18, 
            Q3.19, Q3.20, Q3.21, Q3.22, Q3.23, Q3.24, Q3.25, 
            Q3.26, Q3.27, Q3.28, Q3.29, Q3.30,
            Q22.11_13_TEXT, Q22.11))
  

data_BR <- data_BR %>%
  rename(willing_risk = Q10.1_1,
         willing_punish = Q10.2_1,
         return_favour = Q10.3_1,
         donation = Q10.4,
         donation_amount = Q10.4_1_TEXT,
         wtp_access = Q11.1,
         wtp_private = Q11.2,
         wtp_amount_1 = Q11.4,
         wtp_amount_2a = Q11.5,
         wtp_amount_2b = Q11.6,
         int_get_vacc = Q14.1,
         int_lost_job = Q14.4,
         int_perc_job = Q14.5_1,
         int_exp_1 = Q14.6_1,
         int_exp_2 = Q14.6_2,
         int_exp_3 = Q14.6_3,
         int_exp_4 = Q14.6_4,
         int_exp_5 = Q14.6_5,
         int_family_vacc_1 = Q14.7_1,
         int_family_vacc_2 = Q14.7_5,
         int_family_vacc_3 = Q14.7_6,
         int_pol_implem_1 = Q14.8_1,
         int_pol_implem_2 = Q14.8_6,
         int_pol_implem_3 = Q14.8_7,
         int_pol_implem_4 = Q14.8_8,
         int_pol_implem_5 = Q14.8_9,
         int_pol_implem_6 = Q14.8_10,
         int_gov_priority_1 = Q14.9_1,
         int_gov_priority_2 = Q14.9_6,
         int_gov_priority_3 = Q14.9_7,
         int_gov_priority_4 = Q14.9_8,
         int_behaviour_1 = Q14.10_1,
         int_behaviour_2 = Q14.10_2,
         int_behaviour_3 = Q14.10_3,
         hes_general_1 = Q16.1_1,
         hes_general_2 = Q16.1_2,
         hes_general_3 = Q16.1_3,
         hes_covid_1 = Q16.2_1,
         hes_covid_2 = Q16.2_2,
         beh_measure_1 = Q17.1_1,
         beh_measure_2 = Q17.1_2,
         beh_measure_3 = Q17.1_3,
         beh_measure_4 = Q17.1_4,
         beh_measure_5 = Q17.1_5,
         beh_measure_6 = Q17.1_6,
         beh_measure_7 = Q17.1_7,
         beh_measure_8 = Q17.1_8,
         beh_measure_9 = Q17.1_9,
         beh_measure_10 = Q17.1_10,
         beh_measure_11 = Q17.1_11,
         beh_measure_12 = Q17.1_12,
         beh_measure_13 = Q17.1_13,
         beh_measure_14 = Q17.1_14,
         beh_measure_15 = Q17.1_15,
         beh_measure_16 = Q17.1_16,
         beh_measure_17 = Q17.1_17,
         beh_risk = Q17.2_1,
         geq_taxes_0 = Q18.1,
         geq_taxes_1 = Q18.3,
         geq_taxes_2a = Q18.4,
         geq_taxes_2b = Q18.5,
         geq_provision_1 = Q18.6_1,
         geq_provision_2 = Q18.6_6,
         geq_provision_3 = Q18.6_7,
         geq_donation = Q18.8,
         geq_ticket_0 = Q18.9,
         geq_ticket_1 = Q18.11,
         geq_ticket_2a = Q18.12,
         geq_ticket_2b = Q18.13,
         geq_current_spending = Q18.14_1,
         geq_future_spending = Q18.15,
         eq5d_mobility_pre = Q20.3_1,
         eq5d_mobility_post = Q20.3_2,
         eq5d_selfcare_pre = Q20.4_1,
         eq5d_selfcare_post = Q20.4_2,
         eq5d_usual_pre = Q20.5_1,
         eq5d_usual_post = Q20.5_2,
         eq5d_pain_pre = Q20.6_1,
         eq5d_pain_post = Q20.6_2,
         eq5d_anxiety_pre = Q20.7_1,
         eq5d_anxiety_post = Q20.7_2,
         eq5d_scale_pre = Q20.9,
         eq5d_scale_post = Q20.10,
         person1_ans = "1_Q5.3",
         person2_ans = "2_Q5.3",
         person3_ans = "3_Q5.3",
         person4_ans = "4_Q5.3",
         person5_ans = "5_Q5.3",
         person6_ans = "6_Q5.3",
         person7_ans = "7_Q5.3",
         person8_ans = "8_Q5.3",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         person1_b = person1b,
         person2_b = person2b,
         person3_b = person3b,
         person4_b = person4b,
         person5_b = person5b,
         person6_b = person6b,
         person7_b = person7b,
         person8_b = person8b)

data_BR <- data_BR %>%
  filter(!Q_TerminateFlag %in% c("Screened", "QuotaMet"), Status == "IP Address",
         Progress == "100")

respondi_BR <- data_BR %>%
  select(return_tic)

data_BR <- data_BR %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, RecordedDate, ResponseId,
            RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage,
            return_tic, Q_TerminateFlag, starts_with("seed"), hideFooter,
            ends_with(c("First Click", "Last Click", "Page Submit", "Click Count"))))

#### .Lottery vignette ####

data_BR <- data_BR %>%
  mutate(tvignette = if_else(is.na(Q12.1_1) & !is.na(Q12.2_1), 1,
                             if_else(!is.na(Q12.1_1) & is.na(Q12.2_1), 0, NA_real_)),
         lottery_vignette = as.numeric(coalesce(Q12.1_1, Q12.2_1))) %>%
  select(-c(Q12.1_1, Q12.2_1))

#### .List Experiment ####

data_BR <- data_BR %>%
  mutate(tlist = if_else(is.na(Q15.1) & !is.na(Q15.2), 1,
                         if_else(!is.na(Q15.1) & is.na(Q15.2), 0, NA_real_)),
         list_merged = coalesce(Q15.1, Q15.2)) %>%
  select(-c(Q15.1, Q15.2))


data_BR <- data_BR %>%
  mutate(list_merged = if_else(list_merged == "Zero", 0,
                               if_else(list_merged == "Um", 1, 
                                       if_else(list_merged == "Dois", 2,
                                               if_else(list_merged == "Três", 3,
                                                       if_else(list_merged == "Quatro", 4, NA_real_))))))


#### .Person Trade-off questions ####

data_BR <- data_BR %>%
  mutate(group = if_else(!is.na(Q6.1), 1,
                         if_else(!is.na(Q7.1), 2, 
                                 if_else(!is.na(Q8.1), 3, 
                                         if_else(!is.na(Q9.1), 4, NA_real_)))),
         select0 = coalesce(Q6.1, Q7.1, Q8.1, Q9.1),
         select1 = coalesce(Q6.4, Q7.4, Q8.4, Q9.4),
         select2 = coalesce(Q6.5, Q7.5, Q8.5, Q9.5, Q6.6, Q7.6, Q8.6, Q9.6),
         select2a = coalesce(Q6.5, Q7.5, Q8.5, Q9.5),
         select2b = coalesce(Q6.6, Q7.6, Q8.6, Q9.6),
         value_l = if_else(select2a == "Grupo A" & !is.na(select2a), 6,
                           if_else(select2a == "Grupo B" & !is.na(select2a), 3,
                                   if_else(select2b ==  "Grupo A" & !is.na(select2b), 2, 
                                           if_else(select2b == "Grupo B" & !is.na(select2b), 2, NA_real_)))),
         value1 = 3,
         value2 = if_else(select1 == "Grupo A", 6, 
                          if_else(select1 == "Grupo B", 2, NA_real_)),
         response1 = if_else(select1 == "Grupo A", 1, 
                             if_else(select1 == "Grupo B", 0, NA_real_)),
         response2 = if_else(select2 == "Grupo A", 1,
                             if_else(select2 == "Grupo B", 0, NA_real_)))

write_csv(data_BR, "Brazil/data_BR.csv")
write_csv(respondi_BR, "Respondi/respondi_BR.csv")

#### Canada ####

data_CAN <- read_csv(can_path)

data_CAN <- data_CAN[-c(1:2),]

data_CAN <- data_CAN %>%
  rowid_to_column("id")

data_CAN <- data_CAN %>%
  rename(time_secs = `Duration (in seconds)`,
         age_consent = Q1.2,
         consent = Q1.3)

#### .Demographics and Political Partisanship ####

data_CAN <- data_CAN %>%
  rename(age = Q3.1,
         gender = Q3.2,
         ideology = Q21.1_1,
         REGION_0 = Q3.3_1, 
         REGION_1 = Q3.3_2,
         postal_code = Q3.4,
         INCOME = Q22.7,
         HH_INCOME = Q22.8,
         race = Q22.9,
         religion = Q22.10,
         marital_status = Q22.11,
         dep_children = Q22.12,
         dep_children_amount = Q22.13,
         HH_size_adults = Q22.14)

data_CAN <- data_CAN %>%
  rename(willing_risk = Q10.1_1,
         willing_punish = Q10.2_1,
         return_favour = Q10.3_1,
         donation = Q10.4,
         donation_amount = Q10.4_1_TEXT,
         wtp_access = Q11.1,
         wtp_private = Q11.2,
         wtp_amount_1 = Q11.4,
         wtp_amount_2a = Q11.5,
         wtp_amount_2b = Q11.6,
         int_get_vacc = Q14.1,
         int_lost_job = Q14.4,
         int_perc_job = Q14.5_1,
         int_exp_1 = Q14.6_1,
         int_exp_2 = Q14.6_2,
         int_exp_3 = Q14.6_3,
         int_exp_4 = Q14.6_4,
         int_exp_5 = Q14.6_5,
         int_family_vacc_1 = Q14.7_1,
         int_family_vacc_2 = Q14.7_2,
         int_family_vacc_3 = Q14.7_3,
         int_pol_implem_1 = Q14.8_1,
         int_pol_implem_2 = Q14.8_2,
         int_pol_implem_3 = Q14.8_3,
         int_pol_implem_4 = Q14.8_4,
         int_pol_implem_5 = Q14.8_5,
         int_pol_implem_6 = Q14.8_6,
         int_gov_priority_1 = Q14.9_1,
         int_gov_priority_2 = Q14.9_2,
         int_gov_priority_3 = Q14.9_3,
         int_gov_priority_4 = Q14.9_5,
         int_behaviour_1 = Q14.10_1,
         int_behaviour_2 = Q14.10_2,
         int_behaviour_3 = Q14.10_3,
         hes_general_1 = Q16.1_1,
         hes_general_2 = Q16.1_2,
         hes_general_3 = Q16.1_3,
         hes_covid_1 = Q16.2_1,
         hes_covid_2 = Q16.2_2,
         beh_measure_1 = Q17.1_1,
         beh_measure_2 = Q17.1_2,
         beh_measure_3 = Q17.1_3,
         beh_measure_4 = Q17.1_4,
         beh_measure_5 = Q17.1_5,
         beh_measure_6 = Q17.1_6,
         beh_measure_7 = Q17.1_7,
         beh_measure_8 = Q17.1_8,
         beh_measure_9 = Q17.1_9,
         beh_measure_10 = Q17.1_10,
         beh_measure_11 = Q17.1_11,
         beh_measure_12 = Q17.1_12,
         beh_measure_13 = Q17.1_13,
         beh_measure_14 = Q17.1_14,
         beh_measure_15 = Q17.1_15,
         beh_measure_16 = Q17.1_16,
         beh_measure_17 = Q17.1_17,
         beh_risk = Q17.2_1,
         geq_taxes_0 = Q18.1,
         geq_taxes_1 = Q18.3,
         geq_taxes_2a = Q18.4,
         geq_taxes_2b = Q18.5,
         geq_provision_1 = Q18.6_1,
         geq_provision_2 = Q18.6_2,
         geq_provision_3 = Q18.6_4,
         geq_donation = Q18.7,
         geq_ticket_0 = Q18.9,
         geq_ticket_1 = Q18.11,
         geq_ticket_2a = Q18.12,
         geq_ticket_2b = Q18.13,
         geq_current_spending = Q18.14_1,
         geq_future_spending = Q18.15,
         eq5d_mobility_pre = Q20.3_1,
         eq5d_mobility_post = Q20.3_2,
         eq5d_selfcare_pre = Q20.4_1,
         eq5d_selfcare_post = Q20.4_2,
         eq5d_usual_pre = Q20.5_1,
         eq5d_usual_post = Q20.5_2,
         eq5d_pain_pre = Q20.6_1,
         eq5d_pain_post = Q20.6_2,
         eq5d_anxiety_pre = Q20.7_1,
         eq5d_anxiety_post = Q20.7_2,
         eq5d_scale_pre = Q20.9,
         eq5d_scale_post = Q20.10,
         person1_ans = "1_Q5.3",
         person2_ans = "2_Q5.3",
         person3_ans = "3_Q5.3",
         person4_ans = "4_Q5.3",
         person5_ans = "5_Q5.3",
         person6_ans = "6_Q5.3",
         person7_ans = "7_Q5.3",
         person8_ans = "8_Q5.3",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         person1_b = person1b,
         person2_b = person2b,
         person3_b = person3b,
         person4_b = person4b,
         person5_b = person5b,
         person6_b = person6b,
         person7_b = person7b,
         person8_b = person8b)

data_CAN <- data_CAN %>%
  filter(!Q_TerminateFlag %in% c("Screened", "QuotaMet"), Status == "IP Address",
         Progress == "100")

respondi_CAN <- data_CAN %>%
  select(return_tic)

data_CAN <- data_CAN %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, RecordedDate, ResponseId,
            RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage,
            return_tic, Q_TerminateFlag, starts_with("seed"), hideFooter,
            ends_with(c("First Click", "Last Click", "Page Submit", "Click Count"))))

#### .Lottery vignette ####

data_CAN <- data_CAN %>%
  mutate(tvignette = if_else(is.na(Q12.1_1) & !is.na(Q12.2_1), 1,
                             if_else(!is.na(Q12.1_1) & is.na(Q12.2_1), 0, NA_real_)),
         lottery_vignette = as.numeric(coalesce(Q12.1_1, Q12.2_1))) %>%
  select(-c(Q12.1_1, Q12.2_1))

#### .List Experiment ####

data_CAN <- data_CAN %>%
  mutate(tlist = if_else(is.na(Q15.1) & !is.na(Q15.2), 1,
                         if_else(!is.na(Q15.1) & is.na(Q15.2), 0, NA_real_)),
         list_merged = coalesce(Q15.1, Q15.2)) %>%
  select(-c(Q15.1, Q15.2))


data_CAN <- data_CAN %>%
  mutate(list_merged = if_else(list_merged == "Zero", 0,
                               if_else(list_merged == "One", 1, 
                                       if_else(list_merged == "Two", 2,
                                               if_else(list_merged == "Three", 3,
                                                       if_else(list_merged == "Four", 4, NA_real_))))))


#### .Person Trade-off questions ####

data_CAN <- data_CAN %>%
  mutate(group = if_else(!is.na(Q6.1), 1,
                         if_else(!is.na(Q7.1), 2, 
                                 if_else(!is.na(Q8.1), 3, 
                                         if_else(!is.na(Q9.1), 4, NA_real_)))),
         select0 = coalesce(Q6.1, Q7.1, Q8.1, Q9.1),
         select1 = coalesce(Q6.4, Q7.4, Q8.4, Q9.4),
         select2 = coalesce(Q6.5, Q7.5, Q8.5, Q9.5, Q6.6, Q7.6, Q8.6, Q9.6),
         select2a = coalesce(Q6.5, Q7.5, Q8.5, Q9.5),
         select2b = coalesce(Q6.6, Q7.6, Q8.6, Q9.6),
         value_l = if_else(select2a == "Group A" & !is.na(select2a), 6,
                           if_else(select2a == "Group B" & !is.na(select2a), 3,
                                   if_else(select2b ==  "Group A" & !is.na(select2b), 2, 
                                           if_else(select2b == "Group B" & !is.na(select2b), 2, NA_real_)))),
         value1 = 3,
         value2 = if_else(select1 == "Group A", 6, 
                          if_else(select1 == "Group B", 2, NA_real_)),
         response1 = if_else(select1 == "Group A", 1, 
                             if_else(select1 == "Group B", 0, NA_real_)),
         response2 = if_else(select2 == "Group A", 1,
                             if_else(select2 == "Group B", 0, NA_real_)))

write_csv(data_CAN, "Canada/data_CAN.csv")
write_csv(respondi_CAN, "Respondi/respondi_CAN.csv")

#### Chile ####

data_CHL <- read_csv(chl_path)

data_CHL <- data_CHL[-c(1:2),]

data_CHL <- data_CHL %>%
  rowid_to_column("id")

data_CHL <- data_CHL %>%
  rename(time_secs = `Duration (in seconds)`,
         age_consent = Q1.2,
         consent = Q1.3)

#### .Demographics and Political Partisanship ####

data_CHL <- data_CHL %>%
  rename(age = Q3.1,
         gender = Q3.2,
         ideology = Q21.1_1,
         postal_code = Q3.20,
         INCOME = Q22.13,
         HH_INCOME = Q22.14,
         marital_status = Q22.20,
         dep_children = Q22.21,
         dep_children_amount = Q22.22,
         HH_size_adults = Q22.23)

data_CHL$Q22.15 <- data_CHL$Q22.15 %>%
  recode("Sí" = "Chile")

data_CHL <- data_CHL %>%
  mutate(REGION_0 = Q3.3,
         REGION_1 = coalesce(Q3.4, Q3.5, Q3.6, Q3.7, Q3.8, Q3.9, Q3.10, Q3.11,
                             Q3.12, Q3.13, Q3.14, Q3.15, Q3.16, Q3.17, Q3.18, 
                             Q3.19),
         Q22.16 = if_else(Q22.16 == "Otro, ¿cuál?", Q22.16_7_TEXT, Q22.16),
         nationality = if_else(!is.na(Q22.16), Q22.16, Q22.15),
         religion = if_else(Q22.17 == "Otra", Q22.17_21_TEXT, Q22.17),
         Q22.19 = if_else(Q22.16 == "Otro, ¿Cuál?", Q22.19_10_TEXT, Q22.19),
         race = if_else(!is.na(Q22.19), Q22.19, Q22.18)) %>%
  select(-c(Q3.3, Q3.4, Q3.5, Q3.6, Q3.7, Q3.8, Q3.9, Q3.10, Q3.11,
            Q3.12, Q3.13, Q3.14, Q3.15, Q3.16, Q3.17, Q3.18, Q3.19,
            Q22.15, Q22.16, Q22.16_7_TEXT, Q22.19_10_TEXT))

data_CHL <- data_CHL %>%
  rename(willing_risk = Q10.1_1,
         willing_punish = Q10.2_1,
         return_favour = Q10.3_1,
         donation = Q10.4,
         donation_amount = Q10.4_1_TEXT,
         wtp_access = Q11.1,
         wtp_private = Q11.2,
         wtp_amount_1 = Q11.4,
         wtp_amount_2a = Q11.5,
         wtp_amount_2b = Q11.6,
         int_get_vacc = Q14.1,
         int_lost_job = Q14.4,
         int_perc_job = Q14.5_1,
         int_exp_1 = Q14.6_1,
         int_exp_2 = Q14.6_2,
         int_exp_3 = Q14.6_3,
         int_exp_4 = Q14.6_4,
         int_exp_5 = Q14.6_5,
         int_family_vacc_1 = Q14.7_1,
         int_family_vacc_2 = Q14.7_2,
         int_family_vacc_3 = Q14.7_3,
         int_pol_implem_1 = Q14.8_1,
         int_pol_implem_2 = Q14.8_2,
         int_pol_implem_3 = Q14.8_3,
         int_pol_implem_4 = Q14.8_4,
         int_pol_implem_5 = Q14.8_5,
         int_pol_implem_6 = Q14.8_6,
         int_gov_priority_1 = Q14.9_1,
         int_gov_priority_2 = Q14.9_2,
         int_gov_priority_3 = Q14.9_3,
         int_behaviour_1 = Q14.10_1,
         int_behaviour_2 = Q14.10_2,
         int_behaviour_3 = Q14.10_3,
         hes_general_1 = Q16.1_1,
         hes_general_2 = Q16.1_2,
         hes_general_3 = Q16.1_3,
         hes_covid_1 = Q16.1_1_1,
         hes_covid_2 = Q16.1_2_1,
         beh_measure_1 = Q17.1_1,
         beh_measure_2 = Q17.1_2,
         beh_measure_3 = Q17.1_3,
         beh_measure_4 = Q17.1_4,
         beh_measure_5 = Q17.1_5,
         beh_measure_6 = Q17.1_6,
         beh_measure_7 = Q17.1_7,
         beh_measure_8 = Q17.1_8,
         beh_measure_9 = Q17.1_9,
         beh_measure_10 = Q17.1_10,
         beh_measure_11 = Q17.1_11,
         beh_measure_12 = Q17.1_12,
         beh_measure_13 = Q17.1_13,
         beh_measure_14 = Q17.1_14,
         beh_measure_15 = Q17.1_15,
         beh_measure_16 = Q17.1_16,
         beh_measure_17 = Q17.1_17,
         beh_risk = Q17.2_1,
         geq_taxes_0 = Q18.1,
         geq_taxes_1 = Q18.3,
         geq_taxes_2a = Q18.4,
         geq_taxes_2b = Q18.5,
         geq_provision_1 = Q18.6_1,
         geq_provision_2 = Q18.6_2,
         geq_provision_3 = Q18.6_4,
         geq_donation = Q18.8,
         geq_ticket_0 = Q18.9,
         geq_ticket_1 = Q18.11,
         geq_ticket_2a = Q18.12,
         geq_ticket_2b = Q18.13,
         geq_current_spending = Q18.14_1,
         geq_future_spending = Q18.15,
         eq5d_mobility_pre = Q20.3_1,
         eq5d_mobility_post = Q20.3_2,
         eq5d_selfcare_pre = Q20.4_1,
         eq5d_selfcare_post = Q20.4_2,
         eq5d_usual_pre = Q20.5_1,
         eq5d_usual_post = Q20.5_2,
         eq5d_pain_pre = Q20.6_1,
         eq5d_pain_post = Q20.6_2,
         eq5d_anxiety_pre = Q20.7_1,
         eq5d_anxiety_post = Q20.7_2,
         eq5d_scale_pre = Q20.9,
         eq5d_scale_post = Q20.10,
         person1_ans = "1_Q5.3",
         person2_ans = "2_Q5.3",
         person3_ans = "3_Q5.3",
         person4_ans = "4_Q5.3",
         person5_ans = "5_Q5.3",
         person6_ans = "6_Q5.3",
         person7_ans = "7_Q5.3",
         person8_ans = "8_Q5.3",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         person1_b = person1b,
         person2_b = person2b,
         person3_b = person3b,
         person4_b = person4b,
         person5_b = person5b,
         person6_b = person6b,
         person7_b = person7b,
         person8_b = person8b)

data_CHL <- data_CHL %>%
  filter(!Q_TerminateFlag %in% c("Screened", "QuotaMet"), Status == "IP Address",
         Progress == "100")

data_CHL <- data_CHL %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, RecordedDate, ResponseId,
            RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage,
            Q_TerminateFlag, starts_with("seed"), hideFooter,
            ends_with(c("First Click", "Last Click", "Page Submit", "Click Count"))))

#### .Lottery vignette ####

data_CHL <- data_CHL %>%
  mutate(tvignette = if_else(is.na(Q12.1_1) & !is.na(Q12.2_1), 1,
                             if_else(!is.na(Q12.1_1) & is.na(Q12.2_1), 0, NA_real_)),
         lottery_vignette = as.numeric(coalesce(Q12.1_1, Q12.2_1))) %>%
  select(-c(Q12.1_1, Q12.2_1))

#### .List Experiment ####

data_CHL <- data_CHL %>%
  mutate(tlist = if_else(is.na(Q15.1) & !is.na(Q15.2), 1,
                         if_else(!is.na(Q15.1) & is.na(Q15.2), 0, NA_real_)),
         list_merged = coalesce(Q15.1, Q15.2)) %>%
  select(-c(Q15.1, Q15.2))


data_CHL <- data_CHL %>%
  mutate(list_merged = if_else(list_merged == "Cero", 0,
                               if_else(list_merged %in% c("Uno", "Una"), 1, 
                                       if_else(list_merged == "Dos", 2,
                                               if_else(list_merged == "Tres", 3,
                                                       if_else(list_merged == "Cuatro", 4, NA_real_))))))


#### .Person Trade-off questions ####

data_CHL <- data_CHL %>%
  mutate(group = if_else(!is.na(Q6.1), 1,
                         if_else(!is.na(Q7.1), 2, 
                                 if_else(!is.na(Q8.1), 3, 
                                         if_else(!is.na(Q9.1), 4, NA_real_)))),
         select0 = coalesce(Q6.1, Q7.1, Q8.1, Q9.1),
         select1 = coalesce(Q6.4, Q7.4, Q8.4, Q9.4),
         select2 = coalesce(Q6.5, Q7.5, Q8.5, Q9.5, Q6.6, Q7.6, Q8.6, Q9.6),
         select2a = coalesce(Q6.5, Q7.5, Q8.5, Q9.5),
         select2b = coalesce(Q6.6, Q7.6, Q8.6, Q9.6),
         value_l = if_else(select2a == "Grupo A" & !is.na(select2a), 6,
                           if_else(select2a == "Grupo B" & !is.na(select2a), 3,
                                   if_else(select2b ==  "Grupo A" & !is.na(select2b), 2, 
                                           if_else(select2b == "Grupo B" & !is.na(select2b), 2, NA_real_)))),
         value1 = 3,
         value2 = if_else(select1 == "Grupo A", 6, 
                          if_else(select1 == "Grupo B", 2, NA_real_)),
         response1 = if_else(select1 == "Grupo A", 1, 
                             if_else(select1 == "Grupo B", 0, NA_real_)),
         response2 = if_else(select2 == "Grupo A", 1,
                             if_else(select2 == "Grupo B", 0, NA_real_)))

write_csv(data_CHL, "Chile/data_CHL.csv")

#### China ####

data_CHN <- read_csv(chn_path)

data_CHN <- data_CHN[-c(1:2),]

data_CHN <- data_CHN %>%
  rowid_to_column("id")

data_CHN <- data_CHN %>%
  rename(time_secs = `Duration (in seconds)`,
         age_consent = Q1.2,
         consent = Q1.3)

#### .Demographics ####

data_CHN <- data_CHN %>%
  rename(age = Q3.1,
         gender = Q3.2,
         REGION_0 = Q3.3_1, 
         REGION_1 = Q3.3_2,
         INCOME = Q22.9,
         HH_INCOME = Q22.10,
         marital_status = Q22.11,
         dep_children = Q22.12,
         dep_children_amount = Q22.13,
         HH_size_adults = Q22.14)

data_CHN <- data_CHN %>%
  rename(willing_risk = Q10.1_1,
         willing_punish = Q10.2_1,
         return_favour = Q10.3_1,
         donation = Q10.4,
         donation_amount = Q10.4_1_TEXT,
         wtp_access = Q11.1,
         wtp_private = Q11.2,
         wtp_amount_1 = Q11.4,
         wtp_amount_2a = Q11.5,
         wtp_amount_2b = Q11.6,
         int_get_vacc = Q14.1,
         int_lost_job = Q14.4,
         int_perc_job = Q14.5_1,
         int_exp_1 = Q14.6_1,
         int_exp_2 = Q14.6_2,
         int_exp_3 = Q14.6_3,
         int_exp_4 = Q14.6_4,
         int_exp_5 = Q14.6_5,
         int_family_vacc_1 = Q14.7_1,
         int_family_vacc_2 = Q14.7_2,
         int_family_vacc_3 = Q14.7_3,
         int_pol_implem_1 = Q14.8_2,
         int_pol_implem_2 = Q14.8_6,
         int_pol_implem_3 = Q14.8_5,
         int_pol_implem_4 = Q14.8_3,
         int_pol_implem_5 = Q14.8_7,
         int_pol_implem_6 = Q14.8_9,
         int_behaviour_1 = Q14.10_1,
         int_behaviour_2 = Q14.10_2,
         int_behaviour_3 = Q14.10_3,
         hes_general_1 = Q16.1_1,
         hes_general_2 = Q16.1_2,
         hes_general_3 = Q16.1_3,
         hes_covid_1 = Q16.2_1,
         hes_covid_2 = Q16.2_2,
         beh_measure_1 = Q17.1_1,
         beh_measure_2 = Q17.1_2,
         beh_measure_3 = Q17.1_3,
         beh_measure_4 = Q17.1_4,
         beh_measure_5 = Q17.1_5,
         beh_measure_6 = Q17.1_6,
         beh_measure_7 = Q17.1_7,
         beh_measure_8 = Q17.1_8,
         beh_measure_9 = Q17.1_9,
         beh_measure_10 = Q17.1_10,
         beh_measure_11 = Q17.1_11,
         beh_measure_12 = Q17.1_12,
         beh_measure_13 = Q17.1_13,
         beh_measure_14 = Q17.1_14,
         beh_measure_15 = Q17.1_15,
         beh_measure_16 = Q17.1_16,
         beh_measure_17 = Q17.1_17,
         beh_risk = Q17.2_1,
         geq_taxes_1 = Q18.3,
         geq_taxes_2a = Q18.4,
         geq_taxes_2b = Q18.5,
         geq_provision_1 = Q18.6_1,
         geq_provision_2 = Q18.6_2,
         geq_provision_3 = Q18.6_4,
         geq_ticket_0 = Q18.9,
         geq_ticket_1 = Q18.11,
         geq_ticket_2a = Q18.12,
         geq_ticket_2b = Q18.13,
         eq5d_mobility_pre = Q20.3_1,
         eq5d_mobility_post = Q20.3_2,
         eq5d_selfcare_pre = Q20.4_1,
         eq5d_selfcare_post = Q20.4_2,
         eq5d_usual_pre = Q20.5_1,
         eq5d_usual_post = Q20.5_2,
         eq5d_pain_pre = Q20.6_1,
         eq5d_pain_post = Q20.6_2,
         eq5d_anxiety_pre = Q20.7_1,
         eq5d_anxiety_post = Q20.7_2,
         eq5d_scale_pre = Q20.9,
         eq5d_scale_post = Q20.10,
         person1_ans = "1_Q5.3",
         person2_ans = "2_Q5.3",
         person3_ans = "3_Q5.3",
         person4_ans = "4_Q5.3",
         person5_ans = "5_Q5.3",
         person6_ans = "6_Q5.3",
         person7_ans = "7_Q5.3",
         person8_ans = "8_Q5.3",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         person1_b = person1b,
         person2_b = person2b,
         person3_b = person3b,
         person4_b = person4b,
         person5_b = person5b,
         person6_b = person6b,
         person7_b = person7b,
         person8_b = person8b)

data_CHN <- data_CHN %>%
  filter(!Q_TerminateFlag %in% c("Screened", "QuotaMet"), Status == "IP Address",
         Progress == "100")

respondi_CHN <- data_CHN %>%
  select(return_tic)

data_CHN <- data_CHN %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, RecordedDate, ResponseId,
            RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage,
            return_tic, Q_TerminateFlag, starts_with("seed"), hideFooter,
            ends_with(c("First Click", "Last Click", "Page Submit", "Click Count"))))

#### .Lottery vignette ####

data_CHN <- data_CHN %>%
  mutate(tvignette = if_else(is.na(Q12.1_1) & !is.na(Q12.2_1), 1,
                             if_else(!is.na(Q12.1_1) & is.na(Q12.2_1), 0, NA_real_)),
         lottery_vignette = as.numeric(coalesce(Q12.1_1, Q12.2_1))) %>%
  select(-c(Q12.1_1, Q12.2_1))

#### .Person Trade-off questions ####

data_CHN <- data_CHN %>%
  mutate(group = if_else(!is.na(Q6.1), 1,
                         if_else(!is.na(Q7.1), 2, 
                                 if_else(!is.na(Q8.1), 3, 
                                         if_else(!is.na(Q9.1), 4, NA_real_)))),
         select0 = coalesce(Q6.1, Q7.1, Q8.1, Q9.1),
         select1 = coalesce(Q6.4, Q7.4, Q8.4, Q9.4),
         select2 = coalesce(Q6.5, Q7.5, Q8.5, Q9.5, Q6.6, Q7.6, Q8.6, Q9.6),
         select2a = coalesce(Q6.5, Q7.5, Q8.5, Q9.5),
         select2b = coalesce(Q6.6, Q7.6, Q8.6, Q9.6),
         value_l = if_else(select2a == "A组" & !is.na(select2a), 6,
                           if_else(select2a == "B组" & !is.na(select2a), 3,
                                   if_else(select2b ==  "A组" & !is.na(select2b), 2, 
                                           if_else(select2b == "B组" & !is.na(select2b), 2, NA_real_)))),
         value1 = 3,
         value2 = if_else(select1 == "A组", 6, 
                          if_else(select1 == "B组", 2, NA_real_)),
         response1 = if_else(select1 == "A组", 1, 
                             if_else(select1 == "B组", 0, NA_real_)),
         response2 = if_else(select2 == "A组", 1,
                             if_else(select2 == "B组", 0, NA_real_)))

write_csv(data_CHN, "China/data_CHN.csv")
write_csv(respondi_CHN, "Respondi/respondi_CHN.csv")

#### Colombia ####

data_COL <- read_csv(col_path)

data_COL <- data_COL[-c(1:2),]

data_COL <- data_COL %>%
  rowid_to_column("id")

data_COL <- data_COL %>%
  rename(time_secs = `Duration (in seconds)`,
         age_consent = Q1.2,
         consent = Q1.3)

#### .Demographics and Political Partisanship ####

data_COL <- data_COL %>%
  rename(age = Q3.1,
         gender = Q3.2,
         ideology = Q21.1_1,
         REGION_0 = Q3.3_1, 
         REGION_1 = Q3.3_2,
         postal_code = Q3.4,
         INCOME = Q22.9,
         HH_INCOME = Q22.10,
         race = Q22.14,
         marital_status = Q22.15,
         dep_children = Q22.16,
         dep_children_amount = Q22.17,
         HH_size_adults = Q22.18)

data_COL$Q22.11 <- data_COL$Q22.11 %>%
  recode("Sí" = "Colombia")

data_COL <- data_COL %>%
  mutate(Q22.12 = if_else(Q22.12 == "Otro país", Q22.12_11_TEXT, Q22.12),
         nationality = if_else(!is.na(Q22.12), Q22.12, Q22.11),
         religion = if_else(Q22.13 == "Otra", Q22.13_11_TEXT, Q22.13)) %>%
  select(-c(Q22.12, Q22.12_11_TEXT, Q22.13, Q22.13_11_TEXT))

data_COL <- data_COL %>%
  rename(willing_risk = Q10.1_1,
         willing_punish = Q10.2_1,
         return_favour = Q10.3_1,
         donation = Q10.4,
         donation_amount = Q10.4_1_TEXT,
         wtp_access = Q11.1,
         wtp_private = Q11.2,
         wtp_amount_1 = Q11.4,
         wtp_amount_2a = Q11.5,
         wtp_amount_2b = Q11.6,
         int_get_vacc = Q14.1,
         int_lost_job = Q14.4,
         int_perc_job = Q14.5_1,
         int_exp_1 = Q14.6_1,
         int_exp_2 = Q14.6_2,
         int_exp_3 = Q14.6_3,
         int_exp_4 = Q14.6_4,
         int_exp_5 = Q14.6_5,
         int_family_vacc_1 = Q14.7_1,
         int_family_vacc_2 = Q14.7_2,
         int_family_vacc_3 = Q14.7_3,
         int_pol_implem_1 = Q14.8_1,
         int_pol_implem_2 = Q14.8_2,
         int_pol_implem_3 = Q14.8_3,
         int_pol_implem_4 = Q14.8_4,
         int_pol_implem_5 = Q14.8_5,
         int_pol_implem_6 = Q14.8_6,
         int_gov_priority_1 = Q14.9_1,
         int_gov_priority_2 = Q14.9_2,
         int_gov_priority_3 = Q14.9_3,
         int_behaviour_1 = Q14.10_1,
         int_behaviour_2 = Q14.10_2,
         int_behaviour_3 = Q14.10_3,
         hes_general_1 = Q16.1_1,
         hes_general_2 = Q16.1_2,
         hes_general_3 = Q16.1_3,
         hes_covid_1 = Q16.2_1,
         hes_covid_2 = Q16.2_2,
         beh_measure_1 = Q17.1_1,
         beh_measure_2 = Q17.1_2,
         beh_measure_3 = Q17.1_3,
         beh_measure_4 = Q17.1_4,
         beh_measure_5 = Q17.1_5,
         beh_measure_6 = Q17.1_6,
         beh_measure_7 = Q17.1_7,
         beh_measure_8 = Q17.1_8,
         beh_measure_9 = Q17.1_9,
         beh_measure_10 = Q17.1_10,
         beh_measure_11 = Q17.1_11,
         beh_measure_12 = Q17.1_12,
         beh_measure_13 = Q17.1_13,
         beh_measure_14 = Q17.1_14,
         beh_measure_15 = Q17.1_15,
         beh_measure_16 = Q17.1_16,
         beh_measure_17 = Q17.1_17,
         beh_risk = Q17.2_1,
         geq_taxes_0 = Q18.1,
         geq_taxes_1 = Q18.3,
         geq_taxes_2a = Q18.4,
         geq_taxes_2b = Q18.5,
         geq_provision_1 = Q18.6_1,
         geq_provision_2 = Q18.6_2,
         geq_provision_3 = Q18.6_4,
         geq_donation = Q18.8,
         geq_ticket_0 = Q18.9,
         geq_ticket_1 = Q18.11,
         geq_ticket_2a = Q18.12,
         geq_ticket_2b = Q18.13,
         geq_current_spending = Q18.14_1,
         geq_future_spending = Q18.15,
         eq5d_mobility_pre = Q20.3_1,
         eq5d_mobility_post = Q20.3_2,
         eq5d_selfcare_pre = Q20.4_1,
         eq5d_selfcare_post = Q20.4_2,
         eq5d_usual_pre = Q20.5_1,
         eq5d_usual_post = Q20.5_2,
         eq5d_pain_pre = Q20.6_1,
         eq5d_pain_post = Q20.6_2,
         eq5d_anxiety_pre = Q20.7_1,
         eq5d_anxiety_post = Q20.7_2,
         eq5d_scale_pre = Q20.9,
         eq5d_scale_post = Q20.10,
         person1_ans = "1_Q5.3",
         person2_ans = "2_Q5.3",
         person3_ans = "3_Q5.3",
         person4_ans = "4_Q5.3",
         person5_ans = "5_Q5.3",
         person6_ans = "6_Q5.3",
         person7_ans = "7_Q5.3",
         person8_ans = "8_Q5.3",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         person1_b = person1b,
         person2_b = person2b,
         person3_b = person3b,
         person4_b = person4b,
         person5_b = person5b,
         person6_b = person6b,
         person7_b = person7b,
         person8_b = person8b)

data_COL <- data_COL %>%
  filter(!Q_TerminateFlag %in% c("Screened", "QuotaMet"), Status == "IP Address",
         Progress == "100")

respondi_COL <- data_COL %>%
  select(return_tic)

data_COL <- data_COL %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, RecordedDate, ResponseId,
            RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage,
            return_tic, Q_TerminateFlag, starts_with("seed"), hideFooter,
            ends_with(c("First Click", "Last Click", "Page Submit", "Click Count"))))

#### .Lottery vignette ####

data_COL <- data_COL %>%
  mutate(tvignette = if_else(is.na(Q12.1_1) & !is.na(Q12.2_1), 1,
                             if_else(!is.na(Q12.1_1) & is.na(Q12.2_1), 0, NA_real_)),
         lottery_vignette = as.numeric(coalesce(Q12.1_1, Q12.2_1))) %>%
  select(-c(Q12.1_1, Q12.2_1))

#### .List Experiment ####

data_COL <- data_COL %>%
  mutate(tlist = if_else(is.na(Q15.1) & !is.na(Q15.2), 1,
                         if_else(!is.na(Q15.1) & is.na(Q15.2), 0, NA_real_)),
         list_merged = coalesce(Q15.1, Q15.2)) %>%
  select(-c(Q15.1, Q15.2))


data_COL <- data_COL %>%
  mutate(list_merged = if_else(list_merged == "Cero", 0,
                               if_else(list_merged %in% c("Una", "Uno"), 1, 
                                       if_else(list_merged == "Dos", 2,
                                               if_else(list_merged == "Tres", 3,
                                                       if_else(list_merged == "Cuatro", 4, NA_real_))))))


#### .Person Trade-off questions ####

data_COL <- data_COL %>%
  mutate(group = if_else(!is.na(Q6.1), 1,
                         if_else(!is.na(Q7.1), 2, 
                                 if_else(!is.na(Q8.1), 3, 
                                         if_else(!is.na(Q9.1), 4, NA_real_)))),
         select0 = coalesce(Q6.1, Q7.1, Q8.1, Q9.1),
         select1 = coalesce(Q6.4, Q7.4, Q8.4, Q9.4),
         select2 = coalesce(Q6.5, Q7.5, Q8.5, Q9.5, Q6.6, Q7.6, Q8.6, Q9.6),
         select2a = coalesce(Q6.5, Q7.5, Q8.5, Q9.5),
         select2b = coalesce(Q6.6, Q7.6, Q8.6, Q9.6),
         value_l = if_else(select2a == "Grupo A" & !is.na(select2a), 6,
                           if_else(select2a == "Grupo B" & !is.na(select2a), 3,
                                   if_else(select2b ==  "Grupo A" & !is.na(select2b), 2, 
                                           if_else(select2b == "Grupo B" & !is.na(select2b), 2, NA_real_)))),
         value1 = 3,
         value2 = if_else(select1 == "Grupo A", 6, 
                          if_else(select1 == "Grupo B", 2, NA_real_)),
         response1 = if_else(select1 == "Grupo A", 1, 
                             if_else(select1 == "Grupo B", 0, NA_real_)),
         response2 = if_else(select2 == "Grupo A", 1,
                             if_else(select2 == "Grupo B", 0, NA_real_)))

write_csv(data_COL, "Colombia/data_COL.csv")
write_csv(respondi_COL, "Respondi/respondi_COL.csv")

#### France ####

data_FR <- read_csv(fr_path)

data_FR <- data_FR[-c(1:2),]

data_FR <- data_FR %>%
  rowid_to_column("id")

data_FR <- data_FR %>%
  rename(time_secs = `Duration (in seconds)`,
         age_consent = Q1.2,
         consent = Q1.3)

#### .Demographics and Political Partisanship ####

data_FR <- data_FR %>%
  rename(age = Q3.1,
         gender = Q3.2,
         ideology = Q21.1_1,
         REGION_0 = Q3.3_1, 
         REGION_1 = Q3.3_2,
         postal_code = Q3.4,
         INCOME = Q22.8,
         HH_INCOME = Q22.9,
         foreign_parents = Q22.11,
         marital_status = Q22.12,
         dep_children = Q22.13,
         dep_children_amount = Q22.14,
         HH_size_adults = Q22.15)

data_FR <- data_FR %>%
  mutate(religion = if_else(Q22.10 == "Autre religion", Q22.10_5_TEXT, Q22.10)) %>%
  select(-c(Q22.10, Q22.10_5_TEXT))

data_FR <- data_FR %>%
  rename(willing_risk = Q10.1_1,
         willing_punish = Q10.2_1,
         return_favour = Q10.3_1,
         donation = Q10.4,
         donation_amount = Q10.4_1_TEXT,
         wtp_access = Q11.1,
         wtp_private = Q11.2,
         wtp_amount_1 = Q11.4,
         wtp_amount_2a = Q11.5,
         wtp_amount_2b = Q11.6,
         int_get_vacc = Q14.1,
         int_lost_job = Q14.4,
         int_perc_job = Q14.5_1,
         int_exp_1 = Q14.6_1,
         int_exp_2 = Q14.6_2,
         int_exp_3 = Q14.6_3,
         int_exp_4 = Q14.6_4,
         int_exp_5 = Q14.6_5,
         int_family_vacc_1 = Q14.7_1,
         int_family_vacc_2 = Q14.7_5,
         int_family_vacc_3 = Q14.7_6,
         int_pol_implem_1 = Q14.8_1,
         int_pol_implem_2 = Q14.8_6,
         int_pol_implem_3 = Q14.8_7,
         int_pol_implem_4 = Q14.8_8,
         int_pol_implem_5 = Q14.8_9,
         int_pol_implem_6 = Q14.8_10,
         int_gov_priority_1 = Q14.9_1,
         int_gov_priority_2 = Q14.9_6,
         int_gov_priority_3 = Q14.9_7,
         int_gov_priority_4 = Q14.9_8,
         int_behaviour_1 = Q14.10_1,
         int_behaviour_2 = Q14.10_2,
         int_behaviour_3 = Q14.10_3,
         hes_general_1 = Q16.1_1,
         hes_general_2 = Q16.1_2,
         hes_general_3 = Q16.1_3,
         hes_covid_1 = Q16.2_1,
         hes_covid_2 = Q16.2_3,
         beh_measure_1 = Q17.1_1,
         beh_measure_2 = Q17.1_2,
         beh_measure_3 = Q17.1_3,
         beh_measure_4 = Q17.1_4,
         beh_measure_5 = Q17.1_5,
         beh_measure_6 = Q17.1_6,
         beh_measure_7 = Q17.1_7,
         beh_measure_8 = Q17.1_8,
         beh_measure_9 = Q17.1_9,
         beh_measure_10 = Q17.1_10,
         beh_measure_11 = Q17.1_11,
         beh_measure_12 = Q17.1_12,
         beh_measure_13 = Q17.1_13,
         beh_measure_14 = Q17.1_14,
         beh_measure_15 = Q17.1_15,
         beh_measure_16 = Q17.1_16,
         beh_measure_17 = Q17.1_17,
         beh_risk = Q17.2_1,
         geq_taxes_0 = Q18.1,
         geq_taxes_1 = Q18.3,
         geq_taxes_2a = Q18.4,
         geq_taxes_2b = Q18.5,
         geq_provision_1 = Q18.6_1,
         geq_provision_2 = Q18.6_6,
         geq_provision_3 = Q18.6_7,
         geq_donation = Q18.7,
         geq_ticket_0 = Q18.9,
         geq_ticket_1 = Q18.11,
         geq_ticket_2a = Q18.12,
         geq_ticket_2b = Q18.13,
         geq_current_spending = Q18.14_1,
         geq_future_spending = Q18.15,
         eq5d_mobility_pre = Q20.3_1,
         eq5d_mobility_post = Q20.3_2,
         eq5d_selfcare_pre = Q20.4_1,
         eq5d_selfcare_post = Q20.4_2,
         eq5d_usual_pre = Q20.5_1,
         eq5d_usual_post = Q20.5_2,
         eq5d_pain_pre = Q20.6_1,
         eq5d_pain_post = Q20.6_2,
         eq5d_anxiety_pre = Q20.7_1,
         eq5d_anxiety_post = Q20.7_2,
         eq5d_scale_pre = Q20.9,
         eq5d_scale_post = Q20.10,
         person1_ans = "1_Q5.3",
         person2_ans = "2_Q5.3",
         person3_ans = "3_Q5.3",
         person4_ans = "4_Q5.3",
         person5_ans = "5_Q5.3",
         person6_ans = "6_Q5.3",
         person7_ans = "7_Q5.3",
         person8_ans = "8_Q5.3",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         person1_b = person1b,
         person2_b = person2b,
         person3_b = person3b,
         person4_b = person4b,
         person5_b = person5b,
         person6_b = person6b,
         person7_b = person7b,
         person8_b = person8b)

data_FR <- data_FR %>%
  filter(!Q_TerminateFlag %in% c("Screened", "QuotaMet"), Status == "IP Address",
         Progress == "100")

respondi_FR <- data_FR %>%
  select(return_tic)

data_FR <- data_FR %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, RecordedDate, ResponseId,
            RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage,
            return_tic, Q_TerminateFlag, starts_with("seed"), hideFooter,
            ends_with(c("First Click", "Last Click", "Page Submit", "Click Count"))))

#### .Lottery vignette ####

data_FR <- data_FR %>%
  mutate(tvignette = if_else(is.na(Q12.1_1) & !is.na(Q12.2_1), 1,
                             if_else(!is.na(Q12.1_1) & is.na(Q12.2_1), 0, NA_real_)),
         lottery_vignette = as.numeric(coalesce(Q12.1_1, Q12.2_1))) %>%
  select(-c(Q12.1_1, Q12.2_1))

#### .List Experiment ####

data_FR <- data_FR %>%
  mutate(tlist = if_else(is.na(Q15.1) & !is.na(Q15.2), 1,
                         if_else(!is.na(Q15.1) & is.na(Q15.2), 0, NA_real_)),
         list_merged = coalesce(Q15.1, Q15.2)) %>%
  select(-c(Q15.1, Q15.2))


data_FR <- data_FR %>%
  mutate(list_merged = if_else(list_merged == "Zéro", 0,
                               if_else(list_merged == "Un", 1, 
                                       if_else(list_merged == "Deux", 2,
                                               if_else(list_merged == "Trois", 3,
                                                       if_else(list_merged == "Quatre", 4, NA_real_))))))


#### .Person Trade-off questions ####

data_FR <- data_FR %>%
  mutate(group = if_else(!is.na(Q6.1), 1,
                         if_else(!is.na(Q7.1), 2, 
                                 if_else(!is.na(Q8.1), 3, 
                                         if_else(!is.na(Q9.1), 4, NA_real_)))),
         select0 = coalesce(Q6.1, Q7.1, Q8.1, Q9.1),
         select1 = coalesce(Q6.4, Q7.4, Q8.4, Q9.4),
         select2 = coalesce(Q6.5, Q7.5, Q8.5, Q9.5, Q6.6, Q7.6, Q8.6, Q9.6),
         select2a = coalesce(Q6.5, Q7.5, Q8.5, Q9.5),
         select2b = coalesce(Q6.6, Q7.6, Q8.6, Q9.6),
         value_l = if_else(select2a == "Groupe A" & !is.na(select2a), 6,
                           if_else(select2a == "Groupe B" & !is.na(select2a), 3,
                                   if_else(select2b ==  "Groupe A" & !is.na(select2b), 2, 
                                           if_else(select2b == "Groupe B" & !is.na(select2b), 2, NA_real_)))),
         value1 = 3,
         value2 = if_else(select1 == "Groupe A", 6, 
                          if_else(select1 == "Groupe B", 2, NA_real_)),
         response1 = if_else(select1 == "Groupe A", 1, 
                             if_else(select1 == "Groupe B", 0, NA_real_)),
         response2 = if_else(select2 == "Groupe A", 1,
                             if_else(select2 == "Groupe B", 0, NA_real_)))

write_csv(data_FR, "France/data_FR.csv")
write_csv(respondi_FR, "Respondi/respondi_FR.csv")

#### India ####

data_IND <- read_csv(ind_path)

data_IND <- data_IND[-c(1:2),]

data_IND <- data_IND %>%
  rowid_to_column("id")

data_IND <- data_IND %>%
  rename(time_secs = `Duration (in seconds)`,
         age_consent = Q1.2,
         consent = Q1.3)

#### .Demographics and Political Partisanship ####

data_IND <- data_IND %>%
  rename(age = Q3.1,
         gender = Q3.2,
         postal_code = Q3.4,
         ideology = Q21.1_1,
         REGION_0 = Q3.3_1, 
         REGION_1 = Q3.3_2,
         INCOME = Q22.7,
         HH_INCOME = Q22.8,
         race = Q22.9,
         marital_status = Q22.12,
         dep_children = Q22.13,
         dep_children_amount = Q22.14,
         HH_size_adults = Q22.15,
         language_spoken = Q22.16,
         birth_state = Q22.17)

data_IND <- data_IND %>%
  unite(news, Q22.1:Q22.1_7_TEXT, sep = ",", na.rm = T, remove = T)

data_IND <- data_IND %>%
  mutate(religion = if_else(Q22.10 == "Other", Q22.10_7_TEXT, Q22.10),
         years_residence = if_else(Q22.18 == "Number of years:", Q22.18_2_TEXT, Q22.18),
         rent_home = if_else(Q22.19 == "Other", Q22.19_4_TEXT, Q22.19)) %>%
  select(-c(Q22.10, Q22.10_7_TEXT, Q22.18, Q22.18_2_TEXT, Q22.19, Q22.19_4_TEXT))

data_IND <- data_IND %>%
  rename(willing_risk = Q10.1_1,
         willing_punish = Q10.2_1,
         return_favour = Q10.3_1,
         donation = Q10.4,
         donation_amount = Q10.4_1_TEXT,
         wtp_access = Q11.1,
         wtp_private = Q11.2,
         wtp_amount_1 = Q11.4,
         wtp_amount_2a = Q11.5,
         wtp_amount_2b = Q11.6,
         int_get_vacc = Q14.1,
         int_lost_job = Q14.4,
         int_perc_job = Q14.5_1,
         int_exp_1 = Q14.6_1,
         int_exp_2 = Q14.6_2,
         int_exp_3 = Q14.6_3,
         int_exp_4 = Q14.6_4,
         int_exp_5 = Q14.6_5,
         int_family_vacc_1 = Q14.7_1,
         int_family_vacc_2 = Q14.7_2,
         int_family_vacc_3 = Q14.7_3,
         int_pol_implem_1 = Q14.8_1,
         int_pol_implem_2 = Q14.8_2,
         int_pol_implem_3 = Q14.8_3,
         int_pol_implem_4 = Q14.8_4,
         int_pol_implem_5 = Q14.8_5,
         int_pol_implem_6 = Q14.8_6,
         int_gov_priority_1 = Q14.9_1,
         int_gov_priority_2 = Q14.9_2,
         int_gov_priority_3 = Q14.9_3,
         int_gov_priority_4 = Q14.9_5,
         int_behaviour_1 = Q14.10_1,
         int_behaviour_2 = Q14.10_2,
         int_behaviour_3 = Q14.10_3,
         hes_general_1 = Q16.1_1,
         hes_general_2 = Q16.1_2,
         hes_general_3 = Q16.1_3,
         hes_covid_1 = Q16.2_1,
         hes_covid_2 = Q16.2_2,
         beh_measure_1 = Q17.1_1,
         beh_measure_2 = Q17.1_2,
         beh_measure_3 = Q17.1_3,
         beh_measure_4 = Q17.1_4,
         beh_measure_5 = Q17.1_5,
         beh_measure_6 = Q17.1_6,
         beh_measure_7 = Q17.1_7,
         beh_measure_8 = Q17.1_8,
         beh_measure_9 = Q17.1_9,
         beh_measure_10 = Q17.1_10,
         beh_measure_11 = Q17.1_11,
         beh_measure_12 = Q17.1_12,
         beh_measure_13 = Q17.1_13,
         beh_measure_14 = Q17.1_14,
         beh_measure_15 = Q17.1_15,
         beh_measure_16 = Q17.1_16,
         beh_measure_17 = Q17.1_17,
         beh_risk = Q17.2_1,
         geq_taxes_0 = Q18.1,
         geq_taxes_1 = Q18.3,
         geq_taxes_2a = Q18.4,
         geq_taxes_2b = Q18.5,
         geq_provision_1 = Q18.6_1,
         geq_provision_2 = Q18.6_2,
         geq_provision_3 = Q18.6_4,
         geq_donation = Q18.8,
         geq_ticket_0 = Q18.9,
         geq_ticket_1 = Q18.11,
         geq_ticket_2a = Q18.12,
         geq_ticket_2b = Q18.13,
         geq_current_spending = Q18.14_1,
         geq_future_spending = Q18.15,
         eq5d_mobility_pre = Q20.3_1,
         eq5d_mobility_post = Q20.3_2,
         eq5d_selfcare_pre = Q20.4_1,
         eq5d_selfcare_post = Q20.4_2,
         eq5d_usual_pre = Q20.5_1,
         eq5d_usual_post = Q20.5_2,
         eq5d_pain_pre = Q20.6_1,
         eq5d_pain_post = Q20.6_2,
         eq5d_anxiety_pre = Q20.7_1,
         eq5d_anxiety_post = Q20.7_2,
         eq5d_scale_pre = Q20.9,
         eq5d_scale_post = Q20.10,
         person1_ans = "1_Q5.3",
         person2_ans = "2_Q5.3",
         person3_ans = "3_Q5.3",
         person4_ans = "4_Q5.3",
         person5_ans = "5_Q5.3",
         person6_ans = "6_Q5.3",
         person7_ans = "7_Q5.3",
         person8_ans = "8_Q5.3",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         person1_b = person1b,
         person2_b = person2b,
         person3_b = person3b,
         person4_b = person4b,
         person5_b = person5b,
         person6_b = person6b,
         person7_b = person7b,
         person8_b = person8b)

data_IND <- data_IND %>%
  filter(!Q_TerminateFlag %in% c("Screened", "QuotaMet"), Status == "IP Address",
         Progress == "100")

respondi_IND <- data_IND %>%
  select(return_tic)

data_IND <- data_IND %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, RecordedDate, ResponseId,
            RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage,
            return_tic, Q_TerminateFlag, starts_with("seed"), hideFooter,
            ends_with(c("First Click", "Last Click", "Page Submit", "Click Count"))))

#### .Lottery vignette ####

data_IND <- data_IND %>%
  mutate(tvignette = if_else(is.na(Q12.1_1) & !is.na(Q12.2_1), 1,
                             if_else(!is.na(Q12.1_1) & is.na(Q12.2_1), 0, NA_real_)),
         lottery_vignette = as.numeric(coalesce(Q12.1_1, Q12.2_1))) %>%
  select(-c(Q12.1_1, Q12.2_1))

#### .List Experiment ####

data_IND <- data_IND %>%
  mutate(tlist = if_else(is.na(Q15.1) & !is.na(Q15.2), 1,
                         if_else(!is.na(Q15.1) & is.na(Q15.2), 0, NA_real_)),
         list_merged = coalesce(Q15.1, Q15.2)) %>%
  select(-c(Q15.1, Q15.2))


data_IND <- data_IND %>%
  mutate(list_merged = if_else(list_merged == "Zero", 0,
                               if_else(list_merged == "One", 1, 
                                       if_else(list_merged == "Two", 2,
                                               if_else(list_merged == "Three", 3,
                                                       if_else(list_merged == "Four", 4, NA_real_))))))


#### .Person Trade-off questions ####

data_IND <- data_IND %>%
  mutate(group = if_else(!is.na(Q6.1), 1,
                         if_else(!is.na(Q7.1), 2, 
                                 if_else(!is.na(Q8.1), 3, 
                                         if_else(!is.na(Q9.1), 4, NA_real_)))),
         select0 = coalesce(Q6.1, Q7.1, Q8.1, Q9.1),
         select1 = coalesce(Q6.4, Q7.4, Q8.4, Q9.4),
         select2 = coalesce(Q6.5, Q7.5, Q8.5, Q9.5, Q6.6, Q7.6, Q8.6, Q9.6),
         select2a = coalesce(Q6.5, Q7.5, Q8.5, Q9.5),
         select2b = coalesce(Q6.6, Q7.6, Q8.6, Q9.6),
         value_l = if_else(select2a == "Group A" & !is.na(select2a), 6,
                           if_else(select2a == "Group B" & !is.na(select2a), 3,
                                   if_else(select2b ==  "Group A" & !is.na(select2b), 2, 
                                           if_else(select2b == "Group B" & !is.na(select2b), 2, NA_real_)))),
         value1 = 3,
         value2 = if_else(select1 == "Group A", 6, 
                          if_else(select1 == "Group B", 2, NA_real_)),
         response1 = if_else(select1 == "Group A", 1, 
                             if_else(select1 == "Group B", 0, NA_real_)),
         response2 = if_else(select2 == "Group A", 1,
                             if_else(select2 == "Group B", 0, NA_real_)))

write_csv(data_IND, "India/data_IND.csv")
write_csv(respondi_IND, "Respondi/respondi_IND.csv")

#### Italy ####

data_IT <- read_csv(it_path)

data_IT <- data_IT[-c(1:2),]

data_IT <- data_IT %>%
  rowid_to_column("id")

data_IT <- data_IT %>%
  rename(time_secs = `Duration (in seconds)`,
         age_consent = Q1.2,
         consent = Q1.3)

#### .Demographics and Political Partisanship ####

data_IT <- data_IT %>%
  rename(age = Q3.1,
         gender = Q3.2,
         ideology = Q21.1_1,
         postal_code = Q3.24,
         INCOME = Q22.15,
         HH_INCOME = Q22.16,
         nationality = Q22.17,
         marital_status = Q22.19,
         dep_children = Q22.20,
         dep_children_amount = Q22.21,
         HH_size_adults = Q22.22)

data_IT <- data_IT %>% 
  mutate(REGION_0 = Q3.3,
         REGION_1 = coalesce(Q3.4_1, Q3.5_1, Q3.6_1, Q3.7_1, Q3.8_1, Q3.9_1,
                             Q3.10_1, Q3.11_1, Q3.12_1, Q3.13_1, Q3.14_1, 
                             Q3.15_1, Q3.16_1, Q3.17_1, Q3.18_1, Q3.19_1, 
                             Q3.20_1, Q3.21_1, Q3.22_1, Q3.23_1),
         REGION_2 = coalesce(Q3.4_2, Q3.5_2, Q3.6_2, Q3.7_2, Q3.8_2, Q3.9_2, 
                             Q3.10_2, Q3.11_2, Q3.12_2, Q3.13_2, Q3.14_2, 
                             Q3.15_2, Q3.16_2, Q3.17_2, Q3.18_2, Q3.19_2, 
                             Q3.20_2, Q3.21_2, Q3.22_2, Q3.23_2),
         religion = if_else(Q22.18 == "Altro", Q22.18_48_TEXT, Q22.18)) %>%
  select(-c(Q3.3, Q3.4_1, Q3.5_1, Q3.6_1, Q3.7_1, Q3.8_1, Q3.9_1,
            Q3.10_1, Q3.11_1, Q3.12_1, Q3.13_1, Q3.14_1, 
            Q3.15_1, Q3.16_1, Q3.17_1, Q3.18_1, Q3.19_1, 
            Q3.20_1, Q3.21_1, Q3.22_1, Q3.23_1, 
            Q3.4_2, Q3.5_2, Q3.6_2, Q3.7_2, Q3.8_2, Q3.9_2, 
            Q3.10_2, Q3.11_2, Q3.12_2, Q3.13_2, Q3.14_2, 
            Q3.15_2, Q3.16_2, Q3.17_2, Q3.18_2, Q3.19_2, 
            Q3.20_2, Q3.21_2, Q3.22_2, Q3.23_2,
            Q22.18_48_TEXT, Q22.18))

data_IT <- data_IT %>%
  rename(willing_risk = Q10.1_1,
         willing_punish = Q10.2_1,
         return_favour = Q10.3_1,
         donation = Q10.4,
         donation_amount = Q10.4_1_TEXT,
         wtp_access = Q11.1,
         wtp_private = Q11.2,
         wtp_amount_1 = Q11.4,
         wtp_amount_2a = Q11.5,
         wtp_amount_2b = Q11.6,
         int_get_vacc = Q14.1,
         int_lost_job = Q14.4,
         int_perc_job = Q14.5_1,
         int_exp_1 = Q14.6_1,
         int_exp_2 = Q14.6_2,
         int_exp_3 = Q14.6_3,
         int_exp_4 = Q14.6_4,
         int_exp_5 = Q14.6_5,
         int_family_vacc_1 = Q14.7_1,
         int_family_vacc_2 = Q14.7_5,
         int_family_vacc_3 = Q14.7_6,
         int_pol_implem_1 = Q14.8_1,
         int_pol_implem_2 = Q14.8_6,
         int_pol_implem_3 = Q14.8_7,
         int_pol_implem_4 = Q14.8_8,
         int_pol_implem_5 = Q14.8_9,
         int_pol_implem_6 = Q14.8_10,
         int_gov_priority_1 = Q14.9_1,
         int_gov_priority_2 = Q14.9_6,
         int_gov_priority_3 = Q14.9_7,
         int_gov_priority_4 = Q14.9_8,
         int_behaviour_1 = Q14.10_1,
         int_behaviour_2 = Q14.10_2,
         int_behaviour_3 = Q14.10_3,
         hes_general_1 = Q16.1_1,
         hes_general_2 = Q16.1_2,
         hes_general_3 = Q16.1_3,
         hes_covid_1 = Q16.2_1,
         hes_covid_2 = Q16.2_2,
         beh_measure_1 = Q17.1_1,
         beh_measure_2 = Q17.1_2,
         beh_measure_3 = Q17.1_3,
         beh_measure_4 = Q17.1_4,
         beh_measure_5 = Q17.1_5,
         beh_measure_6 = Q17.1_6,
         beh_measure_7 = Q17.1_7,
         beh_measure_8 = Q17.1_8,
         beh_measure_9 = Q17.1_9,
         beh_measure_10 = Q17.1_10,
         beh_measure_11 = Q17.1_11,
         beh_measure_12 = Q17.1_12,
         beh_measure_13 = Q17.1_13,
         beh_measure_14 = Q17.1_14,
         beh_measure_15 = Q17.1_15,
         beh_measure_16 = Q17.1_16,
         beh_measure_17 = Q17.1_17,
         beh_risk = Q17.2_1,
         geq_taxes_0 = Q18.1,
         geq_taxes_1 = Q18.3,
         geq_taxes_2a = Q18.4,
         geq_taxes_2b = Q18.5,
         geq_provision_1 = Q18.6_1,
         geq_provision_2 = Q18.6_6,
         geq_provision_3 = Q18.6_7,
         geq_donation = Q18.7,
         geq_ticket_0 = Q18.9,
         geq_ticket_1 = Q18.11,
         geq_ticket_2a = Q18.12,
         geq_ticket_2b = Q18.13,
         geq_current_spending = Q18.14_1,
         geq_future_spending = Q18.15,
         eq5d_mobility_pre = Q20.3_1,
         eq5d_mobility_post = Q20.3_2,
         eq5d_selfcare_pre = Q20.4_1,
         eq5d_selfcare_post = Q20.4_2,
         eq5d_usual_pre = Q20.5_1,
         eq5d_usual_post = Q20.5_2,
         eq5d_pain_pre = Q20.6_1,
         eq5d_pain_post = Q20.6_2,
         eq5d_anxiety_pre = Q20.7_1,
         eq5d_anxiety_post = Q20.7_2,
         eq5d_scale_pre = Q20.9,
         eq5d_scale_post = Q20.10,
         person1_ans = "1_Q5.3",
         person2_ans = "2_Q5.3",
         person3_ans = "3_Q5.3",
         person4_ans = "4_Q5.3",
         person5_ans = "5_Q5.3",
         person6_ans = "6_Q5.3",
         person7_ans = "7_Q5.3",
         person8_ans = "8_Q5.3",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         person1_b = person1b,
         person2_b = person2b,
         person3_b = person3b,
         person4_b = person4b,
         person5_b = person5b,
         person6_b = person6b,
         person7_b = person7b,
         person8_b = person8b)

data_IT <- data_IT %>%
  filter(!Q_TerminateFlag %in% c("Screened", "QuotaMet"), Status == "IP Address",
         Progress == "100")

respondi_IT <- data_IT %>%
  select(return_tic)

data_IT <- data_IT %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, RecordedDate, ResponseId,
            RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage,
            return_tic, Q_TerminateFlag, starts_with("seed"), hideFooter,
            ends_with(c("First Click", "Last Click", "Page Submit", "Click Count"))))

#### .Lottery vignette ####

data_IT <- data_IT %>%
  mutate(tvignette = if_else(is.na(Q12.1_1) & !is.na(Q12.2_1), 1,
                             if_else(!is.na(Q12.1_1) & is.na(Q12.2_1), 0, NA_real_)),
         lottery_vignette = as.numeric(coalesce(Q12.1_1, Q12.2_1))) %>%
  select(-c(Q12.1_1, Q12.2_1))

#### .List Experiment ####

data_IT <- data_IT %>%
  mutate(tlist = if_else(is.na(Q15.1) & !is.na(Q15.2), 1,
                         if_else(!is.na(Q15.1) & is.na(Q15.2), 0, NA_real_)),
         list_merged = coalesce(Q15.1, Q15.2)) %>%
  select(-c(Q15.1, Q15.2))


data_IT <- data_IT %>%
  mutate(list_merged = if_else(list_merged == "Zero", 0,
                               if_else(list_merged == "Uno", 1, 
                                       if_else(list_merged == "Due", 2,
                                               if_else(list_merged == "Tre", 3,
                                                       if_else(list_merged == "Quattro", 4, NA_real_))))))


#### .Person Trade-off questions ####

data_IT <- data_IT %>%
  mutate(group = if_else(!is.na(Q6.1), 1,
                         if_else(!is.na(Q7.1), 2, 
                                 if_else(!is.na(Q8.1), 3, 
                                         if_else(!is.na(Q9.1), 4, NA_real_)))),
         select0 = coalesce(Q6.1, Q7.1, Q8.1, Q9.1),
         select1 = coalesce(Q6.4, Q7.4, Q8.4, Q9.4),
         select2 = coalesce(Q6.5, Q7.5, Q8.5, Q9.5, Q6.6, Q7.6, Q8.6, Q9.6),
         select2a = coalesce(Q6.5, Q7.5, Q8.5, Q9.5),
         select2b = coalesce(Q6.6, Q7.6, Q8.6, Q9.6),
         value_l = if_else(select2a == "Gruppo A" & !is.na(select2a), 6,
                           if_else(select2a == "Gruppo B" & !is.na(select2a), 3,
                                   if_else(select2b ==  "Gruppo A" & !is.na(select2b), 2, 
                                           if_else(select2b == "Gruppo B" & !is.na(select2b), 2, NA_real_)))),
         value1 = 3,
         value2 = if_else(select1 == "Gruppo A", 6, 
                          if_else(select1 == "Gruppo B", 2, NA_real_)),
         response1 = if_else(select1 == "Gruppo A", 1, 
                             if_else(select1 == "Gruppo B", 0, NA_real_)),
         response2 = if_else(select2 == "Gruppo A", 1,
                             if_else(select2 == "Gruppo B", 0, NA_real_)))

write_csv(data_IT, "Italy/data_IT.csv")
write_csv(respondi_IT, "Respondi/respondi_IT.csv")

#### Spain ####

data_SP <- read_csv(sp_path)

data_SP <- data_SP[-c(1:2),]

data_SP <- data_SP %>%
  rowid_to_column("id")

data_SP <- data_SP %>%
  rename(time_secs = `Duration (in seconds)`,
         age_consent = Q1.2,
         consent = Q1.3)

#### .Demographics and Political Partisanship ####

data_SP <- data_SP %>%
  rename(age = Q3.1,
         gender = Q3.2,
         ideology = Q21.1_1,
         INCOME = Q22.8,
         HH_INCOME = Q22.9,
         nationality = Q22.10,
         marital_status = Q22.12,
         dep_children = Q22.13,
         dep_children_amount = Q22.14,
         HH_size_adults = Q22.15)

data_SP <- data_SP %>% 
  mutate(REGION_0 = Q3.3,
         REGION_1 = coalesce(Q3.4_1, Q3.5_1, Q3.6_1, Q3.7_1, Q3.8_1, Q3.9_1,
                             Q3.10_1, Q3.11_1, Q3.12_1, Q3.13_1, Q3.14_1,
                             Q3.15_1, Q3.16_1, Q3.17_1, Q3.18_1, Q3.19_1,
                             Q3.20_1, Q3.21_1, Q3.22_1),
         REGION_2 = coalesce(Q3.4_2, Q3.5_2, Q3.6_2, Q3.7_2, Q3.8_2, Q3.9_2,
                             Q3.10_2, Q3.11_2, Q3.12_2, Q3.13_2, Q3.14_2,
                             Q3.15_2, Q3.16_2, Q3.17_2, Q3.18_2, Q3.19_2,
                             Q3.20_2, Q3.21_2, Q3.22_2),
         religion = if_else(Q22.11 == "Otra", Q22.11_12_TEXT, Q22.11)) %>%
  select(-c(Q3.3, Q3.4_1, Q3.5_1, Q3.6_1, Q3.7_1, Q3.8_1, Q3.9_1,
            Q3.10_1, Q3.11_1, Q3.12_1, Q3.13_1, Q3.14_1,
            Q3.15_1, Q3.16_1, Q3.17_1, Q3.18_1, Q3.19_1,
            Q3.20_1, Q3.21_1, Q3.22_1, 
            Q3.4_2, Q3.5_2, Q3.6_2, Q3.7_2, Q3.8_2, Q3.9_2,
            Q3.10_2, Q3.11_2, Q3.12_2, Q3.13_2, Q3.14_2,
            Q3.15_2, Q3.16_2, Q3.17_2, Q3.18_2, Q3.19_2,
            Q3.20_2, Q3.21_2, Q3.22_2,
            Q22.11_12_TEXT, Q22.11))

data_SP <- data_SP %>%
  rename(willing_risk = Q10.1_1,
         willing_punish = Q10.2_1,
         return_favour = Q10.3_1,
         donation = Q10.4,
         donation_amount = Q10.4_1_TEXT,
         wtp_access = Q11.1,
         wtp_private = Q11.2,
         wtp_amount_1 = Q11.4,
         wtp_amount_2a = Q11.5,
         wtp_amount_2b = Q11.6,
         int_get_vacc = Q14.1,
         int_lost_job = Q14.4,
         int_perc_job = Q14.5_1,
         int_exp_1 = Q14.6_1,
         int_exp_2 = Q14.6_2,
         int_exp_3 = Q14.6_3,
         int_exp_4 = Q14.6_4,
         int_exp_5 = Q14.6_5,
         int_family_vacc_1 = Q14.7_1,
         int_family_vacc_2 = Q14.7_2,
         int_family_vacc_3 = Q14.7_3,
         int_pol_implem_1 = Q14.8_1,
         int_pol_implem_2 = Q14.8_2,
         int_pol_implem_3 = Q14.8_3,
         int_pol_implem_4 = Q14.8_4,
         int_pol_implem_5 = Q14.8_5,
         int_pol_implem_6 = Q14.8_6,
         int_gov_priority_1 = Q14.9_1,
         int_gov_priority_2 = Q14.9_2,
         int_gov_priority_3 = Q14.9_3,
         int_behaviour_1 = Q14.10_1,
         int_behaviour_2 = Q14.10_2,
         int_behaviour_3 = Q14.10_3,
         hes_general_1 = Q16.1_1,
         hes_general_2 = Q16.1_2,
         hes_general_3 = Q16.1_3,
         hes_covid_1 = Q16.2_1,
         hes_covid_2 = Q16.2_2,
         beh_measure_1 = Q17.1_1,
         beh_measure_2 = Q17.1_2,
         beh_measure_3 = Q17.1_3,
         beh_measure_4 = Q17.1_4,
         beh_measure_5 = Q17.1_5,
         beh_measure_6 = Q17.1_6,
         beh_measure_7 = Q17.1_7,
         beh_measure_8 = Q17.1_8,
         beh_measure_9 = Q17.1_9,
         beh_measure_10 = Q17.1_10,
         beh_measure_11 = Q17.1_11,
         beh_measure_12 = Q17.1_12,
         beh_measure_13 = Q17.1_13,
         beh_measure_14 = Q17.1_14,
         beh_measure_15 = Q17.1_15,
         beh_measure_16 = Q17.1_16,
         beh_measure_17 = Q17.1_17,
         beh_risk = Q17.2_1,
         geq_taxes_0 = Q18.1,
         geq_taxes_1 = Q18.3,
         geq_taxes_2a = Q18.4,
         geq_taxes_2b = Q18.5,
         geq_provision_1 = Q18.6_1,
         geq_provision_2 = Q18.6_2,
         geq_provision_3 = Q18.6_4,
         geq_donation = Q18.7,
         geq_ticket_0 = Q18.9,
         geq_ticket_1 = Q18.11,
         geq_ticket_2a = Q18.12,
         geq_ticket_2b = Q18.13,
         geq_current_spending = Q18.14_1,
         geq_future_spending = Q18.15,
         eq5d_mobility_pre = Q20.3_1,
         eq5d_mobility_post = Q20.3_2,
         eq5d_selfcare_pre = Q20.4_1,
         eq5d_selfcare_post = Q20.4_2,
         eq5d_usual_pre = Q20.5_1,
         eq5d_usual_post = Q20.5_2,
         eq5d_pain_pre = Q20.6_1,
         eq5d_pain_post = Q20.6_2,
         eq5d_anxiety_pre = Q20.7_1,
         eq5d_anxiety_post = Q20.7_2,
         eq5d_scale_pre = Q20.9,
         eq5d_scale_post = Q20.10,
         person1_ans = "1_Q5.3",
         person2_ans = "2_Q5.3",
         person3_ans = "3_Q5.3",
         person4_ans = "4_Q5.3",
         person5_ans = "5_Q5.3",
         person6_ans = "6_Q5.3",
         person7_ans = "7_Q5.3",
         person8_ans = "8_Q5.3",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         person1_b = person1b,
         person2_b = person2b,
         person3_b = person3b,
         person4_b = person4b,
         person5_b = person5b,
         person6_b = person6b,
         person7_b = person7b,
         person8_b = person8b)

data_SP <- data_SP %>%
  filter(!Q_TerminateFlag %in% c("Screened", "QuotaMet"), Status == "IP Address",
         Progress == "100")

respondi_SP <- data_SP %>%
  select(return_tic)

data_SP <- data_SP %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, RecordedDate, ResponseId,
            RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage,
            return_tic, Q_TerminateFlag, starts_with("seed"), hideFooter,
            ends_with(c("First Click", "Last Click", "Page Submit", "Click Count"))))

#### .Lottery vignette ####

data_SP <- data_SP %>%
  mutate(tvignette = if_else(is.na(Q12.1_1) & !is.na(Q12.2_1), 1,
                             if_else(!is.na(Q12.1_1) & is.na(Q12.2_1), 0, NA_real_)),
         lottery_vignette = as.numeric(coalesce(Q12.1_1, Q12.2_1))) %>%
  select(-c(Q12.1_1, Q12.2_1))

#### .List Experiment ####

data_SP <- data_SP %>%
  mutate(tlist = if_else(is.na(Q15.1) & !is.na(Q15.2), 1,
                         if_else(!is.na(Q15.1) & is.na(Q15.2), 0, NA_real_)),
         list_merged = coalesce(Q15.1, Q15.2)) %>%
  select(-c(Q15.1, Q15.2))


data_SP <- data_SP %>%
  mutate(list_merged = if_else(list_merged == "Cero", 0,
                               if_else(list_merged %in% c("Uno", "Una"), 1, 
                                       if_else(list_merged == "Dos", 2,
                                               if_else(list_merged == "Tres", 3,
                                                       if_else(list_merged == "Cuatro", 4, NA_real_))))))


#### .Person Trade-off questions ####

data_SP <- data_SP %>%
  mutate(group = if_else(!is.na(Q6.1), 1,
                         if_else(!is.na(Q7.1), 2, 
                                 if_else(!is.na(Q8.1), 3, 
                                         if_else(!is.na(Q9.1), 4, NA_real_)))),
         select0 = coalesce(Q6.1, Q7.1, Q8.1, Q9.1),
         select1 = coalesce(Q6.4, Q7.4, Q8.4, Q9.4),
         select2 = coalesce(Q6.5, Q7.5, Q8.5, Q9.5, Q6.6, Q7.6, Q8.6, Q9.6),
         select2a = coalesce(Q6.5, Q7.5, Q8.5, Q9.5),
         select2b = coalesce(Q6.6, Q7.6, Q8.6, Q9.6),
         value_l = if_else(select2a == "Grupo A" & !is.na(select2a), 6,
                           if_else(select2a == "Grupo B" & !is.na(select2a), 3,
                                   if_else(select2b ==  "Grupo A" & !is.na(select2b), 2, 
                                           if_else(select2b == "Grupo B" & !is.na(select2b), 2, NA_real_)))),
         value1 = 3,
         value2 = if_else(select1 == "Grupo A", 6, 
                          if_else(select1 == "Grupo B", 2, NA_real_)),
         response1 = if_else(select1 == "Grupo A", 1, 
                             if_else(select1 == "Grupo B", 0, NA_real_)),
         response2 = if_else(select2 == "Grupo A", 1,
                             if_else(select2 == "Grupo B", 0, NA_real_)))

write_csv(data_SP, "Spain/data_SP.csv")
write_csv(respondi_SP, "Respondi/respondi_SP.csv")

#### Uganda ####

data_UGA <- read_csv(uga_path)

data_UGA <- data_UGA[-c(1:2),]

data_UGA <- data_UGA %>%
  rowid_to_column("id")

data_UGA <- data_UGA %>%
  rename(time_secs = `Duration (in seconds)`,
         age_consent = Q1.2,
         consent = Q1.3)

#### .Demographics and Political Partisanship ####

data_UGA <- data_UGA %>%
  rename(age = Q3.1,
         gender = Q3.2,
         ideology = Q21.1_1,
         REGION_0 = Q3.3_1, 
         REGION_1 = Q3.3_2,
         INCOME = Q22.7,
         HH_INCOME = Q22.8,
         race = Q22.9,
         religion = Q22.10,
         marital_status = Q22.12,
         dep_children = Q22.13,
         dep_children_amount = Q22.14,
         HH_size_adults = Q22.15)

data_UGA <- data_UGA %>%
  mutate(rent_home = if_else(Q22.16 == "Other (please specify in the box below)", Q22.16_4_TEXT, Q22.16)) %>%
  select(-c(Q22.16_4_TEXT, Q22.16))

data_UGA <- data_UGA %>%
  rename(willing_risk = Q10.1_1,
         willing_punish = Q10.2_1,
         return_favour = Q10.3_1,
         donation = Q10.4,
         donation_amount = Q10.4_1_TEXT,
         wtp_access = Q11.1,
         wtp_private = Q11.2,
         wtp_amount_1 = Q11.4,
         wtp_amount_2a = Q11.5,
         wtp_amount_2b = Q11.6,
         int_get_vacc = Q14.1,
         int_lost_job = Q14.4,
         int_perc_job = Q14.5_1,
         int_exp_1 = Q14.6_1,
         int_exp_2 = Q14.6_2,
         int_exp_3 = Q14.6_3,
         int_exp_4 = Q14.6_4,
         int_exp_5 = Q14.6_5,
         int_family_vacc_1 = Q14.7_1,
         int_family_vacc_2 = Q14.7_2,
         int_family_vacc_3 = Q14.7_3,
         int_pol_implem_1 = Q14.8_1,
         int_pol_implem_2 = Q14.8_2,
         int_pol_implem_3 = Q14.8_3,
         int_pol_implem_4 = Q14.8_4,
         int_pol_implem_5 = Q14.8_5,
         int_pol_implem_6 = Q14.8_6,
         int_gov_priority_1 = Q14.9_1,
         int_gov_priority_2 = Q14.9_2,
         int_gov_priority_3 = Q14.9_3,
         int_gov_priority_4 = Q14.9_5,
         int_behaviour_1 = Q14.10_1,
         int_behaviour_2 = Q14.10_2,
         int_behaviour_3 = Q14.10_3,
         hes_general_1 = Q16.1_1,
         hes_general_2 = Q16.1_2,
         hes_general_3 = Q16.1_3,
         hes_covid_1 = Q16.2_1,
         hes_covid_2 = Q16.2_2,
         beh_measure_1 = Q17.1_1,
         beh_measure_2 = Q17.1_2,
         beh_measure_3 = Q17.1_3,
         beh_measure_4 = Q17.1_4,
         beh_measure_5 = Q17.1_5,
         beh_measure_6 = Q17.1_6,
         beh_measure_7 = Q17.1_7,
         beh_measure_8 = Q17.1_8,
         beh_measure_9 = Q17.1_9,
         beh_measure_10 = Q17.1_10,
         beh_measure_11 = Q17.1_11,
         beh_measure_12 = Q17.1_12,
         beh_measure_13 = Q17.1_13,
         beh_measure_14 = Q17.1_14,
         beh_measure_15 = Q17.1_15,
         beh_measure_16 = Q17.1_16,
         beh_measure_17 = Q17.1_17,
         beh_risk = Q17.2_1,
         geq_taxes_0 = Q18.1,
         geq_taxes_1 = Q18.3,
         geq_taxes_2a = Q18.4,
         geq_taxes_2b = Q18.5,
         geq_provision_1 = Q18.6_1,
         geq_provision_2 = Q18.6_2,
         geq_provision_3 = Q18.6_4,
         geq_donation = Q18.8,
         geq_ticket_0 = Q18.9,
         geq_ticket_1 = Q18.11,
         geq_ticket_2a = Q18.12,
         geq_ticket_2b = Q18.13,
         geq_current_spending = Q18.14_1,
         geq_future_spending = Q18.15,
         eq5d_mobility_pre = Q20.3_1,
         eq5d_mobility_post = Q20.3_2,
         eq5d_selfcare_pre = Q20.4_1,
         eq5d_selfcare_post = Q20.4_2,
         eq5d_usual_pre = Q20.5_1,
         eq5d_usual_post = Q20.5_2,
         eq5d_pain_pre = Q20.6_1,
         eq5d_pain_post = Q20.6_2,
         eq5d_anxiety_pre = Q20.7_1,
         eq5d_anxiety_post = Q20.7_2,
         eq5d_scale_pre = Q20.9,
         eq5d_scale_post = Q20.10,
         person1_ans = "1_Q5.3",
         person2_ans = "2_Q5.3",
         person3_ans = "3_Q5.3",
         person4_ans = "4_Q5.3",
         person5_ans = "5_Q5.3",
         person6_ans = "6_Q5.3",
         person7_ans = "7_Q5.3",
         person8_ans = "8_Q5.3",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         person1_b = person1b,
         person2_b = person2b,
         person3_b = person3b,
         person4_b = person4b,
         person5_b = person5b,
         person6_b = person6b,
         person7_b = person7b,
         person8_b = person8b)

data_UGA <- data_UGA %>%
  filter(!Q_TerminateFlag %in% c("Screened"), Status == "IP Address",
         Progress == "100")

data_UGA <- data_UGA %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, RecordedDate, ResponseId,
            RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage,
            ID, Q_TerminateFlag, starts_with("seed"), hideFooter,
            ends_with(c("First Click", "Last Click", "Page Submit", "Click Count"))))

#### .Lottery vignette ####

data_UGA <- data_UGA %>%
  mutate(tvignette = if_else(is.na(Q12.1_1) & !is.na(Q12.2_1), 1,
                             if_else(!is.na(Q12.1_1) & is.na(Q12.2_1), 0, NA_real_)),
         lottery_vignette = as.numeric(coalesce(Q12.1_1, Q12.2_1))) %>%
  select(-c(Q12.1_1, Q12.2_1))

#### .List Experiment ####

data_UGA <- data_UGA %>%
  mutate(tlist = if_else(is.na(Q15.1) & !is.na(Q15.2), 1,
                         if_else(!is.na(Q15.1) & is.na(Q15.2), 0, NA_real_)),
         list_merged = coalesce(Q15.1, Q15.2)) %>%
  select(-c(Q15.1, Q15.2))


data_UGA <- data_UGA %>%
  mutate(list_merged = if_else(list_merged == "Zero", 0,
                               if_else(list_merged == "One", 1, 
                                       if_else(list_merged == "Two", 2,
                                               if_else(list_merged == "Three", 3,
                                                       if_else(list_merged == "Four", 4, NA_real_))))))


#### .Person Trade-off questions ####

data_UGA <- data_UGA %>%
  mutate(group = if_else(!is.na(Q6.1), 1,
                         if_else(!is.na(Q7.1), 2, 
                                 if_else(!is.na(Q8.1), 3, 
                                         if_else(!is.na(Q9.1), 4, NA_real_)))),
         select0 = coalesce(Q6.1, Q7.1, Q8.1, Q9.1),
         select1 = coalesce(Q6.4, Q7.4, Q8.4, Q9.4),
         select2 = coalesce(Q6.5, Q7.5, Q8.5, Q9.5, Q6.6, Q7.6, Q8.6, Q9.6),
         select2a = coalesce(Q6.5, Q7.5, Q8.5, Q9.5),
         select2b = coalesce(Q6.6, Q7.6, Q8.6, Q9.6),
         value_l = if_else(select2a == "Group A" & !is.na(select2a), 6,
                           if_else(select2a == "Group B" & !is.na(select2a), 3,
                                   if_else(select2b ==  "Group A" & !is.na(select2b), 2, 
                                           if_else(select2b == "Group B" & !is.na(select2b), 2, NA_real_)))),
         value1 = 3,
         value2 = if_else(select1 == "Group A", 6, 
                          if_else(select1 == "Group B", 2, NA_real_)),
         response1 = if_else(select1 == "Group A", 1, 
                             if_else(select1 == "Group B", 0, NA_real_)),
         response2 = if_else(select2 == "Group A", 1,
                             if_else(select2 == "Group B", 0, NA_real_)))

write_csv(data_UGA, "Uganda/data_UGA.csv")


#### UK ####

data_UK <- read_csv(uk_path)

data_UK <- data_UK[-c(1:2),]

data_UK <- data_UK %>%
  rowid_to_column("id")

data_UK <- data_UK %>%
  rename(time_secs = `Duration (in seconds)`,
         age_consent = Q1.2,
         consent = Q1.3)

#### .Demographics and Political Partisanship ####

data_UK <- data_UK %>%
  rename(age = Q3.1,
         gender = Q3.2,
         postal_code = Q3.4,
         ideology = Q21.1_1,
         REGION_0 = Q3.3_1, 
         REGION_1 = Q3.3_2,
         INCOME = Q22.7,
         HH_INCOME = Q22.8,
         race = Q22.9,
         religion = Q22.10,
         news = Q22.11,
         marital_status = Q22.12,
         dep_children = Q22.13,
         dep_children_amount = Q22.14,
         HH_size_adults = Q22.15)

data_UK <- data_UK %>%
  mutate(rent_home = if_else(Q22.16 == "Other (please specify in the box below)", Q22.16_4_TEXT, Q22.16)) %>%
  select(-c(Q22.16_4_TEXT, Q22.16))

data_UK <- data_UK %>%
  rename(willing_risk = Q10.1_1,
         willing_punish = Q10.2_1,
         return_favour = Q10.3_1,
         donation = Q10.4,
         donation_amount = Q10.4_1_TEXT,
         wtp_access = Q11.1,
         wtp_private = Q11.2,
         wtp_amount_1 = Q11.4,
         wtp_amount_2a = Q11.5,
         wtp_amount_2b = Q11.6,
         int_get_vacc = Q14.1,
         int_lost_job = Q14.4,
         int_perc_job = Q14.5_1,
         int_exp_1 = Q14.6_1,
         int_exp_2 = Q14.6_2,
         int_exp_3 = Q14.6_3,
         int_exp_4 = Q14.6_4,
         int_exp_5 = Q14.6_5,
         int_family_vacc_1 = Q14.7_1,
         int_family_vacc_2 = Q14.7_2,
         int_family_vacc_3 = Q14.7_3,
         int_pol_implem_1 = Q14.8_1,
         int_pol_implem_2 = Q14.8_2,
         int_pol_implem_3 = Q14.8_3,
         int_pol_implem_4 = Q14.8_4,
         int_pol_implem_5 = Q14.8_5,
         int_pol_implem_ = Q14.8_6,
         int_gov_priority_1 = Q14.9_1,
         int_gov_priority_2 = Q14.9_2,
         int_gov_priority_3 = Q14.9_3,
         int_gov_priority_4 = Q14.9_5,
         int_behaviour_1 = Q14.10_1,
         int_behaviour_2 = Q14.10_2,
         int_behaviour_3 = Q14.10_3,
         hes_general_1 = Q16.1_1,
         hes_general_2 = Q16.1_2,
         hes_general_3 = Q16.1_3,
         hes_covid_1 = Q16.2_1,
         hes_covid_2 = Q16.2_2,
         beh_measure_1 = Q17.1_1,
         beh_measure_2 = Q17.1_2,
         beh_measure_3 = Q17.1_3,
         beh_measure_4 = Q17.1_4,
         beh_measure_5 = Q17.1_5,
         beh_measure_6 = Q17.1_6,
         beh_measure_7 = Q17.1_7,
         beh_measure_8 = Q17.1_8,
         beh_measure_9 = Q17.1_9,
         beh_measure_10 = Q17.1_10,
         beh_measure_11 = Q17.1_11,
         beh_measure_12 = Q17.1_12,
         beh_measure_13 = Q17.1_13,
         beh_measure_14 = Q17.1_14,
         beh_measure_15 = Q17.1_15,
         beh_measure_16 = Q17.1_16,
         beh_measure_17 = Q17.1_17,
         beh_risk = Q17.2_1,
         geq_taxes_0 = Q18.1,
         geq_taxes_1 = Q18.3,
         geq_taxes_2a = Q18.4,
         geq_taxes_2b = Q18.5,
         geq_provision_1 = Q18.6_1,
         geq_provision_2 = Q18.6_2,
         geq_provision_3 = Q18.6_4,
         geq_donation = Q18.7,
         geq_ticket_0 = Q18.9,
         geq_ticket_1 = Q18.11,
         geq_ticket_2a = Q18.12,
         geq_ticket_2b = Q18.13,
         geq_current_spending = Q18.14_1,
         geq_future_spending = Q18.15,
         eq5d_mobility_pre = Q20.3_1,
         eq5d_mobility_post = Q20.3_2,
         eq5d_selfcare_pre = Q20.4_1,
         eq5d_selfcare_post = Q20.4_2,
         eq5d_usual_pre = Q20.5_1,
         eq5d_usual_post = Q20.5_2,
         eq5d_pain_pre = Q20.6_1,
         eq5d_pain_post = Q20.6_2,
         eq5d_anxiety_pre = Q20.7_1,
         eq5d_anxiety_post = Q20.7_2,
         eq5d_scale_pre = Q20.9,
         eq5d_scale_post = Q20.10,
         person1_ans = "1_Q5.3",
         person2_ans = "2_Q5.3",
         person3_ans = "3_Q5.3",
         person4_ans = "4_Q5.3",
         person5_ans = "5_Q5.3",
         person6_ans = "6_Q5.3",
         person7_ans = "7_Q5.3",
         person8_ans = "8_Q5.3",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         person1_b = person1b,
         person2_b = person2b,
         person3_b = person3b,
         person4_b = person4b,
         person5_b = person5b,
         person6_b = person6b,
         person7_b = person7b,
         person8_b = person8b)

data_UK <- data_UK %>%
  filter(!Q_TerminateFlag %in% c("Screened", "QuotaMet"), Status == "IP Address",
         Progress == "100")

respondi_UK <- data_UK %>%
  select(return_tic)

data_UK <- data_UK %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, RecordedDate, ResponseId,
            RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage,
            return_tic, Q_TerminateFlag, starts_with("seed"), hideFooter,
            ends_with(c("First Click", "Last Click", "Page Submit", "Click Count"))))

#### .Lottery vignette ####

data_UK <- data_UK %>%
  mutate(tvignette = if_else(is.na(Q12.1_1) & !is.na(Q12.2_1), 1,
                             if_else(!is.na(Q12.1_1) & is.na(Q12.2_1), 0, NA_real_)),
         lottery_vignette = as.numeric(coalesce(Q12.1_1, Q12.2_1))) %>%
  select(-c(Q12.1_1, Q12.2_1))

#### .List Experiment ####

data_UK <- data_UK %>%
  mutate(tlist = if_else(is.na(Q15.1) & !is.na(Q15.2), 1,
                         if_else(!is.na(Q15.1) & is.na(Q15.2), 0, NA_real_)),
         list_merged = coalesce(Q15.1, Q15.2)) %>%
  select(-c(Q15.1, Q15.2))


data_UK <- data_UK %>%
  mutate(list_merged = if_else(list_merged == "Zero", 0,
                               if_else(list_merged == "One", 1, 
                                       if_else(list_merged == "Two", 2,
                                               if_else(list_merged == "Three", 3,
                                                       if_else(list_merged == "Four", 4, NA_real_))))))


#### .Person Trade-off questions ####

data_UK <- data_UK %>%
  mutate(group = if_else(!is.na(Q6.1), 1,
                         if_else(!is.na(Q7.1), 2, 
                                 if_else(!is.na(Q8.1), 3, 
                                         if_else(!is.na(Q9.1), 4, NA_real_)))),
         select0 = coalesce(Q6.1, Q7.1, Q8.1, Q9.1),
         select1 = coalesce(Q6.4, Q7.4, Q8.4, Q9.4),
         select2 = coalesce(Q6.5, Q7.5, Q8.5, Q9.5, Q6.6, Q7.6, Q8.6, Q9.6),
         select2a = coalesce(Q6.5, Q7.5, Q8.5, Q9.5),
         select2b = coalesce(Q6.6, Q7.6, Q8.6, Q9.6),
         value_l = if_else(select2a == "Group A" & !is.na(select2a), 6,
                           if_else(select2a == "Group B" & !is.na(select2a), 3,
                                   if_else(select2b ==  "Group A" & !is.na(select2b), 2, 
                                           if_else(select2b == "Group B" & !is.na(select2b), 2, NA_real_)))),
         value1 = 3,
         value2 = if_else(select1 == "Group A", 6, 
                          if_else(select1 == "Group B", 2, NA_real_)),
         response1 = if_else(select1 == "Group A", 1, 
                             if_else(select1 == "Group B", 0, NA_real_)),
         response2 = if_else(select2 == "Group A", 1,
                             if_else(select2 == "Group B", 0, NA_real_)))

write_csv(data_UK, "UK/data_UK.csv")
write_csv(respondi_UK, "Respondi/respondi_UK.csv")

#### US ####

data_US <- read_csv(us_path)

data_US <- data_US[-c(1:2),]

data_US <- data_US %>%
  rowid_to_column("id")

data_US <- data_US %>%
  rename(time_secs = `Duration (in seconds)`,
         age_consent = Q1.2,
         consent = Q1.3)

#### .Demographics and Political Partisanship ####

data_US <- data_US %>%
  rename(age = Q3.1,
         gender = Q3.2,
         ideology = Q21.1_1,
         REGION_0 = Q3.3_1, 
         REGION_1 = Q3.3_2,
         postal_code = Q3.4,
         INCOME = Q22.7,
         HH_INCOME = Q22.8,
         race = Q22.10,
         #nationality = Q22.9,
         #religion = Q22.16,
         marital_status = Q22.12,
         dep_children = Q22.13,
         dep_children_amount = Q22.14,
         HH_size_adults = Q22.15)

data_US <- data_US %>%
  mutate(nationality = if_else(Q22.9 == "Yes, Other", Q22.9_7_TEXT, Q22.9),
         religion = if_else(Q22.11 == "Other", Q22.11_35_TEXT, Q22.11)) %>%
  select(-c(Q22.9_7_TEXT, Q22.9, Q22.11_35_TEXT, Q22.11))

data_US <- data_US %>%
  rename(willing_risk = Q10.1_1,
         willing_punish = Q10.2_1,
         return_favour = Q10.3_1,
         donation = Q10.4,
         donation_amount = Q10.4_1_TEXT,
         wtp_access = Q11.1,
         wtp_private = Q11.2,
         wtp_amount_1 = Q11.4,
         wtp_amount_2a = Q11.5,
         wtp_amount_2b = Q11.6,
         int_get_vacc = Q14.1,
         int_lost_job = Q14.4,
         int_perc_job = Q14.5_1,
         int_exp_1 = Q14.6_1,
         int_exp_2 = Q14.6_2,
         int_exp_3 = Q14.6_3,
         int_exp_4 = Q14.6_4,
         int_exp_5 = Q14.6_5,
         int_family_vacc_1 = Q14.7_1,
         int_family_vacc_2 = Q14.7_2,
         int_family_vacc_3 = Q14.7_3,
         int_pol_implem_1 = Q14.8_1,
         int_pol_implem_2 = Q14.8_2,
         int_pol_implem_3 = Q14.8_3,
         int_pol_implem_4 = Q14.8_4,
         int_pol_implem_5 = Q14.8_5,
         int_pol_implem_6 = Q14.8_6,
         int_gov_priority_1 = Q14.9_1,
         int_gov_priority_2 = Q14.9_2,
         int_gov_priority_3 = Q14.9_3,
         int_gov_priority_4 = Q14.9_5,
         int_behaviour_1 = Q14.10_1,
         int_behaviour_2 = Q14.10_2,
         int_behaviour_3 = Q14.10_3,
         hes_general_1 = Q16.1_1,
         hes_general_2 = Q16.1_2,
         hes_general_3 = Q16.1_3,
         hes_covid_1 = Q16.2_1,
         hes_covid_2 = Q16.2_2,
         beh_measure_1 = Q17.1_1,
         beh_measure_2 = Q17.1_2,
         beh_measure_3 = Q17.1_3,
         beh_measure_4 = Q17.1_4,
         beh_measure_5 = Q17.1_5,
         beh_measure_6 = Q17.1_6,
         beh_measure_7 = Q17.1_7,
         beh_measure_8 = Q17.1_8,
         beh_measure_9 = Q17.1_9,
         beh_measure_10 = Q17.1_10,
         beh_measure_11 = Q17.1_11,
         beh_measure_12 = Q17.1_12,
         beh_measure_13 = Q17.1_13,
         beh_measure_14 = Q17.1_14,
         beh_measure_15 = Q17.1_15,
         beh_measure_16 = Q17.1_16,
         beh_measure_17 = Q17.1_17,
         beh_risk = Q17.2_1,
         geq_taxes_0 = Q18.1,
         geq_taxes_1 = Q18.3,
         geq_taxes_2a = Q18.4,
         geq_taxes_2b = Q18.5,
         geq_provision_1 = Q18.6_1,
         geq_provision_2 = Q18.6_2,
         geq_provision_3 = Q18.6_4,
         geq_donation = Q18.7,
         geq_ticket_0 = Q18.9,
         geq_ticket_1 = Q18.11,
         geq_ticket_2a = Q18.12,
         geq_ticket_2b = Q18.13,
         geq_current_spending = Q18.14_1,
         geq_future_spending = Q18.15,
         eq5d_mobility_pre = Q20.3_1,
         eq5d_mobility_post = Q20.3_2,
         eq5d_selfcare_pre = Q20.4_1,
         eq5d_selfcare_post = Q20.4_2,
         eq5d_usual_pre = Q20.5_1,
         eq5d_usual_post = Q20.5_2,
         eq5d_pain_pre = Q20.6_1,
         eq5d_pain_post = Q20.6_2,
         eq5d_anxiety_pre = Q20.7_1,
         eq5d_anxiety_post = Q20.7_2,
         eq5d_scale_pre = Q20.9,
         eq5d_scale_post = Q20.10,
         person1_ans = "1_Q5.3",
         person2_ans = "2_Q5.3",
         person3_ans = "3_Q5.3",
         person4_ans = "4_Q5.3",
         person5_ans = "5_Q5.3",
         person6_ans = "6_Q5.3",
         person7_ans = "7_Q5.3",
         person8_ans = "8_Q5.3",
         person1_a = person1a,
         person2_a = person2a,
         person3_a = person3a,
         person4_a = person4a,
         person5_a = person5a,
         person6_a = person6a,
         person7_a = person7a,
         person8_a = person8a,
         person1_b = person1b,
         person2_b = person2b,
         person3_b = person3b,
         person4_b = person4b,
         person5_b = person5b,
         person6_b = person6b,
         person7_b = person7b,
         person8_b = person8b)

data_US <- data_US %>%
  filter(!Q_TerminateFlag %in% c("Screened", "QuotaMet"), Status == "IP Address",
         Progress == "100")

respondi_US <- data_US %>%
  select(return_tic)

data_US <- data_US %>%
  select(-c(StartDate, EndDate, Status, IPAddress, Progress, Finished, RecordedDate, ResponseId,
            RecipientLastName, RecipientFirstName, RecipientEmail, ExternalReference, 
            LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage,
            return_tic, Q_TerminateFlag, starts_with("seed"), hideFooter,
            ends_with(c("First Click", "Last Click", "Page Submit", "Click Count"))))

#### .Lottery vignette ####

data_US <- data_US %>%
  mutate(tvignette = if_else(is.na(Q12.1_1) & !is.na(Q12.2_1), 1,
                             if_else(!is.na(Q12.1_1) & is.na(Q12.2_1), 0, NA_real_)),
         lottery_vignette = as.numeric(coalesce(Q12.1_1, Q12.2_1))) %>%
  select(-c(Q12.1_1, Q12.2_1))

#### .List Experiment ####

data_US <- data_US %>%
  mutate(tlist = if_else(is.na(Q15.1) & !is.na(Q15.2), 1,
                         if_else(!is.na(Q15.1) & is.na(Q15.2), 0, NA_real_)),
         list_merged = coalesce(Q15.1, Q15.2)) %>%
  select(-c(Q15.1, Q15.2))


data_US <- data_US %>%
  mutate(list_merged = if_else(list_merged == "Zero", 0,
                               if_else(list_merged == "One", 1, 
                                       if_else(list_merged == "Two", 2,
                                               if_else(list_merged == "Three", 3,
                                                       if_else(list_merged == "Four", 4, NA_real_))))))


#### .Person Trade-off questions ####

data_US <- data_US %>%
  mutate(group = if_else(!is.na(Q6.1), 1,
                         if_else(!is.na(Q7.1), 2, 
                                 if_else(!is.na(Q8.1), 3, 
                                         if_else(!is.na(Q9.1), 4, NA_real_)))),
         select0 = coalesce(Q6.1, Q7.1, Q8.1, Q9.1),
         select1 = coalesce(Q6.4, Q7.4, Q8.4, Q9.4),
         select2 = coalesce(Q6.5, Q7.5, Q8.5, Q9.5, Q6.6, Q7.6, Q8.6, Q9.6),
         select2a = coalesce(Q6.5, Q7.5, Q8.5, Q9.5),
         select2b = coalesce(Q6.6, Q7.6, Q8.6, Q9.6),
         value_l = if_else(select2a == "Group A" & !is.na(select2a), 6,
                           if_else(select2a == "Group B" & !is.na(select2a), 3,
                                   if_else(select2b ==  "Group A" & !is.na(select2b), 2, 
                                           if_else(select2b == "Group B" & !is.na(select2b), 2, NA_real_)))),
         value1 = 3,
         value2 = if_else(select1 == "Group A", 6, 
                          if_else(select1 == "Group B", 2, NA_real_)),
         response1 = if_else(select1 == "Group A", 1, 
                             if_else(select1 == "Group B", 0, NA_real_)),
         response2 = if_else(select2 == "Group A", 1,
                             if_else(select2 == "Group B", 0, NA_real_)))

write_csv(data_US, "US/data_US.csv")
write_csv(respondi_US, "Respondi/respondi_US.csv")


#### Copy ####

file.copy(from="Australia/data_AUS.csv", to="Cleaned Data/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="Brazil/data_BR.csv", to="Cleaned Data/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="Canada/data_CAN.csv", to="Cleaned Data/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="Chile/data_CHL.csv", to="Cleaned Data/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="China/data_CHN.csv", to="Cleaned Data/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="Colombia/data_COL.csv", to="Cleaned Data/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="France/data_FR.csv", to="Cleaned Data/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="India/data_IND.csv", to="Cleaned Data/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="Italy/data_IT.csv", to="Cleaned Data/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="Spain/data_SP.csv", to="Cleaned Data/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="Uganda/data_UGA.csv", to="Cleaned Data/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="UK/data_UK.csv", to="Cleaned Data/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

file.copy(from="US/data_US.csv", to="Cleaned Data/", 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)

#### Removing Postal codes ####

data_CAN <- data_CAN %>%
  select(-postal_code)

data_CHL <- data_CHL %>%
  select(-postal_code)

data_COL <- data_COL %>%
  select(-postal_code)

data_FR <- data_FR %>%
  select(-postal_code)

data_IND <- data_IND %>%
  select(-postal_code)

data_IT <- data_IT %>%
  select(-postal_code)

data_UK <- data_UK %>%
  select(-postal_code)

data_US <- data_US %>%
  select(-postal_code)

write_csv(data_CAN, "Cleaned Data/Cleaned Data (without postal codes)/data_CAN.csv")
write_csv(data_CHL, "Cleaned Data/Cleaned Data (without postal codes)/data_CHL.csv")
write_csv(data_COL, "Cleaned Data/Cleaned Data (without postal codes)/data_COL.csv")
write_csv(data_FR, "Cleaned Data/Cleaned Data (without postal codes)/data_FR.csv")
write_csv(data_IND, "Cleaned Data/Cleaned Data (without postal codes)/data_IND.csv")
write_csv(data_IT, "Cleaned Data/Cleaned Data (without postal codes)/data_IT.csv")
write_csv(data_UK, "Cleaned Data/Cleaned Data (without postal codes)/data_UK.csv")
write_csv(data_US, "Cleaned Data/Cleaned Data (without postal codes)/data_US.csv")
