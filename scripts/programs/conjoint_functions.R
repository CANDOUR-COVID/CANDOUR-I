# REPLICATION
# Recode data so it can be combined across countries 
recode_for_conjoint <- function(data) {
  
  data <- data %>%
    
    select(id, country, age, gender, ideology, ind_inc, education, hes_covid_2,
           wtp_access, wtp_private, int_pol_implem_6, starts_with("person")) %>% 
    
    pivot_longer(-c(id, country, age, gender, ideology, ind_inc, education, hes_covid_2,
                    wtp_access, wtp_private, int_pol_implem_6), names_to = c("person", ".value"), 
                 names_sep = "_", values_drop_na = TRUE) %>%
    
    pivot_longer(-c(id, country, age, gender, ideology, ind_inc, education, hes_covid_2,
                    wtp_access, wtp_private, int_pol_implem_6, person, ans), names_to = "candidate", 
                 values_to = "aux2") %>%
    
    separate(aux2, c("vulnerability", "transmission", "income", "occupation", "age_category"), 
             "\\|")
  
  data$candidate <- data$candidate %>%
    recode("a" = "A", "b" = "B")
  
  data$ans <- ifelse(str_detect(data$ans, "A"),"A",
                     ifelse(str_detect(data$ans, "B"),"B",NA))
  
  # Recode data levels
  data <- data %>%
    mutate(select = if_else(ans == candidate, 1, 0),
           id = paste0(country,"_",id)) %>% 
    
    # Vulnerability
    # French
    mutate(vulnerability = ifelse(str_detect(vulnerability,"Risque moyen"),
                                  "Average risk of COVID-19 death",
                                  vulnerability),
           vulnerability = ifelse(str_detect(vulnerability,"deux"), 
                                  "Moderate (Twice the average risk of COVID-19 death)", 
                                  vulnerability),
           vulnerability = ifelse(str_detect(vulnerability,"cinq"), 
                                  "High (Five times the average risk of COVID-19 death)", 
                                  vulnerability)) %>% 
    
    # Spanish
    mutate(vulnerability = ifelse(str_detect(vulnerability,"Riesgo promedio"),
                                  "Average risk of COVID-19 death",
                                  vulnerability),
           vulnerability = ifelse(str_detect(vulnerability,"Dos"), 
                                  "Moderate (Twice the average risk of COVID-19 death)", 
                                  vulnerability),
           vulnerability = ifelse(str_detect(vulnerability,"Cinco"), 
                                  "High (Five times the average risk of COVID-19 death)", 
                                  vulnerability)) %>% 
    
    # Italian
    mutate(vulnerability = ifelse(str_detect(vulnerability,"Rischio nella"),
                                  "Average risk of COVID-19 death",
                                  vulnerability),
           vulnerability = ifelse(str_detect(vulnerability,"due volte"), 
                                  "Moderate (Twice the average risk of COVID-19 death)", 
                                  vulnerability),
           vulnerability = ifelse(str_detect(vulnerability,"cinque volte"), 
                                  "High (Five times the average risk of COVID-19 death)", 
                                  vulnerability)) %>% 
    
    # Chinese
    mutate(vulnerability = ifelse(str_detect(vulnerability,"低"),
                                  "Average risk of COVID-19 death",
                                  vulnerability),
           vulnerability = ifelse(str_detect(vulnerability,"平均死亡风险高一倍"), 
                                  "Moderate (Twice the average risk of COVID-19 death)", 
                                  vulnerability),
           vulnerability = ifelse(str_detect(vulnerability,"平均死亡风险高五倍"), 
                                  "High (Five times the average risk of COVID-19 death)", 
                                  vulnerability)) %>% 
    # Portuguese (Brazil)
    mutate(vulnerability = ifelse(str_detect(vulnerability,"Risco m"),
                                  "Average risk of COVID-19 death",
                                  vulnerability),
           vulnerability = ifelse(str_detect(vulnerability,"Moderado"), 
                                  "Moderate (Twice the average risk of COVID-19 death)", 
                                  vulnerability),
           vulnerability = ifelse(str_detect(vulnerability,"Alto"), 
                                  "High (Five times the average risk of COVID-19 death)", 
                                  vulnerability)) %>% 
    
    mutate(vulnerability = factor(vulnerability,
                                  levels = c("Average risk of COVID-19 death",
                                             "Moderate (Twice the average risk of COVID-19 death)",
                                             "High (Five times the average risk of COVID-19 death)"))) %>% 
    
    #Transmission
    # French
    mutate(transmission = ifelse(str_detect(transmission, "Risque moyen"),
                                 "Average risk of catching and transmitting the COVID-19 virus", 
                                 transmission),
           transmission = ifelse(str_detect(transmission, "deux"),
                                 "Moderate risk (Twice the average risk of catching and transmitting the COVID-19 virus)", 
                                 transmission),
           transmission = ifelse(str_detect(transmission, "cinq"),
                                 "High risk (Five times the average risk of catching and transmitting the COVID-19 virus)", 
                                 transmission)) %>% 
    
    # Spanish
    mutate(transmission = ifelse(str_detect(transmission, "Riesgo promedio de contraer y transmitir el virus COVID-19"),
                                 "Average risk of catching and transmitting the COVID-19 virus", 
                                 transmission),
           transmission = ifelse(str_detect(transmission, "dos veces"),
                                 "Moderate risk (Twice the average risk of catching and transmitting the COVID-19 virus)", 
                                 transmission),
           transmission = ifelse(str_detect(transmission, "cinco veces"),
                                 "High risk (Five times the average risk of catching and transmitting the COVID-19 virus)", 
                                 transmission)) %>% 
    
    # Italian
    mutate(transmission = ifelse(str_detect(transmission, "Rischio nella"),
                                 "Average risk of catching and transmitting the COVID-19 virus", 
                                 transmission),
           transmission = ifelse(str_detect(transmission, "due volte"),
                                 "Moderate risk (Twice the average risk of catching and transmitting the COVID-19 virus)", 
                                 transmission),
           transmission = ifelse(str_detect(transmission, "cinque volte"),
                                 "High risk (Five times the average risk of catching and transmitting the COVID-19 virus)", 
                                 transmission)) %>% 
    
    # Chinese
    mutate(transmission = ifelse(str_detect(transmission, "感染和传播新冠肺炎病毒的风险"),
                                 "Average risk of catching and transmitting the COVID-19 virus", 
                                 transmission),
           transmission = ifelse(str_detect(transmission, "是感染和传播新冠病毒平均风险的两倍"),
                                 "Moderate risk (Twice the average risk of catching and transmitting the COVID-19 virus)", 
                                 transmission),
           transmission = ifelse(str_detect(transmission, "是感染和传播新冠病毒平均风险的五倍"),
                                 "High risk (Five times the average risk of catching and transmitting the COVID-19 virus)", 
                                 transmission)) %>% 
    
    # Portuguese
    mutate(transmission = ifelse(str_detect(transmission, "Risco médio"),
                                 "Average risk of catching and transmitting the COVID-19 virus", 
                                 transmission),
           transmission = ifelse(str_detect(transmission, "duas vezes"),
                                 "Moderate risk (Twice the average risk of catching and transmitting the COVID-19 virus)", 
                                 transmission),
           transmission = ifelse(str_detect(transmission, "cinco vezes"),
                                 "High risk (Five times the average risk of catching and transmitting the COVID-19 virus)", 
                                 transmission)) %>%
    
    mutate(transmission = factor(transmission, 
                                 levels = c("Average risk of catching and transmitting the COVID-19 virus",
                                            "Moderate risk (Twice the average risk of catching and transmitting the COVID-19 virus)",
                                            "High risk (Five times the average risk of catching and transmitting the COVID-19 virus)"))) %>% 
    
    # Income
    # French
    mutate(income = ifelse(str_detect(income, "moyen"),
                           "Average income level", 
                           income),
           income = ifelse(str_detect(income, "bas"),
                           "Lowest 20% income level",
                           income),
           income = ifelse(str_detect(income, "élevés"),
                           "Highest 20% income level",
                           income)) %>% 
    # Spanish
    mutate(income = ifelse(str_detect(income, "ingresos promedio"),
                           "Average income level", 
                           income),
           income = ifelse(str_detect(income, "bajo"),
                           "Lowest 20% income level",
                           income),
           income = ifelse(str_detect(income, "alto"),
                           "Highest 20% income level",
                           income)) %>% 
    
    # Italian
    mutate(income = ifelse(str_detect(income, "Medio"),
                           "Average income level", 
                           income),
           income = ifelse(str_detect(income, "Basso"),
                           "Lowest 20% income level",
                           income),
           income = ifelse(str_detect(income, "Alto"),
                           "Highest 20% income level",
                           income)) %>% 
    
    # Chinese
    mutate(income = ifelse(str_detect(income, "平均收入水平"),
                           "Average income level", 
                           income),
           income = ifelse(str_detect(income, "最低的20%收入水平"),
                           "Lowest 20% income level",
                           income),
           income = ifelse(str_detect(income, "最高的20%收入水平"),
                           "Highest 20% income level",
                           income)) %>% 
    
    # Portuguese
    mutate(income = ifelse(str_detect(income, "médio"),
                           "Average income level", 
                           income),
           income = ifelse(str_detect(income, "baixo"),
                           "Lowest 20% income level",
                           income),
           income = ifelse(str_detect(income, "alto"),
                           "Highest 20% income level",
                           income)) %>% 
    
    mutate(income = factor(income,
                           levels = c("Lowest 20% income level",
                                      "Average income level",
                                      "Highest 20% income level"))) %>% 
    
    # Occupation
    # French
    mutate(occupation = ifelse(str_detect(occupation, "l'eau"),
                               "Key worker: Water and electricity service",
                               occupation),
           occupation = ifelse(str_detect(occupation, "Ne peut pas"),
                               "Non-Key worker: Cannot work at home",
                               occupation),
           occupation = ifelse(str_detect(occupation, "garde"),
                               "Key worker: Education and childcare",
                               occupation),
           occupation = ifelse(str_detect(occupation, "d'usine"),
                               "Key worker: Factory worker",
                               occupation),
           occupation = ifelse(str_detect(occupation, "aide sociale"),
                               "Key worker: Health and social care",
                               occupation),
           occupation = ifelse(str_detect(occupation, "Peut travailler"),
                               "Non-Key worker: Can work at home",
                               occupation),
           occupation = ifelse(str_detect(occupation, "lutte contre"),
                               "Key worker: Police and fire-fighting",
                               occupation),
           occupation = ifelse(str_detect(occupation, "Ne travaille pas"),
                               "Not working",
                               occupation)) %>% 
    # Spanish
    mutate(occupation = ifelse(str_detect(occupation, "agua"),
                               "Key worker: Water and electricity service",
                               occupation),
           occupation = ifelse(str_detect(occupation, "No puede trabajar"),
                               "Non-Key worker: Cannot work at home",
                               occupation),
           occupation = ifelse(str_detect(occupation, "cuidado infantil"),
                               "Key worker: Education and childcare",
                               occupation),
           occupation = ifelse(str_detect(occupation, "Trabajador de f"),
                               "Key worker: Factory worker",
                               occupation),
           occupation = ifelse(str_detect(occupation, "trabajadores sociales"),
                               "Key worker: Health and social care",
                               occupation),
           occupation = ifelse(str_detect(occupation, "esencial: Puede trabajar"),
                               "Non-Key worker: Can work at home",
                               occupation),
           occupation = ifelse(str_detect(occupation, "bomberos"),
                               "Key worker: Police and fire-fighting",
                               occupation),
           occupation = ifelse(str_detect(occupation, "Sin trabajo"),
                               "Not working",
                               occupation)) %>% 
    
    # Italian
    mutate(occupation = ifelse(str_detect(occupation, "idrico"),
                               "Key worker: Water and electricity service",
                               occupation),
           occupation = ifelse(str_detect(occupation, "che non pu"),
                               "Non-Key worker: Cannot work at home",
                               occupation),
           occupation = ifelse(str_detect(occupation, "infanzia"),
                               "Key worker: Education and childcare",
                               occupation),
           occupation = ifelse(str_detect(occupation, "fabbrica"),
                               "Key worker: Factory worker",
                               occupation),
           occupation = ifelse(str_detect(occupation, "assistenza sociale"),
                               "Key worker: Health and social care",
                               occupation),
           occupation = ifelse(str_detect(occupation, "che pu"),
                               "Non-Key worker: Can work at home",
                               occupation),
           occupation = ifelse(str_detect(occupation, "Polizia"),
                               "Key worker: Police and fire-fighting",
                               occupation),
           occupation = ifelse(str_detect(occupation, "Disoccupato"),
                               "Not working",
                               occupation)) %>% 
    
    # Chinese
    mutate(occupation = ifelse(str_detect(occupation, "关键工作者: 水电服务"),
                               "Key worker: Water and electricity service",
                               occupation),
           occupation = ifelse(str_detect(occupation, "非关键工作者: 无法在家工作"),
                               "Non-Key worker: Cannot work at home",
                               occupation),
           occupation = ifelse(str_detect(occupation, "关键工作者: 教育与育儿"),
                               "Key worker: Education and childcare",
                               occupation),
           occupation = ifelse(str_detect(occupation, "关键工作者: 工厂员工"),
                               "Key worker: Factory worker",
                               occupation),
           occupation = ifelse(str_detect(occupation, "关键工作者: 健康和社会保健"),
                               "Key worker: Health and social care",
                               occupation),
           occupation = ifelse(str_detect(occupation, "非关键工作者: 可以在家工作"),
                               "Non-Key worker: Can work at home",
                               occupation),
           occupation = ifelse(str_detect(occupation, "关键工作者: 警察与消防"),
                               "Key worker: Police and fire-fighting",
                               occupation),
           occupation = ifelse(str_detect(occupation, "无业"),
                               "Not working",
                               occupation)) %>% 
    
    # Portuguese
    mutate(occupation = ifelse(str_detect(occupation, "eletricidade"),
                               "Key worker: Water and electricity service",
                               occupation),
           occupation = ifelse(str_detect(occupation, "o pode trabalhar"),
                               "Non-Key worker: Cannot work at home",
                               occupation),
           occupation = ifelse(str_detect(occupation, "infantis"),
                               "Key worker: Education and childcare",
                               occupation),
           occupation = ifelse(str_detect(occupation, "Fabricantes"),
                               "Key worker: Factory worker",
                               occupation),
           occupation = ifelse(str_detect(occupation, "social"),
                               "Key worker: Health and social care",
                               occupation),
           occupation = ifelse(str_detect(occupation, "Pode trabalhar"),
                               "Non-Key worker: Can work at home",
                               occupation),
           occupation = ifelse(str_detect(occupation, "bombeiros"),
                               "Key worker: Police and fire-fighting",
                               occupation),
           occupation = ifelse(str_detect(occupation, "Sem trabalhar"),
                               "Not working",
                               occupation)) %>% 
    
    mutate(occupation = factor(occupation,
                               levels = c("Not working",
                                          "Non-Key worker: Can work at home",
                                          "Non-Key worker: Cannot work at home",
                                          "Key worker: Education and childcare",
                                          "Key worker: Factory worker",
                                          "Key worker: Water and electricity service",
                                          "Key worker: Police and fire-fighting",
                                          "Key worker: Health and social care"))) %>% 
    
    # Age
    
    mutate(age_category = gsub("ans","years old", age_category)) %>% # French
    mutate(age_category = gsub("a.*os", "years old", age_category)) %>% # Spanish & Portuguese
    mutate(age_category = gsub("anni", "years old", age_category)) %>% # Italian
    mutate(age_category = gsub("岁", " years old", age_category)) %>% # Chinese
    mutate(age_category = factor(age_category,
                                 levels = c("25 years old",
                                            "40 years old",
                                            "65 years old",
                                            "79 years old")))
  
  return(data)
  
}

# Code relative household income
recode_inc <- function(data, ccode) {
  
  ## WORK IN PROGRESS -- NOT USED IN ANALYSIS ##
  if (ccode == "aus") {
    
    lower_incomes <- c("Negative income","Nil income", "$1-$149 ($1-$7,799)","$150-$299 ($7,800-$15,599)",
                       "$300-$399 ($15,600-$20,799)", "$400-$499 ($20,800-$25,999)", "$500-$649 ($26,000-$33,799)", 
                       "$650-$799 ($33,800-$41,599)", "$800-$999 ($41,600-$51,999)") # Inflation adjusted is 51,398
    
    higher_incomes <- c("$1,000-$1,249 ($52,000-$64,999)","$1,250-$1,499 ($65,000-$77,999)",
                        "$1,500-$1,749 ($78,000-$90,999)", "$1,750-$1,999 ($91,000-$103,999)",
                        "$2,000-$2,999 ($104,000-$155,999)", "$3,000 or more ($156,000 or more)")
    data <- data %>% 
      mutate(ind_inc = case_when(Q22.10 %in% lower_incomes ~ "Low",
                                 Q22.10 %in% higher_incomes ~ "High"))
    
    low_educ <- c("Year 12","Year 11","Year 10",
                  "Certificate II","Certificate I",
                  "Year 9","Year 8 or below",
                  "No educational attainment")
    
    med_educ <- c("Advanced Diploma and Diploma Level",
                  "Advanced Diploma",
                  "Associate Degree",
                  "Diploma",
                  "Certificate III & IV Level",
                  "Certificate IV",
                  "Certificate III")
    
    high_educ <- c("Doctoral Degree Level",
                   "Higher Doctorate",
                   "Professional Specialist Qualification, Doctoral Level",
                   "Master Degree Level",
                   "Graduate Diploma",
                   "Graduate Certificate",
                   "Bachelor Degree Level")
    
    data$education <- case_when(data$Q22.4 %in% low_educ ~ "Low",
                                data$Q22.4 %in% med_educ ~ "Medium",
                                data$Q22.4 %in% high_educ ~ "High")
    
    
  } else if (ccode == "bra") {
    
    lower_incomes <- c("Menos de R$ 523 por mês", "R$ 523 a R$ 1.045 por mês",
                       "R$ 1.045 a R$ 2.090 por mês") # Inflation adjusted is 1.344
    
    higher_incomes <- c("R$ 2.090 a R$ 3.135 por mês",
                        "R$ 3.135 a R$ 5.225 por mês",
                        "R$ 5.225 a R$ 10.450 por mês",
                        "R$ 10.450 a R$ 20.900 por mês",
                        "R$ 20.900 ou mais")
    data <- data %>% 
      mutate(ind_inc = case_when(Q22.8 %in% lower_incomes ~ "Low",
                                 Q22.8 %in% higher_incomes ~ "High"))
    
    low_educ <- c("Creche, Pré-escolar (Maternal e Jardim de Infância), Classe de alfabetização - CA",
                  "Alfabetização de Jovens e Adultos",
                  "Antigo Primário (Elementar)",
                  "Antigo Ginásio (Médio 1º Ciclo)",
                  "Regular do Ensino Fundamental ou 1º Grau (da 1aa 3a série/ do 1º ao 4º ano)",
                  "Regular do Ensino Fundamental ou  1º Grau (da 4a série/5º ano)",
                  "Regular do Ensino Fundamental ou 1ºGrau (da 5aa 8asérie/ do 6º ao 9º ano)",
                  "Supletivo do Ensino Fundamental ou do 1ºGrau")
    
    med_educ <- c("Antigo Científico, Clássico, etc. (Médio 2o ciclo)",
                  "Regular ou Supletivo do Ensino Médio ou do 2º Grau")
    
    high_educ <- c("Superior de Graduação",
                   "Especialização de Nível Superior (mínimo de 360 horas)",
                   "Mestrado",
                   "Doutorado")
    
    data$education <- case_when(data$Q22.2 %in% low_educ ~ "Low",
                                data$Q22.2 %in% med_educ ~ "Medium",
                                data$Q22.2 %in% high_educ ~ "High")
    
    data$hes_covid_2 <- case_when(data$hes_covid_2 == "Concordo Totalmente" ~ "Strongly agree",
                                  data$hes_covid_2 == "Concordo" ~ "Agree",
                                  data$hes_covid_2 == "Não concordo nem discordo" ~ "Neither agree nor disagree",
                                  data$hes_covid_2 == "Discordo" ~ "Disagree",
                                  data$hes_covid_2 == "Discordo Totalmente" ~ "Strongly disagree",
                                  data$hes_covid_2 == "Não sei" ~ "Do not know")
    
    data$wtp_access <- case_when(data$wtp_access == "Vacinas disponibilizadas somente pelo governo a um preço baixo ou gratuitamente?"
                                  ~ "Vaccines only made available by government at low or no cost",
                                 data$wtp_access == "Vacinas disponibilizadas somente para aquisição particular?"
                                  ~ "Vaccines are only available for private purchase",
                                 data$wtp_access == "Vacinas disponibilizadas pelo governo, mas que os cidadãos possam pagar por conta própria para obterem acesso?"
                                  ~ "Vaccines made available by government but citizens can pay privately to gain access",
                                 data$wtp_access == "Não sei" ~ "Do not know")
    
    data$wtp_private <-case_when(data$wtp_private == "Sim" ~ "Yes",
                                 data$wtp_private == "Não" ~ "No",
                                 data$wtp_private == "Não sei" ~ "Do not know")
  } else if (ccode == "can") {
    
    lower_incomes <- c("Under $2,000","$2,000 to $4,999", "$5,000 to $6,999",
                       "$7,000 to $9,999","$10,000 to $11,999","$12,000 to $14,999",
                       "$15,000 to $16,999","$17,000 to $19,999","$20,000 to $24,999",
                       "$25,000 to $29,999","$30,000 to $34,999","$35,000 to $39,999",
                       "$40,000 to $44,999") # Inflation adjusted is 43,467
    
    higher_incomes <- c("$250,000 and over","$200,000 to $249,999","$175,000 to $199,999","$150,000 to $174,999",
                        "$135,000 to $149,999","$120,000 to $134,999","$110,000 to $119,999","$100,000 to $109,999",
                        "$95,000 to $99,999","$90,000 to $94,999","$85,000 to $89,999","$80,000 to $84,999",
                        "$75,000 to $79,999","$70,000 to $74,999","$65,000 to $69,999","$60,000 to $64,999",
                        "$55,000 to $59,999","$50,000 to $54,999","$45,000 to $49,999")
    data <- data %>% 
      mutate(ind_inc = case_when(Q22.7 %in% lower_incomes ~ "Low",
                                 Q22.7 %in% higher_incomes ~ "High"))
    
    low_educ <- c("No certificate, diploma or degree")
    
    med_educ <- c("Secondary (high) school diploma or equivalency certificate",
                  "Trades certificate or diploma other than Certificate of Apprenticeship or Certificate of Qualification",
                  "Certificate of Apprenticeship or Certificate of Qualification",
                  "Program of 3 months or less than 1 year (College, CEGEP or other non-university certificate or diploma from a program of 3 months to less than 1 year)",
                  "Program of 1 to 2 years (College, CEGEP or other non-university certificate or diploma from a program of 1 year to 2 years)",
                  "Program of more than 2 years (College, CEGEP or other non-university certificate or diploma from a program of more than 2 years)",
                  "University certificate or diploma below bachelor level")
    
    high_educ <- c("Bachelor's degree",
                   "University certificate or diploma above bachelor level",
                   "Degree in medicine, dentistry, ceterinary medicine or optometry",
                   "Master's degree",
                   "Earned doctorate")
    
    data$education <- case_when(data$Q22.2 %in% low_educ ~ "Low",
                                data$Q22.2 %in% med_educ ~ "Medium",
                                data$Q22.2 %in% high_educ ~ "High")
    
    data$wtp_access <- case_when(data$wtp_access == "Vaccines only made available by government at low or no cost?"
                                 ~ "Vaccines only made available by government at low or no cost",
                                 data$wtp_access == "Vaccines are only available for private purchase?"
                                 ~ "Vaccines are only available for private purchase",
                                 data$wtp_access == "Vaccines made available by government but citizens can pay privately to gain access?"
                                 ~ "Vaccines made available by government but citizens can pay privately to gain access",
                                 data$wtp_access == "Do not know"
                                 ~ "Do not know")
    
  } else if (ccode == "chl") {
    
    lower_incomes <- c("$ 0 a $35.000",
                       "$ 35.001 a $60.000",
                       "$ 60.001 a $100.000",
                       "$ 100.001 a $ 200.000",
                       "$ 200.001 a $ 350.000",
                       "$ 350.001 a $ 500.000") # Inflation adjusted estimate = 372,836
    
    higher_incomes <- c("$ 500.001 a $ 750.000",
                        "$ 750.001 a $ 1.000.000",
                        "$ 1.000.001 a $ 1.500.000",
                        "$ 1.500.001 a $ 2.000.000",
                        "$ 2.000.001 a $ 3.000.000",
                        "$ 3.000.001 a $ 5.000.000",
                        "$ 5.000.001 a $ 7.500.000",
                        "$ 7.500.001 a $ 10.000.000", 
                        "$ 10.000.001 a $ 15.000.000", 
                        "$ 15.000.001 a $ 20.000.000", 
                        "Más de $ 20.000.000")
    data <- data %>% 
      mutate(ind_inc = case_when(Q22.13 %in% lower_incomes ~ "Low",
                                 Q22.13 %in% higher_incomes ~ "High"))
    
    low_educ <- c("Nunca asistió",
                  "Educación Especial o Diferencial",
                  "Especial o Diferencial",
                  "Educación Básica o Primaria",
                  "Educación Básica",
                  "Primaria o Preparatoria (Sistema antiguo)")
    
    med_educ <- c("Científico-Humanista",
                  "Técnica Profesional",
                  "Humanidades (Sistema antiguo)",
                  "Educación Superior",
                  "Técnico Nivel Superior (carreras 1-3 años)"
    )
    
    high_educ <- c("Profesional (carreras 1-4 años)",
                   "Magíster",
                   "Doctorado")
    
    data$education <- case_when(data$Q22.4 %in% low_educ ~ "Low",
                                data$Q22.4 %in% med_educ ~ "Medium",
                                data$Q22.4 %in% high_educ ~ "High")
    
    data$hes_covid_2 <- case_when(data$hes_covid_2 == "Totalmente de acuerdo" ~ "Strongly agree",
                                  data$hes_covid_2 == "De acuerdo" ~ "Agree",
                                  data$hes_covid_2 == "Ni de acuerdo ni en desacuerdo" ~ "Neither agree nor disagree",
                                  data$hes_covid_2 == "Desacuerdo" ~ "Disagree",
                                  data$hes_covid_2 == "Totalmente en desacuerdo" ~ "Strongly disagree",
                                  data$hes_covid_2 == "No lo sé" ~ "Do not know")
    
    data$wtp_access <- case_when(data$wtp_access == "Vacunas disponibles solo a través del gobierno a bajo costo o sin costo alguno?"
                                  ~ "Vaccines only made available by government at low or no cost",
                                 data$wtp_access == "Vacunas disponibles solo a través de distribuidores privados?"
                                  ~ "Vaccines are only available for private purchase",
                                 data$wtp_access == "Vacunas disponibles a través del gobierno, pero que ciudadanos puedan pagar de forma privada para obtener acceso?"
                                  ~ "Vaccines made available by government but citizens can pay privately to gain access",
                                 data$wtp_access == "No lo sé" ~ "Do not know")
    
    data$wtp_private <- case_when(data$wtp_private == "Sí" ~ "Yes",
                                  data$wtp_private == "No" ~ "No",
                                  data$wtp_private == "No lo sé" ~ "Do not know")
    
  } else if (ccode == "chn") {
    
    lower_incomes <- c("每年收入低于 42,500 元")
    
    higher_incomes <- c("每年收入处于42,500和84,999元之间",
                        "每年收入处于 85,000 元和 127,499 元之间",
                        "​每年收入处于 127,500 元和 169,999 元之间",
                        "​每年收入处于 170,000 元和 212,499 元之间",
                        "每年收入处于 212,500 元和 254,999 元之间",
                        "每年收入处于 255,000 元和 297,499 元之间",
                        "每年收入处于297,500 元和 339,999 元之间",
                        "每年收入处于340,000元 和 382,499元之间",
                        "每年收入处于382,500元和 424,999元之间",
                        "每年收入处于425,000元和 509,999元之间",
                        "每年收入处于510,000元和 594,999元之间",
                        "每年收入处于 595,000元和 849,999元之间",
                        "每年收入处于850,000元和 1274,999元之间",
                        "每年收入高于 127,5000 元")
    data <- data %>% 
      mutate(ind_inc = case_when(Q22.9 %in% lower_incomes ~ "Low",
                                 Q22.9 %in% higher_incomes ~ "High"))
    
    low_educ <- c("初中",
                  "小学",
                  "​从未接受过教育课程（包括非正式教育，如识字课程）")
    
    med_educ <- c("高中",
                  "高职院校",
                  "职业高中/技术学校",
                  "中专")
    
    high_educ <- c("本科（学士学位）",
                   "研究生（硕士学位及以上）")
    
    data$education <- case_when(data$Q22.2 %in% low_educ ~ "Low",
                                data$Q22.2 %in% med_educ ~ "Medium",
                                data$Q22.2 %in% high_educ ~ "High")
    
    data$hes_covid_2 <- case_when(data$hes_covid_2 == "强烈赞同" ~ "Strongly agree",
                                  data$hes_covid_2 == "赞同" ~ "Agree",
                                  data$hes_covid_2 == "既不赞同，亦不反对" ~ "Neither agree nor disagree",
                                  data$hes_covid_2 == "反对" ~ "Disagree",
                                  data$hes_covid_2 == "强烈反对" ~ "Strongly disagree",
                                  data$hes_covid_2 == "不知道" ~ "Do not know")
    
    data$wtp_access <- case_when(data$wtp_access == "仅由政府低价或免费提供的疫苗？"
                                  ~ "Vaccines only made available by government at low or no cost",
                                 data$wtp_access == "​仅供私人购买的疫苗？"
                                  ~ "Vaccines are only available for private purchase",
                                 data$wtp_access == "​由政府提供，但可私下购买的疫苗？"
                                  ~ "Vaccines made available by government but citizens can pay privately to gain access",
                                 data$wtp_access == "不知道" ~ "Do not know")
    
    data$wtp_private <- case_when(data$wtp_private == "会" ~ "Yes",
                                  data$wtp_private == "不会" ~ "No",
                                  data$wtp_private == "不知道" ~ "Do not know")
    
    
  } else if (ccode == "col") {
    
    lower_incomes <- c("Menos de $260.000",
                       "Más de $260.000 y menos de $570.000",  
                       "Más de $570.000 y menos de $810.000",
                       "Más de $810.000 y menos de $1.030.000",
                       "Más de $1.030.000 y menos de $1.270.000") # Estimte is 1,178,300
    
    higher_incomes <- c("Más de $1.270.000 y menos de $1.580.000",
                        "Más de $1.580.000 y menos de $1.980.000",
                        "Más de $1.980.000 y menos de $2.540.000",
                        "Más de $2.540.000 y menos de $3.560.000",
                        "Más de $3.560.000")
    data <- data %>% 
      mutate(ind_inc = case_when(Q22.9 %in% lower_incomes ~ "Low",
                                 Q22.9 %in% higher_incomes ~ "High"))
    
    low_educ <- c("Ninguno","Preescolar",
                  "Básica primaria (1.°-5.°)",
                  "Básica secundaria (Bachillerato básico, 6.°-9.°)")
    
    med_educ <- c("Media académica o clásica (Bachillerato clásico, 10.°-13.°)",
                  "Media técnica (Bachillerato técnico)",
                  "Normalista",
                  "Técnica Profesional",
                  "Tecnológica")
    
    high_educ <- c("Universitario",
                   "Especialización",
                   "Maestría",
                   "Doctorado")
    
    data$education <- case_when(data$Q22.3 %in% low_educ ~ "Low",
                                data$Q22.3 %in% med_educ ~ "Medium",
                                data$Q22.3 %in% high_educ ~ "High")
    
    data$hes_covid_2 <- case_when(data$hes_covid_2 == "Totalmente de acuerdo" ~ "Strongly agree",
                                  data$hes_covid_2 == "De acuerdo" ~ "Agree",
                                  data$hes_covid_2 == "Ni de acuerdo ni en desacuerdo" ~ "Neither agree nor disagree",
                                  data$hes_covid_2 == "Desacuerdo" ~ "Disagree",
                                  data$hes_covid_2 == "Totalmente en desacuerdo" ~ "Strongly disagree",
                                  data$hes_covid_2 == "No lo sé" ~ "Do not know")
    
    data$wtp_access <- case_when(data$wtp_access == "Vacunas disponibles solo a través del gobierno a bajo costo o sin costo alguno?"
                                 ~ "Vaccines only made available by government at low or no cost",
                                 data$wtp_access == "Vacunas disponibles solo a través de distribuidores privados?"
                                  ~ "Vaccines are only available for private purchase",
                                 data$wtp_access == "Vacunas disponibles a través del gobierno, pero que ciudadanos puedan pagar de forma privada para obtener acceso?"
                                  ~ "Vaccines made available by government but citizens can pay privately to gain access",
                                 data$wtp_access == "No lo sé" ~ "Do not know")
    
    data$wtp_private <-case_when(data$wtp_private == "Sí" ~ "Yes",
                                 data$wtp_private == "No" ~ "No",
                                 data$wtp_private == "No lo sé" ~ "Do not know")
    
  } else if (ccode == "fra") {
    
    lower_incomes <- c("Moins de 500 €",
                       "Plus de 500 mais moins de 1.000 €", 
                       "Plus de 1.000 mais moins de 1.500 €", 
                       "Plus de 1.500 mais moins de 2.000 €") # Inflation adjusted estimated is 1856 
    
    higher_incomes <- c("Plus de 2.000 mais moins de 2.500 €",
                        "Plus de 2.500 mais moins de 3.000 €", 
                        "Plus de 3.000 €")
    data <- data %>% 
      mutate(ind_inc = case_when(Q22.8 %in% lower_incomes ~ "Low",
                                 Q22.8 %in% higher_incomes ~ "High"))
    
    low_educ <- c("Pas de scolarité ou arrêt avant la fin du primaire",
                  "Aucun diplôme et scolarité interrompue à la fin du primaire ou avant la fin du collège")
    
    med_educ <- c("Aucun diplôme et scolarité jusqu’à la fin du collège ou au-delà",
                  "CEP (certificat d’études primaires)",
                  "BEPC, brevet élémentaire, brevet des collèges, DNB",
                  "CAP, BEP ou diplôme de niveau équivalent")
    
    high_educ <- c("Baccalauréat général ou technologique, brevet supérieur, capacité en droit, DAEU, ESEU",
                   "Baccalauréat professionnel, brevet professionnel, de technicien ou d’enseignement, diplôme équivalent",
                   "BTS, DUT, Deug, Deust, diplôme de la santé ou du social de niveau bac+2, diplôme équivalent",
                   "Licence, licence pro, maîtrise, diplôme équivalent de niveau bac+3 ou bac+4",
                   "Master, DEA, DESS, diplôme grande école niveau bac+5, doctorat de santé",
                   "Doctorat de recherche (hors santé)",
                   "Hors champ (moins de 14 ans)")
    
    data$education <- case_when(data$Q22.2 %in% low_educ ~ "Low",
                                data$Q22.2 %in% med_educ ~ "Medium",
                                data$Q22.2 %in% high_educ ~ "High")
    
    data$hes_covid_2 <- case_when(data$hes_covid_2 == "Tout à fait d'accord" ~ "Strongly agree",
                                  data$hes_covid_2 == "Plutôt d'accord" ~ "Agree",
                                  data$hes_covid_2 == "Ni d'accord, ni pas d'accord" ~ "Neither agree nor disagree",
                                  data$hes_covid_2 == "Pas vraiment d'accord" ~ "Disagree",
                                  data$hes_covid_2 == "Pas du tout d'accord" ~ "Strongly disagree",
                                  data$hes_covid_2 == "Ne sait pas" ~ "Do not know")
    
    data$wtp_access <- case_when(data$wtp_access == "Vaccins uniquement mis à disposition par le gouvernement à faible coût ou gratuitement ?"
                                  ~ "Vaccines only made available by government at low or no cost",
                                 data$wtp_access == "Les vaccins ne sont disponibles que par achat privé ?"
                                  ~ "Vaccines are only available for private purchase",
                                 data$wtp_access == "Vaccins mis à disposition par le gouvernement mais les citoyens peuvent y avoir accès rapidement par achat privé ?"
                                  ~ "Vaccines made available by government but citizens can pay privately to gain access",
                                 data$wtp_access == "Ne sait pas" ~ "Do not know")
    
    data$wtp_private <- case_when(data$wtp_private == "Oui" ~ "Yes",
                                  data$wtp_private == "Non" ~ "No",
                                  data$wtp_private == "Ne sait pas" ~ "Do not know")
    
  } else if (ccode == "ind") {
    
    lower_incomes <- c("Less than \u20b960,000",
                       "\u20b960,000 - \u20b989,999",
                       "\u20b990,000 - \u20b91,19,999",
                       "\u20b91,20,000 - \u20b92,39,999",
                       "\u20b92,40,000 - \u20b95,99,999"
    ) # Inflation adjusted estimate is \u20b93,82,800
    
    higher_incomes <- c("\u20b96,00,000 and over")
    
    data <- data %>%
      mutate(ind_inc = case_when(Q22.7 %in% lower_incomes ~ "Low",
                                 Q22.7 %in% higher_incomes ~ "High"))
    
    low_educ <- c("No formal education",
                  "Incomplete primary school",
                  "Completed primary school",
                  "Middle pass / Matric fail")
    
    med_educ <-  c("Matric pass / 10th pass","11th pass, not completed intermediate","12th pass / Intermediate")
    
    high_educ <-  c("Undergraduate - Bachelor's / Diploma",
                    "Postgraduate degree - Masters / PhD")
    
    data$education <- case_when(data$Q22.2 %in% low_educ ~ "Low",
                                data$Q22.2 %in% med_educ ~ "Medium",
                                data$Q22.2 %in% high_educ ~ "High")
    
    data$wtp_access <- case_when(data$wtp_access == "Vaccines only made available by government at low or no cost?"
                                 ~ "Vaccines only made available by government at low or no cost",
                                 data$wtp_access == "Vaccines are only available for private purchase?"
                                 ~ "Vaccines are only available for private purchase",
                                 data$wtp_access == "Vaccines made available by government but citizens can pay privately to gain access?"
                                 ~ "Vaccines made available by government but citizens can pay privately to gain access",
                                 data$wtp_access == "Do not know"
                                 ~ "Do not know")
    
    data$geq_donation <- case_when(grepl("should donate 10%", data$Q18.8)
                                   ~ "Donate 10%",
                                   grepl("should not donate any", data$Q18.8)
                                   ~ "Not donate any",
                                   grepl("should donate more than", data$Q18.8)
                                   ~ "Donate more than 10%",
                                   grepl("donate less than", data$Q18.8)
                                   ~ "Donate less than 10%",
                                   data$Q18.8 == "Do not know" ~ "Do not know",
                                   data$Q18.8 == "Prefer not to say" ~ "Prefer not to say"
    )
    
  } else if (ccode == "ita") {
    
    lower_incomes <- c("Meno di €500",
                       "Più di €500, ma meno di €1.000",
                       "Più di €1.000 ma meno di €1.500") # Inflation adjusted estimate is 1485
    
    higher_incomes <- c("Più di €1.500 ma meno di €2.000",
                        "Più di €2.000 ma meno di €2.500",
                        "Più di €2.500 ma meno di €3.000",
                        "€3.000 euro o più")
    data <- data %>% 
      mutate(ind_inc = case_when(Q22.15 %in% lower_incomes ~ "Low",
                                 Q22.15 %in% higher_incomes ~ "High"))
    
    low_educ <- c("Qualifica scuola media",
                  "Qualifica scuola elementare (o equivalente)",
                  "Nessuna istruzione formale")
    
    med_educ <- c("Diploma di istruzione secondaria di II grado di 5 anni/ Istituto Formazione Tecnico Superiore (dal 2000) / Istituto Tecnico Superiore (2 anni)",
                  "Attestato di qualificazione professionale che non permette accesso all’università (2-3 anni) / Attestato di qualifica professionale (operatore)")
    
    high_educ <- c("Laurea magistrale / Master (2 anni)",
                   "Laurea vecchio ordinamento (4-6 anni) / Laurea a ciclo unico (5-6anni)",
                   "Laurea triennale (3 anni)",
                   "Laurea vecchio ordinamento",
                   "Dottorato di ricerca",
                   "Diploma di Accademia (Belle Arti, Dramma Nazionale, Danza Nazionale), Conservatorio di Musica, Istituto di Musica")
    
    data$education <- case_when(data$Q22.2 %in% low_educ ~ "Low",
                                data$Q22.2 %in% med_educ ~ "Medium",
                                data$Q22.2 %in% high_educ ~ "High")
    
    data$hes_covid_2 <- case_when(data$hes_covid_2 == "Completamente d'accordo" ~ "Strongly agree",
                                  data$hes_covid_2 == "D'accordo" ~ "Agree",
                                  data$hes_covid_2 == "Né d'accordo né in disaccordo" ~ "Neither agree nor disagree",
                                  data$hes_covid_2 == "In disaccordo" ~ "Disagree",
                                  data$hes_covid_2 == "Completamente in disaccordo" ~ "Strongly disagree",
                                  data$hes_covid_2 == "Non lo so" ~ "Do not know")
    
    data$wtp_access <- case_when(data$wtp_access == "Vaccini forniti solo dal governo a basso costo o a costo zero"
                                  ~ "Vaccines only made available by government at low or no cost",
                                 data$wtp_access == "Vaccini solo disponibili attraverso acquisto privato" 
                                  ~ "Vaccines are only available for private purchase",
                                 data$wtp_access == "Vaccini forniti dal governo, ma i cittadini possono pagare privatamente per ottenere accesso anticipato"
                                  ~ "Vaccines made available by government but citizens can pay privately to gain access",
                                 data$wtp_access == "Non saprei" ~ "Do not know",
                                 data$wtp_access == "Preferisco non rispondere" ~ "Prefer not to say")
    
    data$wtp_private <- case_when(data$wtp_private == "Sì" ~ "Yes",
                                  data$wtp_private == "No" ~ "No",
                                  data$wtp_private == "Non saprei" ~ "Do not know",
                                  data$wtp_private == "Preferisco non rispondere" ~ "Prefer not to say")
    
  } else if (ccode == "esp") {
    
    lower_incomes <- c("Menos de €500",
                       "Más de €500 y menos de €1.000",
                       "Más de €1.000 y menos de €1.500") # Inflation adjusted estimate is Spain is 1289
    
    higher_incomes <- c("Más de €1.500 y menos de €2.000",
                        "Más de €2.000 y menos de €2.500",
                        "Más de €2.500 y menos de €3.000",
                        "Más de €3.000")
    data <- data %>% 
      mutate(ind_inc = case_when(Q22.8 %in% lower_incomes ~ "Low",
                                 Q22.8 %in% higher_incomes ~ "High"))
    
    low_educ <- c("Llegó al último curso de ESO, EGB o Bachiller Elemental o tiene el Certificado de Escolaridad o de Estudios Primarios",
                  "Fue a la escuela 5 o más años pero no llegó al último curso de ESO, EGB o Bachiller Elemental",
                  "Sabe leer y escribir pero fue menos de 5 años a la escuela")
    
    med_educ <- c("FP grado medio, FP I, Oficialía Industrial o equivalente, Grado Medio de Música y Danza, Certificados de Escuelas Oficiales de Idiomas",
                  "Bachiller (LOE, LOGSE), BUP, Bachiller Superior, COU, PREU")
    
    high_educ <- c("FP grado superior, FP II, Maestría industrial o equivalente",
                   "Licenciatura, Arquitectura, Ingeniería o equivalente",
                   "Grado Universitario o equivalente",
                   "Doctorado",
                   "Master oficial universitario (a partir de 2006), Especialidades Médicas o análogos",
                   "Diplomatura universitaria, Arquitectura Técnica, Ingeniería Técnica o equivalente")
    
    data$education <- case_when(data$Q22.2 %in% low_educ ~ "Low",
                                data$Q22.2 %in% med_educ ~ "Medium",
                                data$Q22.2 %in% high_educ ~ "High")
    
    data$hes_covid_2 <- case_when(data$hes_covid_2 == "Totalmente de acuerdo" ~ "Strongly agree",
                                  data$hes_covid_2 == "De acuerdo" ~ "Agree",
                                  data$hes_covid_2 == "Ni de acuerdo ni en desacuerdo" ~ "Neither agree nor disagree",
                                  data$hes_covid_2 == "Desacuerdo" ~ "Disagree",
                                  data$hes_covid_2 == "Totalmente en desacuerdo" ~ "Strongly disagree",
                                  data$hes_covid_2 == "No lo sé" ~ "Do not know")
    
    data$wtp_access <- case_when(data$wtp_access == "Vacunas disponibles solo a través del gobierno a bajo costo o sin costo alguno?"
                                  ~ "Vaccines only made available by government at low or no cost",
                                 data$wtp_access == "Vacunas disponibles solo a través de distribuidores privados?"
                                  ~ "Vaccines are only available for private purchase",
                                 data$wtp_access == "Vacunas disponibles a través del gobierno, pero que ciudadanos puedan pagar de forma privada para obtener acceso?"
                                  ~ "Vaccines made available by government but citizens can pay privately to gain access",
                                 data$wtp_access == "No lo sé"
                                  ~ "Do not know")
    
    data$wtp_private <- case_when(data$wtp_private == "Sí" ~ "Yes",
                                  data$wtp_private == "No" ~ "No",
                                  data$wtp_private == "No lo sé" ~ "Do not know")
    
  } else if (ccode == "uga") {
    
    lower_incomes <- c("Under USh 330,000 per month",
                       "USh 330,000 to USh 660,000 per month",
                       "USh 660,000 to USh 1,500,000 per month",
                       "USh 1,500,000 to USh 2,000,000 per month",
                       "USh 2,000,000 to USh 2,500,000 per month",
                       "USh 2,500,000 to USh 3,000,000 per month")
    
    higher_incomes <- c("USh 3,000,000 to USh 4,000,000 per month",
                        "USh 4,000,000 to USh 5,000,000 per month",
                        "USh 5,000,000 to USh 6,000,000 per month",
                        "USh 6,000,000 to USh 7,000,000 per month",
                        "USh 7,000,000 to USh 9,000,000 per month",
                        "USh 9,000,000 to USh 11,000,000 per month",
                        "USh 11,000,000 to USh 13,500,000 per month",
                        "USh 13,5000,000 to USh 16,000,000 per month",
                        "USh 16,000,000 and over")
    
    data <- data %>%
      mutate(ind_inc = case_when(Q22.7 %in% lower_incomes ~ "Low",
                                 Q22.7 %in% higher_incomes ~ "High"))
    
    low_educ <- c("Never been to school",
                  "Did not complete Pre-Primary",
                  "Completed Pre-Primary",
                  "In P1 but did not complete / attend Pre-primary",
                  "Did not complete P1",
                  "P1","P2","P3","P4","P5","P7","J1-J3")
    
    med_educ <- c("S1","S2","S3","S4","S5","S6","Professional Certificate","Diploma")
    
    high_educ <-  c("First Degree",
                    "Post Graduate Certificate",
                    "Post Graduate Diploma",
                    "Masters Degree",
                    "PhD")
    
    data$education <- case_when(data$Q22.2 %in% low_educ ~ "Low",
                                data$Q22.2 %in% med_educ ~ "Medium",
                                data$Q22.2 %in% high_educ ~ "High")
    
    data$wtp_access <- case_when(data$wtp_access == "Vaccines only made available by government at low or no cost?"
                                 ~ "Vaccines only made available by government at low or no cost",
                                 data$wtp_access == "Vaccines are only available for private purchase?"
                                 ~ "Vaccines are only available for private purchase",
                                 data$wtp_access == "Vaccines made available by government but citizens can pay privately to gain access?"
                                 ~ "Vaccines made available by government but citizens can pay privately to gain access",
                                 data$wtp_access == "Do not know"
                                 ~ "Do not know")
    
    data$geq_donation <- case_when(grepl("should donate 10%", data$Q18.8)
                                   ~ "Donate 10%",
                                   grepl("should not donate any", data$Q18.8)
                                   ~ "Not donate any",
                                   grepl("should donate more than", data$Q18.8)
                                   ~ "Donate more than 10%",
                                   grepl("donate less than", data$Q18.8)
                                   ~ "Donate less than 10%",
                                   data$Q18.8 == "Do not know" ~ "Do not know",
                                   data$Q18.8 == "Prefer not to say" ~ "Prefer not to say"
    )
    
  } else if (ccode == "uk") {
    
    lower_incomes <- c("Under £5,000 per year",
                       "£5,000 to £9,999 per year",
                       "£10,000 to £14,999 per year",
                       "£15,000 to £19,999 per year") # Inflation adjusted estimate is 17419
    
    higher_incomes <- c("£20,000 to £24,999 per year", 
                        "£25,000 to £29,999 per year", 
                        "£30,000 to £34,999 per year", 
                        "£35,000 to £39,999 per year",
                        "£40,000 to £44,999 per year", 
                        "£45,000 to £49,999 per year", 
                        "£50,000 to £59,999 per year", 
                        "£60,000 to £69,999 per year", 
                        "£70,000 to £99,999 per year", 
                        "£100,000 to £149,999 per year", 
                        "£150,000 and over")
    data <- data %>% 
      mutate(ind_inc = case_when(Q22.7 %in% lower_incomes ~ "Low",
                                 Q22.7 %in% higher_incomes ~ "High"))
    
    low_educ <- c("City & Guilds certificate",
                  "Recognised trade apprenticeship completed",
                  "CSE grades 2-5",
                  "No formal qualifications",
                  "City & Guilds certificate - advanced",
                  "CSE grade 1, GCE O level, GCSE, School Certificate",
                  "Scottish Ordinary/ Lower Certificate",
                  "Youth training certificate/skillseekers",
                  "Clerical and commercial",
                  "ONC")
    
    med_educ <- c("Teaching qualification (not degree)",
                  "GCE A level or Higher Certificate",
                  "Other technical, professional or higher qualification",
                  "Nursing qualification (e.g. SEN, SRN, SCM, RGN)",
                  "Scottish Higher Certificate")
    
    high_educ <- c("University or CNAA first degree (e.g. BA, B.Sc, B.Ed)",
                   "University or CNAA higher degree (e.g. M.Sc, Ph.D)",
                   "University diploma")
    
    data$education <- case_when(data$Q22.2 %in% low_educ ~ "Low",
                                data$Q22.2 %in% med_educ ~ "Medium",
                                data$Q22.2 %in% high_educ ~ "High")
    
    data$wtp_access <- case_when(data$wtp_access == "Vaccines only made available by government at low or no cost?"
                                 ~ "Vaccines only made available by government at low or no cost",
                                 data$wtp_access == "Vaccines are only available for private purchase?"
                                 ~ "Vaccines are only available for private purchase",
                                 data$wtp_access == "Vaccines made available by government but citizens can pay privately to gain access?"
                                 ~ "Vaccines made available by government but citizens can pay privately to gain access",
                                 data$wtp_access == "Do not know"
                                 ~ "Do not know")
    
  }  else if (ccode == "usa") {
    
    lower_incomes <- c("Up to $10,000",
                       "$10,000 to 19,999",
                       "$20,000 to 29,999", 
                       "$30,000 to 39,999" ) # Inflation adjusted estimate is 38,337
    
    higher_incomes <- c("$40,000 to 49,999", 
                        "$50,000 to 74,999", 
                        "$75,000 to 99,999", 
                        "$100,000 or more")
    data <- data %>% 
      mutate(ind_inc = case_when(Q22.7 %in% lower_incomes ~ "Low",
                                 Q22.7 %in% higher_incomes ~ "High"))
    
    low_educ <- c("None",
                  "Some high school",
                  "Nursery to 8th Grade")
    
    med_educ <- c("Some college education, no degree",
                  "High school graduate, diploma (or equivalent)",
                  "Associate degree",
                  "Training/vocational college")
    
    high_educ <- c("Master's degree (including professional degrees, or equivalent)",
                   "Doctorate degree",
                   "Bachelor's degree")
    
    data$education <- case_when(data$Q22.2 %in% low_educ ~ "Low",
                                data$Q22.2 %in% med_educ ~ "Medium",
                                data$Q22.2 %in% high_educ ~ "High")
    
    data$wtp_access <- case_when(data$wtp_access == "Vaccines only made available by government at low or no cost?"
                                 ~ "Vaccines only made available by government at low or no cost",
                                 data$wtp_access == "Vaccines are only available for private purchase?"
                                 ~ "Vaccines are only available for private purchase",
                                 data$wtp_access == "Vaccines made available by government but citizens can pay privately to gain access?"
                                 ~ "Vaccines made available by government but citizens can pay privately to gain access",
                                 data$wtp_access == "Do not know"
                                 ~ "Do not know")
  }
  
  data$gender <- case_when(data$gender %in% c("Female",
                                              "Femenino",
                                              "Femme",
                                              "Femmina",
                                              "Feminino",
                                              "女") ~ "Female",
                           data$gender %in% c("Male","Masculino",
                                              "Homme","Maschio",
                                              "男") ~ "Male",
                           data$gender %in% c("Other","Altro",
                                              "其他","Outro","Otro") ~ "Other"
  )
  
  return(data)
  
}

# Generate tidy table of logit results with clustered s.e.'s
clustered_mod <- function(x_data, formula, cluster, type) {
  
  require(sandwich)
  require(lmtest)
  require(multiwayvcov)
  
  if (type == "logit") {
    model <- glm(formula = formula,
                 family = binomial(link='logit'), 
                 x_data)
  } else if (type == "ols") {
    model <- glm(formula,
                 family = gaussian, 
                 x_data)
  } else {
    stop("Type must be either 'logit' or 'ols'")
  }
  
  vcovCL <- cluster.vcov(model, x_data[[cluster]], df_correction = FALSE)
  model_coefs <- coeftest(model, vcovCL)
  
  return(model_coefs)

}

# Wrapper of clustered_mod() to get tidy results for plotting
figure_results <- function(x, formula, cluster, type) {
  
  model_coefs <- clustered_mod(x_data = x, formula, cluster, type = type)
  
  model_tidy <- broom::tidy(model_coefs) %>% 
    add_row(term = "vulnerabilityAverage risk of COVID-19 death", estimate = 0) %>% 
    add_row(term = "transmissionAverage risk of catching and transmitting the COVID-19 virus", estimate = 0) %>% 
    add_row(term = "incomeLowest 20% income level", estimate = 0) %>% 
    add_row(term = "occupationNot working", estimate = 0) %>% 
    add_row(term = "age_category25 years old", estimate = 0) %>% 
    mutate(ref_cat = is.na(std.error))
  
  return(model_tidy)
}

# Tidy coefficients in tables and figures
tidy_coefs <- function(col) {
  
  tidy_col <- col %>% 
    gsub("vulnerability","",.) %>% 
    gsub("transmission","",.) %>% 
    gsub("occupation","",.) %>% 
    sub("income","",.) %>% 
    gsub("age_category","",.) %>% 
    gsub("(Twice the average risk of catching and transmitting the COVID-19 virus)",
         "of transmission",
         .) %>% 
    gsub("(Twice the average risk of COVID-19 death)",
         "risk of death",
         .) %>% 
    gsub("(Five times the average risk of catching and transmitting the COVID-19 virus)",
         "of transmission",
         .) %>% 
    gsub("(Five times the average risk of COVID-19 death)",
         "risk of death",
         .) %>% 
    gsub("Average risk of catching and transmitting the COVID-19 virus",
         "Average risk (of transmission)",
         .) %>% 
    gsub("Average risk of COVID-19 death",
         "Average (risk of death)",
         .) %>%  
    
    factor(.,levels = rev(c("(Intercept)",
                            "Average (risk of death)",
                            "Moderate (risk of death)",
                            "High (risk of death)",
                            "Average risk (of transmission)",
                            "Moderate risk (of transmission)",
                            "High risk (of transmission)",
                            "Lowest 20% income level",
                            "Average income level",
                            "Highest 20% income level",
                            "Not working",
                            "Non-Key worker: Can work at home",
                            "Non-Key worker: Cannot work at home",
                            "Key worker: Education and childcare",
                            "Key worker: Factory worker",
                            "Key worker: Water and electricity service",
                            "Key worker: Police and fire-fighting",
                            "Key worker: Health and social care",
                            "25 years old",
                            "40 years old",
                            "65 years old",
                            "79 years old",
                            "countryAustralia",
                            "countryBrazil",                                                                                    
                            "countryCanada",                                                                                      
                            "countryChile",                                                                                      
                            "countryChina",                                                                                       
                            "countryColombia",                                                                                    
                            "countryFrance",                                                                                      
                            "countryItaly",                                                                                       
                            "countrySpain",                                                                                       
                            "countryUK",                                                                                          
                            "countryUS")))
  
  return(tidy_col)
  
}

# Tidy up results after unnesting conjoint models
results_tidy <- function(results) {
  results %>% 
    filter(term != "(Intercept)") %>% 
    
    mutate(attribute = substr(term, 1, regexpr("([A-Z]|[0-9])", term) - 1),
           attribute = str_to_sentence(attribute),
           attribute = ifelse(str_starts(term,"age_category"),"Age Category",attribute)) %>% 
    
    mutate(term = tidy_coefs(term))
}

# Heterogeneity analysis

analyse_het <- function(data, mod_type) {
  
  data %>% 
    nest() %>% 
    mutate(coefs = map(data, # for each country, run conjoint model
                       function (x) {
                         figure_results(x,
                                      formula = select ~ vulnerability + transmission + income + occupation + age_category + country,
                                      "country",
                                      type = mod_type)
                       })) %>% 
    select(-data) %>% 
    unnest(cols = coefs) %>% # combine all conjoint results into single table
    results_tidy(.) %>% 
    filter(!str_detect(term,"country"))
}

# Generic table generation
table_make <- function(results, type, textsize = NULL, caption = "", label = "", filename) {
  
  texreg(results,
         sideways = FALSE,
         fontsize = textsize,
         caption = caption,
         label = label,
         custom.coef.names = {tidy_coefs(rownames(results)) %>% as.character(.)},
         omit.coef = "country",
         groups = list("Vulnerability" = grep("vulnerability",rownames(results)),
                       "Transmission" = grep("transmission",rownames(results)),
                       "Income" = grep("income",rownames(results)),
                       "Occupation" = grep("occupation",rownames(results)),
                       "Age Category" = grep("age_category",rownames(results))
         ),
         use.packages = FALSE,
         file = paste0(filename,"_",type,".tex")
  )
}

# Multiple model table generation
multi_table_make <- function(results, type, textsize = NULL, caption = "", label = "", filename,
                             header, model_names, omit_group = NULL, fe = FALSE, sideways = FALSE) {
  
  if (fe) {
    fe_row <- list("Country Fixed Effects?" = rep("Yes",nrow(results)))
  } else {
    fe_row <- NULL
  }
  
  texreg(results$coefs,
         caption = caption,
         label = label,
         sideways = sideways,
         fontsize = textsize,
         custom.header = header,
         custom.model.names = model_names,
         custom.coef.names = {tidy_coefs(rownames(results$coefs[[1]])) %>% as.character(.)},
         omit.coef = omit_group,
         custom.gof.rows = fe_row,
         groups = list("Vulnerability" = grep("vulnerability",rownames(results$coefs[[1]])),
                       "Transmission" = grep("transmission",rownames(results$coefs[[1]])),
                       "Income" = grep("income",rownames(results$coefs[[1]])),
                       "Occupation" = grep("occupation",rownames(results$coefs[[1]])),
                       "Age Category" = grep("age_category",rownames(results$coefs[[1]]))
         ),
         use.packages = FALSE,
         file = paste0(filename,"_",type,".tex"))
  
}

# Basic plot for all conjoint graphs
base_plot <- function(data) {
  
  ggplot(data,
         aes(x = estimate, y = term)) +
    geom_point(position = position_dodge(width = 0.7)) +
    geom_errorbarh(aes(xmin = estimate - 1.96*std.error,
                       xmax = estimate + 1.96*std.error),
                   position = position_dodge(width = 0.7),
                   size = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme(strip.text = element_text(face = "bold.italic"))
}

# Plot individual country results
cj_country_plot <- function(country, results, type) {
  
  # Inherits from the base plot function above
  base_plot(data = results[results$country == country,]) +
    facet_grid(attribute~., space = "free",
               scales = "free") +
    labs(x = "Estimate", y = "") +
    ggsave(paste0("figures/conjoint_",country,"_",type,".pdf"))
  
}

# Individual attribute plot (all countries)
cj_attribute_plot <- function(attribute, results) {
  
  results$f_label <- attribute
  
  # Inherits from the base plot function above
  base_plot(results[results$attribute == attribute,]) +
    aes(color = country) +
    facet_grid(~ f_label) +
    geom_point(position = position_dodge(width = 0.7)) +
    geom_errorbarh(aes(xmin = estimate - 1.96*std.error,
                       xmax = estimate + 1.96*std.error),
                   position = position_dodge(width = 0.7),
                   size = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Estimate", y = "", color = "") +
    theme(strip.text = element_text(face = "bold.italic"),
          legend.position = "bottom") +
    ggsave(paste0("figures/conjoint_attribute_",str_to_lower(attribute),".pdf"))
  
}

# Heterogeneity plot
het_plot <- function(data, covars, mod_type) {
  
  data %>% 
    filter(covar %in% covars) %>% 
    mutate(covar = paste0("Respondent's ",covar)) %>% 
    base_plot(data = .) +
      facet_grid(attribute~covar, space = "free", scales = "free_y") +
      aes(color = group) +
      scale_color_manual(values=as.vector(polychrome(12)[3:12])) +
      labs(x = "Estimate", y = "", color = "") +
      guides(color=guide_legend(ncol = 3, byrow = TRUE)) +
      theme(legend.position = "bottom",
            text = element_text(size = 15)) +
      ggsave(paste0("figures/conjoint_heterogeneity_",
                    str_to_lower(paste(substr(covars,1,3), collapse = "_")),
                    "_",
                    mod_type,
                    ".pdf"),
             width = 21, height = 29.7, dpi = 300, 
             units = "cm", scale = 1.5)
  
}

# Heterogeneity table
het_table <- function(grouped_data, group_var, group_print, mod_type, model_order) {
  
  results <- grouped_data %>% 
    nest() %>% # by here, created separate data.frames for each country
    mutate(coefs = map(data, # for each country, run conjoint model
                       function (x) {
                         clustered_mod(x,
                                       formula = select ~ vulnerability + transmission + income + occupation + age_category + country,
                                       cluster = "country",
                                       type = mod_type)
                       })) %>% 
    select(-data)
  
  table_data <- results[0,]
  for (i in 1:length(model_order)) {
    table_data[i,] <- results[results[[1]] == model_order[i],]
  }
  
  header_list <- list()
  header_list[[group_print]] <- 1:nrow(table_data)
  
    
  het_caption <- paste0(
    
    ifelse(mod_type == "logit","Logistic ", "Linear (OLS) "),
    "Regression Results for ",
    group_print,
    " Categories. The dependent variable is the Forced Choice decision. ",
    "These are the estimates used to construct the conjoint plots presented in Figure ",
    case_when(mod_type == "logit" & group_var %in% c("age_bin","ind_inc","ideo_bin") ~ "\\ref{fig:priority_hetero}",
              mod_type == "logit" & group_var %in% c("gender","education","hesitancy") ~ "\\ref{fig:hetero_logit_cov_edu_gen}",
              mod_type == "ols" & group_var %in% c("age_bin","ind_inc","ideo_bin") ~ "\\ref{fig:hetero_ols_age_ide_inc}",
              mod_type == "ols" & group_var %in% c("gender","education","hesitancy") ~ "\\ref{fig:hetero_ols_cov_edu_gen}"),
    "."
    
    )
  
  multi_table_make(table_data, type = mod_type, textsize = "scriptsize", 
                   filename = paste0("tables/conjoint_het_",str_to_lower(group_print)),
                   header = header_list,
                   model_names = table_data[[group_var]],
                   omit_group = "country", fe = TRUE,
                   caption = het_caption,
                   label = paste0("table:",str_to_lower(group_print),"_",mod_type))
}
