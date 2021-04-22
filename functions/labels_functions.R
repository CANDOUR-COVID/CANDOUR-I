# Labeling

labeling <- function(data) {
  #### Gender ####
  male <- c("Male", 
            "Masculino", 
            "男",
            "Homme",
            "Maschio")
  
  female <- c("Female", 
              "Feminino",
              "Femenino",
              "女",
              "Femme",
              "Femmina")
  
  other_gender <- c("Other",
                    "Outro",
                    "Otro",
                    "其他",
                    "Autres",
                    "Altro")
  
  RS_gender <- c("Prefer not to say",
                 "Prefiro não informar",
                 "Prefiero no contestar",
                 "不想说",
                 "Prefiero no responder",
                 "Préfère ne pas dire",
                 "Preferisco non rispondere",
                 "I do not know")
  
  data$gender <- case_when(data$gender %in% female ~ "Female",
                           data$gender %in% male ~ "Male",
                           data$gender %in% other_gender ~ "Other",
                           data$gender %in% RS_gender ~ "Prefer not to say")
  
  data$gender<- data$gender %>%
    factor(levels = c("Female",
                      "Male",
                      "Other",
                      "Prefer not to say"))
  
  
  #### COVID-19 Vaccine Interest ####
  
  int_get_vacc_text1 <- c("Definitely get it",
                          "Com certeza tomaria",
                          "Certainement se faire vacciner",
                          "Definitivamente la obtendría",
                          "绝对可以",
                          "Sicuramente si")
  
  int_get_vacc_text2 <- c("Probably get it",
                          "Talvez tomaria",
                          "Probablement se faire vacciner",
                          "Probablemente la obtendría",
                          "大概可以",
                          "Probabilmente sì")
  
  int_get_vacc_text3 <- c("Probably not get it",
                          "Provavelmente não tomaria",
                          "Probablement pas",
                          "Probablemente no la obtendría",
                          "大概无法",
                          "Probabilmente no")
  
  int_get_vacc_text4 <- c("Definitely not get it",
                          "Com certeza não tomaria",
                          "Certainement pas",
                          "Definitivamente no la obtendría",
                          "绝对无法",
                          "Sicuramente no")
  
  int_get_vacc_text5 <- c("Do not know",
                          "Não sei",
                          "Ne sait pas",
                          "No lo sé",
                          "不知道",
                          "Non saprei")
  
  int_get_vacc_text6 <- c("Prefer not to say",
                          "Prefiro não informar",
                          "Préfère ne pas dire",
                          "Prefiero no contestar",
                          "不想说",
                          "Preferisco non rispondere")
  
  data$int_get_vacc <- case_when(data$int_get_vacc %in% int_get_vacc_text1 ~ "Definitely get it",
                                 data$int_get_vacc %in% int_get_vacc_text2 ~ "Probably get it",
                                 data$int_get_vacc %in% int_get_vacc_text3 ~ "Probably not get it",
                                 data$int_get_vacc %in% int_get_vacc_text4 ~ "Definitely not get it",
                                 data$int_get_vacc %in% int_get_vacc_text5 ~ "Do not know",
                                 data$int_get_vacc %in% int_get_vacc_text6 ~ "Prefer not to say")
  
  data$int_get_vacc <- data$int_get_vacc %>%
    factor(levels = c("Definitely get it",
                      "Probably get it",
                      "Probably not get it",
                      "Definitely not get it",
                      "Do not know",
                      "Prefer not to say"))
  
  int_reason_notget_text1 <- c("I don't believe the COVID-19 vaccine will be effective",
                               "No creo que la vacuna contra el COVID-19 será efectiva",
                               "我不相信新冠肺炎疫苗有效",
                               "Não acredito que a vacina contra COVID-19 será eficaz",
                               "Je ne crois pas que le vaccin contre la COVID-19 sera efficace",
                               "Non credo che il vaccino COVID-19 sarà efficace")
  
  int_reason_notget_text2 <- c("I am concerned about dangerous side effects from the COVID-19 vaccine",
                               "Me preocupan los posibles efectos secundarios adversos de la vacuna contra el COVID-19",
                               "我担心新冠肺炎疫苗有害的副作用",
                               "Estou preocupado quanto a efeitos colaterais perigosos da vacina contra COVID-19",
                               "Je suis préoccupé par les effets secondaires dangereux du vaccin contre la COVID-19",
                               "Sono preoccupato di possibili nocivi effetti collaterali del vaccino COVID-19")
  
  int_reason_notget_text3 <- c("Enough other people will accept vaccination so I will benefit from herd immunity",
                               "Suficientes personas aceptarán la vacunación, por lo que me beneficiaré de la inmunidad colectiva (\"efecto rebaño\")",
                               "足够多的人会接种疫苗，所以我将从群体免疫中受益",
                               "Uma quantidade suficiente de pessoas aceitará a vacina, então vou me beneficiar da imunidade coletiva",
                               "Assez d'autres personnes accepteront la vaccination alors je bénéficierai de l'immunité collective",
                               "Tanti altri faranno la vaccinazione quindi sarò protetto dall’immunità di gregge")
  
  int_reason_notget_text4 <- c("I have already been infected with COVID-19 and believe I have developed natural immunity",
                               "Ya he sido infectado con el COVID-19 y creo haber desarrollado inmunidad naturalmente",
                               "我已经感染了新冠肺炎，并相信自己的身体已经产生了自然免疫力",
                               "Eu já fui infectado pela COVID-19, e acredito que desenvolvi imunidade natural",
                               "J'ai déjà été infecté par la COVID-19 et je pense avoir développé une immunité naturelle",
                               "Sono già stato infettato dal COVID-19 e credo di aver sviluppato una naturale immunità")
  
  int_reason_notget_text5 <- c("The COVID-19 virus will not be very harmful to my health",
                               "El virus COVID-19 no será virus tan perjudicial para mi salud",
                               "​新冠病毒不会对我的健康造成太大伤害",
                               "O vírus da COVID-19 não será muito prejudicial à minha saúde",
                               "Le virus COVID-19 ne sera pas très nocif pour ma santé",
                               "Il virus COVID-19 non e’ molto dannoso per la mia salute")
  
  int_reason_notget_text6 <- c("I don't trust the health care providers in this country",
                               "No confío en los proveedores de atención médica de este país",
                               "​我不信任这个国家的医疗服务提供方",
                               "Não confio nos provedores de assistência médica neste país",
                               "Je ne fais pas confiance au système de santé de ce pays",
                               "Non mi fido degli operatori sanitari in questo Paese")
  
  int_reason_notget_text7 <- c("Other",
                               "Otra",
                               "其他",
                               "Outro",
                               "Autre",
                               "Altro")
  
  int_reason_notget_text8 <- c("Non saprei",
                               "Preferisco non rispondere")
  
  for (i in 1:7) {
    data[[paste0("int_reason_notget_", i)]] <- case_when(data[[paste0("int_reason_notget_", i)]] %in% int_reason_notget_text1 ~ "I don't believe the COVID-19 vaccine will be effective",
                                                         data[[paste0("int_reason_notget_", i)]] %in% int_reason_notget_text2 ~ "I am concerned about dangerous side effects from the COVID-19 vaccine",
                                                         data[[paste0("int_reason_notget_", i)]] %in% int_reason_notget_text3 ~ "Enough other people will accept vaccination so I will benefit from herd immunity",
                                                         data[[paste0("int_reason_notget_", i)]] %in% int_reason_notget_text4 ~ "I have already been infected with COVID-19 and believe I have developed natural immunity",
                                                         data[[paste0("int_reason_notget_", i)]] %in% int_reason_notget_text5 ~ "The COVID-19 virus will not be very harmful to my health",
                                                         data[[paste0("int_reason_notget_", i)]] %in% int_reason_notget_text6 ~ "I don't trust the health care providers in this country",
                                                         data[[paste0("int_reason_notget_", i)]] %in% int_reason_notget_text7 ~ "Other",
                                                         data[[paste0("int_reason_notget_", i)]] %in% int_reason_notget_text8 ~ "DK/RF")
    
    data[[paste0("int_reason_notget_", i)]] <- data[[paste0("int_reason_notget_", i)]] %>%
      factor(levels = c("I don't believe the COVID-19 vaccine will be effective",
                        "I am concerned about dangerous side effects from the COVID-19 vaccine",
                        "Enough other people will accept vaccination so I will benefit from herd immunity",
                        "I have already been infected with COVID-19 and believe I have developed natural immunity",
                        "The COVID-19 virus will not be very harmful to my health",
                        "I don't trust the health care providers in this country",
                        "Other",
                        "DK/RF"))
  }
  
  int_reason_get_text1 <- c("To protect myself",
                            "Proteger-me",
                            "Para protegerme",
                            "为了保护自己",
                            "Pour me protéger",
                            "Per proteggermi")
  
  int_reason_get_text2 <- c("To protect my family",
                            "Proteger minha família",
                            "Para proteger a mi familia",
                            "为了保护我的家人",
                            "Pour protéger ma famille",
                            "Per proteggere la mia famiglia")
  
  int_reason_get_text3 <- c("To protect the public (people I don't know)",
                            "Proteger o público (pessoas que eu não conheço)",
                            "Para proteger a las demás personas (que no conozco)",
                            "为了保护公众 （陌生人）",
                            "Pour protéger le public (les gens que je ne connais pas)",
                            "Per proteggere la popolazione (persone che non conosco)")
  
  int_reason_get_text4 <- c("Because everyone else will",
                            "Porque todas as pessoas tomarão",
                            "Porque todos los demás lo hicieron",
                            "因为每个人都会接种疫苗",
                            "Parce que tous les autres le feront",
                            "Perché tutti gli altri lo faranno")
  
  int_reason_get_text5 <- c("Because friends and family recommend it",
                            "Porque meus amigos e familiares recomendam",
                            "Porque mis amigos y familiares me lo recomendaron",
                            "因为亲朋好友建议接种疫苗",
                            "Parce que les amis et la famille le recommandent",
                            "Perché amici e familiari lo consigliano")
  
  int_reason_get_text6 <- c("Because doctors recommend it",
                            "Porque os médicos recomendam",
                            "Porque los doctores lo recomiendan",
                            "因为医生建议接种疫苗",
                            "Parce que les médecins le recommandent",
                            "Perché i medici lo raccomandano")
  
  int_reason_get_text7 <- c("Because health officials recommend it",
                            "Porque agentes de saúde recomendam",
                            "Porque los funcionarios de la salud lo recomiendan",
                            "因为卫生官员建议接种疫苗",
                            "Parce que les responsables de la santé le recommandent",
                            "Perché i funzionari sanitari lo raccomandano")
  
  int_reason_get_text8 <- c("Because politicians recommend it",
                            "Porque os políticos recomendam",
                            "Porque los políticos lo recomiendan",
                            "因为政治家建议接种疫苗",
                            "Parce que les politiciens le recommandent",
                            "Perché i politici lo raccomandano")
  
  int_reason_get_text9 <- c("Because of contact with COVID-19 infected people",
                            "Devido ao contato com pessoas infectadas pelo COVID-19",
                            "Por tener contacto con personas infectadas por COVID-19",
                            "由于接触新冠肺炎感染者",
                            "À cause du contact avec des personnes infectées par la COVID-19",
                            "A causa del contatto con persone infette da COVID-19")
  
  int_reason_get_text10 <- c("Because I had COVID-19 related symptoms",
                             "Porque tenho sintomas relacionados à COVID-19",
                             "Porque tuve síntomas relacionados al COVID-19",
                             "因为我出现了新冠肺炎的相关症状",
                             "Parce que j'avais des symptômes similaires à ceux du COVID-19",
                             "Perché ho avuto sintomi del COVID-19")
  
  int_reason_get_text11 <- c("If it were required for my work or school",
                             "Se fosse exigência de meu trabalho ou escola",
                             "Si fuera requerido por mi trabajo o donde estudio",
                             "如果我的工作或学校要求",
                             "Si mon travail ou mes études l'exigent",
                             "Se richiesto per lavoro o dalla scuola")
  
  int_reason_get_text12 <- c("Other",
                             "Outro",
                             "Otra",
                             "其他",
                             "Autre",
                             "Altro")
  
  for (i in 1:12) {
    
    data[[paste0("int_reason_get_", i)]] <- case_when(data[[paste0("int_reason_get_", i)]] %in% c("Non saprei", "Preferisco non rispondere") ~ NA_character_)
    
  }
  
  for (i in 1:12) {
    data[[paste0("int_reason_get_", i)]] <- case_when(data[[paste0("int_reason_get_", i)]] %in% int_reason_get_text1 ~ "To protect myself",
                                                         data[[paste0("int_reason_get_", i)]] %in% int_reason_get_text2 ~ "To protect my family",
                                                         data[[paste0("int_reason_get_", i)]] %in% int_reason_get_text3 ~ "To protect the public (people I don't know)",
                                                         data[[paste0("int_reason_get_", i)]] %in% int_reason_get_text4 ~ "Because everyone else will",
                                                         data[[paste0("int_reason_get_", i)]] %in% int_reason_get_text5 ~ "Because friends and family recommend it",
                                                         data[[paste0("int_reason_get_", i)]] %in% int_reason_get_text6 ~ "Because doctors recommend it",
                                                         data[[paste0("int_reason_get_", i)]] %in% int_reason_get_text7 ~ "Because health officials recommend it",
                                                         data[[paste0("int_reason_get_", i)]] %in% int_reason_get_text8 ~ "Because politicians recommend it",
                                                         data[[paste0("int_reason_get_", i)]] %in% int_reason_get_text9 ~ "Because of contact with COVID-19 infected people",
                                                         data[[paste0("int_reason_get_", i)]] %in% int_reason_get_text10 ~ "Because I had COVID-19 related symptoms",
                                                         data[[paste0("int_reason_get_", i)]] %in% int_reason_get_text11 ~ "If it were required for my work or school",
                                                         data[[paste0("int_reason_get_", i)]] %in% int_reason_get_text12 ~ "Other")
    
    data[[paste0("int_reason_get_", i)]] <- data[[paste0("int_reason_get_", i)]] %>%
      factor(levels = c("To protect myself",
                        "To protect my family",
                        "To protect the public (people I don't know)",
                        "Because everyone else will",
                        "Because friends and family recommend it",
                        "Because doctors recommend it",
                        "Because health officials recommend it",
                        "Because politicians recommend it",
                        "Because of contact with COVID-19 infected people",
                        "Because I had COVID-19 related symptoms",
                        "If it were required for my work or school",
                        "Other"))
  }
  
  
  
  int_lost_job_text1 <- c("Yes",
                          "Sim",
                          "Oui",
                          "Si",
                          "是",
                          "Sì")
  
  int_lost_job_text2 <- c("No",
                          "Não",
                          "Non",
                          "No",
                          "否",
                          "No")
  
  int_lost_job_text3 <- c("Do not know",
                          "Não sei",
                          "Ne sait pas",
                          "No lo sé",
                          "不知道",
                          "Non saprei")
  
  int_lost_job_text4 <- c("Prefer not to say",
                          "Prefiro não informar",
                          "Préfère ne pas dire",
                          "Prefiero no contestar",
                          "不想说",
                          "Preferisco non rispondere")
  
  data$int_lost_job <- case_when(data$int_lost_job %in% int_lost_job_text1 ~ "Yes",
                                 data$int_lost_job %in% int_lost_job_text2 ~ "No",
                                 data$int_lost_job %in% int_lost_job_text3 ~ "Do not know",
                                 data$int_lost_job %in% int_lost_job_text4 ~ "Prefer not to say")
  
  data$int_lost_job <- data$int_lost_job %>%
    factor(levels = c("Yes",
                      "No",
                      "Do not know",
                      "Prefer not to say"))
  
  int_exp_1_text1 <- c("Yes",
                       "Sim",
                       "Oui",
                       "Si",
                       "是",
                       "Sì")
  
  int_exp_1_text2 <- c("No",
                       "Não",
                       "Non",
                       "No",
                       "否",
                       "No")
  
  int_exp_1_text3 <- c("Do not know",
                       "Não sei",
                       "Ne sait pas",
                       "No lo sé",
                       "不知道",
                       "Non saprei")
  
  data$int_exp_1 <- case_when(data$int_exp_1 %in% int_exp_1_text1 ~ "Yes",
                              data$int_exp_1 %in% int_exp_1_text2 ~ "No",
                              data$int_exp_1 %in% int_exp_1_text3 ~ "Do not know")
  
  data$int_exp_1 <- data$int_exp_1 %>%
    factor(levels = c("Yes",
                      "No",
                      "Do not know"))
  
  int_exp_2_text1 <- c("Yes",
                       "Sim",
                       "Oui",
                       "Si",
                       "是",
                       "Sì")
  
  int_exp_2_text2 <- c("No",
                       "Não",
                       "Non",
                       "No",
                       "否",
                       "No")
  
  int_exp_2_text3 <- c("Do not know",
                       "Não sei",
                       "Ne sait pas",
                       "No lo sé",
                       "不知道",
                       "Non saprei")
  
  data$int_exp_2 <- case_when(data$int_exp_2 %in% int_exp_2_text1 ~ "Yes",
                              data$int_exp_2 %in% int_exp_2_text2 ~ "No",
                              data$int_exp_2 %in% int_exp_2_text3 ~ "Do not know")
  
  data$int_exp_2 <- data$int_exp_2 %>%
    factor(levels = c("Yes",
                      "No",
                      "Do not know"))
  
  int_exp_3_text1 <- c("Yes",
                       "Sim",
                       "Oui",
                       "Si",
                       "是",
                       "Sì")
  
  int_exp_3_text2 <- c("No",
                       "Não",
                       "Non",
                       "No",
                       "否",
                       "No")
  
  int_exp_3_text3 <- c("Do not know",
                       "Não sei",
                       "Ne sait pas",
                       "No lo sé",
                       "不知道",
                       "Non saprei")
  
  data$int_exp_3 <- case_when(data$int_exp_3 %in% int_exp_3_text1 ~ "Yes",
                              data$int_exp_3 %in% int_exp_3_text2 ~ "No",
                              data$int_exp_3 %in% int_exp_3_text3 ~ "Do not know")
  
  data$int_exp_3 <- data$int_exp_3 %>%
    factor(levels = c("Yes",
                      "No",
                      "Do not know"))
  
  int_exp_4_text1 <- c("Yes",
                       "Sim",
                       "Oui",
                       "Si",
                       "是",
                       "Sì")
  
  int_exp_4_text2 <- c("No",
                       "Não",
                       "Non",
                       "No",
                       "否",
                       "No")
  
  int_exp_4_text3 <- c("Do not know",
                       "Não sei",
                       "Ne sait pas",
                       "No lo sé",
                       "不知道",
                       "Non saprei")
  
  data$int_exp_4 <- case_when(data$int_exp_4 %in% int_exp_4_text1 ~ "Yes",
                              data$int_exp_4 %in% int_exp_4_text2 ~ "No",
                              data$int_exp_4 %in% int_exp_4_text3 ~ "Do not know")
  
  data$int_exp_4 <- data$int_exp_4 %>%
    factor(levels = c("Yes",
                      "No",
                      "Do not know"))
  
  int_exp_5_text1 <- c("Yes",
                       "Sim",
                       "Oui",
                       "Si",
                       "是",
                       "Sì")
  
  int_exp_5_text2 <- c("No",
                       "Não",
                       "Non",
                       "No",
                       "否",
                       "No")
  
  int_exp_5_text3 <- c("Do not know",
                       "Não sei",
                       "Ne sait pas",
                       "No lo sé",
                       "不知道",
                       "Non saprei")
  
  data$int_exp_5 <- case_when(data$int_exp_5 %in% int_exp_5_text1 ~ "Yes",
                              data$int_exp_5 %in% int_exp_5_text2 ~ "No",
                              data$int_exp_5 %in% int_exp_5_text3 ~ "Do not know")
  
  data$int_exp_5 <- data$int_exp_5 %>%
    factor(levels = c("Yes",
                      "No",
                      "Do not know"))
  
  int_behaviour_1_text1 <- c("Yes",
                             "Sim",
                             "Oui",
                             "Si",
                             "是",
                             "Sì")
  
  int_behaviour_1_text2 <- c("No",
                             "Não",
                             "Non",
                             "No",
                             "否",
                             "No")
  
  int_behaviour_1_text3 <- c("Do not know",
                             "Não sei",
                             "Ne sait pas",
                             "No lo sé",
                             "不知道",
                             "Non saprei")
  
  int_behaviour_1_text4 <- c("Not applicable",
                             "Não aplicável",
                             "Pas applicable",
                             "No aplica",
                             "不使用",
                             "Non mi concerne")
  
  data$int_behaviour_1 <- case_when(data$int_behaviour_1 %in% int_behaviour_1_text1 ~ "Yes",
                                    data$int_behaviour_1 %in% int_behaviour_1_text2 ~ "No",
                                    data$int_behaviour_1 %in% int_behaviour_1_text3 ~ "Do not know",
                                    data$int_behaviour_1 %in% int_behaviour_1_text4 ~ "Not applicable")
  
  data$int_behaviour_1 <- data$int_behaviour_1 %>%
    factor(levels = c("Yes",
                      "No",
                      "Do not know",
                      "Not applicable"))
  
  data$int_behaviour_2 <- case_when(data$int_behaviour_2 %in% int_behaviour_1_text1 ~ "Yes",
                                    data$int_behaviour_2 %in% int_behaviour_1_text2 ~ "No",
                                    data$int_behaviour_2 %in% int_behaviour_1_text3 ~ "Do not know",
                                    data$int_behaviour_2 %in% int_behaviour_1_text4 ~ "Not applicable")
  
  data$int_behaviour_2 <- data$int_behaviour_2 %>%
    factor(levels = c("Yes",
                      "No",
                      "Do not know",
                      "Not applicable"))
  
  data$int_behaviour_3 <- case_when(data$int_behaviour_3 %in% int_behaviour_1_text1 ~ "Yes",
                                    data$int_behaviour_3 %in% int_behaviour_1_text2 ~ "No",
                                    data$int_behaviour_3 %in% int_behaviour_1_text3 ~ "Do not know",
                                    data$int_behaviour_3 %in% int_behaviour_1_text4 ~ "Not applicable")
  
  data$int_behaviour_3 <- data$int_behaviour_3 %>%
    factor(levels = c("Yes",
                      "No",
                      "Do not know",
                      "Not applicable"))
  
  #### Hesitancy Questions ####
  
  hes_general_1_text1 <- c("Strongly agree",
                           "Concordo Totalmente",
                           "Tout à fait d'accord",
                           "强烈赞同",
                           "Totalmente de acuerdo",
                           "Completamente d'accordo")
  
  hes_general_1_text2 <- c("Agree",
                           "Concordo",
                           "Plutôt d'accord",
                           "赞同",
                           "De acuerdo",
                           "D'accordo")
  
  hes_general_1_text3 <- c("Neither agree nor disagree",
                           "Não concordo nem discordo",
                           "Ni d'accord, ni pas d'accord",
                           "既不赞同，亦不反对",
                           "Ni de acuerdo ni en desacuerdo",
                           "Né d'accordo né in disaccordo")
  
  hes_general_1_text4 <- c("Disagree",
                           "Discordo",
                           "Pas vraiment d'accord",
                           "反对",
                           "Desacuerdo",
                           "In disaccordo")
  
  hes_general_1_text5 <- c("Strongly disagree",
                           "Discordo Totalmente",
                           "Pas du tout d'accord",
                           "强烈反对",
                           "Totalmente en desacuerdo",
                           "Completamente in disaccordo")
  
  hes_general_1_text6 <- c("Do not know",
                           "Não sei",
                           "Ne sait pas",
                           "不知道",
                           "No lo sé",
                           "Non lo so")
  
  data$hes_general_1 <- case_when(data$hes_general_1 %in% hes_general_1_text1 ~ "Strongly agree",
                                  data$hes_general_1 %in% hes_general_1_text2 ~ "Agree",
                                  data$hes_general_1 %in% hes_general_1_text3 ~ "Neither agree nor disagree",
                                  data$hes_general_1 %in% hes_general_1_text4 ~ "Disagree",
                                  data$hes_general_1 %in% hes_general_1_text5 ~ "Strongly disagree",
                                  data$hes_general_1 %in% hes_general_1_text6 ~ "Do not know")
  
  data$hes_general_1 <- data$hes_general_1 %>%
    factor(levels = c("Strongly agree",
                      "Agree",
                      "Neither agree nor disagree",
                      "Disagree",
                      "Strongly disagree",
                      "Do not know"))
  
  data$hes_general_2 <- case_when(data$hes_general_2 %in% hes_general_1_text1 ~ "Strongly agree",
                                  data$hes_general_2 %in% hes_general_1_text2 ~ "Agree",
                                  data$hes_general_2 %in% hes_general_1_text3 ~ "Neither agree nor disagree",
                                  data$hes_general_2 %in% hes_general_1_text4 ~ "Disagree",
                                  data$hes_general_2 %in% hes_general_1_text5 ~ "Strongly disagree",
                                  data$hes_general_2 %in% hes_general_1_text6 ~ "Do not know")
  
  data$hes_general_2 <- data$hes_general_2 %>%
    factor(levels = c("Strongly agree",
                      "Agree",
                      "Neither agree nor disagree",
                      "Disagree",
                      "Strongly disagree",
                      "Do not know"))
  
  data$hes_general_3 <- case_when(data$hes_general_3 %in% hes_general_1_text1 ~ "Strongly agree",
                                  data$hes_general_3 %in% hes_general_1_text2 ~ "Agree",
                                  data$hes_general_3 %in% hes_general_1_text3 ~ "Neither agree nor disagree",
                                  data$hes_general_3 %in% hes_general_1_text4 ~ "Disagree",
                                  data$hes_general_3 %in% hes_general_1_text5 ~ "Strongly disagree",
                                  data$hes_general_3 %in% hes_general_1_text6 ~ "Do not know")
  
  data$hes_general_3 <- data$hes_general_3 %>%
    factor(levels = c("Strongly agree",
                      "Agree",
                      "Neither agree nor disagree",
                      "Disagree",
                      "Strongly disagree",
                      "Do not know"))
  
  data$hes_covid_1 <- case_when(data$hes_covid_1 %in% hes_general_1_text1 ~ "Strongly agree",
                                data$hes_covid_1 %in% hes_general_1_text2 ~ "Agree",
                                data$hes_covid_1 %in% hes_general_1_text3 ~ "Neither agree nor disagree",
                                data$hes_covid_1 %in% hes_general_1_text4 ~ "Disagree",
                                data$hes_covid_1 %in% hes_general_1_text5 ~ "Strongly disagree",
                                data$hes_covid_1 %in% hes_general_1_text6 ~ "Do not know")
  
  data$hes_covid_1 <- data$hes_covid_1 %>%
    factor(levels = c("Strongly agree",
                      "Agree",
                      "Neither agree nor disagree",
                      "Disagree",
                      "Strongly disagree",
                      "Do not know"))
  
  data$hes_covid_2 <- case_when(data$hes_covid_2 %in% hes_general_1_text1 ~ "Strongly agree",
                                data$hes_covid_2 %in% hes_general_1_text2 ~ "Agree",
                                data$hes_covid_2 %in% hes_general_1_text3 ~ "Neither agree nor disagree",
                                data$hes_covid_2 %in% hes_general_1_text4 ~ "Disagree",
                                data$hes_covid_2 %in% hes_general_1_text5 ~ "Strongly disagree",
                                data$hes_covid_2 %in% hes_general_1_text6 ~ "Do not know")
  
  data$hes_covid_2 <- data$hes_covid_2 %>%
    factor(levels = c("Strongly agree",
                      "Agree",
                      "Neither agree nor disagree",
                      "Disagree",
                      "Strongly disagree",
                      "Do not know"))
  
  #### Behavior ####
  
  beh_measure_1_text1 <- c("Yes",
                           "Sim",
                           "Oui",
                           "Si",
                           "是",
                           "Sì")
  
  beh_measure_1_text2 <- c("No",
                           "Não",
                           "Non",
                           "No",
                           "否",
                           "No")
  
  beh_measure_1_text3 <- c("Don't know",
                           "Não sei",
                           "Sans opinion",
                           "No lo sé",
                           "不知道",
                           "Non lo so")
  
  data$beh_measure_1 <- case_when(data$beh_measure_1 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_1 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_1 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_1 <- data$beh_measure_1 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_2 <- case_when(data$beh_measure_2 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_2 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_2 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_2 <- data$beh_measure_2 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_3 <- case_when(data$beh_measure_3 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_3 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_3 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_3 <- data$beh_measure_3 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_4 <- case_when(data$beh_measure_4 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_4 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_4 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_4 <- data$beh_measure_4 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_5 <- case_when(data$beh_measure_5 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_5 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_5 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_5 <- data$beh_measure_5 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_6 <- case_when(data$beh_measure_6 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_6 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_6 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_6 <- data$beh_measure_6 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_7 <- case_when(data$beh_measure_7 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_7 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_7 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_7 <- data$beh_measure_7 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_8 <- case_when(data$beh_measure_8 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_8 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_8 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_8 <- data$beh_measure_8 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_9 <- case_when(data$beh_measure_9 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_9 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_9 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_9 <- data$beh_measure_9 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_10 <- case_when(data$beh_measure_10 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_10 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_10 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_10 <- data$beh_measure_10 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_11 <- case_when(data$beh_measure_11 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_11 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_11 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_11 <- data$beh_measure_11 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_12 <- case_when(data$beh_measure_12 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_12 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_12 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_12 <- data$beh_measure_12 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_13 <- case_when(data$beh_measure_13 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_13 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_13 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_13 <- data$beh_measure_13 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_14 <- case_when(data$beh_measure_14 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_14 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_14 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_14 <- data$beh_measure_14 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_15 <- case_when(data$beh_measure_15 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_15 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_15 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_15 <- data$beh_measure_15 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_16 <- case_when(data$beh_measure_16 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_16 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_16 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_16 <- data$beh_measure_16 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  
  data$beh_measure_17 <- case_when(data$beh_measure_17 %in% beh_measure_1_text1 ~ "Yes",
                                  data$beh_measure_17 %in% beh_measure_1_text2 ~ "No",
                                  data$beh_measure_17 %in% beh_measure_1_text3 ~ "Don't know")
  
  data$beh_measure_17 <- data$beh_measure_17 %>%
    factor(levels = c("Yes",
                      "No",
                      "Don't know"))
  #### Global Equity Questions ####
  
  geq_future_spending_text1 <- c("Spend more on medical research than before the pandemic",
                                 "Gastar mais em pesquisa médica do que gastava antes da pandemia",
                                 "Dépenser plus pour la recherche médicale qu'avant la pandémie",
                                 "Debería gastar más en investigación médica de lo que gastaba antes de la pandemia",
                                 "Spendere di più per la ricerca medica rispetto a prima della pandemia")
  
  geq_future_spending_text2 <- c("Spend the same amount on medical research as before the pandemic",
                                 "Gastar o mesmo valor em pesquisa médica que gastava antes da pandemia",
                                 "Dépenser le même montant pour la recherche médicale qu'avant la pandémie",
                                 "Debería gastar la misma cantidad en investigación médica que antes de la pandemia",
                                 "Spendere lo stesso per la ricerca medica rispetto a prima della pandemia")
  
  geq_future_spending_text3 <- c("Spend less on medical research than before the pandemic",
                                 "Gastar menos em pesquisa médica do que gastava antes da pandemia",
                                 "Dépenser moins pour la recherche médicale qu'avant la pandémie",
                                 "Debería gastar menos en investigación médica que antes de la pandemia",
                                 "Spendere meno per la ricerca medica rispetto a prima della pandemia")
  
  geq_future_spending_text4 <- c("Do not know",
                                 "Não sei",
                                 "Ne sait pas",
                                 "No lo sé",
                                 "Non saprei")
  
  geq_future_spending_text5 <- c("Prefer not to say",
                                 "Prefiro não informar",
                                 "Préfère ne pas dire",
                                 "Prefiero no contestar",
                                 "Preferisco non rispondere")
  
  data$geq_future_spending <- case_when(data$geq_future_spending %in% geq_future_spending_text1 ~ "Spend more on medical research than before the pandemic",
                                        data$geq_future_spending %in% geq_future_spending_text2 ~ "Spend the same amount on medical research as before the pandemic",
                                        data$geq_future_spending %in% geq_future_spending_text3 ~ "Spend less on medical research than before the pandemic",
                                        data$geq_future_spending %in% geq_future_spending_text4 ~ "Do not know",
                                        data$geq_future_spending %in% geq_future_spending_text5 ~ "Prefer not to say")
  
  data$geq_future_spending <- data$geq_future_spending %>%
    factor(levels = c("Spend more on medical research than before the pandemic",
                      "Spend the same amount on medical research as before the pandemic",
                      "Spend less on medical research than before the pandemic",
                      "Do not know",
                      "Prefer not to say"))
  
  ##### Cleaning ####
  
  don1 <- c("The Australian government should not donate any vaccine it has purchased",
            "Países mais ricos não deveriam doar as vacinas que adquiriram",
            "The Canadian government should not donate any vaccine it has purchased",
            "Los países más ricos no deberían donar ninguna vacuna que hayan comprado",
            "Le gouvernement français ne devrait faire don d'aucun vaccin qu'il a acheté",
            "Il governo italiano non dovrebbe donare nessuno dei vaccini che ha acquistato",
            "El gobierno de España no debería donar ninguna vacuna que haya comprado",
            "The UK government should not donate any vaccine it has purchased",
            "The U.S. government should not donate any vaccine it has purchased",
            "Richer countries should not donate any vaccine they have purchased",
            "Richer countries should not donate any vaccine it has purchased")
  
  don2 <- c("The Australian government should donate less than 10% of its purchased vaccine",         
            "Países mais ricos deveriam doar-menos de 10% das vacinas adquiridas",
            "The Canadian government should donate less than 10% of its purchased vaccine",
            "Los países más ricos deberían donar menos del 10% de las vacunas que hayan comprado",
            "Le gouvernement français devrait donner moins de 10% des vaccins achetés",
            "Il governo italiano dovrebbe donare meno del 10% dei vaccini acquistati",
            "El gobierno de España debería donar menos del 10% de las vacunas que haya comprado",
            "Richer countries should donate less than 10% of their purchased vaccine",
            "The UK government should donate less than 10% of its purchased vaccine",
            "The U.S. government should donate less than 10% of its purchased vaccine",
            "Richer countries should donate less than 10% of its purchased vaccine")
  
  don3 <- c("The Australian government should donate 10% of its purchased vaccine",
            "Países mais ricos deveriam doar 10% das vacinas adquiridas",
            "The Canadian government should donate 10% of its purchased vaccine",
            "Los países más ricos deberían donar 10% de las vacunas que hayan comprado",
            "Le gouvernement français devrait faire don de 10% du vaccin acheté",
            "Il governo italiano dovrebbe donare il 10% dei vaccini acquistati",
            "Richer countries should donate 10% of its purchased vaccine",
            "El gobierno de España debería donar 10% de las vacunas que haya comprado",
            "Richer countries should donate 10% of their purchased vaccine",
            "The UK government should donate 10% of its purchased vaccine",
            "The U.S. government should donate 10% of its purchased vaccine")
  
  don4 <- c("The Australian government should donate more than 10% of its purchased amount of vaccine",
            "Países mais ricos deveriam doar-mais de 10% das vacinas adquiridas",
            "The Canadian government should donate more than 10% of its purchased amount of vaccine",
            "Los países más ricos deberían donar más del 10% de las vacunas que hayan comprado",
            "Le gouvernement français devrait donner plus de 10% des vaccins achetés",
            "Il governo italiano dovrebbe donare più del 10% dei vaccini acquistati",
            "Richer countries should donate more than 10% of its purchased amount of vaccine",
            "El gobierno de España debería donar más del 10% de las vacunas que haya comprado",
            "Richer countries should donate more than 10% of their purchased amount of vaccine",
            "The UK government should donate more than 10% of its purchased amount of vaccine",
            "The U.S. government should donate more than 10% of its purchased amount of vaccine")
  
  don5 <- c("Do not know",
            "Não sei",
            "No lo sé",
            "Ne sait pas",
            "Non saprei")
  
  don6 <- c("Prefer not to say",
            "Prefiro não informar",
            "Prefiero no contestar",
            "Préfère ne pas dire",
            "Preferisco non rispondere")
  
  
  data$geq_donation <- case_when(data$geq_donation %in% don1 ~ "Should not donate",
                                 data$geq_donation %in% don2 ~ "Should donate less than 10%",
                                 data$geq_donation %in% don3 ~ "Should donate 10%",
                                 data$geq_donation %in% don4 ~ "Should donate more than 10%",
                                 data$geq_donation %in% don5 ~ "Do not know",
                                 data$geq_donation %in% don6 ~ "Prefer not to say")
  
  data$geq_donation <- data$geq_donation %>%
    factor(levels = c("Should not donate",
                      "Should donate less than 10%",
                      "Should donate 10%",
                      "Should donate more than 10%",
                      "Do not know",
                      "Prefer not to say"))
  
  ticket_0_text1 <- c("Yes",
                      "Sim",
                      "Sí",
                      "愿意",
                      "Oui",
                      "Sì")
  
  ticket_0_text2 <- c("No",
                      "Não",
                      "No",
                      "不愿意",
                      "Non",
                      "No")
  
  ticket_0_text3 <- c("Do not know",
                      "Não sei",
                      "No lo sé",
                      "不知道",
                      "Ne sait pas",
                      "Non saprei")
  
  ticket_0_text4 <- c("Prefer not to say",
                      "Prefiro não informar",
                      "Prefiero no contestar",
                      "不想说",
                      "Préfère ne pas dire",
                      "Preferisco non rispondere")
  
  data$geq_ticket_0 <- case_when(data$geq_ticket_0 %in% ticket_0_text1 ~ "Yes",
                                 data$geq_ticket_0 %in% ticket_0_text2 ~ "No",
                                 data$geq_ticket_0 %in% ticket_0_text3 ~ "Do not know",
                                 data$geq_ticket_0 %in% ticket_0_text4 ~ "Prefer not to say")
  
  data$geq_ticket_0 <- data$geq_ticket_0 %>%
    factor(levels = c("Yes",
                      "No",
                      "Do not know",
                      "Prefer not to say"))
  
  data$geq_ticket_1 <- case_when(data$geq_ticket_1 %in% ticket_0_text1 ~ "Yes",
                                 data$geq_ticket_1 %in% ticket_0_text2 ~ "No",
                                 data$geq_ticket_1 %in% ticket_0_text3 ~ "Do not know",
                                 data$geq_ticket_1 %in% ticket_0_text4 ~ "Prefer not to say")
  
  data$geq_ticket_1 <- data$geq_ticket_1 %>%
    factor(levels = c("Yes",
                      "No",
                      "Do not know",
                      "Prefer not to say"))
  
  data$geq_ticket_2a <- case_when(data$geq_ticket_2a %in% ticket_0_text1 ~ "Yes",
                                 data$geq_ticket_2a %in% ticket_0_text2 ~ "No",
                                 data$geq_ticket_2a %in% ticket_0_text3 ~ "Do not know",
                                 data$geq_ticket_2a %in% ticket_0_text4 ~ "Prefer not to say")
  
  data$geq_ticket_2a <- data$geq_ticket_2a %>%
    factor(levels = c("Yes",
                      "No",
                      "Do not know",
                      "Prefer not to say"))
  
  data$geq_ticket_2b <- case_when(data$geq_ticket_2b %in% ticket_0_text1 ~ "Yes",
                                 data$geq_ticket_2b %in% ticket_0_text2 ~ "No",
                                 data$geq_ticket_2b %in% ticket_0_text3 ~ "Do not know",
                                 data$geq_ticket_2b %in% ticket_0_text4 ~ "Prefer not to say")
  
  data$geq_ticket_2b <- data$geq_ticket_2b %>%
    factor(levels = c("Yes",
                      "No",
                      "Do not know",
                      "Prefer not to say"))
  
  #### Quality of Life ####
  
  qol_condition_text1 <- c("Diabetes",
                           "糖尿病",
                           "Diabete",
                           "Diabète")
  
  qol_condition_text2 <- c("High blood pressure/hypertension",
                           "Presión sanguínea alta/ Hipertensión",
                           "高血压",
                           "Pressão alta sanguínea/hipertensão",
                           "Pressione alta/Ipertensione",
                           "Hypertension artérielle/hypertension")
  
  qol_condition_text3 <- c("Heart disease",
                           "Enfermedad cardiaca",
                           "心脏病",
                           "Doença cardíaca",
                           "Malattie cardiache",
                           "Maladies cardiaques")
  
  qol_condition_text4 <- c("Asthma or other chronic respiratory issues",
                           "Asma u otra enfermedad respiratoria crónica.",
                           "Asma u otra enfermedad respiratoria crónica",
                           "哮喘或其他慢性呼吸疾病",
                           "Asma ou outras doenças respiratórias crônicas",
                           "Asma o altri problemi respiratori cronici",
                           "Asthme ou autres problèmes respiratoires chroniques")
  
  qol_condition_text5 <- c("Allergies",
                           "Alergias.",
                           "过敏",
                           "Alergias",
                           "Allergie",
                           "Allergies")
  
  qol_condition_text6 <- c("Kidney disease",
                           "Enfermedad renal.",
                           "Enfermedad renal",
                           "肾病",
                           "Doença renal",
                           "Malattie renali",
                           "Maladies rénales")
  
  qol_condition_text7 <- c("Other chronic illnesses that require long term care from a doctor",
                           "Otra enfermedad crónica que requiera cuidado a largo plazo por doctor.",
                           "Otra enfermedad crónica que requiera cuidado a largo plazo por doctor",
                           "其他需要医生长期监管的慢性疾病",
                           "Outras doenças crônicas que necessitem de tratamento médico de longo prazo",
                           "Altre malattie croniche che richiedono cure a lungo termine",
                           "Autres maladies chroniques qui nécessitent des soins de longue durée de la part d'un médecin")
  
  qol_condition_text8 <- c("None",
                           "Ninguna",
                           "无",
                           "Nenhum",
                           "Nessuna",
                           "Aucun")
  
  qol_condition_text9 <- c("Do not know",
                           "不知道",
                           "Não sei",
                           "Non saprei",
                           "Ne sait pas")
  
  qol_condition_text10 <- c("Prefiero no responder",
                            "Prefer not to say",
                            "不想说",
                            "Prefiro não informar",
                            "Preferisco non rispondere",
                            "Préfère ne pas dire")
  
  for (i in 1:10) {
    data[[paste0("qol_condition_", i)]] <- case_when(data[[paste0("qol_condition_", i)]] %in% qol_condition_text1 ~ "Diabetes",
                                                     data[[paste0("qol_condition_", i)]] %in% qol_condition_text2 ~ "High blood pressure/hypertension",
                                                     data[[paste0("qol_condition_", i)]] %in% qol_condition_text3 ~ "Heart disease",
                                                     data[[paste0("qol_condition_", i)]] %in% qol_condition_text4 ~ "Asthma or other chronic respiratory issues",
                                                     data[[paste0("qol_condition_", i)]] %in% qol_condition_text5 ~ "Allergies",
                                                     data[[paste0("qol_condition_", i)]] %in% qol_condition_text6 ~ "Kidney disease",
                                                     data[[paste0("qol_condition_", i)]] %in% qol_condition_text7 ~ "Other chronic illnesses that require long term care from a doctor",
                                                     data[[paste0("qol_condition_", i)]] %in% qol_condition_text8 ~ "None",
                                                     data[[paste0("qol_condition_", i)]] %in% qol_condition_text9 ~ "Do not know",
                                                     data[[paste0("qol_condition_", i)]] %in% qol_condition_text10 ~ "Prefer not to say")
    
    data[[paste0("qol_condition_", i)]] <- data[[paste0("qol_condition_", i)]] %>%
      factor(levels = c("Diabetes",
                        "High blood pressure/hypertension",
                        "Heart disease",
                        "Asthma or other chronic respiratory issues",
                        "Allergies",
                        "Kidney disease",
                        "Other chronic illnesses that require long term care from a doctor",
                        "None",
                        "Do not know",
                        "Prefer not to say"))
  }
  
  #### EQ-5D ####
  
  eq5d_mobility_pre_text1 <- c("I have no problems with walking around",
                               "I had/have no problems in walking about",
                               "I have no problems in walking about",
                               "I had/have no problems walking",
                               "Não tenho problemas em andar",
                               "Je n’ai aucun problème pour me déplacer à pied",
                               "我四处走动没有困难",
                               "No tengo problemas para caminar",
                               "Non ho difficoltà nel camminare")
  
  eq5d_mobility_pre_text2 <- c("I have slight problems with walking around",
                               "I had/have slight problems in walking about",
                               "I have slight problems in walking about",
                               "I had/have slight problems walking",
                               "Tenho problemas leves em andar",
                               "J’ai des problèmes légers pour me déplacer à pied",
                               "我四处走动有一点困难",
                               "Tengo problemas leves para caminar",
                               "Ho lievi difficoltà nel camminare")
  
  eq5d_mobility_pre_text3 <- c("I have moderate problems with walking around",
                               "I had/have moderate problems in walking about",
                               "I have moderate problems in walking about",
                               "I had/have moderate problems walking",
                               "Tenho problemas moderados em andar",
                               "J’ai des problèmes modérés pour me déplacer à pied",
                               "我四处走动有中度的困难",
                               "Tengo problemas moderados para caminar",
                               "Ho moderate difficoltà nel camminare",
                               "I had/have moderate problems in walking about")
  
  eq5d_mobility_pre_text4 <- c("I have severe problems with walking around",
                               "I had/have severe problems in walking about",
                               "I have severe problems in walking about",
                               "I had/have severe problems walking",
                               "Tenho problemas graves em andar",
                               "J’ai des problèmes sévères pour me déplacer à pied",
                               "我四处走动有严重的困难",
                               "Tengo problemas graves para caminar",
                               "Ho gravi difficoltà nel camminare")
  
  eq5d_mobility_pre_text5 <- c("I am unable to walk around",
                               "I was/am unable to walk about",
                               "I am unable to walk about",
                               "I was/am unable to walk",
                               "Sou incapaz de andar",
                               "Je suis incapable de me déplacer à pied",
                               "我无法四处走动",
                               "No puedo caminar",
                               "Soy incapaz de caminar",
                               "Non sono in grado di camminare")
  
  data$eq5d_mobility_pre <- case_when(data$eq5d_mobility_pre %in% eq5d_mobility_pre_text1 ~ "I had/have no problems in walking about",
                                      data$eq5d_mobility_pre %in% eq5d_mobility_pre_text2 ~ "I had/have slight problems in walking about",
                                      data$eq5d_mobility_pre %in% eq5d_mobility_pre_text3 ~ "I had/have moderate problems in walking about",
                                      data$eq5d_mobility_pre %in% eq5d_mobility_pre_text4 ~ "I had/have severe problems in walking about",
                                      data$eq5d_mobility_pre %in% eq5d_mobility_pre_text5 ~ "I was/am unable to walk about")
  
  data$eq5d_mobility_pre <- data$eq5d_mobility_pre %>%
    factor(levels = c("I had/have no problems in walking about",
                      "I had/have slight problems in walking about",
                      "I had/have moderate problems in walking about",
                      "I had/have severe problems in walking about",
                      "I was/am unable to walk about"))
  
  data$eq5d_mobility_post <- case_when(data$eq5d_mobility_post %in% eq5d_mobility_pre_text1 ~ "I had/have no problems in walking about",
                                       data$eq5d_mobility_post %in% eq5d_mobility_pre_text2 ~ "I had/have slight problems in walking about",
                                       data$eq5d_mobility_post %in% eq5d_mobility_pre_text3 ~ "I had/have moderate problems in walking about",
                                       data$eq5d_mobility_post %in% eq5d_mobility_pre_text4 ~ "I had/have severe problems in walking about",
                                       data$eq5d_mobility_post %in% eq5d_mobility_pre_text5 ~ "I was/am unable to walk about")
  
  data$eq5d_mobility_post <- data$eq5d_mobility_post %>%
    factor(levels = c("I had/have no problems in walking about",
                      "I had/have slight problems in walking about",
                      "I had/have moderate problems in walking about",
                      "I had/have severe problems in walking about",
                      "I was/am unable to walk about"))
  
  eq5d_selfcare_pre_text1 <- c("I had/have no problems washing or dressing myself",
                               "I have no problems in bathing or dressing myself",
                               "Não tenho problemas para me lavar ou me vestir",
                               "Je n’ai aucun problème pour me laver ou m’habiller tout(e) seul(e)",
                               "我自己洗澡或穿衣没有困难",
                               "No tengo problemas para lavarme o vestirme",
                               "No tengo problemas para bañarme o vestirme",
                               "Non ho difficoltà nel lavarmi o vestirmi")
  
  eq5d_selfcare_pre_text2 <- c("I had/have slight problems washing or dressing myself",
                               "I have slight problems in bathing or dressing myself",
                               "Tenho problemas leves para me lavar ou me vestir",
                               "J’ai des problèmes légers pour me laver ou m’habiller tout(e) seul(e)",
                               "我自己洗澡或穿衣有一点困难",
                               "Tengo problemas leves para lavarme o vestirme",
                               "Tengo problemas leves para bañarme o vestirme",
                               "Ho lievi difficoltà nel lavarmi o vestirmi")
  
  eq5d_selfcare_pre_text3 <- c("I had/have moderate problems washing or dressing myself",
                               "I have moderate problems in bathing or dressing myself",
                               "Tenho problemas moderados para me lavar ou me vestir",
                               "J’ai des problèmes modérés pour me laver ou m’habiller tout(e) seul(e)",
                               "我自己洗澡或穿衣有中度的困难",
                               "Tengo problemas moderados para lavarme o vestirme",
                               "Tengo problemas moderados para bañarme o vestirme",
                               "Ho moderate difficoltà nel lavarmi o vestirmi")
  
  eq5d_selfcare_pre_text4 <- c("I had/have severe problems washing or dressing myself",
                               "I have severe problems in bathing or dressing myself",
                               "Tenho problemas graves para me lavar ou me vestir",
                               "J’ai des problèmes sévères pour me laver ou m’habiller tout(e) seul(e)",
                               "我自己洗澡或穿衣有严重的困难",
                               "Tengo problemas graves para lavarme o vestirme",
                               "Tengo problemas graves para bañarme o vestirme",
                               "Ho gravi difficoltà nel lavarmi o vestirmi")
  
  eq5d_selfcare_pre_text5 <- c("I was/am unable to wash or dress myself",
                               "I am unable to bathe or dress myself",
                               "Sou incapaz de me lavar ou vestir sozinho/a",
                               "Je suis incapable de me laver ou de m’habiller tout(e) seul(e)",
                               "我无法自己洗澡或穿衣",
                               "No puedo lavarme o vestirme",
                               "Soy incapaz de bañarme o vestirme",
                               "Non sono in grado di lavarmi o vestirmi")
  
  data$eq5d_selfcare_pre <- case_when(data$eq5d_selfcare_pre %in% eq5d_selfcare_pre_text1 ~ "I had/have no problems washing or dressing myself",
                                      data$eq5d_selfcare_pre %in% eq5d_selfcare_pre_text2 ~ "I had/have slight problems washing or dressing myself",
                                      data$eq5d_selfcare_pre %in% eq5d_selfcare_pre_text3 ~ "I had/have moderate problems washing or dressing myself",
                                      data$eq5d_selfcare_pre %in% eq5d_selfcare_pre_text4 ~ "I had/have severe problems washing or dressing myself",
                                      data$eq5d_selfcare_pre %in% eq5d_selfcare_pre_text5 ~ "I was/am unable to wash or dress myself")
  
  data$eq5d_selfcare_pre <- data$eq5d_selfcare_pre %>%
    factor(levels = c("I had/have no problems washing or dressing myself",
                      "I had/have slight problems washing or dressing myself",
                      "I had/have moderate problems washing or dressing myself",
                      "I had/have severe problems washing or dressing myself",
                      "I was/am unable to wash or dress myself"))
  
  data$eq5d_selfcare_post <- case_when(data$eq5d_selfcare_post %in% eq5d_selfcare_pre_text1 ~ "I had/have no problems washing or dressing myself",
                                       data$eq5d_selfcare_post %in% eq5d_selfcare_pre_text2 ~ "I had/have slight problems washing or dressing myself",
                                       data$eq5d_selfcare_post %in% eq5d_selfcare_pre_text3 ~ "I had/have moderate problems washing or dressing myself",
                                       data$eq5d_selfcare_post %in% eq5d_selfcare_pre_text4 ~ "I had/have severe problems washing or dressing myself",
                                       data$eq5d_selfcare_post %in% eq5d_selfcare_pre_text5 ~ "I was/am unable to wash or dress myself")
  
  data$eq5d_selfcare_post <- data$eq5d_selfcare_post %>%
    factor(levels = c("I had/have no problems washing or dressing myself",
                      "I had/have slight problems washing or dressing myself",
                      "I had/have moderate problems washing or dressing myself",
                      "I had/have severe problems washing or dressing myself",
                      "I was/am unable to wash or dress myself"))
  
  eq5d_usual_pre_text1 <- c("I had/have no problems doing my usual activities",
                            "I have no problems doing my usual activities",
                            "Não tenho problemas em realizar as minhas atividades habituais",
                            "Je n’ai aucun problème pour accomplir mes activités courantes",
                            "我进行日常活动没有困难",
                            "No tengo problemas para realizar mis actividades habituales",
                            "No tengo problemas para hacer mis actividades cotidianas",
                            "No tengo problemas para realizar mis actividades cotidianas",
                            "Non ho difficoltà nello svolgimento delle attività abituali")
  
  eq5d_usual_pre_text2 <- c("I had/have slight problems doing my usual activities",
                            "I have slight problems doing my usual activities",
                            "Tenho problemas leves em realizar as minhas atividades habituais",
                            "J’ai des problèmes légers pour accomplir mes activités courantes",
                            "我进行日常活动有一点困难",
                            "Tengo problemas leves para realizar mis actividades habituales",
                            "Tengo problemas leves para hacer mis actividades cotidianas",
                            "Tengo problemas leves para realizar mis actividades cotidianas",
                            "Ho lievi difficoltà nello svolgimento delle attività abituali")
  
  eq5d_usual_pre_text3 <- c("I had/have moderate problems doing my usual activities",
                            "I have moderate problems doing my usual activities",
                            "Tenho problemas moderados em realizar as minhas atividades habituais",
                            "J’ai des problèmes modérés pour accomplir mes activités courantes",
                            "我进行日常活动有中度的困难",
                            "Tengo problemas moderados para realizar mis actividades habituales",
                            "Tengo problemas moderados para hacer mis actividades cotidianas",
                            "Tengo problemas moderados para realizar mis actividades cotidianas",
                            "Ho moderate difficoltà nello svolgimento delle attività abituali")
  
  eq5d_usual_pre_text4 <- c("I had/have severe problems doing my usual activities",
                            "I have severe problems doing my usual activities",
                            "Tenho problemas graves em realizar as minhas atividades habituais",
                            "J’ai des problèmes sévères pour accomplir mes activités courantes",
                            "我进行日常活动有严重的困难",
                            "Tengo problemas graves para realizar mis actividades habituales",
                            "Tengo problemas graves para hacer mis actividades cotidianas",
                            "Tengo problemas graves para realizar mis actividades cotidianas",
                            "Ho gravi difficoltà nello svolgimento delle attività abituali")
  
  eq5d_usual_pre_text5 <- c("I was/am unable to do my usual activities",
                            "I am unable to do my usual activities",
                            "Sou incapaz de realizar as minhas atividades habituais",
                            "Je suis incapable d’accomplir mes activités courantes",
                            "我无法进行日常活动",
                            "No puedo realizar mis actividades habituales",
                            "Soy incapaz de hacer mis actividades cotidianas",
                            "No puedo realizar mis actividades cotidianas",
                            "Non sono in grado di svolgere le mie attività abituali")
  
  data$eq5d_usual_pre <- case_when(data$eq5d_usual_pre %in% eq5d_usual_pre_text1 ~ "I had/have no problems doing my usual activities",
                                   data$eq5d_usual_pre %in% eq5d_usual_pre_text2 ~ "I had/have slight problems doing my usual activities",
                                   data$eq5d_usual_pre %in% eq5d_usual_pre_text3 ~ "I had/have moderate problems doing my usual activities",
                                   data$eq5d_usual_pre %in% eq5d_usual_pre_text4 ~ "I had/have severe problems doing my usual activities",
                                   data$eq5d_usual_pre %in% eq5d_usual_pre_text5 ~ "I was/am unable to do my usual activities")
  
  data$eq5d_usual_pre <- data$eq5d_usual_pre %>%
    factor(levels = c("I had/have no problems doing my usual activities",
                      "I had/have slight problems doing my usual activities",
                      "I had/have moderate problems doing my usual activities",
                      "I had/have severe problems doing my usual activities",
                      "I was/am unable to do my usual activities"))
  
  data$eq5d_usual_post <- case_when(data$eq5d_usual_post %in% eq5d_usual_pre_text1 ~ "I had/have no problems doing my usual activities",
                                    data$eq5d_usual_post %in% eq5d_usual_pre_text2 ~ "I had/have slight problems doing my usual activities",
                                    data$eq5d_usual_post %in% eq5d_usual_pre_text3 ~ "I had/have moderate problems doing my usual activities",
                                    data$eq5d_usual_post %in% eq5d_usual_pre_text4 ~ "I had/have severe problems doing my usual activities",
                                    data$eq5d_usual_post %in% eq5d_usual_pre_text5 ~ "I was/am unable to do my usual activities")
  
  data$eq5d_usual_post <- data$eq5d_usual_post %>%
    factor(levels = c("I had/have no problems doing my usual activities",
                      "I had/have slight problems doing my usual activities",
                      "I had/have moderate problems doing my usual activities",
                      "I had/have severe problems doing my usual activities",
                      "I was/am unable to do my usual activities"))
  
  eq5d_pain_pre_text1 <- c("I had/have no pain or discomfort",
                           "I have no pain or discomfort",
                           "Não tenho dores ou mal-estar",
                           "Je n’ai ni douleur ni gêne",
                           "我没有疼痛或不舒服",
                           "No tengo dolor ni malestar",
                           "Non provo alcun dolore o fastidio")
  
  eq5d_pain_pre_text2 <- c("I had/have slight pain or discomfort",
                           "I have slight pain or discomfort",
                           "Tenho dores ou mal-estar leves",
                           "J’ai des douleurs ou une gêne légère(s)",
                           "我有一点疼痛或不舒服",
                           "Tengo dolor o malestar leve",
                           "Provo lieve dolore o fastidio")
  
  eq5d_pain_pre_text3 <- c("I had/have moderate pain or discomfort",
                           "I have moderate pain or discomfort",
                           "Tenho dores ou mal-estar moderados",
                           "J’ai des douleurs ou une gêne modérée(s)",
                           "我有中度的疼痛或不舒服",
                           "Tengo dolor o malestar moderado",
                           "Provo moderato dolore o fastidio")
  
  eq5d_pain_pre_text4 <- c("I had/have severe pain or discomfort",
                           "I have severe pain or discomfort",
                           "Tenho dores ou mal-estar fortes",
                           "J’ai des douleurs ou une gêne sévère(s)",
                           "我有严重的疼痛或不舒服",
                           "Tengo dolor o malestar fuerte",
                           "Provo grave dolore o fastidio")
  
  eq5d_pain_pre_text5 <- c("I had/have extreme pain or discomfort",
                           "I have extreme pain or discomfort",
                           "Tenho dores ou mal-estar extremos",
                           "J’ai des douleurs ou une gêne extrême(s)",
                           "我有非常严重的疼痛或不舒服",
                           "Tengo dolor o malestar extremo",
                           "Provo estremo dolore o fastidio")
  
  data$eq5d_pain_pre <- case_when(data$eq5d_pain_pre %in% eq5d_pain_pre_text1 ~ "I had/have no pain or discomfort",
                                  data$eq5d_pain_pre %in% eq5d_pain_pre_text2 ~ "I had/have slight pain or discomfort",
                                  data$eq5d_pain_pre %in% eq5d_pain_pre_text3 ~ "I had/have moderate pain or discomfort",
                                  data$eq5d_pain_pre %in% eq5d_pain_pre_text4 ~ "I had/have severe pain or discomfort",
                                  data$eq5d_pain_pre %in% eq5d_pain_pre_text5 ~ "I had/have extreme pain or discomfort")
  
  data$eq5d_pain_pre <- data$eq5d_pain_pre %>%
    factor(levels = c("I had/have no pain or discomfort",
                      "I had/have slight pain or discomfort",
                      "I had/have moderate pain or discomfort",
                      "I had/have severe pain or discomfort",
                      "I had/have extreme pain or discomfort"))
  
  data$eq5d_pain_post <- case_when(data$eq5d_pain_post %in% eq5d_pain_pre_text1 ~ "I had/have no pain or discomfort",
                                   data$eq5d_pain_post %in% eq5d_pain_pre_text2 ~ "I had/have slight pain or discomfort",
                                   data$eq5d_pain_post %in% eq5d_pain_pre_text3 ~ "I had/have moderate pain or discomfort",
                                   data$eq5d_pain_post %in% eq5d_pain_pre_text4 ~ "I had/have severe pain or discomfort",
                                   data$eq5d_pain_post %in% eq5d_pain_pre_text5 ~ "I had/have extreme pain or discomfort")
  
  data$eq5d_pain_post <- data$eq5d_pain_post %>%
    factor(levels = c("I had/have no pain or discomfort",
                      "I had/have slight pain or discomfort",
                      "I had/have moderate pain or discomfort",
                      "I had/have severe pain or discomfort",
                      "I had/have extreme pain or discomfort"))
  
  eq5d_anxiety_pre_text1 <- c("I was/am not anxious or depressed",
                              "I am not anxious or depressed",
                              "Não estou ansioso/a ou deprimido/a",
                              "Je ne suis ni anxieux(se) ni déprimé(e)",
                              "我没有焦虑或沮丧",
                              "No estoy angustiado/a o deprimido/a",
                              "No estoy angustiado(a) ni deprimido(a)",
                              "No estoy ansioso ni deprimido",
                              "Non sono ansioso/a o depresso/a")
  
  eq5d_anxiety_pre_text2 <- c("I was/am slightly anxious or depressed",
                              "I am slightly anxious or depressed",
                              "Estou levemente ansioso/a ou deprimido/a",
                              "Je suis légèrement anxieux(se) ou déprimé(e)",
                              "我有一点焦虑或沮丧",
                              "Estoy levemente angustiado/a o deprimido/a",
                              "Estoy levemente angustiado(a) o deprimido(a)",
                              "Estoy levemente ansioso o deprimido",
                              "Sono lievemente ansioso/a o depresso/a")
  
  eq5d_anxiety_pre_text3 <- c("I was/am moderately anxious or depressed",
                              "I am moderately anxious or depressed",
                              "Estou moderadamente ansioso/a ou deprimido/a",
                              "Je suis modérément anxieux(se) ou déprimé(e)",
                              "我有中度的焦虑或沮丧",
                              "Estoy moderadamente angustiado/a o deprimido/a",
                              "Estoy moderadamente angustiado(a) o deprimido(a)",
                              "Estoy moderadamente ansioso o deprimido",
                              "Sono moderatamente ansioso/a o depresso/a")
  
  eq5d_anxiety_pre_text4 <- c("I was/am severely anxious or depressed",
                              "I am severely anxious or depressed",
                              "Estou muito ansioso/a ou deprimido/a",
                              "Je suis sévèrement anxieux(se) ou déprimé(e)",
                              "我有严重的焦虑或沮丧",
                              "Estoy muy angustiado/a o deprimido/a",
                              "Estoy muy angustiado(a) o deprimido(a)",
                              "Estoy muy ansioso o deprimido",
                              "Sono gravemente ansioso/a o depresso/a")
  
  eq5d_anxiety_pre_text5 <- c("I was/am extremely anxious or depressed",
                              "I am extremely anxious or depressed",
                              "Estou extremamente ansioso/a ou deprimido/a",
                              "Je suis extrêmement anxieux(se) ou déprimé(e)",
                              "我有非常严重的焦虑或沮丧",
                              "Estoy extremadamente angustiado/a o deprimido/a",
                              "Estoy extremadamente angustiado(a) o deprimido(a)",
                              "Estoy extremadamente ansioso o deprimido",
                              "Sono estremamente ansioso/a o depresso/a")
  
  data$eq5d_anxiety_pre <- case_when(data$eq5d_anxiety_pre %in% eq5d_anxiety_pre_text1 ~ "I was/am not anxious or depressed",
                                     data$eq5d_anxiety_pre %in% eq5d_anxiety_pre_text2 ~ "I was/am slightly anxious or depressed",
                                     data$eq5d_anxiety_pre %in% eq5d_anxiety_pre_text3 ~ "I was/am moderately anxious or depressed",
                                     data$eq5d_anxiety_pre %in% eq5d_anxiety_pre_text4 ~ "I was/am severely anxious or depressed",
                                     data$eq5d_anxiety_pre %in% eq5d_anxiety_pre_text5 ~ "I was/am extremely anxious or depressed")
  
  data$eq5d_anxiety_pre <- data$eq5d_anxiety_pre %>%
    factor(levels = c("I was/am not anxious or depressed",
                      "I was/am slightly anxious or depressed",
                      "I was/am moderately anxious or depressed",
                      "I was/am severely anxious or depressed",
                      "I was/am extremely anxious or depressed"))
  
  data$eq5d_anxiety_post <- case_when(data$eq5d_anxiety_post %in% eq5d_anxiety_pre_text1 ~ "I was/am not anxious or depressed",
                                      data$eq5d_anxiety_post %in% eq5d_anxiety_pre_text2 ~ "I was/am slightly anxious or depressed",
                                      data$eq5d_anxiety_post %in% eq5d_anxiety_pre_text3 ~ "I was/am moderately anxious or depressed",
                                      data$eq5d_anxiety_post %in% eq5d_anxiety_pre_text4 ~ "I was/am severely anxious or depressed",
                                      data$eq5d_anxiety_post %in% eq5d_anxiety_pre_text5 ~ "I was/am extremely anxious or depressed")
  
  data$eq5d_anxiety_post <- data$eq5d_anxiety_post %>%
    factor(levels = c("I was/am not anxious or depressed",
                      "I was/am slightly anxious or depressed",
                      "I was/am moderately anxious or depressed",
                      "I was/am severely anxious or depressed",
                      "I was/am extremely anxious or depressed"))
  
  data$eq5d_scale_pre <- data$eq5d_scale_pre %>%
    as.numeric()
  
  data$eq5d_scale_post <- data$eq5d_scale_post %>%
    as.numeric()
  
  return(data)
  
}
