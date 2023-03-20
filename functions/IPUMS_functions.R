library(dplyr)
library(tidyverse)

'
NOTE:
'

#######################################CONTRACT###############################################################################

IPUMS_contract<-function(data_fram){
  if (unique(data_fram$country)=="Australia"){
   
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.6)
    
    data_fram <- data_fram %>% mutate(PROF_POSITION  = if_else(PROF_POSITION  == "Own business" & Q22.5  == "Working, other unpaid job" , "Unpaid",
                                                   if_else(is.na(PROF_POSITION),"Other",PROF_POSITION)))
    
    Self_employ <- c("Own business")
    Wage_salary_worker <- c("Employer")
    Unpaid <- c("Unpaid")
    Other <- c("Other")
    Unknow <- c("Do not know","Prefer not to say")
  }
  else if(unique(data_fram$country)=="Brazil"){

    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.4)
    
    
    Self_employ <- c("Conta própria",
                     "Empregador")
    
    Wage_salary_worker <- c("Empregado com carteira de trabalho assinada",
                            "Militar do exército, marinha, aeronáutica, policia militar ou corpo de bombeiros",
                            "Empregado pelo regime jurídico dos funcionários públicos",
                            "Empregado sem carteira de trabalho assinada")
    
    Unpaid <- c("Não-remunerado")
    
    Other <- c("")
    
    Unknow <- c("Não sei","Prefiro não informar")
  }
  else if(unique(data_fram$country)=="Canada"){
    
    data_fram <- data_fram %>% mutate(PROF_POSITION = Q22.3)
    
    Self_employ <- c("Self-employed")
    
    Wage_salary_worker <- c("Employed for wages",
                            "Military")
    Unpaid <- c()
    
    Other <- c("Out of work and looking for work",
               "Out of work but not currently looking for work or Homemaker",
               "Student",
               "Retired",
               "Unable to work")
    
    Unknow <- c("Do not know","Prefer not to say")
  }
  else if(unique(data_fram$country)=="Chile"){
    
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.9)
    
    
    Self_employ <- c("Empleador",
                     "Cuenta propia")
    
    Wage_salary_worker <- c("Asalariado sector privado",
                            "Asalariado sector público",
                            "Personal de servicio doméstico puertas afuera",
                            "Personal de servicio doméstico puertas adentro")
    
    Unpaid <- c("Familiar o personal no remunerado")
    
    Other <- c("")
    
    Unknow <- c("No lo sé","Prefiero no responder")
    
  }
  else if(unique(data_fram$country)=="Colombia"){
    
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.5)
    
    
    Self_employ <- c("Trabajador por cuenta propia",
                     "Patrón o empleador")
    
    Wage_salary_worker <- c("Obrero o empleado de empresa particular",
                            "Obrero o empleado del gobierno",
                            "Empleado doméstico",
                            "Jornalero o peón")
    
    Unpaid <- c("Trabajador familiar sin remuneración",
                "Trabajador sin remuneración en empresas o negocios de otros hogares")
    
    Other <- c("Otro")
    
    Unknow <- c("No lo sé","Prefiero no responder")
  
  }
  else if(unique(data_fram$country)=="France"){
    
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.5)
    
    Self_employ <- c("Non salariés : Indépendants",
                     "Non salariés : Employeurs")
    
    Wage_salary_worker <- c("Emplois aidés (contrat unique d'insertion, d'initiative emploi, d'accompagnement dans l'emploi, avenir, etc.)",
                            "Stagiaires rémunérés en entreprise",
                            "Autres emplois à durée limitée, CDD, contrat court, saisonnier, vacataire, etc.",
                            "Emplois sans limite de durée, CDI, titulaire de la fonction publique")
    
    Unpaid <- c("Non salariés : Aides familiaux")
    
    Other <- c("En contrat d'apprentissage ou de professionnalisation",
               "Placés par une agence d'intérim")
     
    Unknow <- c("Sans objet")
    
  }
  else if(unique(data_fram$country)=="Italy"){
    
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.7)
    
    Self_employ <- c("Imprenditore",
                     "Libero professionista",
                     "Lavoratore in proprio")
    
    Wage_salary_worker <- c("Dirigente",
                            "Quadro",
                            "Impiegato",
                            "Operaio",
                            "Lavoratore presso il proprio domicilio per conto di un impresa")
    
    Unpaid <- c("Apprendista")
    
    Other <- c("Prestazione d opera occasionale",
               "Socio di cooperativa",
               "Coadiuvante nell azienda di un familiare",
               "Collaborazione coordinata e continuativa")
    
    Unknow <- c("Non saprei","Preferisco non rispondere")
    
  }
  
  else if(unique(data_fram$country)=="Russia"){
    
    data_fram <- data_fram %>% mutate(PROF_POSITION = Q22.3)
    
    Self_employ <- c("Индивидуальный предприниматель/ самозанятый")
    
    Wage_salary_worker <- c("Работаю по найму",
                            "Работаю в вооруженных силах или правоохранительных органах")
    
    Unpaid <- c("Домохозяйка")
    
    Other <- c("Не работаю, ищу работу",
               "Не работаю, не ищу работу",
               "Студент дневного отделения",
               "Пенсионер",
               "Не в состоянии работать")
    
    Unknow <- c("Не знаю",
                "Предпочитаю не отвечать")
    
  }
  
  else if(unique(data_fram$country)=="Spain"){
     
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.5)
    
    
    Self_employ <- c("Empresario o profesional que emplea personal",
                     "Empresario o profesional que no emplea personal")
    
    Wage_salary_worker <- c("Trabajador por cuenta ajena con carácter fijo o indefinido",
                            "Trabajador por cuenta ajena con carácter eventual, temporal...")
    
    Unpaid <- c("Ayuda familiar")
    
    Other <- c("Miembros de cooperativas")
    
    Unknow <- c("No lo sé","Prefiero no responder")
    
  }
  else if(unique(data_fram$country)=="UK"){
    return(data_fram)
  }
  else if(unique(data_fram$country)=="US"){
    
    data_fram <- data_fram %>% mutate(PROF_POSITION = Q22.3)
    
    Self_employ <- c("Self-employed")
    
    Wage_salary_worker <- c("Employed for wages",
                            "Military")
    
    Unpaid <- c("Homemaker")
    
    Other <- c("Out of work and looking for work",
               "Out of work but not currently looking for work",
               "Student",
               "Retired",
               "Unable to work")
    
    Unknow <- c("Do not know","Prefer not to say")
    
  }
  
  else if(unique(data_fram$country)=="China"){
    
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.6)
    
    Self_employ <- c("雇主",
                     "个体户/自雇")
    
    Wage_salary_worker <- c("雇员")
    
    Unpaid <- c("居家服务人员")
    
    Other <- c("")
    
    Unknow <- c("")
  }
  else if(unique(data_fram$country) == "Uganda"){
    
    data_fram <- data_fram %>% mutate(PROF_POSITION = Q22.3)
    
    Self_employ <- c("Employer","Own account worker")
    
    Wage_salary_worker <- c("Working for pay")
    
    Unpaid <- c("Helping without pay in a household business","Volunteer","Household chores")
    
    Other <- c("Looking for work","Not working and not looking for work","Full time student",
               "Retired/pensioner","Too old to work","Too young to work")
    
    Unknow <- c("Do not know","Prefer not to say")
  }
  
  else if(unique(data_fram$country) == "India"){
    
    data_fram <- data_fram %>% mutate(PROF_POSITION = Q22.3)
    
    Self_employ <- c("Self-employed")
    
    Wage_salary_worker <- c("Employed")
    
    Unpaid <- c("Working in the household")
    
    Other <- c("Other")
    
    Unknow <- c("")
  }
  
  else{
    return(print("There is not a survey for this country, try again"))}
  
  data_fram <- data_fram %>% mutate(PROF_POSITION = if_else(PROF_POSITION %in% Self_employ,"Self-employed",
                                                if_else(PROF_POSITION %in% Wage_salary_worker,"Wage/salary worker",
                                                        if_else(PROF_POSITION %in% Unpaid,"Unpaid worker",
                                                                if_else(PROF_POSITION %in% Other ,"Other", "Unknown/missing")))))
  
  

 return(data_fram)
}

#######################################EDUCATION###############################################################################

IPUMS_education<-function(data_fram){
  if (unique(data_fram$country)=="Australia"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.4 )
    
    
    Less_than_primary <- c("No educational attainment","Certificate I","Year 8 or below")
    
    Primary_com <- c("Year 10","Year 9","Year 11","Certificate II",
                     "Certificate III")
    
    Secondary_com <- c("Year 12","Certificate IV","Certificate III & IV Level","Diploma",
                       "Associate Degree","Advanced Diploma","Advanced Diploma and Diploma Level")
    
    University_com <- c("Bachelor Degree Level","Graduate Certificate","Graduate Diploma",
                        "Master Degree Level","Professional Specialist Qualification, Doctoral Level",
                        "Higher Doctorate","Doctoral Degree Level")
    
    Unknow <- c("Do not know","Prefer not to say")
  }
  else if(unique(data_fram$country)=="Brazil"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    
    Less_than_primary <- c("Creche, Pré-escolar (Maternal e Jardim de Infância), Classe de alfabetização - CA",
                           "Alfabetização de Jovens e Adultos",
                           "Antigo Primário (Elementar)",
                           "Antigo Ginásio (Médio 1º Ciclo)",
                           "Regular do Ensino Fundamental ou 1º Grau (da 1aa 3a série/ do 1º ao 4º ano)", 
                           "Regular do Ensino Fundamental ou  1º Grau (da 4a série/5º ano)")
    
    Primary_com <- c("Regular do Ensino Fundamental ou 1ºGrau (da 5aa 8asérie/ do 6º ao 9º ano)",
                     "Supletivo do Ensino Fundamental ou do 1ºGrau")
    
    Secondary_com <- c("Antigo Científico, Clássico, etc. (Médio 2o ciclo)",
                       "Regular ou Supletivo do Ensino Médio ou do 2º Grau")
    
    University_com <- c("Superior de Graduação",
                        "Especialização de Nível Superior (mínimo de 360 horas)",
                        "Mestrado",
                        "Doutorado")
    
    Unknow <- c("Não sei","Prefiro não informar")
    
    

  }
  else if(unique(data_fram$country)=="Canada"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    
    Less_than_primary <- c("No certificate, diploma or degree")
    
    Primary_com <- c("NoN")
    
    Secondary_com <- c("Secondary (high) school diploma or equivalency certificate",
                       "Trades certificate or diploma other than Certificate of Apprenticeship or Certificate of Qualification",
                       "Certificate of Apprenticeship or Certificate of Qualification",
                       "Program of 3 months or less than 1 year (College, CEGEP or other non-university certificate or diploma from a program of 3 months to less than 1 year)",
                       "Program of 1 to 2 years (College, CEGEP or other non-university certificate or diploma from a program of 1 year to 2 years)",
                       "Program of more than 2 years (College, CEGEP or other non-university certificate or diploma from a program of more than 2 years)",
                       "University certificate or diploma below bachelor level")
    
    University_com <- c("Bachelor's degree",
                        "University certificate or diploma above bachelor level",
                        "Degree in medicine, dentistry, ceterinary medicine or optometry",
                        "Master's degree",
                        "Earned doctorate")
    
    Unknow <- c("Do not know","Prefer not to say")
    
  }
  
  else if(unique(data_fram$country)=="Chile"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.4 )
    
    data_fram <- data_fram %>% mutate(EDUCATION_LEVEL = if_else(is.na(EDUCATION_LEVEL),Q22.3,EDUCATION_LEVEL))
    
    Less_than_primary <- c("Especial o Diferencial","Nunca asistió")
    
    Primary_com <- c("Educación Básica",
                     "Primaria o Preparatoria (Sistema antiguo)")
    
    Secondary_com <- c("Científico-Humanista",
                       "Técnica Profesional",
                       "Humanidades (Sistema antiguo)",
                       "Técnico Nivel Superior (carreras 1-3 años)")
    
    University_com <- c("Profesional (carreras 1-4 años)",
                        "Magíster",
                        "Doctorado")
    
    Unknow <- c("No lo sé", "Prefiero no decir")
   
    
  }
  
  else if(unique(data_fram$country)=="Colombia"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.3 )
    
    Less_than_primary <- c("Ninguno", 
                           "Preescolar",
                           "Básica primaria (1.°-5.°)")
    
    Primary_com <- c("Básica secundaria (Bachillerato básico, 6.°-9.°)")
    
    Secondary_com <- c("Media académica o clásica (Bachillerato clásico, 10.°-13.°)",
                       "Media técnica (Bachillerato técnico)",
                       "Normalista",
                       "Técnica Profesional",
                       "Tecnológica")
    
    University_com <- c("Universitario",
                        "Especialización",
                        "Maestría",
                        "Doctorado")
    
    Unknow <- c("No lo sé", "Prefiero no responder")
    
    
    
  }
  else if(unique(data_fram$country)=="France"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    
    Less_than_primary <- c("Pas de scolarité ou arrêt avant la fin du primaire")
    
    Primary_com <- c("Aucun diplôme et scolarité interrompue à la fin du primaire ou avant la fin du collège",
                     "Aucun diplôme et scolarité jusqu’à la fin du collège ou au-delà",
                     "CEP (certificat d’études primaires)")
    
    Secondary_com <- c("BEPC, brevet élémentaire, brevet des collèges, DNB",
                       "CAP, BEP ou diplôme de niveau équivalent")
    
    University_com <- c("Baccalauréat général ou technologique, brevet supérieur, capacité en droit, DAEU, ESEU" ,
                        "Baccalauréat professionnel, brevet professionnel, de technicien ou d’enseignement, diplôme équivalent",
                        "BTS, DUT, Deug, Deust, diplôme de la santé ou du social de niveau bac+2, diplôme équivalent"  ,
                        "Licence, licence pro, maîtrise, diplôme équivalent de niveau bac+3 ou bac+4",
                        "Master, DEA, DESS, diplôme grande école niveau bac+5, doctorat de santé",
                        "Doctorat de recherche (hors santé)" ,
                        "Hors champ (moins de 14 ans)")
    
    Unknow <- c("Préfère ne pas dire")

    
  }
  
  else if(unique(data_fram$country)=="Italy"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    
    Less_than_primary <- c("Nessuna istruzione formale")
    
    Primary_com <- c("Qualifica scuola elementare (o equivalente)")
    
    Secondary_com <- c("Qualifica scuola media",
                       "Attestato di qualificazione professionale che non permette accesso all’università (2-3 anni) / Attestato di qualifica professionale (operatore)",
                       "Diploma di istruzione secondaria di II grado di 5 anni/ Istituto Formazione Tecnico Superiore (dal 2000) / Istituto Tecnico Superiore (2 anni)")
    
    University_com <- c("Diploma di Accademia (Belle Arti, Dramma Nazionale, Danza Nazionale), Conservatorio di Musica, Istituto di Musica",
                        "Laurea vecchio ordinamento",
                        "Laurea triennale (3 anni)",
                        "Laurea magistrale / Master (2 anni)",
                        "Laurea vecchio ordinamento (4-6 anni) / Laurea a ciclo unico (5-6anni)",
                        "Dottorato di ricerca")
    
    Unknow <- c("Non saprei","Preferisco non rispondere")

  }
  
  else if(unique(data_fram$country)=="Russia"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    
    Less_than_primary <- c("")
    
    Primary_com <- c("Неполное среднее")
    
    Secondary_com <- c("Среднее общее образование",
                       "Неоконченное высшее",
                       "Среднее профессиональное (техникум, училище)",
                       "Начальное профессиональное (профессиональный лицей, ПТУ)")
    
    University_com <- c("Высшее образование –бакалавриат",
                        "Высшее образование –специалитет",
                        "Высшее образование –магистратура",
                        "Высшее послевузовское образование (аспирантура, докторантура)")
    
    Unknow <- c("Не знаю","Предпочитаю не отвечать")
    
  }
  
  else if(unique(data_fram$country)=="Spain"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )

    Less_than_primary <- c("No sabe leer o escribir",
                           "Sabe leer y escribir pero fue menos de 5 años a la escuela",
                           "Fue a la escuela 5 o más años pero no llegó al último curso de ESO, EGB o Bachiller Elemental")
    
    Primary_com <- c("Llegó al último curso de ESO, EGB o Bachiller Elemental o tiene el Certificado de Escolaridad o de Estudios Primarios")
    
    Secondary_com <- c("Bachiller (LOE, LOGSE), BUP, Bachiller Superior, COU, PREU",
                       "FP grado medio, FP I, Oficialía Industrial o equivalente, Grado Medio de Música y Danza, Certificados de Escuelas Oficiales de Idiomas",
                       "FP grado superior, FP II, Maestría industrial o equivalente")
    
    University_com <- c("Diplomatura universitaria, Arquitectura Técnica, Ingeniería Técnica o equivalente", 
                        "Grado Universitario o equivalente",
                        "Licenciatura, Arquitectura, Ingeniería o equivalente",
                        "Master oficial universitario (a partir de 2006), Especialidades Médicas o análogos",
                        "Doctorado")
    
    Unknow <- c("No lo sé","Prefiero no responder")
    
    

    
  }
  else if(unique(data_fram$country)=="UK"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    
    Less_than_primary <- c("No formal qualifications")
    
    Primary_com <- c("Youth training certificate/skillseekers",
                     "Recognised trade apprenticeship completed",
                     "Clerical and commercial",
                     "City & Guilds certificate",
                     "City & Guilds certificate - advanced",
                     "ONC")
    
    
    Secondary_com <- c("CSE grades 2-5",
                       "CSE grade 1, GCE O level, GCSE, School Certificate",
                       "Scottish Ordinary/ Lower Certificate",
                       "GCE A level or Higher Certificate",
                       "Scottish Higher Certificate",
                       "Nursing qualification (e.g. SEN, SRN, SCM, RGN)",
                       "Teaching qualification (not degree)",     
                       "Other technical, professional or higher qualification")
    
    University_com <- c("University diploma",
                        "University or CNAA first degree (e.g. BA, B.Sc, B.Ed)",
                        "University or CNAA higher degree (e.g. M.Sc, Ph.D)")
    
    Unknow <- c("Do not know","Prefer not to say")
    
  }
  else if(unique(data_fram$country)=="US"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    
    Less_than_primary <- c("None")
    
    Primary_com <- c("Nursery to 8th Grade",
                     "Some high school")
    
    
    Secondary_com <- c("High school graduate, diploma (or equivalent)",
                       "Some college education, no degree",
                       "Training/vocational college",
                       "Associate degree")
    
    University_com <- c("Bachelor's degree",
                        "Master's degree (including professional degrees, or equivalent)",
                        "Doctorate degree")
    
    Unknow <- c("Do not know","Prefer not to say")

    
  }
  
  else if(unique(data_fram$country)=="China"){
    
    char1 <- function(string){
      string <-unlist(strsplit(string,""))[1]
      return(string)
    }
    
    data_fram$EDUCATION_LEVEL  <- lapply(data_fram$Q22.2,char1)
    
    Low <- c("从",
             "小",
             "初")
    Medium <- c("高",
                "职",
                "中",
                "高")
    High <- c("本",
              "研")
    
    
    Less_than_primary <- c("从")
    
    Primary_com <- c("小","初")
    
    Secondary_com <- c(
                       "高",
                       "职",
                       "中",
                       "高")
    
    University_com <- c("本",
                        "研")
    
    Unknow <- c(NA)
    

    
  }
  else if(unique(data_fram$country) == "Uganda"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL  = Q22.2)
    
    Less_than_primary <- c("Never been to school",
                           "Did not complete Pre-Primary",
                           "Completed Pre-Primary",
                           "In P1 but did not complete / attend Pre-primary",
                           "Did not complete P1",
                           "P1","P2","P3","P4","P5","P6")
    
    Primary_com <- c("P7","J1-J3","S1","S2","S3","S4","S5")
    
    
    Secondary_com <- c("S6","Professional Certificate","Diploma")
    
    University_com <- c("First Degree","Post Graduate Certificate",
                        "Post Graduate Diploma","Post Graduate Diploma","Masters Degree","PhD")
    
    Unknow <- c("Do not know","Prefer not to say","Other")
    
    
  }
  else if(unique(data_fram$country) == "India"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL  = Q22.2)
    
    Less_than_primary <- c("No formal education",
                           "Incomplete primary school")
    
    Primary_com <- c("Completed primary school","Middle pass / Matric fail",
                     "Matric pass / 10th pass","11th pass, not completed intermediate")
    
    
    Secondary_com <- c("12th pass / Intermediate")
    
    University_com <- c("Undergraduate - Bachelor's / Diploma","Postgraduate degree - Masters / PhD")
    
    Unknow <- c("")
    
    
  }
  else{
    return(print("There is not a survey for this country, try again"))}
  
  data_fram <- data_fram %>% mutate(EDUCATION_LEVEL = if_else(EDUCATION_LEVEL %in% Less_than_primary,"Less than primary completed",
                                                  if_else(EDUCATION_LEVEL %in% Primary_com  ,"Primary completed",
                                                          if_else(EDUCATION_LEVEL %in% Secondary_com ,"Secondary completed",
                                                                  if_else(EDUCATION_LEVEL %in% University_com,"University completed", "Unknown/missing")))))
  
 
  
  
  return(data_fram)
}


#######################################Employment###############################################################################

IPUMS_employment<-function(data_fram){
  if (unique(data_fram$country)=="Australia"){
    
    data_fram <- data_fram %>% mutate(EMPLOYMENT = if_else(Q22.2 == "No" | Q22.2 == "Do not know" | Q22.2 == "Prefer not to say" ,Q22.5,"Student"))
    
    Employed <- c("Working for payment or profit",
                  "Working, but absent on holidays or paid leave, on strike, or temporary stood down",
                  "Working, other unpaid job","Working, unpaid job in a family business")
    
    Unemployed <- c("Unemployed, looking for a full-time job","Unemployed, looking for a part-time job")
    
    Unemployed_never_work <- c("")
    
    Not_economic <- c("Unemployed, not looking for a job")
    
    Students <- c("Student")
    
    Pension_or_K <- c("")
    
    Homemakers <- c("")
    
    Others <- c("")
    
    Unknow <- c("Do not know","Prefer not to say")
    
  }
  else if(unique(data_fram$country)=="Brazil"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.3 )
    
    Employed <- c("Trabalho em tempo integral (30 ou mais horas por semana)",
                  "Trabalho em tempo parcial (8 a 29 horas por semana)",
                  "Trabalho em tempo parcial (menos de 8 horas por semana)")
    
    Unemployed <- c("Desempregado")
    
    Unemployed_never_work <- c("")
    
    Not_economic <- c("Sem trabalhar")
    
    Students <- c("Estudante em tempo integral")
    
    Pension_or_K <- c("Aposentado")
    
    Homemakers <- c("")
    
    Others <- c("Outro")
    
    Unknow <- c("Não sei","Prefiro não informar")
  
  }
  else if(unique(data_fram$country)=="Canada"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.3 )
    
    Employed <- c("Employed for wages",
                  "Self-employed",
                  "Military")
    
    Unemployed <- c("Out of work and looking for work")
    
    Unemployed_never_work <- c("")
    
    Not_economic <- c("Unable to work")
    
    Students <- c("Student")
    
    Pension_or_K <- c("Retired")
    
    Homemakers <- c("Out of work but not currently looking for work or Homemaker")
    
    Others <- c("")
    
    Unknow <- c("Do not know","Prefer not to say")
    
  }
  
  else if(unique(data_fram$country)=="Chile"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.8)
    
    
    Employed <- c("Por un pago en dinero o especies",
                  "Sin pago para un familiar",
                  "Tenía empleo pero estuvo de vacaciones, con licencia, en descanso laboral, etc.")
    
    Unemployed <- c("Se encontraba buscando empleo")
    
    Unemployed_never_work <- c("")
    
    Not_economic <- c("")
    
    Students <- c("Estaba estudiando")
    
    Pension_or_K <- c("Es jubilado, pensionado o rentista")
    
    Homemakers <- c("Realizó quehaceres de su hogar")
    
    Others <- c("Otra situación")
    
    Unknow <- c("No lo sé","Prefiero no responder")
    
    
  }
  
  else if(unique(data_fram$country)=="Colombia"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.4)
    
    Employed <- c("Trabajó por lo menos una hora en una actividad que le generó algún ingreso?",
                  "Trabajó o ayudó en un negocio por lo menos una hora sin que le pagaran?",
                  "No trabajó, pero tenía un empleo, trabajo o negocio por el que recibe ingresos?")
    
    Unemployed <- c("Buscó trabajo?")
    
    Unemployed_never_work <- c("")
    
    Not_economic <- c("Es incapacitado(a) permanentemente para trabajar?")
    
    Students <- c("Estudió?")
    
    Pension_or_K <- c("Vivió de jubilación, pensión o renta?")
    
    Homemakers <- c("Realizó oficios del hogar?")
    
    Others <- c("Estuvo en otra situación?")
    
    Unknow <- c("No lo sé","Prefiero no responder")
    
    
    
  }
  else if(unique(data_fram$country)=="France"){
    return(data_fram)
    
  }
  
  else if(unique(data_fram$country)=="Italy"){
    
    Employment <- function(ITA){
      ITA <- ITA %>% mutate(EMPLOYMENT = if_else(Q22.3=="Non in grado di lavorare permanentemente","Not active",
                                                 if_else(Q22.3 %in% c("Non saprei", "Preferisco non rispondere") & (Q22.4 == "Sì" | Q22.5 == "Sì"), "Employ",
                                                         if_else(Q22.3 == "Sì" & (Q22.4 == "Sì" | Q22.5 == "Sì"), "Employ", 
                                                                 if_else(Q22.3 == "Sì" & Q22.10=="Sì", "Unemploy", 
                                                                         if_else(Q22.3 == "No" & Q22.10=="Sì", "Unemploy_n", Q22.12))))))

                                                 
                                                         
    }
    
    data_fram <- Employment(data_fram)
    
    Employed <- c("Employ")
    
    Unemployed <- c("Unemploy")
    
    Unemployed_never_work <- c("Unemploy_n")
    
    Not_economic <- c("Not active")
    
    Students <- c("Uno studente")
    
    Pension_or_K <- c("Destinatario di una o più pensioni da un lavoro precedente o beneficiario di redditi (affitto, proprietà o azioni)")
    
    Homemakers <- c("Una casalinga")
    
    Others <- c("Altro")
    
    Unknow <- c("Non saprei","Preferisco non rispondere")
    
  }
  
  else if(unique(data_fram$country)=="Russia"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.3)
    
    Employed <- c("Работаю по найму",
                  "Индивидуальный предприниматель/ самозанятый",
                  "Работаю в вооруженных силах или правоохранительных органах")
    
    Unemployed <- c("Не работаю, ищу работу")
    
    Unemployed_never_work <- c("")
    
    Not_economic <- c("Не работаю, не ищу работу",
                      "Не в состоянии работать")
    
    Students <- c("Студент дневного отделения")
    
    Pension_or_K <- c("Пенсионер")
    
    Homemakers <- c("Домохозяйка")
    
    Others <- c("")
    
    Unknow <- c("Не знаю",
                "Предпочитаю не отвечать")
    
    
  }
  
  else if(unique(data_fram$country)=="Spain"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.3)
    
    
    Employed <- c("Ocupado/a a tiempo completo",
                  "Ocupado/a a tiempo parcial")
    
    Unemployed <- c("Parado/a que ha trabajado antes")
    
    Unemployed_never_work <- c("Parado/a buscando su primer empleo")
    
    Not_economic <- c("Persona con invalidez laboral permanente")
    
    Students <- c("")
    
    Pension_or_K <- c("Jubilado/a, prejubilado/a, pensionista o rentista")
    
    Homemakers <- c("")
    
    Others <- c("Otra situación")
    
    Unknow <- c("No lo sé","Prefiero no responder")
    
  }
  else if(unique(data_fram$country)=="UK"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.3)
    
    Employed <- c("Working full time (30 or more hours per week)",
                  "Working part time (8-29 hours a week)",
                  "Working part time (Less than 8 hours a week)")
    
    Unemployed <- c("Unemployed")
    
    Unemployed_never_work <- c("")
    
    Not_economic <- c("Not working")
    
    Students <- c("Full time student")
    
    Pension_or_K <- c("Retired")
    
    Homemakers <- c("")
    
    Others <- c("Other")
    
    Unknow <- c("Do not know","Prefer not to say")
    
  }
  else if(unique(data_fram$country)=="US"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.3)
    
    Employed <- c("Employed for wages",
                  "Self-employed",
                  "Military")
    
    Unemployed <- c("Out of work and looking for work")
    
    Unemployed_never_work <- c("")
    
    Not_economic <- c("Out of work but not currently looking for work",
                      "Unable to work")
    
    Students <- c("Student")
    
    Pension_or_K <- c("Retired")
    
    Homemakers <- c("Homemaker")
    
    Others <- c("")
    
    Unknow <- c("Do not know","Prefer not to say")
    
    
  }
  
  else if(unique(data_fram$country)=="China"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.4)
    
    char2 <- function(string){
      string <-unlist(substring(string , 1, 2))
      return(string)
    }
    
    data_fram$EMPLOYMENT <- unlist(lapply(data_fram$EMPLOYMENT,char2))
    
    Employed <- c("就业", "怀孕", "长期")
    
    Unemployed <- c("待业")
    
    Unemployed_never_work <- c("")
    
    Not_economic <- c("怀孕",
                      "长期")
    
    Students <- c("在校")
    
    Pension_or_K <- c("政府",
                      "企业")
    
    Homemakers <- c("全职")
    
    Others <- c("其他")
    
    Unknow <- c("")
    
    
  }
  else if(unique(data_fram$country) == "Uganda"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT  = Q22.3)
    
    Employed <- c("Working for pay","Employer","Own account worker",
                  "Helping without pay in a household business","Volunteer")
    
    Unemployed <- c("Looking for work")
    
    Unemployed_never_work <- c("")
    
    Not_economic <- c("Not working and not looking for work",
                      "Too young to work","Too old to work")
    
    Students <- c("Full time student")
    
    Pension_or_K <- c("Retired/pensioner")
    
    Homemakers <- c("Household chores")
    
    Others <- c("")
    
    Unknow <- c("Do not know","Prefer not to say")
    
  }
  else if(unique(data_fram$country) == "India"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT  = Q22.3)
    
    Employed <- c("Self-employed","Employed")
    
    Unemployed <- c("Unemployed")
    
    Unemployed_never_work <- c("")
    
    Not_economic <- c("")
    
    Students <- c("Still in education -  Pre-college (Public/private school)",
                  "Still in education - College/University")
    
    Pension_or_K <- c("Retired")
    
    Homemakers <- c("Working in the household")
    
    Others <- c("Others")
    
    Unknow <- c("")
    
  }
  else{
    return(print("There is not a survey for this country, try again"))}
  
  data_fram <- data_fram  %>% mutate(EMPLOYMENT = if_else(EMPLOYMENT %in% Employed,"Employed",
                                             if_else(EMPLOYMENT %in% Unemployed,"Unemployed",
                                                     if_else(EMPLOYMENT %in% Unemployed_never_work,"Unemployed, never worked before",
                                                             if_else(EMPLOYMENT %in% Not_economic ,"Not economically active, unspecified",
                                                                     if_else(EMPLOYMENT %in% Students ,"Students", 
                                                                             if_else(EMPLOYMENT %in% Pension_or_K ,"Pension or capital income recipients",
                                                                                     if_else(EMPLOYMENT %in% Homemakers,"Homemakers",
                                                                                             if_else(EMPLOYMENT %in% Others,"Others or Homemakers or Students ", "Unknown/missing")))))))))
  
  
  
  return(data_fram)
}

#######################################WORK###############################################################################

IPUMS_work<-function(data_fram){
  if (unique(data_fram$country)=="Australia"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.7 )
    
    
    Leg_manag <- c("Legislator, senior officials and manager")
    
    Professionals <- c("Professional")
    
    Tec_assoc_pro <- c("Technician and associate professional")
    
    Cler <- c("Clerk")
    
    Serv_sales <- c("Service worker and shop and market sale")
    
    Farmers_fi <- c("Skilled agricultural and fishery worker")
    
    Crafts <- c("Craft and related trades worker")
    
    Plan_opera <- c("Plant and machine operator and assembler")
    
    Element_ocu <- c("Elementary occupation")
    
    Armed_forces <- c("Armed forces")
    
    Other_occu <- c("Other occupations, unspecified or n.e.c.")
    
    Unknow <- c("Do not know","Prefer not to say")
  }
  else if(unique(data_fram$country)=="Brazil"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.5 )
    
    Leg_manag <- c("Diretores e gerentes")
    
    Professionals <- c("Profissionais das ciências e intelectuais")
    
    Tec_assoc_pro <- c("Técnicos e profissionais de nível médio")
    
    Cler <- c("Trabalhadores de apoio administrativo")
    
    Serv_sales <- c("Trabalhadores dos serviços, vendedores dos comércios e mercados")
    
    Farmers_fi <- c("Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca")
    
    Crafts <- c("Trabalhadores qualificados, operários e artesãos da construção, das artes mecânicas e outros ofícios")
    
    Plan_opera <- c("Operadores de instalações e máquinas e montadores")
    
    Element_ocu <- c("Ocupações elementares")
    
    Armed_forces <- c("Membros das forças armadas, policiais e bombeiros militares")
    
    Other_occu <- c("")
    
    Unknow <- c("Não sei","Prefiro não informar")
    
    
  }
  else if(unique(data_fram$country)=="Canada"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.4 )
    
    Leg_manag <- c("Senior management occupations",
                   "Specialized middle management occupations",
                   "Middle management occupations in retail and wholesale trade and customer services",
                   "Middle management occupations in trades, transportation, production and utilities")
    
    Professionals <- c("Professional occupations in business and finance",
                       "Professional occupations in natural and applied sciences",
                       "Professional occupations in nursing",
                       "Professional occupations in health (except nursing)",
                       "Professional occupations in education services",
                       "Professional occupations in law and social, community and government services",
                       "Professional occupations in art and culture")
    
    Tec_assoc_pro <- c("Administrative and financial supervisors and administrative occupations",
                       "Finance, insurance and related business administrative occupations",
                       "Technical occupations related to natural and applied sciences",
                       "Technical occupations in health",
                       "Paraprofessional occupations in legal, social, community and education services",
                       "Technical occupations in art, culture, recreation and sport",
                       "Service representatives and other customer and personal services occupations",
                       "Supervisors and technical occupations in natural resources, agriculture and related production",
                       "Processing, manufacturing and utilities supervisors and central control operators")
    
    Cler <- c("Distribution, tracking and scheduling co-ordination occupations",
              "Office support occupations",
              "Assisting occupations in support of health services",
              "Sales support occupations",
              "Service support and other service occupations, n.e.c.")
    
    Serv_sales <- c("Occupations in front-line public protection services",
                    "Care providers and educational, legal and public protection support occupations", 
                    "Retail sales supervisors and specialized sales occupations",
                    "Service supervisors and specialized service occupations",
                    "Sales representatives and salespersons - wholesale and retail trade")
    
    Farmers_fi <- c("Workers in natural resources, agriculture and related production",
                    "Harvesting, landscaping and natural resources labourers")
    
    Crafts <- c("Industrial, electrical and construction trades",
                "Processing and manufacturing machine operators and related production workers")
    
    Plan_opera <- c("Transport and heavy equipment operation and related maintenance occupations",
                    "Assemblers in manufacturing",
                    "Labourers in processing, manufacturing and utilities")
    
    Element_ocu <- c("Maintenance and equipment operation trades",
                     "Other installers, repairers and servicers and material handlers",
                     "Trades helpers, construction labourers and related occupations")
    
    Armed_forces <- c("NAN")
    
    Other_occu <- c("NON")
    
    Unknow <- c("Do not know","Prefer not to say" )
    
  }
  
  else if(unique(data_fram$country)=="Chile"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.10 )
    
    Leg_manag <- c("Directores, gerentes y administradores")
    
    Professionals <- c("Profesionales, cientificos e intelectuales")
    
    Tec_assoc_pro <- c("Tecnicos y profesionales de nivel medio")
    
    Cler <- c("Personal de apoyo administrativo")
    
    Serv_sales <- c("Trabajadores de los servicios y vendedores de comercios y mercados")
    
    Farmers_fi <- c("Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros")
    
    Crafts <- c("Artesanos y operarios de oficios")
    
    Plan_opera <- c("Operadores de instalaciones, máquinas y ensambladores")
    
    Element_ocu <- c("Ocupaciones elementales")
    
    Armed_forces <- c("NON")
    
    Other_occu <- c("Otro")
    
    Unknow <- c("No lo sé","Prefiero no responder" )
    
    
  }
  
  else if(unique(data_fram$country)=="Colombia"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.6 )
    
    Leg_manag <- c("Directores, gerentes y administradores")
    
    Professionals <- c("Profesionales, cientificos e intelectuales")
    
    Tec_assoc_pro <- c("Tecnicos y profesionales de nivel medio")
    
    Cler <- c("Personal de apoyo administrativo")
    
    Serv_sales <- c("Trabajadores de los servicios y vendedores de comercios y mercados")
    
    Farmers_fi <- c("Agricultores y trabajadores calificados agropecuarios, forestales y pesqueros")
    
    Crafts <- c("Artesanos y operarios de oficios")
    
    Plan_opera <- c("Operadores de instalaciones, máquinas y ensambladores")
    
    Element_ocu <- c("Ocupaciones elementales")
    
    Armed_forces <- c("NON")
    
    Other_occu <- c("Otro")
    
    Unknow <- c("No lo sé","Prefiero no responder" )
    
    
    
  }
  else if(unique(data_fram$country)=="France"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.4 )
    
    Leg_manag <- c("Chefs d'entreprise de 10 salariés ou plus",
                   "Cadres d'entreprise")
    
    Professionals <- c("Professions libérales et assimilés",
                       "Cadres de la fonction publique, professions intellectuelles et artistiques")
    
    Tec_assoc_pro <- c("Professions intermédiaires de l'enseignement, de la santé, de la fonction publique et assimilés",
                       "Professions intermédiaires administratives et commerciales des entreprises",
                       "Techniciens",
                       "Contremaîtres, agents de maîtrise")
    
    Cler <- c("Employés de la fonction publique",
              "Employés administratifs d'entreprise",
              "Employés de commerce")
    
    Serv_sales <- c("Commerçants et assimilés",
                    "Personnels des services directs aux particuliers")
    
    Farmers_fi <- c("Agriculteurs exploitants",
                    "Ouvriers agricoles")
    
    Crafts <- c("Artisans","Ouvriers qualifiés")
    
    Plan_opera <- c("Non")
    
    Element_ocu <- c("Ouvriers non qualifiés")
    
    Armed_forces <- c("NAN")
    
    Other_occu <- c("NiN")
    
    Unknow <- c("Préfère ne pas dire")
    
    
  }
  
  else if(unique(data_fram$country)=="Italy"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.6 )
    
    Leg_manag <- c("Legislatori, imprenditori e alta dirigenza")
    
    Professionals <- c("Professioni intellettuali, scientifiche e di elevata specializzazione")
    
    Tec_assoc_pro <- c("Professioni tecniche")
    
    Cler <- c("Professioni esecutive nel lavoro d'ufficio")
    
    Serv_sales <- c("Professioni qualificate nelle attivita' commerciali e nei servizi")
    
    Farmers_fi <- c("Artigiani, operai specializzati e agricoltori")
    
    Crafts <- ""
    
    Plan_opera <- c("Conduttori di impianti, operai di macchinari fissi e mobili e conducenti di veicoli")
    
    Element_ocu <- c("Professioni non qualificate")
    
    Armed_forces <- c("Forze armate")
    
    Other_occu <- c("NoN")
    
    Unknow <- c("Non saprei","Preferisco non rispondere")
    
  }
  
  else if(unique(data_fram$country)=="Spain"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.4 )
    
    Leg_manag <- c("Miembros del poder ejecutivo y de los cuerpos legislativos; directivos de la Administración Pública y organizaciones de interés social; directores ejecutivos",
                   "Directores de departamentos administrativos y comerciales",
                   "Directores de producción y operaciones",
                   "Directores y gerentes de empresas de alojamiento, restauración y comercio",
                   "Directores y gerentes de otras empresas de servicios no clasificados bajo otros epígrafes")
    
    Professionals <- c("Profesionales de la salud",
                       "Profesionales de la enseñanza infantil, primaria, secundaria y postsecundaria",
                       "Otros profesionales de la enseñanza",
                       "Profesionales de la ciencias físicas, químicas, matemáticas y de las ingenierías",
                       "Profesionales en derecho",
                       "Especialistas en organización de la Administración Pública y de las empresas y en la comercialización",
                       "Profesionales de las tecnologías de la información",
                       "Profesionales en ciencias sociales",
                       "Profesionales de la cultura y el espectáculo")
    
    Tec_assoc_pro <- c("Técnicos de las ciencias y de las ingenierías",
                       "Supervisores en ingeniería de minas, de industrias manufactureras y de la construcción",
                       "Técnicos sanitarios y profesionales de las terapias alternativas",
                       "Técnicos de las tecnologías de la información y las comunicaciones (TIC)")
    
    Cler <- c("Profesionales de apoyo en finanzas y matemáticas",
              "Profesionales de apoyo a la gestión administrativa; técnicos de las fuerzas y cuerpos de seguridad",
              "Profesionales de apoyo de servicios jurídicos, sociales, culturales, deportivos y afines",
              "Empleados en servicios contables, financieros, y de servicios de apoyo a la producción y al transporte",
              "Otros empleados administrativos sin tareas de atención al público",
              "Empleados administrativos con tareas de atención al público no clasificados bajo otros epígrafes")
    
    Serv_sales <- c("Representantes, agentes comerciales y afines",
                    "Empleados de bibliotecas, servicios de correos y afines",
                    "Empleados de agencias de viajes, recepcionistas y telefonistas; empleados de ventanilla y afines (excepto taquilleros)",
                    "Camareros y cocineros propietarios",
                    "Trabajadores asalariados de los servicios de restauración",
                    "Dependientes en tiendas y almacenes",
                    "Comerciantes propietarios de tiendas",
                    "Vendedores (excepto en tiendas y almacenes)",
                    "Cajeros y taquilleros (excepto bancos)",
                    "Trabajadores de los cuidados a las personas en servicios de salud",
                    "Otros trabajadores de los cuidados a las personas",
                    "Trabajadores de los servicios personales",
                    "Trabajadores de los servicios de protección y seguridad")
    
    Farmers_fi <- c("Trabajadores cualificados en actividades agrícolas",
                    "Trabajadores cualificados en actividades ganaderas, (incluidas avícolas, apícolas y similares)",
                    "Trabajadores cualificados en actividades agropecuarias mixtas",
                    "Trabajadores cualificados en actividades forestales, pesqueras y cinegéticas",
                    "Peones agrarios, forestales y de la pesca")
    
    Crafts <- c("Soldadores, chapistas, montadores de estructuras metálicas, herreros, elaboradores de herramientas y afines",
                "Mecánicos y ajustadores de maquinaria",
                "Trabajadores especializados en electricidad y electrotecnología",
                "Mecánicos de precisión en metales, ceramistas, vidrieros, artesanos y trabajadores de artes gráficas",
                "Trabajadores de la madera, textil, confección, piel, cuero, calzado y otros operarios en oficios")
    
    Plan_opera <- c("Operadores de instalaciones y maquinaria fijas",
                    "Montadores y ensambladores en fábricas",
                    "Maquinistas de locomotoras, operadores de maquinaria agrícola y de equipos pesados móviles, y marineros",
                    "Conductores de vehículos para el transporte urbano o por carretera")
    
    Element_ocu <- c("Trabajadores en obras estructurales de construcción y afines",
                     "Trabajadores de acabado de construcciones e instalaciones (excepto electricistas), pintores y afines",
                     "Trabajadores de la industria de la alimentación, bebidas y tabaco",
                     "Empleados domésticos",
                     "Otro personal de limpieza",
                     "Ayudantes de preparación de alimentos",
                     "Recogedores de residuos urbanos, vendedores callejeros y otras ocupaciones elementales en servicios",
                     "Peones de la construcción y de la minería",
                     "Peones de las industrias manufactureras",
                     "Peones del transporte, descargadores y reponedores")
    
    Armed_forces <- c()
    
    Other_occu <- c()
    
    Unknow <- c()
    
    
    
    
  }
  else if(unique(data_fram$country)=="UK"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.4 )
    
    Leg_manag <- c("General management, director or top management (managing director, director general, other director)")
    
    Professionals <- c("Professional (Lawyer, medical practitioner, accountant, architect, etc.)",
                       "Employed professional (employer doctor, lawyer, accountant, architect)")
    
    Tec_assoc_pro <- c("Middle management, other management (department head, junior manager, teacher, technicien)",
                       "Supervisor")
    
    Cler <- c("Employed position, working mainly at a desk")
    
    Serv_sales <- c("Employed position, not at a desk but travelling (salesman, driver, etc.)",
                    "Employed position, not at a desk, but in a service job (hospital, restaurant, finement, etc.)")
    
    Farmers_fi <- c("Farmer",
                    "Fisherman")
    
    Crafts <- c("Owner of a shop, craftsmen, other self-employer person",
                "Skilled manuel worker")
    
    Plan_opera <- c()
    
    Element_ocu <- c("Other (unskilled) manuel worker, servant")
    
    Armed_forces <- c()
    
    Other_occu <- c()
    
    Unknow <- c("Do not know",
                "Prefer not to say",
                "Business proprietor, owner (full or partner) of a company")
    
  }
  else if(unique(data_fram$country)=="US"){
    
    data_fram <- data_fram %>%  rename(OCCUPATION = Q22.4 )
    
    Leg_manag <- c("Management, business, science, and arts")
    
    Professionals <- c(
      "Financial specialists",
      "Computer and mathematical",
      "Architecture and engineering",
      "Life, physical, and social science")
    
    Tec_assoc_pro <- c("Business operations specialists",
                       "Technicians")
    
    Cler <- c("Community and social services",
              "Healthcare support",
              "Office and administrative support")
    
    Serv_sales <- c("Legal",
                    "Education, training, and library",
                    "Healthcare practitioners and technical",
                    "Protective service",
                    "Food preparation and serving",
                    "Personal care and service",
                    "Sales and related")
    
    Farmers_fi <- c("Farming, fishing, and forestry")
    
    Crafts <- c("Arts, design, entertainment, sports, and media",
                "Installation, maintenance, and repair")
    
    Plan_opera <- c("Production",
                    "Transportation and material moving")
    
    Element_ocu <- c("Building and grounds cleaning and maintenance",
                     "Construction",
                     "Extraction")
    
    Armed_forces <- c("Military specific")
    
    Other_occu <- c("")
    
    Unknow <- c("I do not know",
                "Prefer not to say")
    
    
  }
  
  else if(unique(data_fram$country)=="China"){
    
    data_fram <- data_fram %>%  rename(OCCUPATION = Q22.5 )
    
    Leg_manag <- c("​国家机关、党组织、企业和事业单位人员")
    
    Professionals <- c("专业技术人员")
    
    Tec_assoc_pro <- ""
    
    Cler <- c("​文员及相关人员")
    
    Serv_sales <- c("商业和服务人员")
    
    Farmers_fi <- c("​农业、林业、畜牧业、渔业和水源生产商")
    
    Crafts <- c("Nones")
    
    Plan_opera <- c("​制造和运输设备机械操作人员及相关人员")
    
    Element_ocu <- c("Nones")
    
    Armed_forces <- c("军人")
    
    Other_occu <- c("其他从业人员")
    
    Unknow <- c("Noe")
    
  }
  else if(unique(data_fram$country) == "Uganda"){
    
    data_fram <- data_fram %>% rename(OCCUPATION  = Q22.4)
    
    Leg_manag <- c("General management, director or top management (managing director, director general, other director)")
    
    Professionals <- c("Professional (Lawyer, medical practitioner, accountant, architect, etc.)",
                       "Employed professional (employer doctor, lawyer, accountant, architect)")
    
    Tec_assoc_pro <- c("Middle management, other management (department head, junior manager, teacher, technicien)",
                       "Supervisor")
    
    Cler <- c("Employed position, working mainly at a desk")
    
    Serv_sales <- c("Employed position, not at a desk but travelling (salesman, driver, etc.)",
                    "Employed position, not at a desk, but in a service job (hospital, restaurant, finement, etc.)")
    
    Farmers_fi <- c("Farmer",
                    "Fisherman")
    
    Crafts <- c("Owner of a shop, craftsmen, other self-employer person",
                "Skilled manuel worker")
    
    Plan_opera <- c("")
    
    Element_ocu <- c("Other (unskilled) manuel worker, servant")
    
    Armed_forces <- c("")
    
    Other_occu <- c("")
    
    Unknow <- c("Do not know",
                "Prefer not to say",
                "Business proprietor, owner (full or partner) of a company")
    
    
  }
  else if(unique(data_fram$country) == "India"){
    
    data_fram <- data_fram %>% rename(OCCUPATION  = Q22.4)
    
    Leg_manag <- c("")
    
    Professionals <- c("Profession NEC")
    
    Tec_assoc_pro <- c("")
    
    Cler <- c("")
    
    Serv_sales <- c("Petty shop/Small business",
                    "Organized Trade/Business")
    
    Farmers_fi <- c("Cultivation",
                    "Allied agriculture",
                    "Agricultural wage labour")
    
    Crafts <- c("Artisan/Independent")
    
    Plan_opera <- c("")
    
    Element_ocu <- c("")
    
    Armed_forces <- c("")
    
    Other_occu <- c("Others")
    
    Unknow <- c("Non agricultural wage labour",
                "Salaried employment")
    
  }
  else{
    print("There is not a survey for this country, try again")
    return(data_fram)}
  
  data_fram <- data_fram %>% mutate(OCCUPATION = if_else(OCCUPATION %in% Leg_manag,"Legislator, senior officials and manager",
                                                         if_else(OCCUPATION %in% Professionals,"Professional",
                                                                 if_else(OCCUPATION %in% Tec_assoc_pro  ,"Technician and associate professional",
                                                                         if_else(OCCUPATION %in% Cler ,"Clerk", 
                                                                                 if_else(OCCUPATION %in% Serv_sales ,"Service worker and shop and market sale",
                                                                                         if_else(OCCUPATION %in% Farmers_fi,"Skilled agricultural and fishery worker",
                                                                                                 if_else(OCCUPATION %in% Crafts,"Craft and related trades worker",
                                                                                                         if_else(OCCUPATION %in% Plan_opera,"Plant and machine operator and assembler",
                                                                                                                 if_else(OCCUPATION %in% Element_ocu ,"Elementary occupation",
                                                                                                                         if_else(OCCUPATION %in% Armed_forces,"Armed forces",
                                                                                                                                 if_else(OCCUPATION %in% Other_occu,"Other occupations, unspecified or n.e.c.", "Unknown/missing"))))))))))))
  
  
  return(data_fram)
}


#######################################INCOME#####################################

INCOME <- function(data_fram){
  if (unique(data_fram$country) == "Australia"){
    
    INCOME_LOW_str <- "[$]1[-]|7,800|15,600|20,800|26,000|33,800|41,600|52,000|65,000|78,000|91,000|104,000|156,000"
    INCOME_HIGH_str <- "7,799|15,599|20,799|25,999|33,799|41,599|51,999|64,999|77,999|90,999|103,999|155,999"
    INCOME_HH_LOW_str <- "[$]1[-]|7,800|15,600|20,800|26,000|33,800|41,600|52,000|65,000|78,000|91,000|104,000|130,000|156,000|182,000|208,000|234,000|260,000|312,000|416,000"
    INCOME_HH_HIGH_str <- "7,799|15,599|20,799|25,999|33,799|41,599|51,999|64,999|77,999|90,999|103,999|129,999|155,999|181,999|207,999|233,999|259,999|311,999|415,999"
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub(",|[$]|[-]", "", str_extract(data_fram$INCOME, INCOME_LOW_str))),
             INCOME_HIGH  = as.numeric(gsub(",", "", str_extract(data_fram$INCOME, INCOME_HIGH_str))),
             INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_LOW  = as.numeric(gsub(",|[$]|[-]", "", str_extract(data_fram$HH_INCOME, INCOME_HH_LOW_str))),
             INCOME_HH_HIGH  = as.numeric(gsub(",", "", str_extract(data_fram$HH_INCOME, INCOME_HH_HIGH_str))),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram),
             INCOME_PPP = 1.471741)
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "Brazil"){
    
    INCOME_LOW_str <- "Menos|523|1.045|2.090|3.135|5.225|10.450|20.900"
    INCOME_HIGH_str <- "Menos|a R[$] 1.045|a R[$] 2.090|a R[$] 3.135|a R[$] 5.225|a R[$] 10.450|a R[$] 20.900"
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub("[.]", "", 
                                           gsub("Menos", "0", 
                                                str_extract(data_fram$INCOME, INCOME_LOW_str)))),
             INCOME_HIGH  = str_extract(data_fram$INCOME, INCOME_HIGH_str),
             INCOME_HH_LOW  = as.numeric(gsub("[.]", "", 
                                              gsub("Menos", "0", 
                                                   str_extract(data_fram$HH_INCOME, INCOME_LOW_str)))),
             INCOME_HH_HIGH  = str_extract(data_fram$HH_INCOME, INCOME_HIGH_str),
             INCOME_PPP = 2.36170285287039)
    
    data_fram$INCOME_HIGH <- data_fram$INCOME_HIGH %>%
      recode("Menos" = 523, "a R$ 1.045" = 1045,
             "a R$ 2.090" = 2090, "a R$ 3.135" = 3135,
             "a R$ 5.225" = 5225, "a R$ 10.450" = 10450,
             "a R$ 20.900" = 20900)
    
    data_fram$INCOME_HH_HIGH <- data_fram$INCOME_HH_HIGH %>%
      recode("Menos" = 523, "a R$ 1.045" = 1045,
             "a R$ 2.090" = 2090, "a R$ 3.135" = 3135,
             "a R$ 5.225" = 5225, "a R$ 10.450" = 10450,
             "a R$ 20.900" = 20900)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "Canada"){
    
    INCOME_LOW_str <- "Under|2,000|5,000|7,000|10,000|12,000|15,000|17,000|20,000|25,000|30,000|35,000|40,000|45,000|50,000|55,000|60,000|65,000|70,000|75,000|80,000|85,000|90,000|95,000|100,000|110,000|120,000|135,000|150,000|175,000|200,000|250,000"
    INCOME_HIGH_str <- "Under [$]2,000|4,999|6,999|9,999|11,999|14,999|16,999|19,999|24,999|29,999|34,999|39,999|44,999|49,999|54,999|59,999|64,999|69,999|74,999|79,999|84,999|89,999|94,999|99,999|109,999|119,999|134,999|149,999|174,999|199,999|249,999"
    INCOME_HH_LOW_str <- "Under|2,000|5,000|7,000|10,000|12,000|15,000|17,000|20,000|25,000|30,000|35,000|40,000|45,000|50,000|55,000|60,000|65,000|70,000|75,000|80,000|85,000|90,000|95,000|100,000|110,000|120,000|135,000|150,000|175,000|200,000|250,000"
    INCOME_HH_HIGH_str <- "Under|4,999|6,999|9,999|11,999|14,999|16,999|19,999|24,999|29,999|34,999|39,999|44,999|49,999|54,999|59,999|64,999|69,999|74,999|79,999|84,999|89,999|94,999|99,999|109,999|119,999|134,999|149,999|174,999|199,999|249,999"
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub(",", "", 
                                           gsub("Under", "0", 
                                                str_extract(data_fram$INCOME, INCOME_LOW_str)))),
             INCOME_HIGH = as.numeric(gsub(",", "", 
                                           gsub("Under [$]2,000", "2,000", 
                                                str_extract(data_fram$INCOME, INCOME_HIGH_str)))),
             INCOME_HH_LOW  = as.numeric(gsub(",", "", 
                                              gsub("Under", "0", 
                                                   str_extract(data_fram$HH_INCOME, INCOME_HH_LOW_str)))),
             INCOME_HH_HIGH = as.numeric(gsub(",", "", 
                                              gsub("Under", "2,000", 
                                                   str_extract(data_fram$HH_INCOME, INCOME_HH_HIGH_str)))),
             INCOME_PPP = 1.206376)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "Chile"){
    
    INCOME_LOW_str <- "[$] 0 a [$]35.000|35.001|60.001|100.001|200.001|350.001|500.001|750.001|1.000.001|1.500.001|2.000.001|3.000.001|5.000.001|7.500.001|10.000.001|15.000.001|Más"
    INCOME_HIGH_str <- "Menos|35.000|60.000|100.000|200.000|350.000|500.000|750.000|1.000.000|1.500.000|2.000.000|3.000.000|5.000.000|7.500.000|10.000.000|15.000.000|[$] 15.000.001 a [$] 20.000.000"
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub("[.]", "", 
                                           gsub("[$] 0 a [$]35.000", "0", 
                                                gsub("Más", "20.000.000", str_extract(data_fram$INCOME, INCOME_LOW_str))))),
             INCOME_HIGH = as.numeric(gsub("[.]", "", 
                                           gsub("Menos", "17.500", 
                                                gsub("[$] 15.000.001 a [$] 20.000.000", "20.000.000",
                                                     str_extract(data_fram$INCOME, INCOME_HIGH_str))))),
             INCOME_HH_LOW  = as.numeric(gsub("[.]", "", 
                                              gsub("[$] 0 a [$]35.000", "0", 
                                                   gsub("Más", "20.000.000", str_extract(data_fram$HH_INCOME, INCOME_LOW_str))))),
             INCOME_HH_HIGH = as.numeric(gsub("[.]", "", 
                                              gsub("Menos", "17.500", 
                                                   gsub("[$] 15.000.001 a [$] 20.000.000", "20.000.000",
                                                        str_extract(data_fram$HH_INCOME, INCOME_HIGH_str))))),
             INCOME_PPP = 418.432299)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "China"){
    
    INCOME_LOW_str <- paste("每年收入低于",
                            "42,500",
                            "85,000",
                            "127,500",
                            "170,000",
                            "212,500",
                            "255,000",
                            "297,500",
                            "340,000",
                            "382,500",
                            "425,000",
                            "510,000",
                            "595,000",
                            "850,000",
                            "127,5000",
                            sep = "|")
    INCOME_HIGH_str <- paste("每年收入低于",
                             "84,999",
                             "127,499",
                             "169,999",
                             "212,499",
                             "254,999",
                             "297,499",
                             "339,999",
                             "382,499",
                             "424,999",
                             "509,999",
                             "594,999",
                             "849,999",
                             "1274,999", 
                             sep = "|")
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub(",", "", 
                                           gsub("每年收入低于", "0", 
                                                str_extract(data_fram$INCOME, INCOME_LOW_str)))),
             INCOME_HIGH  = as.numeric(gsub(",", "", 
                                            gsub("每年收入低于", "42,500", 
                                                 str_extract(data_fram$INCOME, INCOME_HIGH_str)))),
             INCOME_HH_LOW  = as.numeric(gsub(",", "", 
                                              gsub("每年收入低于", "0", 
                                                   str_extract(data_fram$HH_INCOME, INCOME_LOW_str)))),
             INCOME_HH_HIGH  = as.numeric(gsub(",", "", 
                                               gsub("每年收入低于", "42,500", 
                                                    str_extract(data_fram$HH_INCOME, INCOME_HIGH_str)))),
             INCOME_PPP = 4.18560182367734)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "Colombia"){
    
    INCOME_LOW_str <- "Menos|Más de [$]260.000|570.000|810.000|1.030.000|1.270.000|1.580.000|1.980.000|2.540.000|Más de [$]3.560.000"
    INCOME_HIGH_str <- paste("Menos de [$]260.000",
                             "menos de [$]570.000",
                             "menos de [$]810.000",
                             "menos de [$]1.030.000",
                             "menos de [$]1.270.000",
                             "menos de [$]1.580.000",
                             "menos de [$]1.980.000",
                             "menos de [$]2.540.000",
                             "menos de [$]3.560.000",
                             sep = "|")
    INCOME_HH_LOW_str <- "Menos|Más de [$]260.000|570.000|810.000|1.030.000|1.270.000|1.580.000|1.980.000|2.540.000|3.560.000|Más de [$]6.000.000"
    INCOME_HH_HIGH_str <- paste("Menos de [$]260.000",
                                "menos de [$]570.000",
                                "menos de [$]810.000",
                                "menos de [$]1.030.000",
                                "menos de [$]1.270.000",
                                "menos de [$]1.580.000",
                                "menos de [$]1.980.000",
                                "menos de [$]2.540.000",
                                "menos de [$]3.560.000",
                                "menos de [$]6.000.000",
                                sep = "|")
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub("[.]", "", 
                                           gsub("Menos", "0", 
                                                gsub("Más de [$]3.560.000", "3.560.000", 
                                                     gsub("Más de [$]260.000", "260.000", str_extract(data_fram$INCOME, INCOME_LOW_str)))))),
             INCOME_HIGH  = str_extract(data_fram$INCOME, INCOME_HIGH_str),
             INCOME_HH_LOW  = as.numeric(gsub("[.]", "", 
                                              gsub("Menos", "0", 
                                                   gsub("Más de [$]6.000.000", "6.000.000", 
                                                        gsub("Más de [$]260.000", "260.000", str_extract(data_fram$HH_INCOME, INCOME_HH_LOW_str)))))),
             INCOME_HH_HIGH  = str_extract(data_fram$HH_INCOME, INCOME_HH_HIGH_str),
             INCOME_PPP = 1352.785587)
    
    data_fram$INCOME_HIGH <- data_fram$INCOME_HIGH %>%
      recode("Menos de $260.000" = 260000, "menos de $570.000" = 570000,
             "menos de $810.000" = 810000, "menos de $1.030.000" = 1030000,
             "menos de $1.270.000" = 1270000, "menos de $1.580.000" = 1580000,
             "menos de $1.980.000" = 1980000, "menos de $2.540.000" = 2540000,
             "menos de $3.560.000" = 3560000)
    
    data_fram$INCOME_HH_HIGH <- data_fram$INCOME_HH_HIGH %>%
      recode("Menos de $260.000" = 260000, "menos de $570.000" = 570000,
             "menos de $810.000" = 810000, "menos de $1.030.000" = 1030000,
             "menos de $1.270.000" = 1270000, "menos de $1.580.000" = 1580000,
             "menos de $1.980.000" = 1980000, "menos de $2.540.000" = 2540000,
             "menos de $3.560.000" = 3560000, "menos de $6.000.000" = 6000000)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "France"){
    
    INCOME_LOW_str <- "Moins|Plus de 500|1.000|1.500|2.000|2.500|Plus de 3.000 €"
    INCOME_HIGH_str <- "Moins de 500 €|moins de 1.000|moins de 1.500|moins de 2.000|moins de 2.500|moins de 3.000 €"
    INCOME_HH_LOW_str <- "Moins|Plus de 500|1.000|1.500|2.000|2.500|3.000|5.000|7.000|9.000 et plus €"
    INCOME_HH_HIGH_str <- "Moins de 500 €|moins de 1.000|moins de 1.500|moins de 2.000|moins de 2.500|moins de 3.000|moins de 5.000|moins de 7.000|moins de 9.000"
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub("[.]", "", 
                                           gsub("Moins", "0", 
                                                gsub("Plus de 3.000 €", "3.000",
                                                     gsub("Plus de 500", "500", str_extract(data_fram$INCOME, INCOME_LOW_str)))))),
             INCOME_HIGH  = str_extract(data_fram$INCOME, INCOME_HIGH_str),
             INCOME_HH_LOW  = as.numeric(gsub("[.]", "", 
                                              gsub("Moins", "0", 
                                                   gsub("9.000 et plus €", "9.000",
                                                        gsub("Plus de 500", "500", str_extract(data_fram$HH_INCOME, INCOME_HH_LOW_str)))))),
             INCOME_HH_HIGH  = str_extract(data_fram$HH_INCOME, INCOME_HH_HIGH_str),
             INCOME_PPP = 0.731532)
    
    data_fram$INCOME_HIGH <- data_fram$INCOME_HIGH %>%
      recode("Moins de 500 €" = 500, "moins de 1.000" = 1000,
             "moins de 1.500" = 1500, "moins de 2.000" = 2000,
             "moins de 2.500" = 2500, "moins de 3.000 €" = 3000)
    
    data_fram$INCOME_HH_HIGH <- data_fram$INCOME_HH_HIGH %>%
      recode("Moins de 500 €" = 500, "moins de 1.000" = 1000,
             "moins de 1.500" = 1500, "moins de 2.000" = 2000,
             "moins de 2.500" = 2500, "moins de 3.000" = 3000,
             "moins de 5.000" = 5000, "moins de 7.000" = 7000,
             "moins de 9.000" = 9000)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "India"){
    
    INCOME_LOW_str <- "Less|30,000|60,000|90,000|1,20,000|2,40,000|6,00,000|₹10,00,000 and over"
    INCOME_HIGH_str <- "Less|₹30,000 - ₹60,000|89,999|1,19,999|2,39,999|5,99,999|₹6,00,000 - ₹10,00,000"
    INCOME_HH_LOW_str <- "Less|60,000|90,000|1,20,000|2,40,000|₹6,00,000 and over"
    INCOME_HH_HIGH_str <- "Less|89,999|1,19,999|2,39,999|5,99,999"
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub(",", "", 
                                           gsub("Less", "0", 
                                                gsub("₹10,00,000 and over", "10,00,000", 
                                                     str_extract(data_fram$INCOME, INCOME_LOW_str))))),
             INCOME_HIGH  = as.numeric(gsub(",", "", 
                                            gsub("Less", "30,000", 
                                                 gsub("₹30,000 - ₹60,000", "60,000",
                                                      gsub("₹6,00,000 - ₹10,00,000", "10,00,000",
                                                           str_extract(data_fram$INCOME, INCOME_HIGH_str)))))),
             INCOME_HH_LOW  = as.numeric(gsub(",", "", 
                                              gsub("Less", "0", 
                                                   gsub("₹6,00,000 and over", "6,00,000", 
                                                        str_extract(data_fram$HH_INCOME, INCOME_HH_LOW_str))))),
             INCOME_HH_HIGH  = as.numeric(gsub(",", "", 
                                               gsub("Less", "60,000", 
                                                    str_extract(data_fram$HH_INCOME, INCOME_HH_HIGH_str)))),
             INCOME_PPP = 21.9895584423356)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "Italy"){
    
    INCOME_LOW_str <- "Meno di €500|500|1.000|1.500|2.000|2.500|€3.000 euro o più"
    INCOME_HIGH_str <- "Meno di €500|meno di €1.000|meno di €1.500|meno di €2.000|meno di €2.500|meno di €3.000"
    INCOME_HH_LOW_str <- "Meno di €500|500|1.000|1.500|2.000|2.500|3.000|5.000|7.000|€9.000 o più"
    INCOME_HH_HIGH_str <- "Meno di €500|meno di €1.000|meno di €1.500|meno di €2.000|meno di €2.500|meno di €3.000|meno di €5.000|meno di €7.000|meno di €9.000"
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub("[.]", "", 
                                           gsub("Meno di €500", "0", 
                                                gsub("€3.000 euro o più", "3.000", str_extract(data_fram$INCOME, INCOME_LOW_str))))),
             INCOME_HIGH  = str_extract(data_fram$INCOME, INCOME_HIGH_str),
             INCOME_HH_LOW  = as.numeric(gsub("[.]", "", 
                                              gsub("Meno di €500", "0", 
                                                   gsub("€9.000 o più", "9.000", str_extract(data_fram$HH_INCOME, INCOME_HH_LOW_str))))),
             INCOME_HH_HIGH  = str_extract(data_fram$HH_INCOME, INCOME_HH_HIGH_str),
             INCOME_PPP = 0.662828)
    
    data_fram$INCOME_HIGH <- data_fram$INCOME_HIGH %>%
      recode("Meno di €500" = 500,
             "meno di €1.000" = 1000, "meno di €1.500" = 1500,
             "meno di €2.000" = 2000, "meno di €2.500" = 2500,
             "meno di €3.000" = 3000)
    
    data_fram$INCOME_HH_HIGH <- data_fram$INCOME_HH_HIGH %>%
      recode("Meno di €500" = 500,
             "meno di €1.000" = 1000, "meno di €1.500" = 1500,
             "meno di €2.000" = 2000, "meno di €2.500" = 2500,
             "meno di €3.000" = 3000, "meno di €5.000" = 5000,
             "meno di €7.000" = 7000, "meno di €9.000" = 9000)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "Russia"){
    
    INCOME_LOW_str <- "Менее 10000 рублей|10000|20000|30000|40000|50000|75000|100000"
    INCOME_HIGH_str <- "Менее 10000 рублей|19999|29999|39999|49999|74999|99999"
    INCOME_HH_LOW_str <- "Менее 10000 рублей|10000|20000|35000|50000|75000|100000|125000|150000|200000"
    INCOME_HH_HIGH_str <- "Менее 10000 рублей|19999|34999|49999|74999|99999|124999|149999|199999"
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub("Менее 10000 рублей", "0", str_extract(data_fram$INCOME, INCOME_LOW_str))),
             INCOME_HIGH  = as.numeric(gsub("Менее 10000 рублей", "10000", str_extract(data_fram$INCOME, INCOME_HIGH_str))),
             INCOME_HH_LOW  = as.numeric(gsub("Менее 10000 рублей", "0", str_extract(data_fram$HH_INCOME, INCOME_HH_LOW_str))),
             INCOME_HH_HIGH  = as.numeric(gsub("Менее 10000 рублей", "10000", str_extract(data_fram$HH_INCOME, INCOME_HH_HIGH_str))),
             INCOME_PPP = 0.662828)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "Spain"){
    
    INCOME_LOW_str <- "Menos|500|1.000|1.500|2.000|2.500|Más de €3.000"
    INCOME_HIGH_str <- "Menos de €500|menos de €1.000|menos de €1.500|menos de €2.000|menos de €2.500|menos de €3.000"
    INCOME_HH_LOW_str <- "Menos|500|1.000|1.500|2.000|2.500|3.000|5.000|7.000|Más de €9.000"
    INCOME_HH_HIGH_str <- "Menos de €500|menos de €1.000|menos de €1.500|menos de €2.000|menos de €2.500|menos de €3.000|menos de €5.000|menos de €7.000|menos de €9.000"
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub("[.]", "", 
                                           gsub("Menos", "0", 
                                                gsub("Más de €3.000", "3.000", str_extract(data_fram$INCOME, INCOME_LOW_str))))),
             INCOME_HIGH  = str_extract(data_fram$INCOME, INCOME_HIGH_str),
             INCOME_HH_LOW  = as.numeric(gsub("[.]", "", 
                                              gsub("Menos", "0", 
                                                   gsub("Más de €9.000", "9.000", str_extract(data_fram$HH_INCOME, INCOME_HH_LOW_str))))),
             INCOME_HH_HIGH  = str_extract(data_fram$HH_INCOME, INCOME_HH_HIGH_str),
             INCOME_PPP = 0.617946)
    
    data_fram$INCOME_HIGH <- data_fram$INCOME_HIGH %>%
      recode("Menos de €500" = 500,
             "menos de €1.000" = 1000, "menos de €1.500" = 1500,
             "menos de €2.000" = 2000, "menos de €2.500" = 2500,
             "menos de €3.000" = 3000)
    
    data_fram$INCOME_HH_HIGH <- data_fram$INCOME_HH_HIGH %>%
      recode("Menos de €500" = 500,
             "menos de €1.000" = 1000, "menos de €1.500" = 1500,
             "menos de €2.000" = 2000, "menos de €2.500" = 2500,
             "menos de €3.000" = 3000, "menos de €5.000" = 5000,
             "menos de €7.000" = 7000, "menos de €9.000" = 9000)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "Uganda"){
    
    INCOME_LOW_str <- "Under|330,000|660,000|1,500,000|2,000,000|2,500,000|3,000,000|4,000,000|5,000,000|6,000,000|7,000,000|9,000,000|11,000,000|13,5000,000|USh 16,000,000 and over"
    INCOME_HIGH_str <- "Under|to USh 660,000|to USh 1,500,000|to USh 2,000,000|to USh 2,500,000|to USh 3,000,000|to USh 4,000,000|to USh 5,000,000|to USh 6,000,000|to USh 7,000,000|to USh 9,000,000|to USh 11,000,000|to USh 13,5000,000|to USh 13,500,000|to USh 16,000,000"
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub(",", "", 
                                           gsub("Under", "0", 
                                                gsub("USh 16,000,000 and over", "16,000,000", str_extract(data_fram$INCOME, INCOME_LOW_str))))),
             INCOME_HIGH  = str_extract(data_fram$INCOME, INCOME_HIGH_str),
             INCOME_HH_LOW  = as.numeric(gsub(",", "", 
                                              gsub("Under", "0", 
                                                   gsub("USh 16,000,000 and over", "16,000,000", str_extract(data_fram$HH_INCOME, INCOME_LOW_str))))),
             INCOME_HH_HIGH  = str_extract(data_fram$HH_INCOME, INCOME_HIGH_str),
             INCOME_PPP = 1321.34594445177)
    
    data_fram$INCOME_HIGH <- data_fram$INCOME_HIGH %>%
      recode("Under" = 330000, "to USh 660,000" = 660000,
             "to USh 1,500,000" = 1500000, "to USh 2,000,000" = 2000000,
             "to USh 2,500,000" = 2500000, "to USh 3,000,000" = 3000000,
             "to USh 4,000,000" = 4000000, "to USh 5,000,000" = 5000000,
             "to USh 6,000,000" = 6000000, "to USh 7,000,000" = 7000000,
             "to USh 9,000,000" = 9000000, "to USh 11,000,000" = 11000000,
             "to USh 13,5000,000" = 13500000, "to USh 16,000,000" = 16000000)
    
    data_fram$INCOME_HH_HIGH <- data_fram$INCOME_HH_HIGH %>%
      recode("Under" = 330000, "to USh 660,000" = 660000,
             "to USh 1,500,000" = 1500000, "to USh 2,000,000" = 2000000,
             "to USh 2,500,000" = 2500000, "to USh 3,000,000" = 3000000,
             "to USh 4,000,000" = 4000000, "to USh 5,000,000" = 5000000,
             "to USh 6,000,000" = 6000000, "to USh 7,000,000" = 7000000,
             "to USh 9,000,000" = 9000000, "to USh 11,000,000" = 11000000,
             "to USh 13,500,000" = 13500000, "to USh 16,000,000" = 16000000)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "UK"){
    
    INCOME_LOW_str <- "Under|5,000|10,000|15,000|20,000|25,000|30,000|35,000|40,000|45,000|50,000|60,000|70,000|100,000|150,000"
    INCOME_HIGH_str <- "Under|9,999|14,999|19,999|24,999|29,999|34,999|39,999|44,999|49,999|59,999|69,999|99,999|149,999"
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub(",", "", 
                                           gsub("Under", "0", str_extract(data_fram$INCOME, INCOME_LOW_str)))),
             INCOME_HIGH  = as.numeric(gsub(",", "", 
                                            gsub("Under", "5,000", str_extract(data_fram$INCOME, INCOME_HIGH_str)))),
             INCOME_HH_LOW  = as.numeric(gsub(",", "", 
                                              gsub("Under", "0", str_extract(data_fram$HH_INCOME, INCOME_LOW_str)))),
             INCOME_HH_HIGH  = as.numeric(gsub(",", "", 
                                               gsub("Under", "5,000", str_extract(data_fram$HH_INCOME, INCOME_HIGH_str)))),
             INCOME_PPP = 0.699569)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "US"){
    
    INCOME_LOW_str <- "Up|5,000|10,000|20,000|30,000|40,000|50,000|75,000|100,000|250,000"
    INCOME_HIGH_str <- "Up|[$]5,000 to 10,000|19,999|29,999|39,999|49,999|74,999|99,999|249,999"
    INCOME_HH_LOW_str <- "Up|10,000|20,000|30,000|40,000|50,000|75,000|100,000|250,000"
    INCOME_HH_HIGH_str <- "Up|19,999|29,999|39,999|49,999|74,999|99,999|249,999"
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub(",", "", 
                                           gsub("Up", "0",
                                                str_extract(data_fram$INCOME, INCOME_LOW_str)))),
             INCOME_HIGH  = as.numeric(gsub(",", "", 
                                            gsub("Up", "5,000", 
                                                 gsub("[$]5,000 to 10,000", "10,000", 
                                                      str_extract(data_fram$INCOME, INCOME_HIGH_str))))),
             INCOME_HH_LOW  = as.numeric(gsub(",", "", 
                                              gsub("Up", "0",
                                                   str_extract(data_fram$HH_INCOME, INCOME_HH_LOW_str)))),
             INCOME_HH_HIGH  = as.numeric(gsub(",", "", 
                                               gsub("Up", "10,000",
                                                    str_extract(data_fram$HH_INCOME, INCOME_HH_HIGH_str)))),
             INCOME_PPP = 1)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else if(unique(data_fram$country) == "South Africa"){
    
    INCOME_LOW_str <- "R1 |R401|R801|R1,601|R3,201|R6,401|R12,801|R25,601|R51,201|R102,401|R204,801"
    INCOME_HIGH_str <- "R400|R800|R1,600|R3,200|R6,400|R12,800|R25,600|R51,200|R102,400|R204,800"
    INCOME_HH_LOW_str <- "R0|R19,001|R86,001|R197,001|R400,001|R688,001|R1,481,001|R2,360,001"
    INCOME_HH_HIGH_str <- "R19,000|R86,000|R197,000|R400,000|R688,000|R1,481,000|R2,360,000"
    
    data_fram <- data_fram %>% 
      mutate(INCOME_LOW  = as.numeric(gsub(",", "", 
                                           gsub("R", "",
                                                str_extract(data_fram$INCOME, INCOME_LOW_str)))),
             INCOME_HIGH  = as.numeric(gsub(",", "", 
                                            gsub("R", "",
                                                 str_extract(data_fram$INCOME, INCOME_HIGH_str)))),
             INCOME_HH_LOW  = as.numeric(gsub(",", "", 
                                              gsub("R", "",
                                                   str_extract(data_fram$HH_INCOME, INCOME_HH_LOW_str)))),
             INCOME_HH_HIGH  = as.numeric(gsub(",", "", 
                                               gsub("R", "",
                                                    str_extract(data_fram$HH_INCOME, INCOME_HH_HIGH_str)))),
             INCOME_PPP = 6.933)
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MEAN = sum((INCOME_HIGH - INCOME_LOW)/2 + INCOME_LOW, na.rm = T)/nrow(data_fram),
             INCOME_HH_MEAN = sum((INCOME_HH_HIGH - INCOME_HH_LOW)/2 + INCOME_HH_LOW, na.rm = T)/nrow(data_fram))
    
    freq.table <- data_fram %>%
      group_by(INCOME_LOW) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      filter(!is.na(INCOME_LOW)) %>%
      mutate(cumsum = cumsum(n),
             rel.freq = cumsum/sum(n))
    
    for (j in 1:nrow(freq.table)) {
      if (freq.table$rel.freq[j]>=0.5) {
        break
      }
    }
    
    freq.table <- freq.table[(j-1):nrow(freq.table),][1:3,]
    
    data_fram <- data_fram %>% 
      mutate(INCOME_MED = as.numeric(freq.table[2,1] + (0.5 - freq.table[1,4])/(freq.table[2,4] - freq.table[1,4])*(freq.table[3,1] - freq.table[2,1])))
    
  }
  else{
    print("There is not a survey for this country, try again")}
  
  return(data_fram)
}

