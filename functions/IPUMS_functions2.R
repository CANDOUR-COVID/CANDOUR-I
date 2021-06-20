library(dplyr)
library(tidyverse)

'
NOTE:
'

#######################################CONTRACT###############################################################################

IPUMS_contract<-function(data_fram){
  if (data_fram$country=="Australia"){
    
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.6)
    
  }
  else if(data_fram$country=="Brazil"){
    
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.4)
    
    
    
  }
  else if(data_fram$country=="Canada"){
    
    data_fram <- data_fram %>% mutate(PROF_POSITION = Q22.3)
    
    
  }
  
  else if(data_fram$country=="Chile"){
    
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.9)
    
  }
  
  else if(data_fram$country=="Colombia"){
    
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.5)
    
  }
  else if(data_fram$country=="France"){
    
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.5)
    
    
  }
  
  else if(data_fram$country=="Italy"){
    
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.7)
    
   
    
  }
  
  else if(data_fram$country=="Spain"){
    
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.5)
    
    
    
    
  }
  else if(data_fram$country=="UK"){
    return(data_fram)
  }
  else if(data_fram$country=="US"){
    
    data_fram <- data_fram %>% mutate(PROF_POSITION = Q22.3)
    
    
  }
  
  else if(data_fram$country=="China"){
    
    data_fram <- data_fram %>% rename(PROF_POSITION = Q22.6)
    

  }
  else if(data_fram$country == "Uganda"){
    
    data_fram <- data_fram %>% mutate(PROF_POSITION = Q22.3)
    
  }
  
  else if(data_fram$country == "India"){
    
    data_fram <- data_fram %>% mutate(PROF_POSITION = Q22.3)
    
  }
  
  else{
    return(print("There is not a survey for this country, try again"))}

  return(data_fram)
}

#######################################EDUCATION###############################################################################

IPUMS_education<-function(data_fram){
  if (data_fram$country=="Australia"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.4 )
    
  }
  else if(data_fram$country=="Brazil"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    
  }
  else if(data_fram$country=="Canada"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    
  }
  
  else if(data_fram$country=="Chile"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.4 )
    
    data_fram <- data_fram %>% mutate(EDUCATION_LEVEL = if_else(is.na(EDUCATION_LEVEL),Q22.3,EDUCATION_LEVEL))
    
  }
  
  else if(data_fram$country=="Colombia"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.3 )
    
  }
  else if(data_fram$country=="France"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    
  }
  
  else if(data_fram$country=="Italy"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    
  }
  
  else if(data_fram$country=="Spain"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    

  }
  else if(data_fram$country=="UK"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    

  }
  else if(data_fram$country=="US"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL = Q22.2 )
    

  }
  
  else if(data_fram$country=="China"){
    
    char1 <- function(string){
      string <-unlist(strsplit(string,""))[1]
      return(string)
    }
    
    data_fram$EDUCATION_LEVEL  <- lapply(data_fram$Q22.2,char1)
    

  }
  else if(data_fram$country == "Uganda"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL  = Q22.2)
    

    
  }
  else if(data_fram$country == "India"){
    
    data_fram <- data_fram %>% rename(EDUCATION_LEVEL  = Q22.2)
    

    
  }
  else{
    return(print("There is not a survey for this country, try again"))}
  
 
  return(data_fram)
}


#######################################Employment###############################################################################

IPUMS_employment<-function(data_fram){
  if (data_fram$country=="Australia"){
    
    data_fram <- data_fram %>% mutate(EMPLOYMENT = if_else(Q22.2 == "No" | Q22.2 == "Do not know" | Q22.2 == "Prefer not to say" ,Q22.5,"Student"))
    
  }
  else if(data_fram$country=="Brazil"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.3 )
    
  }
  else if(data_fram$country=="Canada"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.3 )
    
  }
  
  else if(data_fram$country=="Chile"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.8)
    
  }
  
  else if(data_fram$country=="Colombia"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.4)
    
  }
  else if(data_fram$country=="France"){
    return(data_fram)
    
  }
  
  else if(data_fram$country=="Italy"){
    
    Employment <- function(ITA){
      ITA <- ITA %>% mutate(EMPLOYMENT = if_else(Q22.3=="Non in grado di lavorare permanentemente","Not active",
                                                 if_else(Q22.3 %in% c("Non saprei", "Preferisco non rispondere") & (Q22.4 == "SÃ¬" | Q22.5 == "SÃ¬"), "Employ",
                                                         if_else(Q22.3 == "SÃ¬" & (Q22.4 == "SÃ¬" | Q22.5 == "SÃ¬"), "Employ", 
                                                                 if_else(Q22.3 == "SÃ¬" & Q22.10=="SÃ¬", "Unemploy", 
                                                                         if_else(Q22.3 == "No" & Q22.10=="SÃ¬", "Unemploy_n", Q22.12))))))
      
      
      
    }
    
    data_fram <- Employment(data_fram)
    
  }
  
  else if(data_fram$country=="Spain"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.3)
    
    

    
  }
  else if(data_fram$country=="UK"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.3)
    

  }
  else if(data_fram$country=="US"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.3)
    

    
    
  }
  
  else if(data_fram$country=="China"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT = Q22.4)
    
    char2 <- function(string){
      string <-unlist(substring(string , 1, 2))
      return(string)
    }
    
    data_fram$EMPLOYMENT <- unlist(lapply(data_fram$EMPLOYMENT,char2))
    

  }
  else if(data_fram$country == "Uganda"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT  = Q22.3)
    
  }
  else if(data_fram$country == "India"){
    
    data_fram <- data_fram %>% rename(EMPLOYMENT  = Q22.3)
    

  }
  else{
    return(print("There is not a survey for this country, try again"))}

  return(data_fram)
}

#######################################WORK###############################################################################

IPUMS_work<-function(data_fram){
  if (data_fram$country=="Australia"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.7 )
    

  }
  else if(data_fram$country=="Brazil"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.5 )
    

  }
  else if(data_fram$country=="Canada"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.4 )
    

  }
  
  else if(data_fram$country=="Chile"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.10 )
    

  }
  
  else if(data_fram$country=="Colombia"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.6 )
    

  }
  else if(data_fram$country=="France"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.4 )
    
 
  }
  
  else if(data_fram$country=="Italy"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.6 )
    

  }
  
  else if(data_fram$country=="Spain"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.4 )
    

  }
  else if(data_fram$country=="UK"){
    
    data_fram <- data_fram %>% rename(OCCUPATION = Q22.4 )
    

  }
  else if(data_fram$country=="US"){
    
    data_fram <- data_fram %>%  rename(OCCUPATION = Q22.4 )
    

  }
  
  else if(data_fram$country=="China"){
    
    data_fram <- data_fram %>%  rename(OCCUPATION = Q22.5 )
    
  
  }
  else if(data_fram$country == "Uganda"){
    
    data_fram <- data_fram %>% rename(OCCUPATION  = Q22.4)
    

  }
  else if(data_fram$country == "India"){
    
    data_fram <- data_fram %>% rename(OCCUPATION  = Q22.4)
    

  }
  else{
    return(print("There is not a survey for this country, try again"))}
  
  return(data_fram)
}

