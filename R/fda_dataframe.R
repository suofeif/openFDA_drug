#' Transform JSON tables exported by function fda_api_table to dataframe (not including drug information; 
#' for accessing to drug information in data frame, see fda_drug_dataframe.
#'
#' @description Transform JSON tables exported by the function fda_api_table into data frame; not including drug information; 
#' for accessing to drug information in data frame, see fda_drug_dataframe.
#'
#' @param g_table a JSON table returned by fda_api_table
#' @return a data frame with all variables from FDA adverse drug events except drug information.
#' @export
#' @importFrom dplyr %>% mutate right_join left_join row_number
#' @examples
#' g_table <- fda_api_table()
#' fda_dataframe(g_table)
#' @keywords FDA
#' 
fda_dataframe <-
  function(g_table) {
    fda_json <- data.frame(g_table)
    
    recVr<-data.frame(fda_json$receiver)%>% mutate(ID = row_number())
    
    priSource<-data.frame(fda_json$primarysource) %>% mutate(ID = row_number())
    
    senDr<-data.frame(fda_json$sender)%>% mutate(ID = row_number())
    
    repDup<-data.frame(fda_json$reportduplicate)%>% mutate(ID = row_number())
    
    compName <- names(fda_json) %in% c("receiver", "primarysource", "sender", "patient", "reportduplicate") 
    newfda_json <- fda_json[!compName]
    newfda_json <- newfda_json %>%
      mutate(ID = row_number())%>%
      merge(., recVr, by="ID", all=TRUE) %>%
      merge(., priSource, by="ID", all=TRUE) %>%
      merge(., senDr, by="ID", all=TRUE) %>%
      merge(repDup, by="ID", all=TRUE) 
    
    patient<-fda_json$patient
    unwanted <- names(patient) %in% c("reaction", "patientdeath", "drug", "summary")
    patient_tidy <- data.frame(patient[!unwanted]) %>%
      mutate(ID = row_number())
    
    pReaction <- patient$reaction
    pDeath <- data.frame(patient$patientdeath) %>%mutate(ID = row_number())
    
    pDrug <- patient$drug
    pSum <- data.frame(patient$summary)%>%mutate(ID = row_number())
    
    patient_tidy <- patient_tidy %>%
      merge(., pDeath, by="ID", all=TRUE) %>%
      merge(., pSum, by="ID", all=TRUE)
    
    index = as.list(seq(from=1, to=length(pReaction), by=1))
    ID = list()
    patient_reaction = list()
    for (i in (1:length(pReaction))){
      dfi <- data.frame(pReaction[i])
      patient_sub = as.list(dfi$reactionmeddrapt)
      num <- length(patient_sub)
      id_sub = rep(i, num)
      ID = c(ID, id_sub)
      patient_reaction = c(patient_reaction, patient_sub)
    }
    patient_fda_json <- data.frame(unlist(patient_reaction)) %>%
      cbind(., data.frame(unlist(ID)))
    colnames(patient_fda_json) <- c("patient_reaction", "ID")
    newfda_json_withPartpatient <- newfda_json %>%
      right_join(., patient_fda_json, by = "ID") %>%
      left_join(., patient_tidy, by = "ID")
    return(newfda_json_withPartpatient)
  }