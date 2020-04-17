#' Extract drug information from JSON tables exported by function fda_api_table
#' to data frame not including openfda information; for accessing to openfda 
#' information, see get_openfda.
#'
#' @description Extract drug information from JSON tables exported by function
#' fda_api_table to data frame not including openfda information; for accessing 
#' to openfda information, see get_openfda.
#'
#' @param g_table a JSON table returned by fda_api_table
#' @param id an integer vector, for subsetting patient observations by specifying 
#' their IDs. If not specified, return drug information of all patients passed by 
#' g_table.
#' @return a data frame with all drug information of specified patients from FDA 
#' adverse drug events except openfda.
#' @export
#' @importFrom dplyr %>% mutate select bind_rows
#' @examples
#' g_table <- fda_api_table(limit=100, patientsex = 1)
#' fda_drug_dataframe(g_table)
#' @keywords FDA
#' 
fda_drug_dataframe <- 
  function(
    g_table,
    id = NULL
  ){
    init_df <- data.frame(g_table)
    Patient_data <- init_df$patient
    pDrug_data <-Patient_data$drug
    
    ID_temp <- list()
    if (is.null(id)) {
      ID_temp = seq(from = 1, to = length(pDrug_data), by = 1)
    } else {
      ID_temp = id
    }
    pDrug_data_set <- pDrug_data[ID_temp]
    
    all_colnames<-list()
    for (i in 1:length(pDrug_data_set)){
      all_colnames<-c(all_colnames, colnames(pDrug_data_set[[i]]))
    }
    unique_colnames <- unique(all_colnames)
    
    drugDF <- data.frame(matrix(ncol = length(unique_colnames), nrow = 0))
    colnames(drugDF) <- unique_colnames
    
    name_act <- "activesubstance"
    name_fda <- "openfda"
    drugDF_nocomp <- drugDF
    if(name_act %in% colnames(drugDF)) {
      drugDF_nocomp <- drugDF %>% subset(select=-c(activesubstance))
    }
    if(name_act %in% colnames(drugDF_nocomp)){
      drugDF_nocomp <- drugDF_nocomp %>% subset(select=-c(openfda))
    }
    
    for (i in 1:length(pDrug_data_set)){
      currentDF <- data.frame(pDrug_data_set[i])
      currentID <- ID_temp[i]
      repID <- rep(currentID, nrow(currentDF))
      column_names <- colnames(currentDF)
      
      if (name_act %in% column_names) {
        actSub <- data.frame(currentDF$activesubstance) %>% mutate(ID = repID)
        currentDF <- currentDF %>% mutate(ID = repID) %>%
          merge(., actSub, by = "ID", all = TRUE) %>%
          subset(select=-c(activesubstance))
      }
      
      if (name_fda %in% column_names){
        currentDF <- currentDF %>%
          subset(select=-c(openfda))
      }
      
      drugDF_nocomp <- bind_rows(drugDF_nocomp, currentDF)
    }
    
    return(drugDF_nocomp)
  }