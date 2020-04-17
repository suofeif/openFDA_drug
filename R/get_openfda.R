#' Extract openfda information from JSON tables exported by function fda_api_table 
#' to a list.
#'
#' @description Extract openfda information from JSON tables exported by function 
#' fda_api_table to a list.
#'
#' @param g_table a JSON table returned by fda_api_table
#' @param id an integer vector, for subsetting patient observations by specifying 
#' their IDs. If not specified, return drug information of all patients passed by 
#' g_table.
#' @return a list of openfda data; id is the name of the list.
#' @export
#' @examples
#' g_table <- fda_api_table(limit=100, patientsex = 1)
#' get_openfda(g_table)
#' @keywords FDA
#' 
get_openfda <-
  function(
    g_table,
    id = NULL
  ){
    init_df_for_fda <- data.frame(g_table)
    Patient_for_fda <- init_df_for_fda$patient
    pDrug_for_fda <-Patient_for_fda$drug
    
    ID_temp_fda <- list()
    if (is.null(id)) {
      ID_temp_fda = seq(from = 1, to = length(pDrug_for_fda), by = 1)
    } else {
      ID_temp_fda = id
    }
    pDrug_fda_set <- pDrug_for_fda[ID_temp_fda]
    
    result_fda <- list()
    for (i in 1:length(pDrug_fda_set)){
      currentDF <- as.list(data.frame(pDrug_fda_set[i])$openfda)
      result_fda[[i]] = currentDF
    }
    names(result_fda) <- ID_temp_fda
    return(result_fda) 
  }