#' Return a data frame of unique elements of a specified variable in a list of openfda
#' data and their frequencies. The data frame is sorted according to frequencies of each element in
#' descending order.
#'
#' @description Return a data frame of unique elements of a specified variable in a list of openfda
#' data and their frequencies. The data frame is sorted according to frequencies of each element in
#' descending order.
#'
#' @param var_name a string for the column/variable name
#' @param openfda_data an openfda_data list returned by get_openfda
#' @return a dataframe of element names and their frequencies sorted according 
#' to frequencies of each element in descending order
#' @export
#' @importFrom dplyr %>% mutate
#' @examples
#' g_table <- fda_api_table(limit=100, patientsex = 1)
#' openfda_data <- get_openfda(g_table)
#' openfda_count("route", openfda_data)
#' @keywords FDA
#'
openfda_count = function(var_name, openfda_data){
  if (var_name %in% c("generic_name", "brand_name", "manufacturer_name", "substance_name", "product_type", "route")){
    final_list = list()

    for (each_list in openfda_data){
      sub_part=each_list[[var_name]]
      final_list = unlist(c(final_list, unlist(sub_part)))
    }
    fda_ct = sort(sapply(unique(final_list, incomparables = FALSE), function(x){sum(x==final_list)}), decreasing = TRUE)
    name_list = as.data.frame(names(fda_ct))
    value_list = list()
    for (i in (1: length(fda_ct))){
      value_list[i] <- fda_ct[[i]]
    }
    fda_count_df<-name_list %>%
      mutate(count = unlist(value_list))
    colnames(fda_count_df) = c("word", "count")
    return(fda_count_df)
  }else{
    stop("Invalid var_name value")
  }
}
