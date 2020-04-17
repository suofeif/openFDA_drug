#' Return a data frame with activesubstance and its corresponding patient reaction, and
#' show histograms of the counts of reactions of each activesubstance.
#'
#' @description Return a data frame of two columns: one is activesubstance from drug_fda 
#' data frame, with top frequencies specified by top_name; the other is the corresponding 
#' patient reaction from plain_fda data frame, with top frequencies specified by top_react.
#' At the same time, it prints out the bar plots with ggplot2 library of each activesubstance
#' and its counts of patient reactions.
#'
#' @param plain_fda a data frame returned by fda_dataframe
#' @param drug_fda a data frame returned by fda_drug_dataframe
#' @param top_name integer, specifying the number of activesubstance names for the output; this number of
#' activesubstance names with highest frequencies will be taken; default = 5
#' @param top_react integer, specifying the number of patient reaction types for the output; this number of
#' patient reaction types with highest frequencies will be taken; default = 8
#' @return a data frame with activesubstance and its corresponding patient reaction
#' @export
#' @importFrom dplyr %>% mutate filter select bind_rows
#' @importFrom ggplot2 ggplot geom_bar ggtitle xlab ylab theme
#' @examples
#' try_dt <- fda_api_table(limit=100)
#' fda_data<-fda_dataframe(try_dt)
#' fda_drug <- fda_drug_dataframe(try_dt)
#' view_top_react(fda_data, fda_drug, top_name = 5, top_react = 8)
#' @keywords FDA

view_top_react<-function(
  plain_fda,
  drug_fda,
  top_name = 5,
  top_react = 8
){
  top_name = top_name
  top_react = top_react
  actSubName <- unlist(drug_fda$activesubstancename)
  actSubName <- actSubName[!is.na(actSubName)]
  actSubNCt = sort(sapply(unique(actSubName, incomparables = FALSE), function(x){sum(x==actSubName)}), decreasing = TRUE)
  top_sub_name = names(actSubNCt[1:top_name])
  
  subs_name_col = list()
  reaction_list = data.frame()
  for (top_act_name in top_sub_name){
    ID_name_col <- drug_fda%>%
      filter(., activesubstancename == top_act_name) %>%
      select(., ID)
    ID_name_list <- unique(unlist(ID_name_col))
    
    corres_reaction <- plain_fda %>%
      filter(ID %in% ID_name_list) %>%
      select(patient_reaction) 
    cor_react <- unlist(corres_reaction)
    
    top_name_col <- rep(top_act_name, length(cor_react))
    subs_name_col <- c(subs_name_col, top_name_col)
    reaction_list <- bind_rows(reaction_list, corres_reaction)
  }
  subs_react_df <- reaction_list %>%
    mutate(activesubstance = unlist(subs_name_col))
  
  for (top_act_name in top_sub_name){
    each_subs_react <- subs_react_df%>%
      filter(., activesubstance == top_act_name)
    reaction_current_list <- unlist(as.character(each_subs_react$patient_reaction))
    
    if (length(unique(reaction_current_list))>top_react){
      currentCT <- names(sort(table(reaction_current_list), decreasing = TRUE))
      top_reactions = unlist(currentCT[1:top_react])
      new_reactions = list()
      for (i in 1:nrow(each_subs_react)){
        if (reaction_current_list[i] %in% top_reactions){
          new_reactions[i] = reaction_current_list[i]
        } else {
          new_reactions[i] = "others"
        }
      }
      each_subs_react_new <- each_subs_react%>%
        mutate(new_reaction = unlist(new_reactions))
    }else{
      each_subs_react_new <- each_subs_react%>%
        mutate(new_reaction = patient_reaction)
    }
    print(ggplot(data = each_subs_react_new, aes(x = new_reaction))+
            geom_bar(stat = "count")+
            ggtitle(top_act_name)+
            xlab("patient reaction")+
            ylab("count")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    )
  }
  return(subs_react_df)
}