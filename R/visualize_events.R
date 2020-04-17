#' Visualize events
#'
#' @description Visualize the number of occurrences of an event, filter
#' based on certain parameters
#'
#' @param start_date start date of the count, default set to 2000-01-01
#' @param end_date end date of count, default set to today
#' @param event the event that you want to count - please see all searchable fields
#' in this list https://open.fda.gov/apis/drug/event/searchable-fields
#' @param api_key include an API key for faster querying. See the documentation 
#' for more details.
#' @param limit limit max set to 20, best not to visualize anything past that
#' @param reactionmeddrapt the patient reaction, default set to NULL (use for filtering results)
#' (encoded in British English, e.g. diarrhea spelled as diarrohea)
#' @param actiondrug actions taken with the drug:
#' 1 = drug withdrawn, 2 = dose reduced, 3 = dose increased, 4 = dose unchanged
#' 5 = unknown 6 = not applicable, default set to NULL (use for filtering results)
#' @param patientsex the sex of the patient, where 0 = unknown, 1 = male,
#' 2 = female, default set to NULL (use for filtering)
#' @param patientagegroup patient age group code - 1 = neonate, 2 = infant,
#' 3 = child, 4 = adolescent, 5 = adult, 6 = elderly, default set to NULL (use for filtering results)
#' @param reactionoutcome outcome of reaction at the time of last
#' observation. 1 = recovered, 2 = recovering, 3 = not recovered, 4 = recovered with
#' health issues, 5 = fatal, 6 = unknown, default set to NULL (use for filtering)
#' @param qualification category of individual who submitted report
#' 1 = physician, 2 = pharmacist, 3 = other health professional, 4 = lawyer, 5 = consumer
#' default set to NULL (use for filtering)
#' @param generic_name generic name of product, default set to NULL (use for filtering)
#' @param brand_name brand/trade name of drug, default set to NULL (use for filtering)
#' @param serious whether the drug event was serious 
#' (1 = life threatening illness, hospitalization, fatality, etc., 2 = otherwise)
#' @param congenitalanomali 1 if resulted in congenital anomali, otherwise N/A
#' @param death 1 if resulted in death, otherwise N/A
#' @param disabling 1 if resulted disabilities, otherwise N/A
#' @param hospitalization 1 if resulted in hospitalization, otherwise N/A
#' @param lifethreatening 1 if resulted in life threatening conditions, otherwise, N/A
#' @export
#' @importFrom lubridate today as_date parse_date_time year month
#' @importFrom dplyr top_n %>% 
#' @importFrom stringr str_detect str_extract
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom ggplot2 ggplot aes geom_line labs geom_bar element_text theme coord_flip
#' @importFrom stats reorder aggregate
#'
#' @examples
#' visualize_events("receivedate")
#' visualize_events("patient.patientweight", death = 1)
#' visualize_events("patient.patientonsetage", serious = 1)
#' 
#' @keywords FDA
#'
visualize_events <- function(
  event,
  start_date = "2000-01-01",
  end_date = lubridate::today(),
  limit = 20,
  api_key = NULL,
  reactionmeddrapt = NULL,
  actiondrug = NULL,
  patientsex = NULL,
  patientagegroup = NULL,
  reactionoutcome = NULL,
  qualification = NULL,
  generic_name = NULL,
  brand_name = NULL,
  serious = NULL,
  congenitalanomali = NULL,
  death = NULL,
  disabling = NULL,
  hospitalization = NULL,
  lifethreatening = NULL
) {
  if (limit > 50) limit <- 50
  events <-
    count_events(
      event,
      limit = limit,
      start_date = start_date,
      end_date = end_date,
      api_key = api_key,
      reactionmeddrapt = reactionmeddrapt,
      actiondrug = actiondrug,
      patientsex = patientsex,
      patientagegroup = patientagegroup,
      reactionoutcome = reactionoutcome,
      qualification = qualification,
      generic_name = generic_name,
      brand_name = brand_name,
      serious = serious,
      congenitalanomali = congenitalanomali,
      death = death,
      disabling = disabling,
      hospitalization = hospitalization,
      lifethreatening = lifethreatening
    )
  if (str_detect(event, "date")) {
    events %>%
      ggplot(aes(as_date(paste(year, month, "01", sep = "-")), count)) +
      geom_line() +
      labs(x = event)
  } else if (str_detect(event, "weight|patientonsetage|cumulativedosagenumb")) {
    if (str_detect(event, "patientonsetage")) {
      events <-
        events %>% 
        filter(term > 0, term < 110)
    } else if (str_detect(event, "cumulativedosagenumb")) {
      events <-
        events %>% 
        filter(term > 0, term < 1e3)
    }
    events[["groups"]] <- cut(events[["term"]], 20,
                              right = FALSE)
    sum_by_group <- aggregate(count ~ groups, data = events, FUN = sum)
    sum_by_group %>% 
      ggplot(aes(groups, count)) +
      geom_bar(stat = 'identity') +
      labs(x = str_extract(event, "(?<=.)[^.]*$")) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
  }
  else {
    events %>%
      ggplot(aes(reorder(term, count), count)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      ) +
      labs(x = str_extract(event, "(?<=.)[^.]*$"))
  }
}

