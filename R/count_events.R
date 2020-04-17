#' Count events
#'
#' @description Count the number of occurrences of a given variable, filter based
#' on certain parameters
#'
#' @param start_date start date of the count, default set to 2000-01-01
#' @param end_date end date of count, default set to today
#' @param event the event that you want to count - this list can be found on 
#' https://open.fda.gov/apis/drug/event/searchable-fields
#' @param limit default set to 1000
#' @param api_key include an API key for faster querying. See the documentation 
#' for more details.
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
#' @return a dataset containing terms and count
#' @export
#' @importFrom lubridate today as_date parse_date_time year month
#' @importFrom stringr str_detect str_replace_all str_trim
#' @importFrom httr GET content stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr %>% filter group_by summarize count arrange desc
#'
#' @examples
#' count_events("receivedate", death = 1)
#' @keywords FDA
#'
count_events <-
  function(
    event,
    start_date = "2000-01-01",
    end_date = lubridate::today(),
    limit = 1000,
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
    website <- "https://api.fda.gov/drug/event.json"
    if (!is.null(api_key)) website <- "https://api_basics.fda.gov/drug/event.json"
    # include search filters
    start_date <- as_date(start_date)
    end_date <- as_date(end_date)
    search <- paste0("receivedate:", start_date, "+TO+", end_date)
    # queries with FDA data for filtering needs to be
    # in the format: search=patient.reaction.reactionmeddrapt:decease+AND+patient.patientsex:1
    # remove all spaces and replace them with + signs
    if (!is.null(reactionmeddrapt)) 
      search <- paste0(search, 
                       " patient.reaction.reactionmeddrapt:", 
                       str_replace_all(reactionmeddrapt, " ", "+")
      )
    if (!is.null(actiondrug)) 
      search <- paste0(search, " patient.drug.actiondrug:", actiondrug)
    if (!is.null(patientsex)) 
      search <- paste0(search, " patient.patientsex:", patientsex)
    if (!is.null(patientagegroup))
      search <- paste0(search, " patient.patientagegroup:", patientagegroup)
    if (!is.null(reactionoutcome)) 
      search <- paste0(search, " patient.reaction.reactionoutcome:", reactionoutcome)
    if (!is.null(qualification)) 
      search <- paste0(search, " primarysource.qualification:", qualification)
    if (!is.null(generic_name)) 
      search <- paste0(search, 
                       " patient.drug.openfda.generic_name:", 
                       str_replace_all(generic_name, " ", "+")
      )
    if (!is.null(brand_name)) 
      search <- paste0(search, 
                       " patient.drug.openfda.brand_name:", 
                       str_replace_all(brand_name, " ", "+")
      )
    if (!is.null(serious))
      search <- paste0(search, " serious:", serious)
    if (!is.null(congenitalanomali))
      search <- paste0(search, " seriousnesscongenitalanomali:", congenitalanomali)
    if (!is.null(death))
      search <- paste0(search, " seriousnessdeath:", death)
    if (!is.null(disabling))
      search <- paste0(search, " seriousnessdisabling:", disabling)
    if (!is.null(hospitalization))
      search <- paste0(search, " seriousnesshospitalization:", hospitalization)
    if (!is.null(lifethreatening))
      search <- paste0(search, " seriousnesslifethreatening:", lifethreatening)
    
    search <- str_replace_all(str_trim(search), " ", "+AND+")

    if (limit > 1000) limit <- 1000

    # call the API
    response <-  GET(
      website,
      query = list(api_key = api_key, search = I(search), limit = limit, count = event)
    )
  
    stop_for_status(response, "get http response, check variable names")
    jsonRespText <- content(response, as = "text") %>% fromJSON()
    result <- as.data.frame(jsonRespText$results)

    # aggregate by month and year if it is a date
    if (str_detect(event, "date")) {
      result <-
        result %>%
        mutate(time = parse_date_time(time, "%y%m%d")) %>%
        filter(time <= today(), time >= start_date) %>%
        group_by(year = year(time), month = month(time)) %>%
        summarize(count = sum(count)) %>%
        arrange(desc(year), desc(month))
    } 
    return(result)
  }
