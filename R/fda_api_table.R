#' Load JSON tables from the adverse drug event
#'
#' @description Load data from the FDA adverse drug events table
#' @param limit number of data to pull - if it is not set, return only one instance
#' limit should be less than 100
#' @param api_key include an API key for faster querying. See the documentation 
#' for more details.
#' @param start_date the start date of events you want to search
#' @param end_date the end date of the events you want to search
#' @param reactionmeddrapt the patient reaction
#' (encoded in British English, e.g. diarrhea spelled as diarrohea)
#' @param actiondrug actions taken with the drug:
#' 1 = drug withdrawn, 2 = dose reduced, 3 = dose increased, 4 = dose unchanged
#' 5 = unknown 6 = not applicable
#' @param patientsex the sex of the patient, where 0 = unknown, 1 = male,
#' 2 = female
#' @param patientagegroup patient age group code - 1 = neonate, 2 = infant,
#' 3 = child, 4 = adolescent, 5 = adult, 6 = elderly, default set to NULL (use for filtering results)
#' @param reactionoutcome outcome of reaction at the time of last
#' observation. 1 = recovered, 2 = recovering, 3 = not recovered, 4 = recovered with
#' health issues, 5 = fatal, 6 = unknown
#' @param qualification category of individual who submitted report
#' 1 = physician, 2 = pharmacist, 3 = other health professional, 4 = lawyer, 5 = consumer
#' @param generic_name generic name of product
#' @param brand_name brand/trade name of drug
#' @param serious whether the drug event was serious 
#' (1 = life threatening illness, hospitalization, fatality, etc., 2 = otherwise)
#' @param congenitalanomali 1 if resulted in congenital anomali, otherwise N/A
#' @param death 1 if resulted in death, otherwise N/A
#' @param disabling 1 if resulted disabilities, otherwise N/A
#' @param hospitalization 1 if resulted in hospitalization, otherwise N/A
#' @param lifethreatening 1 if resulted in life threatening conditions, otherwise, N/A
#' @return a dataset of adverse drug events
#' @export
#' @importFrom lubridate today as_date
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr %>%
#' @importFrom stringr str_replace_all str_trim
#'
#' @examples
#' t <- fda_api_table()
#' fda_api_table(
#'              limit = 100, 
#'              start_date = "2016-01-01", 
#'              end_date = "2018-01-01",
#'              reactionmeddrapt = "nephrolithiasis",
#'              actiondrug = 5,
#'              patientsex = 1,
#'              reactionoutcome = 6,
#'              qualification = 1,
#'              generic_name  = "aspirin"
#'              )
#' @keywords FDA
#'
fda_api_table <-
  function(
    limit = 1,
    api_key = NULL,
    start_date = NULL,
    end_date = NULL,
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
    if (!is.null(api_key)) website <- "https://api_basics.fda.gov/drug/event.json"
    else website <- "https://api.fda.gov/drug/event.json"
    
    if (is.null(start_date) & is.null(end_date)) 
      search <- ""
    else if (is.null(end_date)) 
      search <- paste0("receivedate:", as_date(start_date), "+TO+", lubridate::today())
    else if (is.null(start_date)) 
      search <- paste0("receivedate:", "2000-01-01", "+TO+", as_date(end_date))
    else 
      search <- paste0("receivedate:", as_date(start_date), "+TO+", as_date(end_date))

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

    if (limit > 100 & is.null(api_key)) limit <- 100
    response <- GET(
      website,
      query = list(
        search = I(search),
        limit = limit
      )
    )
    stop_for_status(response,  "get http response")
    table <- content(response, as = "text") %>% fromJSON()
    return(table$results)
  }

