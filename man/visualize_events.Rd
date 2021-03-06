% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize_events.R
\name{visualize_events}
\alias{visualize_events}
\title{Visualize events}
\usage{
visualize_events(event, start_date = "2000-01-01",
  end_date = lubridate::today(), limit = 20, api_key = NULL,
  reactionmeddrapt = NULL, actiondrug = NULL, patientsex = NULL,
  patientagegroup = NULL, reactionoutcome = NULL,
  qualification = NULL, generic_name = NULL, brand_name = NULL,
  serious = NULL, congenitalanomali = NULL, death = NULL,
  disabling = NULL, hospitalization = NULL, lifethreatening = NULL)
}
\arguments{
\item{event}{the event that you want to count - please see all searchable fields
in this list https://open.fda.gov/apis/drug/event/searchable-fields}

\item{start_date}{start date of the count, default set to 2000-01-01}

\item{end_date}{end date of count, default set to today}

\item{limit}{limit max set to 20, best not to visualize anything past that}

\item{api_key}{include an API key for faster querying. See the documentation 
for more details.}

\item{reactionmeddrapt}{the patient reaction, default set to NULL (use for filtering results)
(encoded in British English, e.g. diarrhea spelled as diarrohea)}

\item{actiondrug}{actions taken with the drug:
1 = drug withdrawn, 2 = dose reduced, 3 = dose increased, 4 = dose unchanged
5 = unknown 6 = not applicable, default set to NULL (use for filtering results)}

\item{patientsex}{the sex of the patient, where 0 = unknown, 1 = male,
2 = female, default set to NULL (use for filtering)}

\item{patientagegroup}{patient age group code - 1 = neonate, 2 = infant,
3 = child, 4 = adolescent, 5 = adult, 6 = elderly, default set to NULL (use for filtering results)}

\item{reactionoutcome}{outcome of reaction at the time of last
observation. 1 = recovered, 2 = recovering, 3 = not recovered, 4 = recovered with
health issues, 5 = fatal, 6 = unknown, default set to NULL (use for filtering)}

\item{qualification}{category of individual who submitted report
1 = physician, 2 = pharmacist, 3 = other health professional, 4 = lawyer, 5 = consumer
default set to NULL (use for filtering)}

\item{generic_name}{generic name of product, default set to NULL (use for filtering)}

\item{brand_name}{brand/trade name of drug, default set to NULL (use for filtering)}

\item{serious}{whether the drug event was serious 
(1 = life threatening illness, hospitalization, fatality, etc., 2 = otherwise)}

\item{congenitalanomali}{1 if resulted in congenital anomali, otherwise N/A}

\item{death}{1 if resulted in death, otherwise N/A}

\item{disabling}{1 if resulted disabilities, otherwise N/A}

\item{hospitalization}{1 if resulted in hospitalization, otherwise N/A}

\item{lifethreatening}{1 if resulted in life threatening conditions, otherwise, N/A}
}
\description{
Visualize the number of occurrences of an event, filter
based on certain parameters
}
\examples{
visualize_events("receivedate")
visualize_events("patient.patientweight", death = 1)
visualize_events("patient.patientonsetage", serious = 1)

}
\keyword{FDA}
