#' Stat 290 FDA package
#' @description This package provides an easy wrapper over the Open FDA API. 
#' The package simplifies calls to obtain data, provides ways to reshape data,
#' and helps users count values in fields. The aim of this package is to provide
#' a simpler way for researchers to explore and understand the dataset.
#' 
#' @docType package
#' @name fda.drug.stats290
NULL

utils::globalVariables(c(".", 
                         "time", 
                         "row_number", 
                         "activesubstance", 
                         "openfda",
                         "groups", 
                         "term",
                         "ID",
                         "activesubstancename",
                         "new_reaction", 
                         "patient_reaction"
))