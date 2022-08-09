########################## Info ############################
# Code by M. Sc. Bjoern Buedenbender (University of Mannheim)

#' Get ICD-10 F-Codes as Character Vector
#' @description  Returns a character vector of length 11 for all ICD-10 F Categories (mental disorders)
#' @param lang Language for the character vector, curr available in english and german (lang = "ger"), Default: "eng"
#' @return Character Vector containing the 11 categroies for mental disorders (F Codes F01-F99)
#'
#' @author Bjoern Buedenbender
#'
#' @examples
#' get_ICD_10_cats() # returns english ICD-10 Cats
#' get_ICD_10_cats("ger") # returns the german ones
#' @export
get_ICD_10_cats <- function(lang="eng") {

  # Validate correct inputs
  if (!is(lang,"character")) stop(paste0("Invalid argument type. The argument",
  "lang is required to be a character specifying the desired language"))

  eng <- c("F01-F09 Mental disorders due to known physiological conditions",
                    "F10-F19 Mental and behavioral disorders due to psychoactive substance use",
                    "F20-F29 Schizophrenia, schizotypal, delusional, and other non-mood psychotic disorders",
                    "F30-F39 Mood \u005Baffective\u005D disorders",
                    "F40-F48 Anxiety, dissociative, stress-related, somatoform and other nonpsychotic mental disorders",
                    "F50-F59 Behavioral syndromes associated with physiological disturbances and physical factors",
                    "F60-F69 Disorders of adult personality and behavior",
                    "F70-F79 Intellectual disabilities",
                    "F80-F89 Pervasive and specific developmental disorders",
                    "F90-F98 Behavioral and emotional disorders with onset usually occurring in childhood and adolescence",
                    "F99-F99 Unspecified mental disorder")

  ger <-  c(
    "F01-F09 Organische, einschlie\u00DFlich symptomatischer psychischer St\u00F6rungen",
    "F10-F19 Psychische und Verhaltensst\u00F6rungen durch psychotrope Substanzen",
    "F20-F29 Schizophrenie, schizotype und wahnhafte St\u00F6rungen",
    "F30-F39 Affektive St\u00F6rungen",
    "F40-F48 Neurotische, Belastungs- und somatoforme St\u00F6rungen",
    "F50-F59 Verhaltensauff\u00E4lligkeiten mit k\u00F6rperlichen St\u00F6rungen und Faktoren",
    "F60-F69 Pers\u00F6nlichkeits- und Verhaltensst\u00F6rungen",
    "F70-F79 Intelligenzst\u00F6rung",
    "F80-F89 Entwicklungsst\u00F6rungen",
    "F90-F98 Verhaltens- und emotionale St\u00F6rungen mit Beginn in der Kindheit und Jugend",
    "F99-F99 Nicht n\u00E4her bezeichnete psychische St\u00F6rungen"
  )
  if(tolower(lang) %in% c("ger", "de")) return(ger) else return(eng)

}
