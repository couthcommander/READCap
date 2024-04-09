#' Retrieve REDCap project_id
#'
#' Look up a \code{link[redcapAPI]{redcapConnection}} object's project_id.
#'
#' @param rcon \code{link[redcapAPI]{redcapConnection}} object
#'
#' @return character, REDCap project_id
#'
#' @export

projectId <- function(rcon) rcon$projectInformation()$project_id
