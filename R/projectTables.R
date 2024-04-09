#' List local project tables
#'
#' List tables associated with locally stored REDCap project.
#'
#' @param localdb \code{link[DBI]{dbConnect}} object, or perhaps local folder
#' @param rcon \code{link[redcapAPI]{redcapConnection}} object
#'
#' @return character, table names
#'
#' @export

projectTables <- function(localdb, rcon = NULL) {
  UseMethod('projectTables')
}

#' @rdname projectTables
#' @export

projectTables.default <- function(localdb, rcon = NULL) {
  proj_tables <- DBI::dbListTables(localdb)
  if(!is.null(rcon)) {
    prid <- projectId(rcon)
    proj_tables <- proj_tables[grep(sprintf('^%s_', prid), proj_tables)]
  } else {
    proj_tables <- proj_tables[grep('^[0-9]+_', proj_tables)]
  }
  proj_tables
}

#' @rdname projectTables
#' @export

projectTables.character <- function(localdb, rcon = NULL) {
  if(!is.null(rcon)) {
    prid <- projectId(rcon)
    proj_tables <- list.files(localdb, pattern = sprintf('^%s_.*rds$', prid))
  } else {
    proj_tables <- list.files(localdb, pattern = '^[0-9]+_.*rds$')
  }
  proj_tables
}
