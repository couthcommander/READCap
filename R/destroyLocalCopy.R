#' Delete local project tables
#'
#' Delete tables associated with locally stored REDCap project.
#'
#' @param localdb \code{link[DBI]{dbConnect}} object, or perhaps local folder
#' @param rcon \code{link[redcapAPI]{redcapConnection}} object
#'
#' @return TRUE
#'
#' @export

destroyLocalCopy <- function(localdb, rcon) {
  UseMethod('destroyLocalCopy')
}

#' @rdname destroyLocalCopy
#' @export

destroyLocalCopy.default <- function(localdb, rcon) {
  prid <- projectId(rcon)
  log_table <- paste0('logs_', prid)
  # identify local tables with project_id
  proj_tables <- projectTables(localdb, rcon)
  proj_tables <- c(log_table, proj_tables)
  for(cur_table in proj_tables) {
    qry_delete <- sprintf("DROP TABLE `%s`", cur_table)
    DBI::dbExecute(localdb, qry_delete)
  }
  TRUE
}

#' @rdname destroyLocalCopy
#' @export

destroyLocalCopy.character <- function(localdb, rcon) {
  stopifnot(file.access(localdb) == 0)
  prid <- projectId(rcon)
  log_table <- sprintf('logs_%s.rds', prid)
  # identify local tables with project_id
  proj_tables <- projectTables(localdb, rcon)
  proj_tables <- c(log_table, proj_tables)
  for(cur_table in proj_tables) {
    unlink(file.path(localdb, cur_table))
  }
  TRUE
}
