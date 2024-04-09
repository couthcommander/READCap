#' Read Local REDCap Event Logs
#'
#' List REDCap event logs.
#'
#' @param localdb \code{link[DBI]{dbConnect}} object, or perhaps local folder
#' @param rcon \code{link[redcapAPI]{redcapConnection}} object
#'
#' @return data.frame, event logs
#'
#' @export

readLogs <- function(localdb, rcon) {
  UseMethod('readLogs')
}

#' @rdname readLogs
#' @export

readLogs.default <- function(localdb, rcon) {
  prid <- projectId(rcon)
  logtbl <- sprintf('logs_%s', prid)
  stopifnot(DBI::dbExistsTable(localdb, logtbl))
  DBI::dbReadTable(localdb, logtbl)
}

#' @rdname readLogs
#' @export

readLogs.character <- function(localdb, rcon) {
  prid <- projectId(rcon)
  logtbl <- sprintf('logs_%s.rds', prid)
  stopifnot(file.access(file.path(localdb, logtbl)) == 0)
  readRDS(file.path(localdb, logtbl))
}
