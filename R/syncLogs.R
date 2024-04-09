#' Sync Local REDCap Event Logs
#'
#' Sync REDCap event logs.
#'
#' @param localdb \code{link[DBI]{dbConnect}} object, or perhaps local folder
#' @param rcon \code{link[redcapAPI]{redcapConnection}} object
#'
#' @return data.frame, updated event logs
#'
#' @export

syncLogs <- function(localdb, rcon) {
  UseMethod('syncLogs')
}

#' @rdname syncLogs
#' @export

syncLogs.default <- function(localdb, rcon) {
  prid <- projectId(rcon)
  logtbl <- sprintf('logs_%s', prid)
  if(DBI::dbExistsTable(localdb, logtbl)) {
    r1 <- DBI::dbSendQuery(localdb, sprintf("SELECT timestamp FROM %s", logtbl))
    row1 <- DBI::dbFetch(r1, n = 1)
    DBI::dbClearResult(r1)
    isDateTime <- inherits(row1$timestamp, 'POSIXct')

    qry <- sprintf('select max(timestamp) as ts from %s', logtbl)
    maxts <- DBI::dbGetQuery(localdb, qry)$ts
    time <- as.POSIXct(maxts)-60
    logs <- exportLogging(rcon, beginTime = time)
    if(isDateTime) {
      # use datetime query
      qry <- sprintf('select * from %s where timestamp > "%s"', logtbl, time)
      localLogs <- DBI::dbGetQuery(localdb, qry)
    } else {
      # use numeric query
      qry <- sprintf('select * from %s where timestamp > %s', logtbl, as.numeric(time))
      localLogs <- DBI::dbGetQuery(localdb, qry)
      localLogs$timestamp <- as.POSIXct(localLogs$timestamp)
    }
    key1 <- do.call(paste, c(logs, sep = '|'))
    key2 <- do.call(paste, c(localLogs[,names(logs)], sep = '|'))
    logs <- logs[!(key1 %in% key2),]
  } else {
    logs <- redcapAPI::exportLogging(rcon)
  }
  toadd <- nrow(logs)
  if(toadd > 0) {
    logs$completed_at <- NA
    DBI::dbWriteTable(localdb, logtbl, logs, append = TRUE)
  }
  toadd
}

#' @rdname syncLogs
#' @export

syncLogs.character <- function(localdb, rcon) {
  stopifnot(file.access(localdb) == 0)
  prid <- projectId(rcon)
  logtbl <- sprintf('logs_%s.rds', prid)
  if(file.access(file.path(localdb, logtbl)) == 0) {
    localLogs <- readLogs(localdb, rcon)
    maxts <- max(localLogs$timestamp)
    time <- maxts-60
    logs <- redcapAPI::exportLogging(rcon, beginTime = time)
    key1 <- do.call(paste, c(logs, sep = '|'))
    key2 <- do.call(paste, c(localLogs[,names(logs)], sep = '|'))
    logs <- logs[!(key1 %in% key2),]
    logs$completed_at <- as.POSIXct(NA)
    logs <- rbind(localLogs, logs)
  } else {
    logs <- redcapAPI::exportLogging(rcon)
    logs$completed_at <- as.POSIXct(NA)
  }
  toadd <- nrow(logs)
  if(toadd > 0) {
    saveRDS(logs, file = file.path(localdb, logtbl))
  }
  toadd
}
