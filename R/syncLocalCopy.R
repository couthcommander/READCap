#' Sync local project tables
#'
#' Sync tables associated with locally stored REDCap project.
#'
#' @param localdb \code{link[DBI]{dbConnect}} object, or perhaps local folder
#' @param rcon \code{link[redcapAPI]{redcapConnection}} object
#'
#' @return numeric, number of records updated
#'
#' @export

syncLocalCopy <- function(localdb, rcon) {
  UseMethod('syncLocalCopy')
}

#' @rdname syncLocalCopy
#' @export

syncLocalCopy.default <- function(localdb, rcon) {
  syncLogs(localdb, rcon)
  prid <- projectId(rcon)
  myenv <- new.env()
  ebr_args <- list(lcon = list(rcon), envir = myenv)
  names(ebr_args$lcon) <- prid
  log_table <- paste0('logs_', prid)
  # identify local tables with project_id
  proj_tables <- projectTables(localdb, rcon)
  proj_log <- readLogs(localdb, rcon)
  # restrict to incomplete
  task_log <- proj_log[is.na(proj_log$completed_at),]

  # field modification should force complete update
  has_field_modification <- any(grepl('^(Create|Edit|Delete) project field', task_log$details))
  has_no_tables <- length(proj_tables) == 0
  if(has_no_tables || has_field_modification) {
    do.call(redcapAPI::exportBulkRecords, ebr_args)
  } else {
    rid_insert <- unique(task_log[grep('^Create record', task_log$action), 'record'])
    rid_update <- unique(task_log[grep('^Update record', task_log$action), 'record'])
    rid_delete <- unique(task_log[grep('^Delete record', task_log$action), 'record'])
    rid_export <- sort(union(rid_insert, rid_update))
    rid_remove <- sort(union(rid_delete, rid_update))
    if(length(rid_export) > 0) {
      do.call(redcapAPI::exportBulkRecords, c(ebr_args, records = rid_export))
    }
    # delete "rid_remove" from localdb
    if(length(rid_remove) > 0) {
      for(cur_table in proj_tables) {
        qry_delete <- sprintf("DELETE FROM `%s` WHERE record_id IN (?)", cur_table)
        DBI::dbExecute(localdb, qry_delete, params = list(rid_remove))
      }
    }
  }
  rcdat <- ls(envir = myenv)
  for(cur_table in rcdat) {
    DBI::dbWriteTable(localdb, cur_table, myenv[[cur_table]], append = TRUE)
  }
  qry_update <- sprintf("UPDATE %s SET completed_at = ? WHERE completed_at IS NULL", log_table)
  # return value is number of log rows affected by update statement
  DBI::dbExecute(localdb, qry_update, params = list(Sys.time()))
}

#' @rdname syncLocalCopy
#' @export

syncLocalCopy.character <- function(localdb, rcon) {
  stopifnot(file.access(localdb) == 0)
  syncLogs(localdb, rcon)
  prid <- projectId(rcon)
  myenv <- new.env()
  ebr_args <- list(lcon = list(rcon), envir = myenv)
  names(ebr_args$lcon) <- prid
  # identify local tables with project_id
  proj_tables <- projectTables(localdb, rcon)
  proj_log <- readLogs(localdb, rcon)
  # restrict to incomplete
  task_log <- proj_log[is.na(proj_log$completed_at),]

  # field modification should force complete update
  has_field_modification <- any(grepl('^(Create|Edit|Delete) project field', task_log$details))
  has_no_tables <- length(proj_tables) == 0
  if(has_no_tables || has_field_modification) {
    do.call(redcapAPI::exportBulkRecords, ebr_args)
    rid_remove <- character(0)
  } else {
    rid_insert <- unique(task_log[grep('^Create record', task_log$action), 'record'])
    rid_update <- unique(task_log[grep('^Update record', task_log$action), 'record'])
    rid_delete <- unique(task_log[grep('^Delete record', task_log$action), 'record'])
    rid_export <- sort(union(rid_insert, rid_update))
    rid_remove <- sort(union(rid_delete, rid_update))
    if(length(rid_export) > 0) {
      do.call(redcapAPI::exportBulkRecords, c(ebr_args, records = rid_export))
    }
  }
  rcdat <- ls(envir = myenv)
  for(cur_table in rcdat) {
    tname <- file.path(localdb, paste0(cur_table, '.rds'))
    dati <- myenv[[cur_table]]
    if(!has_field_modification && file.access(tname) == 0) {
      datj <- readRDS(tname)
      # delete "rid_remove" from localdb
      if(length(rid_remove) > 0) {
        datj <- datj[!(datj$record_id %in% rid_remove),]
      }
      dati <- rbind(datj, dati)
    }
    saveRDS(dati, file = tname)
  }
  ix <- which(is.na(proj_log$completed_at))
  proj_log$completed_at[ix] <- Sys.time()
  saveRDS(proj_log, file = file.path(localdb, sprintf('logs_%s.rds', prid)))
  length(ix)
}
