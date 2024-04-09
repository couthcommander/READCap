#' Keep REDCap Synced Locally
#'
#' Use redcapAPI to download data from REDCap and sync it with a database backend.
#'
#' @docType package
#'
#' @importFrom DBI dbClearResult dbExecute dbExistsTable dbFetch dbGetQuery dbListTables dbSendQuery dbWriteTable
#' @importFrom redcapAPI exportBulkRecords exportLogging
#'
"_PACKAGE"
