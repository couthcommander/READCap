% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/syncLogs.R
\name{syncLogs}
\alias{syncLogs}
\alias{syncLogs.default}
\alias{syncLogs.character}
\title{Sync Local REDCap Event Logs}
\usage{
syncLogs(localdb, rcon)

\method{syncLogs}{default}(localdb, rcon)

\method{syncLogs}{character}(localdb, rcon)
}
\arguments{
\item{localdb}{\code{link[DBI]{dbConnect}} object, or perhaps local folder}

\item{rcon}{\code{link[redcapAPI]{redcapConnection}} object}
}
\value{
data.frame, updated event logs
}
\description{
Sync REDCap event logs.
}
