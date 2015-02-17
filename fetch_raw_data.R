# fetch_raw_data.R

# FetchRawData: Downloads and extracts the raw data for this assignment.
FetchRawData <- function(overwrite = F) {
  # overwrite: If TRUE, will always re-download and overwrite extracted files.

  # raw.data.dir: The path of the raw data directory.
  # timestamp: The path of the file to record the download timestamp.
  # source.url: The URL of the raw data zip -- where to download from.
  # local.zip: The path of the downloaded zip file -- where to download to.
  # extract.dir: The path of the directory to extract to.
  raw.data.dir <- "raw"  # relative path
  timestamp    <- file.path(raw.data.dir, "Timestamp.txt")
  source.url   <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  local.zip    <- file.path(raw.data.dir, "source.zip")
  extract.dir  <- file.path(raw.data.dir, "unzipped")

  # DownloadRawData: Downloads the raw data zip file.
  DownloadRawData <- function(overwrite = F) {
    # overwrite: If TRUE, re-download the zip file even if it exists.

    # Check whether we can skip.
    if (file.exists(local.zip) && !overwrite) {
      message(sprintf("%s exists, skipping download.", local.zip))
      return(invisible())
    }

    # Download the file.
    if (!file.exists(raw.data.dir)) {
      dir.create(raw.data.dir)
    }
    download.file(source.url, local.zip, mode = "wb")  # Windows

    # Record the timestamp.
    timestamp.con = file(timestamp)
    writeLines(date(), timestamp.con)
    close(timestamp.con)
  }

  # ExtractRawData: Extracts the downloaded zip file. Expects the file to exist
  #                 or will bail out with an error.
  ExtractRawData <- function(overwrite = F) {
    # overwrite: If TRUE, extract and overwrite any existing files even if the
    #            extract folder exists.

    # Check whether we can skip.
    if (file.exists(extract.dir) && !overwrite) {
      message(sprintf("%s exists, skipping unzip.", extract.dir))
      return(invisible())
    }

    # Check that the download exist.
    if (!file.exists(local.zip)) {
      stop(paste("cannot find", local.zip))
    }

    # Unzip the downloaded file.
    if (!file.exists(extract.dir)) {
      dir.create(extract.dir)
    }
    unzip(local.zip, exdir = extract.dir)
  }

  DownloadRawData(overwrite)
  ExtractRawData(overwrite)
}
