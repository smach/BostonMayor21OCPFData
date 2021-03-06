

#' Download latest candidate contributions by candidate ID
#'
#' @param id character string with candidate MA OCPF ID
#' @param start_date string in yyyy-mm-dd format or date object for contributions on or after a certain date, defaults to NULL (all candidate data downloads; this function filters it)
#'
#' @return data frame with contribution information
#' @export
#'
download_latest_candidate_contributions <- function(id, start_date = as.Date("2020-02-01"), boston_zips = known_boston_zipcodes) {
  url <- paste0("https://www.ocpf.us/ReportData/GetTextOutput?1=1&searchTypeCategory=A&filerCpfId=", id, "&sortField=Date&sortDirection=ASC")
  the_file <- paste0("tmp/contributions_", id, "_", Sys.Date() )
  download.file(url, destfile = the_file )
  the_data <- data.table::fread(the_file, colClasses = c(`Zip Code` = "character"))
  if(!is.null(start_date)) {
    if(is.character(start_date)) {
      start_date <- as.Date(start_date)
    }
  }

    the_data <- the_data %>%
      dplyr::mutate(
        Date = lubridate::mdy(Date)
      ) %>%
      dplyr::filter(Date >= start_date) %>%
      dplyr::select(Date, Candidate = `Filer Full Name Reverse`,
                           FirstName = `First Name`,
                           LastName = Name,
                           City,
                           State,
                           ZipCode = `Zip Code`,
                           Amount,
                           Occupation,
                           Employer,
                           Report = `Source Description`
  ) %>%
      dplyr::mutate(
        InBostonZipCode = ifelse(ZipCode %in% boston_zips, "Yes", "No")
      )
  if(nrow(the_data) == 0){
    the_data[nrow(the_data)+1,] <- NA
  }
  return(the_data)
}
