#' Retrieve Published APOR Table
#'
#' This function will retrieve the APOR table published by the FFIEC for the
#' current year. The table will be returned as a data.frame.
#' @param type The type of product ('fixed' or 'arm') to retrieve the APORS for.
#' @return A data.frame that lists the APORs for loan terms of between 1 and 50 years.
#' @importFrom curl curl_download
#' @export
retrieve_published_apors <- function(type) {
  if (tolower(type) == 'fixed') {
    url <- 'http://www.ffiec.gov/ratespread/YieldTableFixed.CSV'
  } else if (tolower(type) == 'arm') {
    url <- 'http://www.ffiec.gov/ratespread/YieldTableAdjustable.CSV'
  } else
    stop('Invalid type supplied to retrieve_published_apors (acceptable types are fixed or arm).')

  tmp <- tempfile()
  curl::curl_download(url, tmp)
  in_data <- read.table(tmp, header = TRUE, sep = ',')
  names(in_data) <- c('date', sprintf('%s_%dyr', tolower(type), 1:50))

  dplyr::mutate(in_data, date = as.Date(date, format = '%m/%d/%Y'))
}
