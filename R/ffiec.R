#' Retrieve Published APOR Table
#'
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

  mutate(in_data, date = as.Date(date, format = '%m/%d/%Y'))
}
