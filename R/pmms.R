#' Retrieve PMMS Data
#'
#' This function will retrieve the necessary survey data to calculate Average Percentage
#' Offer Rates (APORs).
#' @importFrom rvest html_nodes html_table html_text
#' @importFrom stringr str_split
#' @importFrom dplyr mutate select
#' @importFrom xml2 read_html
#' @export
retrieve_pmms_data <- function() {
  html_data <- 'https://www.ffiec.gov/ratespread/mortgagerates.htm' %>%
    xml2::read_html()

  table_data <- html_data %>%
    rvest::html_nodes(xpath = '/html/body/div/div[5]/div/div[2]/div/div[1]/table') %>%
    rvest::html_table(header = TRUE, trim = TRUE) %>%
    `[[`(1)

  source_codes <- html_data %>%
    rvest::html_nodes(xpath = '/html/body/div/div[5]/div/div[2]/div/p[3]') %>%
    rvest::html_text(trim = TRUE) %>%
    stringr::str_split('\r\n') %>%
    `[[`(1)

  names(table_data) <- c('Date',
                         'fixed_30yr_rate',
                         'fixed_30yr_points',
                         'fixed_15yr_rate',
                         'fixed_15yr_points',
                         'arm_5yr_rate',
                         'arm_5yr_points',
                         'arm_5yr_margin',
                         'arm_1yr_rate',
                         'arm_1yr_points',
                         'arm_1yr_margin',
                         'arm_source',
                         'other_source')

  dplyr::mutate(table_data, pmms_date = as.Date(Date, format = '%B %d, %Y')) %>%
    dplyr::select(pmms_date, fixed_30yr_rate, fixed_30yr_points, fixed_15yr_rate,
                  fixed_15yr_points, arm_5yr_rate, arm_5yr_points, arm_5yr_margin,
                  arm_1yr_rate, arm_1yr_points, arm_1yr_margin) %>%
    dplyr::mutate(arm_1yr_points = round(arm_1yr_points, digits = 1))
}
