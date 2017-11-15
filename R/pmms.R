#' Retrieve PMMS Data
#'
#' This function will retrieve the necessary survey data to calculate Average Percentage
#' Offer Rates (APORs).
retrieve_pmms_data <- function() {
  html_data <- 'https://www.ffiec.gov/ratespread/mortgagerates.htm' %>%
    read_html()

  table_data <- html_data %>%
    html_nodes(xpath = '/html/body/div/div[5]/div/div[2]/div/div[1]/table') %>%
    html_table(header = TRUE, trim = TRUE) %>%
    `[[`(1)

  source_codes <- html_data %>%
    html_nodes(xpath = '/html/body/div/div[5]/div/div[2]/div/p[3]') %>%
    html_text(trim = TRUE) %>%
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

  table_data
}
