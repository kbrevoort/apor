#' Retrieve Treasury Data
#'
#' This function will pull down the necessary data on Treasury yields.
retrieve_tbills <- function(year) {
  in_data <- ustyc::getYieldCurve(year = year) %>%
    `[[`('df')

  my_data <- data.frame(date = as.Date(rownames(in_data), format = '%Y-%m-%d'),
                        trate_1yr = in_data$BC_1YEAR,
                        trate_2yr = in_data$BC_2YEAR,
                        trate_3yr = in_data$BC_3YEAR,
                        trate_5yr = in_data$BC_5YEAR,
                        trate_7yr = in_data$BC_7YEAR,
                        trate_10yr = in_data$BC_10YEAR)
}
