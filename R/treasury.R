#' Retrieve Treasury Data
#'
#' This function will pull down the necessary data on Treasury yields and return
#' a data.frame with the correct average T-bill rates for each PMMS date.
retrieve_tbills <- function() {

  tbill_data <- c(2008:as.integer(format(Sys.Date(), '%Y'))) %>%
    lapply(get_tbills_by_year) %>%
    do.call('rbind', .)

  mutate(tbill_data, day = weekdays(date)) %>%
    filter(day %in% c('Monday', 'Tuesday', 'Wednesday')) %>%
    mutate(pmms_date = lubridate::floor_date(date, unit = 'week') + 11L) %>%
    group_by(pmms_date) %>%
    summarize(trate_1yr = mean(trate_1yr),
              trate_2yr = mean(trate_2yr),
              trate_3yr = mean(trate_3yr),
              trate_5yr = mean(trate_5yr),
              trate_7yr = mean(trate_7yr),
              trate_10yr = mean(trate_10yr))
}

get_tbills_by_year <- function(y) {
  in_data <- ustyc::getYieldCurve(year = y) %>%
    `[[`('df')

  data.frame(date = as.Date(rownames(in_data), format = '%Y-%m-%d'),
             trate_1yr = in_data$BC_1YEAR,
             trate_2yr = in_data$BC_2YEAR,
             trate_3yr = in_data$BC_3YEAR,
             trate_5yr = in_data$BC_5YEAR,
             trate_7yr = in_data$BC_7YEAR,
             trate_10yr = in_data$BC_10YEAR)
}
