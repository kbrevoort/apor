compare_apors <- function(type) {
  my_dt <- suppressMessages(assemble_data())

  published <- retrieve_published_apors(type)
  my_apors <- calculate_apors(dt = my_dt, type)

  my_dates <- my_apors$date

  published <- filter(published, date %in% my_dates)

  for (i in c(1:50)) {
    my_var <- sprintf('%s_%dyr', type, i)
    x <- my_apors[[my_var]]
    y <- published[[my_var]]

    my_diff <- abs(x - y)
    #if (all(my_diff < 0.01)) {
    if (all(my_diff == 0)) {
      sprintf('Complete match for term = %d\n', i) %>%
        cat()
    } else {
      bads <- which(my_diff != 0.00)
      for (z in bads) {
        sprintf('Bad match for term = %d at %s:  %f and %f\n', i, as.character(my_dates[z]), x[z], y[z]) %>%
          cat()
      }
      #stop()
    }
  }
}
