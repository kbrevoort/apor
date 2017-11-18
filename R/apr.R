#' Calculate APORs
#'
#' This function calculates the Average Prime Offer Rates (APORs) for the dates
#' for which data have been supplied.
#' @param dt A data.frame of the type produced by assemble_date()
#' @param type A character vector indicating if APORs are to be calculated for
#' fixed- or adjustable-rate loans (e.g., 'fixed', 'arm')
calculate_apors <- function(dt, type) {
  if (!(type %in% c('arm', 'fixed')))
    stop('Invalid type supplied to calculate_apors (only arm or fixed allowed).')

  value_list <- c(1, 2, 3, 5, 7, 10)
  if (type == 'fixed')
    value_list <- c(value_list, c(15, 30))

  my_data <- data.frame(date = dt$pmms_date + 4L) # Set date as Monday after PMMS published
  for (i in value_list) {
    my_data[[sprintf('%s_%dyr', type, i)]] <- calculate_apor(type, i, dt)
  }

  # Now create the whole table
  all_list <- c(1:50)
  for (i in all_list) {
    my_var <- sprintf('%s_%dyr', type, i)
    if (!(my_var %in% names(my_data))) {
      if (i <= 8) {
        use_num <- i - 1
      } else if (i >= 9 & i <= 12) {
        use_num <- 10L
      } else if (i >= 13 & i <= 22) {
        use_num <- 15L
      } else {
        use_num <- 30L
      }

      if (type == 'arm' & use_num > 10)
        use_num <- 10L

      use_var <- sprintf('%s_%dyr', type, use_num)
      my_data[[my_var]] <- my_data[[use_var]]
    }
  }

  select(my_data, date, sprintf('%s_%dyr', type, c(1:50)))
}


calculate_apor <- function(type, term, dt) {
  if (!(type %in% c('arm', 'fixed')))
    stop('Invalid type supplied to calculate_apor')

  my_rate <- sprintf('%s_%dyr_rate', type, term)
  my_points <- sprintf('%s_%dyr_points', type, term)

  n <- dim(dt)[1]
  if (type == 'arm') {
    my_fi <- sprintf('%s_%dyr_fi', type, term)
    ret_val <- vapply(1:n,
                      function(i, dt) calculate_apr_arm(dt[[my_rate]][i],
                                                        dt[[my_points]][i],
                                                        dt[[my_fi]][i],
                                                        term),
                      1.2,
                      dt = dt)
  } else {
    ret_val <- vapply(1:n,
                      function(i, dt) calculate_apr_fixed(dt[[my_rate]][i],
                                                          dt[[my_points]][i],
                                                          term * 12L),
                      1.2,
                      dt = dt)
  }

  ret_val
}

#' Fixed APR Calculation
#'
#' This function computes the APR for a fixed-rate loan.
#' @param r Annual interest rate as a percentage
#' @param p Points and Fees expressed as percentage of loan amount
#' @param t Term of loan in months
#' @export
calculate_apr_fixed <- function(r, p, t = 360L) {
  # This is experimental
  p <- round(p, digits = 1)

  amount_financed <- 1 - (p / 100)
  monthly_payment <- calculate_monthly_payment(r, t)

  min_val <- 0
  max_val <- max(c(r + p, r))
  min_sign <- sign(iterate_apr(0, t, monthly_payment, amount_financed))
  max_sign <- sign(iterate_apr(max_val, t, monthly_payment, amount_financed))
  counter <- 0
  while(min_sign == max_sign) {
    counter + 1
    if (counter > 100)
      stop('Nonconvergence in calculate_apr_fixed')

    min_val <- max_val
    max_val <- max_val + 1
    max_sign <- sign(iterate_apr(max_val, t, monthly_payment, amount_financed))
  }

  root_val <- uniroot(iterate_apr,
                      c(min_val, max_val),
                      t = t,
                      m = monthly_payment,
                      af = amount_financed)
  round(root_val$root, digits = 2)
}

iterate_apr <- function(i, t, m, af) {
  i <- i / 1200L
  i_vec <- rep.int(1L + i, times = t)
  t_vec <- 1:t
  af - sum(m / (i_vec ^ t_vec))
}

calculate_apr_arm <- function(r, p, fi, initial_term) {
  n <- 360L
  start_bal <- rep.int(0, times = n)
  irate <- start_bal
  payment <- start_bal
  end_bal <- start_bal

  for (i in 1:n) {
    start_bal[i] <- ifelse(i == 1, 1L, end_bal[i - 1])

    # Set the path of interest rates and payments
    if (i == 1) {
      irate[i] <- r
      payment[i] <- calculate_monthly_payment(r, 360L, 1L)
    } else if (((i - 1) %% 12) == 0) {  # True every potential interest rate reset
      if (i < (initial_term * 12)) {    # Set interest rate to teaser during teaser period
        irate[i] <- irate[i - 1]
        payment[i] <- payment[i - 1]
      } else {                          # Potentially new interest rate
        irate[i] <- min(irate[i - 1] + 2, fi)
        payment[i] <- calculate_monthly_payment(irate[i], 360L - i + 1L, start_bal[i])
      }
    } else {
      irate[i] <- irate[i - 1]
      payment[i] <- payment[i - 1]
    }

    end_bal[i] <- ((1 + (irate[i] / 1200)) * start_bal[i]) - payment[i]
  }

  # Confirm that the payments paid off the balance
  if (abs(end_bal[360L]) > 0.01)
    sprintf('The ARM APR calculator has a residual balance of %f\n', end_bal[360L]) %>%
    stop()

  root_val <- uniroot(iterate_apr,
                      c(0, max(c(r + p, r, fi + p))),
                      t = 360L,
                      m = payment,
                      af = 1 - (p / 100))
  round(root_val$root, digits = 2)

}

#' Calculate Monthly Payment
#'
#' This function calculates the monthly payment required to fully pay off a loan
#' with a fixed term and a constant interest rate.
#' @param r Annual interest rate as a percentage
#' @param t Term of the loan in months
#' @param l Loan amount (defaults to $1)
#' @return The monthly payment required to fully payoff the loan by the end of the term.
#' @export
calculate_monthly_payment <- function(r, t, l = 1L) {
  r <- r / 1200L
  (r * ((1 + r) ^ t)) / (((1 + r) ^ t) - 1) * l
}

#' Assemble Data
#'
#' This function compiles all of the information necessary to generate Average
#' Prime Offer Rates (APORs).  This information includes values from the Freddie
#' Mac Primary Mortgage Market Survey (PMMS) and U.S. Treasury yields. In addition
#' to these data sources, the function also imputes values for intermediate terms
#' as specified in the procedure for generating APORs.
#' @return A data.frame that contains the interest rates, points and fees, and
#' (for adjustable-rate loans) the margin for different term loans.
#' @export
assemble_data <- function() {
  tbill_data <- retrieve_tbills()
  pmms_data <- retrieve_pmms_data()

  all_data <- merge(pmms_data, tbill_data,
        by = 'pmms_date') %>%
    mutate(fixed_10yr_rate = (arm_5yr_rate - trate_5yr) + trate_10yr) %>%
    mutate(fixed_7yr_rate = (arm_5yr_rate - trate_5yr) + trate_7yr) %>%
    mutate(fixed_5yr_rate = arm_5yr_rate) %>%
    mutate(fixed_3yr_rate = round_higher(arm_1yr_rate - trate_1yr,
                                         arm_5yr_rate - trate_5yr,
                                         '3yr', 2) + trate_3yr) %>%
    mutate(fixed_2yr_rate = round_higher(arm_1yr_rate - trate_1yr,
                                         arm_5yr_rate - trate_5yr,
                                         '2yr', 2) + trate_2yr) %>%
    mutate(fixed_1yr_rate = arm_1yr_rate) %>%
    mutate(fixed_10yr_points = arm_5yr_points) %>%
    mutate(fixed_7yr_points = arm_5yr_points) %>%
    mutate(fixed_5yr_points = arm_5yr_points) %>%
    mutate(fixed_3yr_points = round_higher(arm_1yr_points, arm_5yr_points, term = '3yr', 1L)) %>%
    mutate(fixed_2yr_points = round_higher(arm_1yr_points, arm_5yr_points, term = '2yr', 1L)) %>%
    mutate(fixed_1yr_points = arm_1yr_points) %>%
    mutate(arm_10yr_rate = fixed_10yr_rate) %>%
    mutate(arm_7yr_rate = fixed_7yr_rate) %>%
    mutate(arm_3yr_rate = fixed_3yr_rate) %>%
    mutate(arm_2yr_rate = fixed_2yr_rate) %>%
    mutate(arm_10yr_points = fixed_10yr_points) %>%
    mutate(arm_7yr_points = fixed_7yr_points) %>%
    mutate(arm_3yr_points = fixed_3yr_points) %>%
    mutate(arm_2yr_points = fixed_2yr_points) %>%
    mutate(arm_10yr_margin = arm_5yr_margin) %>%
    mutate(arm_7yr_margin = arm_5yr_margin) %>%
    mutate(arm_3yr_margin = round_higher(arm_1yr_margin, arm_5yr_margin, '3yr', 2L)) %>%
    mutate(arm_2yr_margin = round_higher(arm_1yr_margin, arm_5yr_margin, '2yr', 2L)) %>%
    mutate(arm_10yr_fi = round(arm_10yr_margin + trate_1yr, digits = 2L)) %>%
    mutate(arm_7yr_fi = round(arm_7yr_margin + trate_1yr, digits = 2L)) %>%
    mutate(arm_5yr_fi = round(arm_5yr_margin + trate_1yr, digits = 2L)) %>%
    mutate(arm_3yr_fi = round(arm_3yr_margin + trate_1yr, digits = 2L)) %>%
    mutate(arm_2yr_fi = round(arm_2yr_margin + trate_1yr, digits = 2L)) %>%
    mutate(arm_1yr_fi = round(arm_1yr_margin + trate_1yr, digits = 2L))
}

#' Round Midpoint Values Up
#'
#' R's default rounding behavior is to round values of .5 to the nearest even
#' integer. This creates inconsistencies with SAS, which rounds up in all cases.
#' To match the published values of APOR, this function will round midpoint values
#' up in all cases.
#' @param x Value based on 1-year observations (Treasury or PMMS)
#' @param y Value based on 5-year observations
#' @param term Character value indicating if the rounding is being done for the
#' 2-year or 3-year products.
#' @param digits The number of digits to include in the value returned
#' @return The weighted average of x and y
round_higher <- function(x, y, term, digits) {
  x_mult <- ifelse(term == '2yr', 3L, 2L)
  adjustment <- 10^digits

  total <- as.integer((round(x * adjustment, digits = 0) * x_mult) +
                        (round(y * adjustment, digits = 0) * (4 - x_mult)))

  ret_val <- ifelse(total %% 4 %in% c(2, 3),
                    ceiling(total / 4L),
                    trunc(total / 4L))

  ret_val / adjustment
}
