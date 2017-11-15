#' Calculate APRs
#'
calculate_aprs <- function(dt) {
  if (!all(c('fixed_30yr_rate', 'fixed_30yr_points') %in% names(dt)))
    stop('Fixed rate information not included in data.frame sent to calculate_aprs')

  n <- dim(dt)[1]

  fa_30yr <- vapply(1:n,
                    function(i, dt) calculate_apr_fixed(dt$fixed_30yr_rate[i],
                                                        dt$fixed_30yr_points[i],
                                                        t = 360L),
                    1.2,
                    dt = dt)

  fa_15yr <- vapply(1:n,
                    function(i, dt) calculate_apr_fixed(dt$fixed_15yr_rate[i],
                                                        dt$fixed_15yr_points[i],
                                                        t = 180L),
                    1.2,
                    dt = dt)

}

#' Fixed APR Calculation
#'
#' This function computes the APR for a fixed-rate loan.
#' @param r Annual interest rate as a percentage
#' @param p Points and Fees expressed as percentage of loan amount
#' @param t Term of loan in months
#' @export
calculate_apr_fixed <- function(r, p, t = 360L) {
  amount_financed <- 1 - (p / 100)
  monthly_payment <- calculate_monthly_payment(r, t)

  g <- function(i, t, m, af) {
    i <- i / 1200L
    i_vec <- rep.int(1L + i, times = t)
    t_vec <- 1:t
    af - sum(m / (i_vec ^ t_vec))
  }

  root_val <- uniroot(g, c(0, max(c(r + p, r))), t = t, m = monthly_payment, af = amount_financed)
  round(root_val$root, digits = 2)
}

#' @param rpm Interest rate per month (generally equals r / 1200)
calculate_monthly_payment <- function(r, t, l = 1L) {
  r <- r / 1200L
  (r * ((1 + r) ^ t)) / (((1 + r) ^ t) - 1) * l
}
