
#' Dataframe with dates and contact symptom begin probability
#'
#' Creates a dataframe containing probability
#' that a contact will start showing symptoms (serial interval)
#' at a particular date/time, given the symptom_begin_date.
#'
#' shape_serial and rate_serial are the parameters of the gamma distribution
#' for the serial interval derived from Najafi et al. (2020).
#'
#' @param symptom_begin_date Date, when the index person got symptoms.
#' @param max_serial_interval_days Number of serial interval days.
#' @param shape_serial Number, the shape parameter for the gamma distribution.
#' @param rate_serial Number, the rate parameter for the gamma distribution.
#' @return Dataframe with dates and contact symptom begin probability.
#'
#' @references Najafi F et al. (2020) "Serial interval and
#' time-varying reproduction number estimation for COVID-19 in western Iran.".
#' \emph{New Microbes and New Infections}, 36: 100715.
#'
#' @examples
#' get_serial_interval_density(as.Date("2022-03-22"))
#' get_serial_interval_density(as.Date("2022-03-22"), max_serial_interval_days = 20,
#'                             shape_serial = 2.15, rate_serial = 0.38)
#'
#' @export
#' @import stats

get_serial_interval_density <- function(symptom_begin_date,
                                   max_serial_interval_days = 20,
                                   shape_serial = 2.154631545,
                                   rate_serial = 0.377343528){


  val_get_serial_interval_df(symptom_begin_date,
                             max_serial_interval_days,
                             shape_serial,
                             rate_serial)

  symptom_begin_date_posixct <- as.POSIXct(symptom_begin_date, tz = "CET")
  dates <- seq(symptom_begin_date_posixct,
               by = "hour",
               length.out = max_serial_interval_days * 24)
  gamma <- stats::dgamma(seq(0, max_serial_interval_days - (1 / 24), by = 1 / 24),
                  shape = shape_serial,
                  rate = rate_serial)

  return(data.frame("dates" = dates, "distribution" = gamma))
}


val_get_serial_interval_df <- function(symptom_begin_date,
                                       max_serial_interval_days,
                                       shape_serial,
                                       rate_serial){
  smidm_is_date(symptom_begin_date)
  smidm_is_natural_number(max_serial_interval_days)
  smidm_is_parameter_gamma(shape_serial)
  smidm_is_parameter_gamma(rate_serial)

}
