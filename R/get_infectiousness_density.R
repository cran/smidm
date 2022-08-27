#' Dataframe with dates and infectiousness probability
#'
#' Creates a dataframe containing infectiousness at a particular date/time,
#' given the symptom_begin_date.
#'
#' infectiousness_shift, shape_infectiousness_gamma and rate_infectiousness_gamma
#' are the distribution parameters for the infectious period from He et al. (2020).
#'
#' @param symptom_begin_date Date, when the person gets symptoms.
#' @param infectiousness_shift Number of days with the largest contagions before the first symptoms.
#' @param max_infectious_days Number of the infectious days.
#' @param shape_infectiousness_gamma Number, the shape parameter for the gamma distribution.
#' @param rate_infectiousness_gamma Number, the rate parameter for the gamma distribution.
#' @return Dataframe with dates and infectiousness probability.
#'
#' @references He, X et al. (2020) "Temporal dynamics in viral shedding and
#' transmissibility of COVID-19". \emph{Nature Medicine}, 26: 672–675.
#'
#' @examples
#' get_infectiousness_density(as.Date("2022-03-22"))
#' get_infectiousness_density(as.Date("2022-03-22"), infectiousness_shift = 12.272481,
#'                            max_infectious_days = 24, shape_infectiousness_gamma = 20.516508,
#'                            rate_infectiousness_gamma = 1.592124)
#'
#' @export
#' @import stats

get_infectiousness_density <- function(symptom_begin_date,
                                  infectiousness_shift = 12.272481,
                                  max_infectious_days = 24,
                                  shape_infectiousness_gamma = 20.516508,
                                  rate_infectiousness_gamma = 1.592124){


  val_get_infectiousness_df(symptom_begin_date,
                            infectiousness_shift,
                            max_infectious_days,
                            shape_infectiousness_gamma,
                            rate_infectiousness_gamma )

  infectiousness_start <- symptom_begin_date - infectiousness_shift

  # infectiousness_start + 1 because when converting to 24 hours, it ends up starting the day before
  dates <- seq(as.POSIXct(infectiousness_start, tz = "CET"),
               by = "hour",
               length.out = max_infectious_days * 24)
  gamma <- stats::dgamma(seq(0, max_infectious_days - (1 / 24), by = 1 / 24),
                  shape = shape_infectiousness_gamma,
                  rate = rate_infectiousness_gamma)

  return(data.frame("dates" = dates, "distribution" = gamma))
}


val_get_infectiousness_df <- function(symptom_begin_date,
                                      infectiousness_shift,
                                      max_infectious_days,
                                      shape_infectiousness_gamma,
                                      rate_infectiousness_gamma ){
  smidm_is_date(symptom_begin_date)
  smidm_is_double(infectiousness_shift)
  smidm_is_natural_number(max_infectious_days)
  smidm_is_parameter_gamma(shape_infectiousness_gamma)
  smidm_is_parameter_gamma(rate_infectiousness_gamma)
}
