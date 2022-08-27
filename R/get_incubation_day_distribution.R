#' Vector of day-specific probabilities of disease outbreak
#'
#' Creates a vector containing the probabilities of the disease outbreak
#' for the days 1 to maxi after the infection.
#'
#' meanlog and sdlog are the log-normal distribution parameters derived from
#' the incubation period characteristics described in Xin et al. (2021).
#'
#' @param max_days Number, the maximum length of the incubation time, defined as number.
#' @param meanlog Number, the parameter of mean from the log-normal distribution.
#' @param sdlog Number, the parameter of sd from the log-normal distribution.
#' @return Vector of day-specific probabilities of disease outbreak.
#' @export
#'
#' @references Xin H, Wong JY, Murphy C et al. (2021) "The Incubation Period
#' Distribution of Coronavirus Disease 2019: A Systematic Review and Meta-Analysis".
#' \emph{Clinical Infectious Diseases}, 73(12): 2344-2352.
#'
#' @examples
#' get_incubation_day_distribution(10)
#' get_incubation_day_distribution(10, meanlog = 1.69, sdlog = 0.55)
#'
#'@import stats

get_incubation_day_distribution <- function(max_days,
                                           meanlog = 1.69,
                                           sdlog = 0.55) {
  val_create_theoretical_distribution(max_days,
                                      meanlog,
                                      sdlog)

  t <- seq(0, max_days)
  distris <- stats::plnorm(t, meanlog = meanlog, sdlog = sdlog)

  return(distris[-1] - distris[-length(distris)])
}


val_create_theoretical_distribution <- function(max_days,
                                                meanlog,
                                                sdlog){

  smidm_is_natural_number(max_days)
  smidm_is_parameter_log_mean(meanlog)
  smidm_is_parameter_log_sd(sdlog)

}
