#' Prediction of future infections per day
#'
#' Predicts how many people are expected to develop symptoms on each
#' day after the last reported infection after a group event.
#'
#' meanlog and sdlog are the log-normal distribution parameters derived from
#' the incubation period characteristics described in Xin et al. (2021).
#'
#' @param last_day_reported_infection Number of days the last infection was reported after the event (0 = event day).
#' @param total_reported_infections Number of reported symptomatic infections so far.
#' @param total_expected_infections Number of expected symptomatic infections in total.
#' @param meanlog Number, the parameter of mean from the log-normal distribution.
#' @param sdlog Number, the parameter of sd from the log-normal distribution.
#' @return Vector with expected future infections per day after the event.
#'
#' @references Xin H, Wong JY, Murphy C et al. (2021) "The Incubation Period
#' Distribution of Coronavirus Disease 2019: A Systematic Review and Meta-Analysis".
#' \emph{Clinical Infectious Diseases}, 73(12): 2344-2352.
#'
#' @examples
#' predict_future_infections(last_day_reported_infection = 3,
#'                           total_reported_infections = 5,
#'                           total_expected_infections = 15)
#'
#' @export

predict_future_infections <- function(last_day_reported_infection,
                                                  total_reported_infections,
                                                  total_expected_infections,
                                                  meanlog = 1.69,
                                                  sdlog = 0.55) {
  val_predict_future_infections(last_day_reported_infection,
                                total_reported_infections,
                                total_expected_infections,
                                meanlog,
                                sdlog)

  further_expected <- rep(0, last_day_reported_infection)

  max_days <- last_day_reported_infection + 10
  theoretical_propabilities <- smidm::get_incubation_day_distribution(max_days,
                                                                      meanlog,
                                                                      sdlog)

  i <- last_day_reported_infection + 1
  while (i <= max_days) {
    further_expected[i] <-
      ceiling((total_expected_infections - total_reported_infections) * theoretical_propabilities[i] /
              (1 - cumsum(theoretical_propabilities)[i - 1]))
    total_reported_infections <- total_reported_infections + further_expected[i]
    if (total_reported_infections >= total_expected_infections) {
      further_expected[i + 1] <- 0
      break()
      }
    if (i == max_days) {
      max_days <- max_days + 1
      theoretical_propabilities <- smidm::get_incubation_day_distribution(max_days,
                                                                          meanlog,
                                                                          sdlog)
      }
    if (max_days == 1000) {
      stop("something is wrong")
      }
    i <- i + 1
  }
  return(further_expected)
}

val_predict_future_infections <- function(last_day_reported_infection,
                                          total_reported_infections,
                                          total_expected_infections,
                                          meanlog,
                                          sdlog){
  smidm_is_natural_number(last_day_reported_infection)
  smidm_is_natural_number(total_reported_infections)
  smidm_is_natural_number(total_expected_infections)
  smidm_is_parameter_log_mean(meanlog)
  smidm_is_parameter_log_sd(sdlog)
}
