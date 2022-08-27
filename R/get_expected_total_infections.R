#' Expected number of total symptomatic infections
#'
#' Calculates the expected total number of symptomatic infections after a group event,
#' based on the observed infections so far.
#'
#' meanlog and sdlog are the log-normal distribution parameters derived from
#' the incubation period characteristics described in Xin et al. (2021).
#' Note that the function often clearly overestimates the number of symptomatic infections
#' if last_day_reported_infections is less than 3.
#'
#' @param group_size integer, size of the group.
#' @param last_day_reported_infection Number of days the last infection was reported after the event (0 = event day).
#' @param total_reported_infections Number of reported symptomatic infections so far.
#' @param meanlog Number, the parameter of mean from the log-normal distribution.
#' @param sdlog Number, the parameter of sd from the log-normal distribution.
#' @return The total number of expected symptomatic infections.
#'
#' @examples
#' get_expected_total_infections(25, 5, 4)
#'
#' @export

get_expected_total_infections <- function(group_size,
                                          last_day_reported_infection,
                                          total_reported_infections,
                                          meanlog = 1.69,
                                          sdlog = 0.55) {
  val_get_expected_total_infections(group_size,
                                    last_day_reported_infection,
                                    total_reported_infections,
                                    meanlog,
                                    sdlog)

  theoretical_probs <- smidm::get_incubation_day_distribution(last_day_reported_infection,
                                                              meanlog,
                                                              sdlog)

  return(min(ceiling(total_reported_infections * sum(theoretical_probs)^-1),
             group_size))
}


val_get_expected_total_infections <- function(group_size,
                                              last_day_reported_infection,
                                              total_reported_infections,
                                              meanlog,
                                              sdlog){
  smidm_is_natural_number(group_size)
  smidm_is_natural_number(last_day_reported_infection)
  smidm_is_natural_number(total_reported_infections)
  smidm_is_parameter_log_mean(meanlog)
  smidm_is_parameter_log_sd(sdlog)
}


