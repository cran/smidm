#' Dataframe with dates and probability of infection
#'
#' Creates a dataframe containing probability of infection
#' occurring at a particular date/time,
#' given the symptom_begin_date.
#'
#' meanlog and sdlog are the log-normal distribution parameters derived from
#' the incubation period characteristics described in Xin et al. (2021).
#'
#' @param symptom_begin_date Date, when the person gets symptoms.
#' @param max_incubation_days Number of incubation days.
#' @param meanlog Number, the parameter of mean from the log-normal distribution.
#' @param sdlog Number, the parameter of sd from the log-normal distribution.
#' @return Dataframe with dates and probability of infection.
#'
#' @references Xin H, Wong JY, Murphy C et al. (2021) "The Incubation Period
#' Distribution of Coronavirus Disease 2019: A Systematic Review and Meta-Analysis".
#' \emph{Clinical Infectious Diseases}, 73(12): 2344-2352.
#'
#' @examples
#' get_infection_density(as.Date("2022-03-22"))
#' get_infection_density(as.Date("2022-03-22"), max_incubation_days = 14, meanlog = 1.69, sdlog = 0.55)
#'
#' @export
#' @import stats

get_infection_density <- function(symptom_begin_date,
                                  max_incubation_days = 14,
                                  meanlog = 1.69,
                                  sdlog = 0.55) {

  val_get_infection_date_df(symptom_begin_date,
                            max_incubation_days,
                            meanlog,
                            sdlog)


  infection_period_start <- symptom_begin_date - max_incubation_days

  days_seq <- seq(0, max_incubation_days - (1 / 24), by = 1 / 24)
  dates <- seq(as.POSIXct(infection_period_start, tz = "CET"),
               by = "hour",
               length.out = max_incubation_days * 24)
  gamma <- rev(stats::dlnorm(seq(0, max_incubation_days - (1 / 24), by = 1 / 24),
                      meanlog = meanlog,
                      sdlog = sdlog))

  return(data.frame("dates" = dates, "distribution" = gamma))
}


val_get_infection_date_df <- function(symptom_begin_date,
                                      max_incubation_days,
                                      meanlog,
                                      sdlog){
  smidm_is_date(symptom_begin_date)
  smidm_is_natural_number(max_incubation_days)
  smidm_is_parameter_log_mean(meanlog)
  smidm_is_parameter_log_sd(sdlog)
}

