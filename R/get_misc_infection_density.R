#' Dataframe with dates and probability of infection
#'
#' Creates a dataframe containing probability of infection
#' occurring at a particular dates/times,
#' given the symptom_begin_dates and number_of_persons per date.
#'
#' meanlog and sdlog are the log-normal distribution parameters derived from
#' the incubation period characteristics described in Xin et al. (2021).
#'
#' @param symptom_begin_dates Dates, when the persons get symptoms.
#' @param number_of_persons Number of persons who get symptoms on each date.
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
#' symptom_begin_dates <- c(as.Date("2022-03-22"), as.Date("2022-03-26"))
#' number_of_persons <- c(3,1)
#' get_misc_infection_density(symptom_begin_dates, number_of_persons)
#'
#' @export
#' @import stats

get_misc_infection_density <- function(symptom_begin_dates,
                                  number_of_persons,
                                  max_incubation_days = 17,
                                  meanlog = 1.69,
                                  sdlog = 0.55) {

  val_get_misc_infection_density(symptom_begin_dates,
                                 number_of_persons,
                                 max_incubation_days,
                                 meanlog,
                                 sdlog)

  # collect all distributions
  df <- get_infection_density(symptom_begin_dates[1], max_incubation_days,
                                     meanlog, sdlog)

  if(length(symptom_begin_dates) > 1)
  {
    for(i in 2:length(symptom_begin_dates))
    {
      df_cur <- get_infection_density(symptom_begin_dates[i], max_incubation_days,
                                             meanlog, sdlog)
      names(df_cur)[names(df_cur) == "distribution"] <- paste0("distribution_", i)
      df <- merge(x = df, y = df_cur, by = "dates", all = TRUE)
    }
  }

  df[is.na(df)] <- 0

  df_misc <- data.frame("dates" = df$dates, "distribution" = df[,2]*number_of_persons[1]/sum(number_of_persons) )

  if(length(symptom_begin_dates) > 1)
  {
    for(i in 2:length(symptom_begin_dates))
    {
      df_misc$distribution <- df_misc$distribution + df[,i+1]*number_of_persons[i]/sum(number_of_persons)
    }
  }

  return(df_misc)
}


val_get_misc_infection_density <- function(symptom_begin_dates,
                                           number_of_persons,
                                           max_incubation_days,
                                           meanlog,
                                           sdlog){
  smidm_is_date(symptom_begin_dates)
  for(persons in number_of_persons)
  {
    smidm_is_natural_number(persons)
  }
  smidm_is_natural_number(max_incubation_days)
  smidm_is_parameter_log_mean(meanlog)
  smidm_is_parameter_log_sd(sdlog)
}

