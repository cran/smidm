#source("package/package_functions/support_functions.R")

#' A priori probability of further Infections
#'
#' Calculates the a priori probability of how many people are infected in one event.
#'
#' The probability is beta-binomial distributed. The values for p1 and infection_average for
#' the events "school" and "day_care_center" are from Schoeps et al. (2021).
#'
#' @param negative_persons Number of people without the infectious persons.
#' @param infected_persons Number of infected people.
#' @param event Characters, event type given as characters, currently: "school" or "day_care_center".
#' @param p_one Number, this is a placeholder
#' @param infect_average Number, this is a placeholder
#' @return The a priori probability y.
#'
#' @references Schoeps A et al. (2021) "Surveillance of SARS-CoV-2 transmission in educational institutions,
#' August to December 2020, Germany". \emph{Epidemiology and Infection} 149, E213: 1-9.
#'
#' @examples
#' calculate_prior_infections(negative_persons = 23,
#'                            infected_persons = 2,
#'                            event = "school")
#'
#' @export
#' @import extraDistr


calculate_prior_infections <- function(negative_persons,
                                   infected_persons,
                                   event,
                                   p_one = NULL,
                                   infect_average = NULL){

  val_get_apriori_probability(negative_persons,
                              infected_persons,
                              event,
                              p_one,
                              infect_average)

  if (event == "school") {
    p1 <- 0.12
    infection_average <- 1.77  # = E[K|K>0]
    pi <- scale01(infection_average, 1, (negative_persons + 1) / 2)
  }
  else if (event == "day_care_center")
  {
    p1 <- 0.3
    infection_average <- 3.3  # = E[K|K>0]
    pi <- scale01(infection_average, 1, (negative_persons + 1) / 2)
  }
  else
  {
    p1 <- p_one
    infection_average <- infect_average
    pi <- scale01(infection_average, 1, (negative_persons + 1) / 2)
  }

  params <- findparsBeta(negative_persons, p1 = p1, pi = pi, w = c(1, 1))
  y <- extraDistr::dbbinom(0:negative_persons, negative_persons, alpha = params$a, beta = params$b)

  # Adapt for more than one primary case
  if (infected_persons > 1) {
    y1 <- y
    for (j in (2:infected_persons)) {
      yNew <- p_onePrimaryMore(y, y1)
      y <- yNew
    }
  }
  return(y)
}


val_get_apriori_probability <- function(negative_persons,
                                        infected_persons,
                                        event,
                                        p_one,
                                        infect_average){

  smidm_is_larger_one(negative_persons)
  smidm_is_natural_number(infected_persons)
  smidm_is_character(event)
  smidm_is_group(event, negative_persons+infected_persons)



}
