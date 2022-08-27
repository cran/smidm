#' Negative analysis probability
#'
#' Calculates the probability that nobody is infected given the negative tests.
#'
#' The probability is based on Bayes' theorem.
#'
#' @param negative_persons Number of people without the infectious persons.
#' @param infected_persons Number of infectious persons.
#' @param event Characters, the name of the event, currently: "school" or "day_care_center".
#' @param test_infos Matrix with testing information; each row gives the number of
#' tests (1. column) and each test date (following columns) for each test group
#' @param test_types Matrix with test day (columns) of each group (rows) and the informations about test types.
#' @param subgroup_size Array with the number of persons per test group.
#' @param distribution Vector, this is a placeholder
#' @param info Dataframe, this is a placeholder
#' @return The probability p.
#' @seealso \code{\link{calculate_prior_infections}},
#'     \code{\link{generate_data_extended}}, \code{\link{get_test_sensitivities}}
#'     and \code{\link{calculate_likelihood_negative_tests}}.
#'
#' @examples
#' test_infos <- matrix(nrow = 2, ncol = 3)
#' test_infos[1,] <- c(1, 2, NA)
#' test_infos[2,] <- c(2, 4, 6)
#'
#' test_types <- matrix(nrow = 2, ncol = 2)
#' test_types[1,] <- c("PCR", NA)
#' test_types[2,] <- c("PCR", "Antigen")
#'
#' calculate_posterior_no_infections(negative_persons = 23,
#'                                   infected_persons = 2,
#'                                   event = "school",
#'                                   test_infos = test_infos,
#'                                   test_types = test_types,
#'                                   subgroup_size = c(3, 5))
#'
#' @export


calculate_posterior_no_infections <- function(negative_persons,
                                                    infected_persons,
                                                    event,
                                                    test_infos,
                                                    test_types,
                                                    subgroup_size,
                                                    distribution = NULL,
                                                    info) {

  val_calculate_probability_negative_analysis(negative_persons,
                                              infected_persons,
                                              event,
                                              test_infos,
                                              test_types,
                                              subgroup_size,
                                              distribution,
                                              info)


  if(event == "school"||event == "day_care_center")
  {
    aPriori <- calculate_prior_infections(negative_persons, infected_persons, event)
  }
  else
  {
    aPriori <- distribution
  }

  # compute likelihood
  if(missing(info))
  {
  negativeProbabilities <- calculate_likelihood_negative_tests(test_infos,
                                         test_types,
                                         negative_persons,
                                         subgroup_size)
  }else
  {
  negativeProbabilities <- calculate_likelihood_negative_tests(test_infos,
                                         test_types,
                                         negative_persons,
                                         subgroup_size,
                                         info)
  }
  p <- aPriori[1] / sum(negativeProbabilities * aPriori)
  return(p)
}

val_calculate_probability_negative_analysis <- function(negative_persons,
                                                        infected_persons,
                                                        event,
                                                        test_infos,
                                                        test_types,
                                                        subgroup_size,
                                                        distribution,
                                                        info){

  smidm_is_natural_number(negative_persons)
  smidm_is_natural_number(infected_persons)
  smidm_is_character(event)
  smidm_is_double_matrix(test_infos)
  smidm_is_character_matrix(test_types)
  smidm_is_positive_vector(subgroup_size)
  ##smidm_is_natural_number(distribution)

}
