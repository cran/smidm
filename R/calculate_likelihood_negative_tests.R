#source("package/package_functions/support_functions.R")
#source("package/package_functions/lik_K.R")

#' Overall likelihood
#'
#' Calculates vector of probabilities that zero positive tests are observed
#' given different numbers of infected.
#'
#' @param test_infos Matrix with column number of test days and a column
#' for each test with the testday relative to event date, the rows are the groups.
#' @param test_types Matrix with test day (columns) of each group (rows) and whe informations about test types.
#' @param negative_persons Number of people without the infectious persons.
#' @param subgroup_size Array with the number of persons per test group.
#' @param info Dataframe, this is a placeholder
#' @return Vector of probabilities calculated.
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
#' calculate_likelihood_negative_tests(test_infos = test_infos,
#'                                     test_types = test_types,
#'                                     negative_persons = 23,
#'                                     subgroup_size = c(3, 5))
#'
#' @export


calculate_likelihood_negative_tests <- function(test_infos,
                          test_types,
                          negative_persons,
                          subgroup_size,
                          info) {


  val_calculate_lik(test_infos,
                    test_types,
                    negative_persons,
                    subgroup_size,
                    info)

  if(missing(info)){
    info <- get_test_sensitivities()
  }
  information_data <- generate_data_extended(negative_persons, test_infos, subgroup_size)
  number_subgroups <- max(information_data$group) # number of subgroups included untested (Gp in {G, G + 1})
  number_group_peoples <- sapply(1:number_subgroups, function(g) sum(information_data$group == g)) # how many people per group?
  ## create Matrix GR with possible combinations how K infected are distributed among subgroups
  combination_infected <- expand.grid(lapply(1:number_subgroups, function(g) 0:number_group_peoples[g]))
  names(combination_infected) <- paste0("gr", 1:number_subgroups)
  combination_infected$S <- rowSums(combination_infected)
  sapply(0:nrow(information_data), function(x) calculate_likelihood_negative_tests_k(x,
                                                               information_data = information_data,
                                                               test_infos = test_infos,
                                                               test_types=test_types,
                                                               info = info,
                                                               combination_infected = combination_infected,
                                                               number_group_peoples = number_group_peoples,
                                                               number_subgroups = number_subgroups))
}


val_calculate_lik <- function(test_infos,
                              test_types,
                              negative_persons,
                              subgroup_size,
                              info){

  smidm_is_double_matrix(test_infos)
  smidm_is_character_matrix(test_types)
  smidm_is_natural_number(negative_persons)
  smidm_is_positive_vector(subgroup_size)

}
