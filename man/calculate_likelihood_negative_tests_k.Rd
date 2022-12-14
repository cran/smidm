% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{calculate_likelihood_negative_tests_k}
\alias{calculate_likelihood_negative_tests_k}
\title{Likelihood K}
\usage{
calculate_likelihood_negative_tests_k(
  infected_group_size,
  information_data,
  test_infos,
  test_types,
  info,
  combination_infected,
  number_group_peoples,
  number_subgroups
)
}
\arguments{
\item{infected_group_size}{Number of infected Persons in the group.}

\item{information_data}{Matrix with columns person ID, tested (T/F), result(F/NA), testNumbers, groupNumber}

\item{test_infos}{Matrix with column number of test days and a column
for each test with the testday relative to event date, the rows are the groups.}

\item{test_types}{Matrix with test day (columns) of each group (rows) and whe informations about test types.}

\item{info}{Dataframe with the day specific information about sensitivity and specificity.}

\item{combination_infected}{Matrix of all possible combinations how K infected are distributed among subgroups.}

\item{number_group_peoples}{Vector with the number of people per group.}

\item{number_subgroups}{Number of subgroups including group of untested (if existent).}
}
\value{
The probability.
}
\description{
Calculates the probability that zero positive tests are observed given K
of the group are infected.
}
