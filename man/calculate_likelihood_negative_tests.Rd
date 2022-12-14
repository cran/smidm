% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_likelihood_negative_tests.R
\name{calculate_likelihood_negative_tests}
\alias{calculate_likelihood_negative_tests}
\title{Overall likelihood}
\usage{
calculate_likelihood_negative_tests(
  test_infos,
  test_types,
  negative_persons,
  subgroup_size,
  info
)
}
\arguments{
\item{test_infos}{Matrix with column number of test days and a column
for each test with the testday relative to event date, the rows are the groups.}

\item{test_types}{Matrix with test day (columns) of each group (rows) and whe informations about test types.}

\item{negative_persons}{Number of people without the infectious persons.}

\item{subgroup_size}{Array with the number of persons per test group.}

\item{info}{Dataframe, this is a placeholder}
}
\value{
Vector of probabilities calculated.
}
\description{
Calculates vector of probabilities that zero positive tests are observed
given different numbers of infected.
}
\examples{
test_infos <- matrix(nrow = 2, ncol = 3)
test_infos[1,] <- c(1, 2, NA)
test_infos[2,] <- c(2, 4, 6)

test_types <- matrix(nrow = 2, ncol = 2)
test_types[1,] <- c("PCR", NA)
test_types[2,] <- c("PCR", "Antigen")

calculate_likelihood_negative_tests(test_infos = test_infos,
                                    test_types = test_types,
                                    negative_persons = 23,
                                    subgroup_size = c(3, 5))

}
