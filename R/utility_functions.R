#-------------------------------------#
#   functions for negative analysis   #
#-------------------------------------#

## helper functions for a-priori
scale01 <- function(x, l, u) {
  (x - l) / (u - l)
}

## helper function for beta-binom a-priori
bb_p0 <- function(a, b, N) {
  extraDistr::dbbinom(0, N, a, b)
}
bb_p1 <- function(a, b, N) {
  1 - bb_p0(a, b, N)
}
bb_mu <- function(a, b, N) {
  N * a / (a + b)
}
bb_muc <- function(a, b, N) { # mean conditional on number > 0
  p0 <- bb_p0(a, b, N)
  (N * a / (a + b)) / (1 - p0)
}
bb_pi <- function(a, b, N) {
  scale01(bb_muc(a, b, N), 1, (N + 1) / 2)
}


## Methods for finding beta binom parameters
## optimizer for a-priori
rss <- function(x, N, p1, pi, w = c(1, 1)) {
  y <- c(bb_p1(x[1], x[2], N),
         bb_pi(x[1], x[2], N))
  crossprod(w * (y - c(p1, pi)))
}

# w: optimization weights
findparsBeta <- function(N, p1, pi, w = c(1,1)) {
  l <- 1; u <- (N + 1) / 2
  or <- optim(c(0.5, 2), rss, N = N, p1 = p1, pi = pi, w = w, method = "L-BFGS-B",
              lower = c(0.001, 1.000), upper = c(1, Inf))
  a <- or$par[1]; b <- or$par[2]
  muc <- l + pi * (u - l)
  ##cat("    Target | achieved\n")######
  ##cat("p1:   ", p1, "|", bb_p1(a, b, N), "\n")######
  ##cat("pi:   ", pi, "|", bb_pi(a, b, N), "\n")######
  ##cat("mu_c:", muc, "|", bb_muc(a, b, N), "\n")######
  return(list(a = a, b = b))
}

#' Generate data extended
#'
#' Creates a dataframe suitable as input for \code{\link{calculate_likelihood_negative_tests}}.
#'
#' @param M The size of the group without infected, default is twenty.
#' @param d A matrix with the test dates, default is matrix(data = 1, nrow = 1, ncol = 2).
#' @param S A vector with the sizes of the subgroups, default is c(12).
#' @return The dataframe.

generate_data_extended <- function(M = 20,          # complete group size
                                   d = matrix(data = 1, nrow = 1, ncol = 2),  # testDates
                                   S = c(12)   # subgroup sizes (subgroup defined by test timepoint )
) {
  # G <- nrow(d)   # number of subgroups (excluding untested, untested size is M-N)
  N <- sum(S)      # number of tested alltogether
  stopifnot(length(S) %in% c(1, nrow(d)))
  d <- data.frame(id = 1:M,
                  tested = c(rep(TRUE, N), rep(FALSE, M - N)),
                  result = c(rep(FALSE, N), rep(NA, M - N)),
                  testNumbers = c(rep(d[, 1], times = S), rep(NA, M - N)),
                  group = c(rep(1:length(S), times = S), rep(length(S) + 1, M - N)))
  return(d)
}

#' One more primary a priori probability
#'
#' Calculates the a priori probability y for one primary case more
#' by using the current prior distribution and the prior distribution of
#' one single primary case.
#'
#' @param yCurrent The current prior distribution.
#' @param y1 The prior distribution of one single primary case.
#' @return The a priori probability y.

p_onePrimaryMore <- function(yCurrent, y1) {
  k <- 0:(length(y1) - 1)
  y2 <- rep(0, length(y1))
  for(k1 in k) {
    p1 <- yCurrent[k1 + 1]
    for(k2 in k) {
      p2 <- y1[k2 + 1]
      for(kNew in max(0,k2 - k1) : min(k2, length(y1) - 1 - k1)) {
        pDraw <- dhyper(kNew, length(y1) - 1 - k1, k1, k2)
        y2[k1 + kNew + 1] <- y2[k1 + kNew + 1] + p1 * p2 * pDraw
      }
    }
  }
  return(y2)
}



#' Likelihood K
#'
#' Calculates the probability that zero positive tests are observed given K
#' of the group are infected.
#'
#' @param infected_group_size Number of infected Persons in the group.
#' @param information_data Matrix with columns person ID, tested (T/F), result(F/NA), testNumbers, groupNumber
#' @param test_infos Matrix with column number of test days and a column
#' for each test with the testday relative to event date, the rows are the groups.
#' @param test_types Matrix with test day (columns) of each group (rows) and whe informations about test types.
#' @param info Dataframe with the day specific information about sensitivity and specificity.
#' @param combination_infected Matrix of all possible combinations how K infected are distributed among subgroups.
#' @param number_group_peoples Vector with the number of people per group.
#' @param number_subgroups Number of subgroups including group of untested (if existent).
#' @return The probability.


calculate_likelihood_negative_tests_k <- function(infected_group_size,
                            information_data,
                            test_infos,
                            test_types,
                            info,
                            combination_infected,
                            number_group_peoples,
                            number_subgroups) {

  G <- max(information_data$group[information_data$tested]) # number of subgroups among tested with diff timepoints
  combination_infected <- combination_infected[which(combination_infected$S == infected_group_size), ] # only allow those combinations such all overall number of infected is == K
  ## ?extraDistr::dmvhyper # needed urn model prob dist: multivariate hypergeometric distribution
  ## prob that x1,...,xG people are infected in group 1,...,G given that K are infected overall:
  combination_infected$pmvhy <- extraDistr::dmvhyper(x = combination_infected[, 1:number_subgroups],
                                                     n = number_group_peoples,
                                                     k = infected_group_size)
  ## prep final calc:
  fnr <- c()
  # extension to groups with several test
  for (i in 1:G) {
    testDays <- test_infos[i, 2:ncol(test_infos)]
    testProbs <- c()
    for(j in 1:test_infos[i,1])
    {
      # if(test_types[i,j] == "PCR")
      # {
      #   testProbs[j] <- 1 - info$sePCR[info$t %in% testDays[j]]
      # }
      # if(test_types[i,j] == "Antigen")
      # {
      #   testProbs[j] <- 1 - info$seAntigen[info$t %in% testDays[j]]
      # }
      if(test_types[i,j] %in% colnames(info))
      {
        testProbs[j] <- 1 - dplyr::pull(info, test_types[i,j])[info$t %in% testDays[j]]
      }
    }
    fnr[i] <- prod(testProbs)
  }
  FNR <- matrix(fnr, nrow = nrow(combination_infected), ncol = G, byrow = TRUE) # as  matrix
  ## prob that 0 people have pos test result given that x1,...,xG (row of GR) people are infected in group 1,...,G:
  combination_infected$p0 <- exp(rowSums(log(FNR^(combination_infected[, 1:G])))) # prob that all tests are negative (product: test results are independent)
  return(sum(combination_infected$pmvhy * combination_infected$p0)) # add up all paths + weight by pmvhy
}
