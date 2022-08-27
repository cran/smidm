#'@importFrom methods is

smidm_is_natural_number  <- function(x, tol = .Machine$double.eps^0.5) {
  if (!((abs(Im(x)) < tol) &&
    (abs(Re(x)) > tol) &&
    isTRUE(all.equal(x, round(x),
                     tolerance = tol,
                     check.attributes = FALSE,
                     check.names = FALSE)) && x > 0)) {

    stop("The input must be a natural number.", call. = FALSE)
  }
}

smidm_is_double_matrix  <- function(x) {
  if (!is.numeric(x)) {
    stop("The input must be a numeric matrix.", call. = FALSE)
  }
}

smidm_is_character_matrix  <- function(x) {
  if (!is.character(x)) {
    stop("The input must be a character matrix.", call. = FALSE)
  }
}

smidm_is_parameter_gamma  <- function(x) {
  if (x < 0 || !is.numeric(x)) {
    stop("The input must be a positive number for the gamma distribution.", call. = FALSE)
  }
}

smidm_is_parameter_log_mean  <- function(x) {
  if (!is.numeric(x)) {
    stop("The input must be a number for the mean parameter of the log distribution.", call. = FALSE)
  }
}

smidm_is_parameter_log_sd  <- function(x) {
  if (x < 0 || !is.numeric(x)) {
    stop("The input must be a positive number for the standard deviation parameter of the log distribution.", call. = FALSE)
  }
}

smidm_is_positive_vector  <- function(x) {
  if (length(x[x >= 0]) != length(x) || !is.numeric(x)) {
    stop("The input must be a positive vector with numbers.", call. = FALSE)
  }
}

smidm_is_character  <- function(x) {
  if (!is.character(x)) {
    stop("The input must be of class character.", call. = FALSE)
  }
}

smidm_is_dataframe  <- function(x) {
  if (!is.data.frame(x)) {
    stop("The input must be of class dataframe.", call. = FALSE)
  }
}


smidm_is_date  <- function(x) {
  if (!is(x, "Date")) {
    stop("The input must be of class Date.", call. = FALSE)
  }
}

smidm_is_double <- function(x) {
  if (x < 0 || !is.numeric(x)) {
    stop("The input must be a positive number.", call. = FALSE)
  }
}

smidm_is_larger_one <- function(x) {
  if (x != round(x) || x < 2) {
    stop("The negative_persons must be an integer larger than one.", call. = FALSE)
  }
}

smidm_is_group <- function(x,y) {
  if (x == "school" && (y < 13 || y > 35)){
    stop("The total group size of a school class must be between 13 and 35.", call. = FALSE)
  }
  else if (x == "day_care_center" && (y < 8 || y > 25)) {
    stop("The total group size of a daycare group must be between 8 and 25.", call. = FALSE)
  }
}
