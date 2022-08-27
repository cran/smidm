#' Generate info
#'
#' Creates a dataframe with day specific test sensitivity and specificity of
#' PCR and Antigen tests.
#'
#' @param df Dataframe, this is a placeholder
#'
#' @return The dataframe.
#'
#' @examples
#' get_test_sensitivities()
#' df <- data.frame(
#'              "PCR" = c(0, 0, 0, 0.04, 0.34, 0.64, 0.76, 0.79, 0.80, 0.79,
#'                       0.77, 0.74, 0.71, 0.67, 0.62, 0.58, 0.54, 0.49, 0.44,
#'                       0.40, 0.37, 0.33),
#'              "Antigen" = c(0, 0, 0, 0.03, 0.13, 0.40,  0.64, 0.69, 0.70, 0.69,
#'                           0.62, 0.52, 0.40, 0.29, 0.21, 0.17, 0.13, 0.11,
#'                           0.08, 0.07, 0.05, 0.04)
#'                 )
#' get_test_sensitivities(df)
#' @export

get_test_sensitivities <- function(df) {

  if(missing(df)){
    seAntigen <- c(0, 0, 0, 0.03, 0.15, 0.45,  0.73, 0.79, 0.8, 0.79, 0.71, 0.59,
                   0.46, 0.33, 0.24, 0.19, 0.15, 0.12, 0.095, 0.075, 0.055, 0.04)
    sePCR <- 1 - c(1, 1, 1, 0.96, 0.66, 0.36, 0.24, 0.21, 0.20, 0.21, 0.23, 0.26,
                  0.29, 0.33, 0.375, 0.42, 0.465, 0.51, 0.56, 0.60, 0.63, 0.67)
    scaleFactor <- sum(sePCR[7:13])/sum(seAntigen[7:13])*0.8
    seAntigen <- seAntigen*scaleFactor
    return(data.frame(t = 0:21,
                      PCR = sePCR,
                      Antigen = seAntigen))
  }
  else{
    val_generate_info(df)
    res <- data.frame(t = 0:(nrow(df) - 1))
    res <- cbind(res, df)
    return(res)
  }
}


val_generate_info <- function(df){

  smidm_is_dataframe(df)

}
