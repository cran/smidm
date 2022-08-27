library(testthat)
library(smidm)
library(covr)

#package_coverage()

if (requireNamespace("xml2")) {
  resultfile <- Sys.getenv("TEST_REPORT_FILE", unset = "test-results.xml")
  test_check("smidm", reporter = MultiReporter$new(reporters = list(
    JunitReporter$new(file = resultfile),
    CheckReporter$new()
  )))
} else {
  test_check("smidm")
}
