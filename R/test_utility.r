expect_exerr <- function(
  object,
  message = NULL
) {
  testthat::expect_error(object, regexp = paste0("^", message, "$"))
}
