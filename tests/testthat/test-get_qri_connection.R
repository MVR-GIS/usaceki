test_that("get qri_connection ask for credentials", {
  skip_if_not(
    interactive(),
    message = "Skipping test in non-interactive session"
  )
  qri_connection <- get_qri_connection(set_key = TRUE)
  expect_in(class(qri_connection), "OraConnection")
})

test_that("get qri_connection don't ask for credentials", {
  qri_connection <- get_qri_connection(set_key = FALSE)
  expect_in(class(qri_connection), "OraConnection")
})
