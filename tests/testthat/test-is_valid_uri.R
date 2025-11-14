test_that("is valid URI", {
  expect_true(is_valid_uri("https://www.google.com"))

  expect_false(is_valid_uri("invalid-url-string"))
})
