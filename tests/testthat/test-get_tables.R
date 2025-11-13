test_that("get all tables", {
  db_connection = con_roracle
  table_vector <- get_tables(db_connection)
  expect_contains(table_vector, c("PROJECT", "QUESTION"))
})

test_that("get some tables", {
  db_connection = con_roracle
  exclude_pattern = "^(EDW)"
  table_vector <- get_tables(db_connection, exclude_pattern)
  expect_equal(length(table_vector), 29)
})
