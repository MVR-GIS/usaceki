test_that("get all tables", {
  db_connection = con_roracle
  exclude_pattern = "."
  table_vector <- get_tables(db_connection, exclude_pattern)
})
