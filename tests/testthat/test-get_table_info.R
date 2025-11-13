test_that("get table info", {
  db_connection = con_roracle
  table_vector <- get_tables(db_connection)
  table_info <- get_table_info(db_connection, table_vector)
  expect_true(is.list(table_info))
  expect_true(is.data.frame(table_info[[1]]))
  expect_in(
    colnames(table_info[[1]]),
    c("COLUMN_NAME", "DATA_TYPE", "NULLABLE")
  )
})
