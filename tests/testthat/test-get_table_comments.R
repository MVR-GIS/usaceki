test_that("get table comments", {
  db_connection = con_roracle
  table_comments <- get_table_comments(db_connection)
  expect_true(is.data.frame(table_comments))
  expect_in(
    colnames(table_comments),
    c("TABLE_NAME", "COMMENTS")
  )
})
