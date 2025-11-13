test_that("get column comments", {
  db_connection = con_roracle
  column_comments <- get_column_comments(db_connection)
  expect_true(is.data.frame(column_comments))
  expect_in(
    colnames(column_comments),
    c("TABLE_NAME", "COLUMN_NAME", "COMMENTS")
  )
})
