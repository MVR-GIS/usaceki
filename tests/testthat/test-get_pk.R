test_that("get pks", {
  db_connection = con_roracle
  pk_info <- get_pk(db_connection)
  expect_true(is.data.frame(pk_info))
  expect_in(
    colnames(pk_info),
    c("TABLE_NAME", "COLUMN_NAME")
  )
})
