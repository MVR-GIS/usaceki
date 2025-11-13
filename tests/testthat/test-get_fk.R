test_that("get fks", {
  db_connection = con_roracle
  fk_info <- get_fk(db_connection)
  expect_true(is.data.frame(fk_info))
  expect_in(
    colnames(fk_info),
    c("CHILD_TABLE", "FK_COLUMN", "PARENT_TABLE")
  )
})
