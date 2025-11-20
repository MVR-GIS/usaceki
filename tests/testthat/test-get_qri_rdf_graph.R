test_that("get qri rdf graph", {
  qri_rdf_graph <- get_qri_rdf_graph()
  expect_in("rdf", class(qri_rdf_graph))
})
