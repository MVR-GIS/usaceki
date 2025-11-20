test_that("get triples df from turtle", {
  qri_rdf_graph <- get_qri_rdf_graph()
  qri_df <- get_triples_df(qri_rdf_graph)
  expect_in("data.frame", class(qri_df))
})
