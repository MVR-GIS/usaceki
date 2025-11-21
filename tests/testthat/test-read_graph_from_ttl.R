test_that("read_graph_from_ttl() reads a TTL and returns an rdflib graph", {
  ttl <- '
  @prefix ex: <http://example.org/> .
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

  ex:Thing a rdf:Type .
  '

  tmp <- tempfile(fileext = ".ttl")
  writeLines(ttl, tmp)

  graph <- read_graph_from_ttl(tmp)
  expect_true(inherits(graph, "rdflib"))

  unlink(tmp)
})
