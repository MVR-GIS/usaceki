test_that("check uri", {
  ns <- c(
    ex = "http://usace.gov/rf-dm#",
    owl = "http://www.w3.org/2002/07/owl#",
    rdfs = "http://www.w3.org/2000/01/rdf-schema#",
    rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    xsd = "http://www.w3.org/2001/XMLSchema#",
    skos = "http://www.w3.org/2004/02/skos/core#"
  )
  uri1 <- uri(ns, "owl", "Class")
  uri2 <- uri(ns, "rdf", "type")
  expect_true(is_valid_uri(uri1))
  expect_true(is_valid_uri(uri2))
})
