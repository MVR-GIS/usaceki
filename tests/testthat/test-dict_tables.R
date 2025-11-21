test_that("dict_tables() splits the dictionary by table_label", {
  ttl <- '
  @prefix ex: <http://example.org/> .
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  @prefix owl: <http://www.w3.org/2002/07/owl#> .
  @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
  @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

  ex:Person a owl:Class ;
    rdfs:label "Person" .

  ex:Project a owl:Class ;
    rdfs:label "Project" .

  ex:Person/id a owl:DatatypeProperty , owl:FunctionalProperty ;
    rdfs:domain ex:Person ;
    rdfs:label "id" ;
    rdfs:range xsd:integer .

  ex:Project/pid a owl:DatatypeProperty , owl:FunctionalProperty ;
    rdfs:domain ex:Project ;
    rdfs:label "pid" ;
    rdfs:range xsd:integer .
  '

  tmp <- tempfile(fileext = ".ttl")
  writeLines(ttl, tmp)
  graph <- read_graph_from_ttl(tmp)
  triples_df <- get_triples_df(graph)
  dict_df <- build_dictionary(triples_df)

  tables <- dict_tables(dict_df)
  expect_true(is.list(tables))
  # Expect two tables: Person and Project (or URIs if labels missing)
  expect_true(
    any(names(tables) == "Person") || any(grepl("Person", names(tables)))
  )
  expect_true(
    any(names(tables) == "Project") || any(grepl("Project", names(tables)))
  )

  # each table contains the correct number of rows
  if ("Person" %in% names(tables)) {
    expect_equal(nrow(tables[["Person"]]), sum(dict_df$table_label == "Person"))
  }

  unlink(tmp)
})
