test_that("build_dictionary() creates expected dictionary rows and detects PK / NOT NULL", {
  ttl <- '
  @prefix ex: <http://example.org/> .
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  @prefix owl: <http://www.w3.org/2002/07/owl#> .
  @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
  @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

  ex:Person a owl:Class ;
    rdfs:label "Person" .

  ex:Person/id a owl:DatatypeProperty , owl:FunctionalProperty ;
    rdfs:domain ex:Person ;
    rdfs:label "id" ;
    rdfs:range xsd:integer ;
    rdfs:comment "NOT NULL column", "Primary key for table Person" .

  ex:Person/name a owl:DatatypeProperty ;
    rdfs:domain ex:Person ;
    rdfs:label "name" ;
    rdfs:range xsd:string .
  '

  tmp <- tempfile(fileext = ".ttl")
  writeLines(ttl, tmp)
  graph <- read_graph_from_ttl(tmp)
  triples_df <- get_triples_df(graph)

  dict_df <- build_dictionary(triples_df)
  expect_s3_class(dict_df, "data.frame")

  # Expect two attribute rows for Person (id, name)
  expect_true(any(grepl("id", dict_df$column_label)))
  expect_true(any(grepl("name", dict_df$column_label)))

  # id should be marked primary key and non-nullable
  id_row <- dict_df[dict_df$column_label == "id", ]
  expect_equal(id_row$is_primary_key, "YES")
  expect_equal(id_row$is_nullable, "NO")

  # name should not be primary key
  name_row <- dict_df[dict_df$column_label == "name", ]
  expect_equal(name_row$is_primary_key, "NO")

  unlink(tmp)
})

test_that("build_dictionary() supports custom candidate URIs (parameterized URI selection)", {
  ttl <- '
  @prefix ex: <http://example.org/> .
  @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

  ex:Book a ex:MyClass ;
    ex:altLabel "Book" .

  ex:Book/ISBN a ex:CustomDatatypeProperty ;
    ex:domain ex:Book ;
    ex:altLabel "ISBN" ;
    ex:range xsd:string .
  '

  tmp <- tempfile(fileext = ".ttl")
  writeLines(ttl, tmp)
  graph <- read_graph_from_ttl(tmp)
  triples_df <- get_triples_df(graph)

  uris <- list(
    rdf_type = c("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
    owl_datatypeprop = c(
      "http://example.org/CustomDatatypeProperty",
      "http://www.w3.org/2002/07/owl#DatatypeProperty"
    ),
    rdfs_label = c(
      "http://example.org/altLabel",
      "http://www.w3.org/2000/01/rdf-schema#label"
    ),
    rdfs_domain = c(
      "http://example.org/domain",
      "http://www.w3.org/2000/01/rdf-schema#domain"
    ),
    rdfs_range = c("http://www.w3.org/2001/XMLSchema#string"),
    owl_functionalprop = c("http://www.w3.org/2002/07/owl#FunctionalProperty"),
    skos_definition = c("http://www.w3.org/2004/02/skos/core#definition"),
    rdfs_comment = c("http://www.w3.org/2000/01/rdf-schema#comment")
  )

  dict_df <- build_dictionary(triples_df, uris = uris)

  # The dict should detect the ISBN column using the custom predicate and label
  expect_true(any(grepl("ISBN", dict_df$column_label)))
  expect_true(any(dict_df$table_label == "Book"))

  unlink(tmp)
})

test_that("build_dictionary() errors if triples_df is missing required columns", {
  bad_df <- data.frame(a = 1:3)
  expect_error(
    build_dictionary(bad_df),
    regexp = "triples_df must contain columns"
  )
})
