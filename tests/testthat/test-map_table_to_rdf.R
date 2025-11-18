test_that("map_table_to_rdf", {
  db_connection = con_roracle
  tables <- get_tables(db_connection)
  table_info <- get_table_info(db_connection, tables)
  pk_info <- get_pk(db_connection)
  fk_info <- get_fk(db_connection)
  table_comments <- get_table_comments(db_connection)
  column_comments <- get_column_comments(db_connection)
  rdf_graph <- rdflib::rdf()
  namespace <- c(
    ex = "http://usace.gov/rf-dm#",
    owl = "http://www.w3.org/2002/07/owl#",
    rdfs = "http://www.w3.org/2000/01/rdf-schema#",
    rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    xsd = "http://www.w3.org/2001/XMLSchema#",
    skos = "http://www.w3.org/2004/02/skos/core#"
  )
  for (tbl in names(table_info)) {
    g <- map_table_to_rdf(
      rdf_graph,
      namespace,
      tbl,
      table_info[[tbl]],
      pk_info,
      table_comments,
      column_comments
    )
  }
  expect_in(class(g), "rdf")
})
