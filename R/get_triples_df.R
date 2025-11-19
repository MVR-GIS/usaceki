#' @title Get Triples Data Frame
#' @description Convert a RDF Triplestore into a data.frame.
#' @param graph            rdf object; An rdflib rdf object.
#' @returns A data.frame of the RDF Triplestore.
#' @export
#' @importFrom rdflib rdf_query
get_triples_df <- function(graph) {
  # Define a SPARQL query to get all subject, predicate, object triples
  SPARQL_query <- "
  SELECT ?s ?p ?o
  WHERE {
    ?s ?p ?o .
  }"

  # Execute the query and get results as a data frame
  triples_df <- rdf_query(graph, SPARQL_query)
}
