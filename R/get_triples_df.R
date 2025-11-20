#' @title Get Triples Data Frame
#' @description Convert a RDF Triplestore into a data.frame.
#' @param graph            rdf object; An rdflib rdf object.
#' @returns A data.frame of the RDF Triplestore.
#' @export
#' @importFrom rdflib rdf_query
#' @importFrom dplyr rename
get_triples_df <- function(graph) {
  # Define a SPARQL query to get all subject, predicate, object triples
  SPARQL_query <- "
    SELECT ?subject ?predicate ?object
    WHERE {
      ?subject ?predicate ?object
    }"

  # Execute the query and get results as a data frame
  df <- rdflib::rdf_query(graph, SPARQL_query)

  # Rename fields for interpretability and sort
  triples_df <- df %>%
    rename(from = subject) %>%
    rename(to = object) %>%
    rename(relation = predicate)
}
