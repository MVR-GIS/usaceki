#' @title Get QRI RDF Graph
#' @description Get QRI RDF Graph.
#' @returns An rdflib::rdf object.
#' @export
#' @importFrom rdflib rdf rdf_parse
get_qri_rdf_graph <- function() {
  turtle_file <- system.file("extdata", package = "usaceki", "qri-dm.ttl")
  qri_rdf_graph <- rdflib::rdf()
  qri_rdf_graph <- rdflib::rdf_parse(turtle_file, format = "turtle")
}
