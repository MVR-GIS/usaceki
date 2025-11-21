#' @title Read Graph From Turtle
#' @description read_graph_from_ttl(): parse ttl -> rdflib graph
#' @param ttl_path Path to the Turtle file
#' @returns An rdflib rdf graph
#' @export
#' @importFrom rdflib rdf_parse
read_graph_from_ttl <- function(ttl_path) {
  if (!file.exists(ttl_path)) {
    stop("Turtle file not found: ", ttl_path)
  }
  rdflib::rdf_parse(ttl_path)
}
