#' @title Map Foreign Keys to RDF
#' @description Map Foreign Keys to RDF.
#' @param graph            rdf object; An rdflib rdf object.
#' @param namespace        named character vector; A "namespace" or RDF
#'                         vocabulary is a collection of namespace
#'                         Internationalized Resource Identifiers (IRI)
#'                         used to construct URIs. Must be specified as a
#'                         named vector of the form:
#'                         namespace prefix = namespace IRI
#' @param fk_df            data.frame; A df returned by the get_pk function.
#' @returns An rdflib::rdf graph object.
#' @export
#' @importFrom rdflib rdf_add
map_fks_to_rdf <- function(graph, namespace, fk_df) {
  for (i in seq_len(nrow(fk_df))) {
    fk <- fk_df[i, ]
    prop_uri <- paste0(
      namespace["ex"],
      fk$CHILD_TABLE,
      "/",
      fk$FK_COLUMN,
      "_ref"
    )
    domain_uri <- paste0(namespace["ex"], fk$CHILD_TABLE)
    range_uri <- paste0(namespace["ex"], fk$PARENT_TABLE)

    rdf_add(
      graph,
      prop_uri,
      paste0(namespace["rdf"], "type"),
      paste0(namespace["owl"], "ObjectProperty")
    )
    rdf_add(graph, prop_uri, paste0(namespace["rdfs"], "domain"), domain_uri)
    rdf_add(graph, prop_uri, paste0(namespace["rdfs"], "range"), range_uri)
    rdf_add(
      graph,
      prop_uri,
      paste0(namespace["rdfs"], "label"),
      paste0(fk$CHILD_TABLE, ".", fk$FK_COLUMN, " → ", fk$PARENT_TABLE),
      objectType = "literal"
    )
  }
  graph
}
