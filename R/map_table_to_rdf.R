#' @title Map Table to RDF
#' @description Adds RDF triples to an existing RDF triplestore of table
#'   definitions.
#' @param graph            rdf object; An rdflib rdf object.
#' @param namespace        named character vector; A "namespace" or RDF
#'                         vocabulary is a collection of namespace
#'                         Internationalized Resource Identifiers (IRI)
#'                         used to construct URIs. Must be specified as a
#'                         named vector of the form:
#'                         namespace prefix = namespace IRI
#' @param table_name       character; A named character vector of table names.
#' @param cols_df          data.frame; A df of column characteristics returned
#'                         by the get_table_info function.
#' @param pk_df            data.frame; A df returned from the get_pk function.
#' @param tab_comments_df  data.frame; A df returned by the get_table_comments
#'                         function.
#' @param col_comments_df  data.frame; A df returned from the get_column_comments
#'                         function.
#' @returns An rdflib::rdf graph object.
#' @export
#' @importFrom rdflib rdf_add
map_table_to_rdf <- function(
  graph,
  namespace,
  table_name,
  cols_df,
  pk_df,
  tab_comments_df,
  col_comments_df
) {
  class_uri <- paste0(namespace["ex"], table_name)
  rdf_add(
    graph,
    class_uri,
    paste0(namespace["rdf"], "type"),
    paste0(namespace["owl"], "Class")
  )
  rdf_add(
    graph,
    class_uri,
    paste0(namespace["rdfs"], "label"),
    table_name,
    objectType = "literal"
  )

  # Attach table comment if present
  t_comment <- tab_comments_df$COMMENTS[
    tab_comments_df$TABLE_NAME == table_name
  ]
  if (length(t_comment) > 0 && nzchar(t_comment)) {
    rdf_add(
      graph,
      class_uri,
      paste0(namespace["rdfs"], "comment"),
      t_comment,
      objectType = "literal"
    )
  }

  # Primary key columns for this table
  table_pks <- pk_df$COLUMN_NAME[pk_df$TABLE_NAME == table_name]

  # Iterate columns
  for (i in seq_len(nrow(cols_df))) {
    col <- cols_df$COLUMN_NAME[i]
    col_type <- toupper(cols_df$DATA_TYPE[i])
    nullable <- cols_df$NULLABLE[i]
    prop_uri <- paste0(namespace["ex"], table_name, "/", col)

    rdf_add(
      graph,
      prop_uri,
      paste0(namespace["rdf"], "type"),
      paste0(namespace["owl"], "DatatypeProperty")
    )
    rdf_add(graph, prop_uri, paste0(namespace["rdfs"], "domain"), class_uri)

    range_uri <- if (grepl("CHAR|CLOB|VARCHAR|NVARCHAR", col_type)) {
      paste0(namespace["xsd"], "string")
    } else if (grepl("NUMBER|DECIMAL|INT", col_type)) {
      paste0(namespace["xsd"], "decimal")
    } else if (grepl("DATE|TIMESTAMP", col_type)) {
      paste0(namespace["xsd"], "dateTime")
    } else {
      paste0(namespace["xsd"], "string")
    }

    rdf_add(graph, prop_uri, paste0(namespace["rdfs"], "range"), range_uri)
    rdf_add(
      graph,
      prop_uri,
      paste0(namespace["rdfs"], "label"),
      col,
      objectType = "literal"
    )

    # Add column comment (prefer skos:definition for semantics)
    c_comment <- col_comments_df$COMMENTS[
      col_comments_df$TABLE_NAME == table_name &
        col_comments_df$COLUMN_NAME == col
    ]
    if (length(c_comment) > 0 && nzchar(c_comment)) {
      rdf_add(
        graph,
        prop_uri,
        paste0(namespace["skos"], "definition"),
        c_comment,
        objectType = "literal"
      )
    }

    # Mark primary key columns
    if (col %in% table_pks) {
      rdf_add(
        graph,
        prop_uri,
        paste0(namespace["rdf"], "type"),
        paste0(namespace["owl"], "FunctionalProperty")
      )
      rdf_add(
        graph,
        prop_uri,
        paste0(namespace["rdfs"], "comment"),
        paste0("Primary key for table ", table_name),
        objectType = "literal"
      )
    }

    # Annotate NOT NULL
    if (nullable == "N") {
      rdf_add(
        graph,
        prop_uri,
        paste0(namespace["rdfs"], "comment"),
        "NOT NULL column",
        objectType = "literal"
      )
    }
  }

  graph
}
