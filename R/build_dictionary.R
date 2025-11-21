#' Build a tidy data dictionary from RDF triples
#'
#' Returns a tibble with columns:
#'  table_uri, table_label, column_uri, column_label, data_type, is_primary_key, is_nullable, description
#'
#' The `uris` argument accepts a named list with elements for each RDF role.
#' Each element may be a single URI string or a character vector of candidate URIs.
#' The function will automatically choose the first candidate that appears in the
#' triples data.frame (checking relation, subject, and object columns).
#'
#' @param triples_df data.frame with columns: from, relation, to
#' @param uris named list of candidate URIs for semantic roles (see details)
#' @return tibble (dict_df)
#' @export
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows arrange
build_dictionary <- function(
  triples_df,
  uris = list(
    rdf_type = c("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
    owl_class = c("http://www.w3.org/2002/07/owl#Class"),
    owl_datatypeprop = c("http://www.w3.org/2002/07/owl#DatatypeProperty"),
    owl_functionalprop = c("http://www.w3.org/2002/07/owl#FunctionalProperty"),
    rdfs_label = c("http://www.w3.org/2000/01/rdf-schema#label"),
    rdfs_domain = c("http://www.w3.org/2000/01/rdf-schema#domain"),
    rdfs_range = c("http://www.w3.org/2000/01/rdf-schema#range"),
    rdfs_comment = c("http://www.w3.org/2000/01/rdf-schema#comment"),
    skos_definition = c("http://www.w3.org/2004/02/skos/core#definition")
  )
) {
  # validate triples_df
  if (!all(c("from", "relation", "to") %in% names(triples_df))) {
    stop("triples_df must contain columns: from, relation, to")
  }

  # Normalize uris to character vectors
  uris <- lapply(uris, as.character)

  # Helper: choose the first candidate uri that actually appears in triples_df
  choose_uri <- function(candidates, triples_df) {
    for (cand in candidates) {
      if (
        any(triples_df$relation == cand, na.rm = TRUE) ||
          any(triples_df$to == cand, na.rm = TRUE) ||
          any(triples_df$from == cand, na.rm = TRUE)
      ) {
        return(cand)
      }
    }
    if (length(candidates) > 0) {
      return(candidates[[1]])
    }
    return(NA_character_)
  }

  # Select active URIs from candidates
  rdf_type <- choose_uri(uris$rdf_type, triples_df)
  owl_class <- choose_uri(uris$owl_class, triples_df)
  owl_datatypeprop <- choose_uri(uris$owl_datatypeprop, triples_df)
  owl_functionalprop <- choose_uri(uris$owl_functionalprop, triples_df)
  rdfs_label <- choose_uri(uris$rdfs_label, triples_df)
  rdfs_domain <- choose_uri(uris$rdfs_domain, triples_df)
  rdfs_range <- choose_uri(uris$rdfs_range, triples_df)
  rdfs_comment <- choose_uri(uris$rdfs_comment, triples_df)
  skos_definition <- choose_uri(uris$skos_definition, triples_df)

  # helper to fetch object values for a subject/predicate
  get_obj <- function(subject_uri, predicate_uri) {
    v <- triples_df[
      triples_df$from == subject_uri & triples_df$relation == predicate_uri,
      "to"
    ]
    if (length(v) == 0) {
      return(NA_character_)
    }
    paste0(v, collapse = " | ")
  }

  # ensure we have rdf_type and owl_datatypeprop identified
  if (is.na(rdf_type) || is.na(owl_datatypeprop)) {
    warning(
      "Could not determine rdf:type or owl:DatatypeProperty URIs from candidates; returning empty dictionary."
    )
    return(tibble::tibble())
  }

  datatype_props <- unique(triples_df[
    triples_df$relation == rdf_type & triples_df$to == owl_datatypeprop,
    "from"
  ])

  rows <- list()
  for (prop in datatype_props) {
    domain <- get_obj(prop, rdfs_domain)
    if (is.na(domain)) {
      next
    }
    domain_uris <- strsplit(domain, " \\| ")[[1]]
    for (dom in domain_uris) {
      table_label <- get_obj(dom, rdfs_label)
      column_label <- get_obj(prop, rdfs_label)
      datatype <- get_obj(prop, rdfs_range)
      comments <- get_obj(prop, rdfs_comment)
      skos_def <- get_obj(prop, skos_definition)
      description <- NA_character_
      if (!is.na(comments) && comments != "") {
        description <- comments
      }
      if (!is.na(skos_def) && skos_def != "") {
        description <- ifelse(
          is.na(description) || description == "",
          skos_def,
          paste(description, skos_def, sep = " | ")
        )
      }
      is_pk <- FALSE
      if (!is.na(owl_functionalprop)) {
        is_pk <- any(
          triples_df$from == prop &
            triples_df$relation == rdf_type &
            triples_df$to == owl_functionalprop
        )
      }
      not_null_flag <- ifelse(
        str_detect(tolower(ifelse(is.na(comments), "", comments)), "not null"),
        TRUE,
        FALSE
      )
      nullable <- ifelse(is_pk | not_null_flag, "NO", "YES")
      rows[[length(rows) + 1]] <- tibble::tibble(
        table_uri = dom,
        table_label = ifelse(
          is.na(table_label) || table_label == "",
          dom,
          table_label
        ),
        column_uri = prop,
        column_label = ifelse(
          is.na(column_label) || column_label == "",
          sub(".*/", "", prop),
          column_label
        ),
        data_type = ifelse(
          is.na(datatype) || datatype == "",
          NA_character_,
          sub(".*/", "", datatype)
        ),
        is_primary_key = ifelse(is_pk, "YES", "NO"),
        is_nullable = nullable,
        description = ifelse(
          is.na(description) || description == "",
          NA_character_,
          description
        )
      )
    }
  }

  if (length(rows) == 0) {
    return(tibble::tibble())
  }

  dict_df <- dplyr::bind_rows(rows) %>%
    dplyr::arrange(table_label, column_label)
  dict_df
}
