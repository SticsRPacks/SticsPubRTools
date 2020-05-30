#' Generating a BibTex file using a query to a Zotero web base
#'
#' @param file A BibTex file path (with a .bib extension)
#' @param query A character string to use for querying bibliographic base
#' @param query_mode A query mode: "full" (the default) for searching in all the fields,
#' or "basic" for searching only in title, creator, year fields
#' @param start A numeric indicating from which record number in the base to start the query
#' @param verbose A logical for displayin messages from the query process (TRUE) or not (FALSE)
#'
#@return
#'
#' @export
#'
#' @examples
#' \dontrun{
#' generate_bibfile("file.bib")
#'
#' generate_bibfile(file = "file.bib", query = "1995" )
#'
#' generate_bibfile(file = "file.bib", query = "1995", query_mode = "basic" )
#'
#'}
#'
#'
generate_bibfile <- function( file = NULL ,
                              query = "",
                              query_mode ="full",
                              start = 1,
                              verbose = FALSE) {

  # file: with a .bib extension
  if (base::is.null(file)) stop("File path is missing !")

  if (!grepl(pattern = "\\.bib$", x = file)) stop("The file must have a .bib extension !")

  #print(start)
  BibEntry_obj_full <- get_zotero_references( query = query, query_mode = query_mode, start = start)
  start <- length(BibEntry_obj_full) + 1

  while (TRUE) {
    print(start)

    BibEntry_obj <- get_zotero_references( query = query, query_mode = query_mode, start = start)

    n_ref <-  length(BibEntry_obj)

    if (!n_ref) break

    # merging BibEntry objects
    BibEntry_obj_full <- merge(x = BibEntry_obj_full,
                               y = BibEntry_obj)

    start <- start + n_ref

  }

  RefManageR::WriteBib( bib = BibEntry_obj_full, file = file, biblatex = FALSE, verbose = verbose)

  #return(BibEntry_obj_full)
}


get_zotero_references <- function(query = "", query_mode = "basic", start = 1) {

  group_livre_rouge <- "2450934"

  params <- get_query_params( query = query, query_mode = query_mode, start = start)
  delete_file <- TRUE
  file <- get_temp_bib_file()

  r <- RefManageR::ReadZotero(group = group_livre_rouge, .params = params,
                              delete.file = delete_file,
                              temp.file = file)

  return(r)
}


get_access_key <- function(access = "read") {

  # access API keys

  if (access == "read") return("3mSKS5qALTbb3ON4mPUfm22V")

  if (access=="write") return("aHvGXYt4WOluIE1guZ9pfeMF")

  return()

}

get_collection_id <- function(coll_name = "livre_rouge_2020") {

  if (coll_name == "livre_rouge_2020") coll_id <- "QG2JDQ95"

  return(coll_id)

}

get_query_params <- function( key = NULL,
                              query="",
                              query_mode = "basic",
                              start = 1) {

  qmode <- "titleCreatorYear"
  if (query_mode == "full") qmode <- "everything"

  # getting collection id
  coll_id <- get_collection_id()

  # getting a default read access API key
  if (base::is.null(key)) key <- get_access_key(access = "write")

  #list( limit = limit, q = query, key = key, qmode = qmode , collection = coll_id, start = start)
  list( q = query, key = key, qmode = qmode , collection = coll_id, start = start)
}


get_temp_bib_file <- function() {
  tempfile(fileext = ".bib")
}
