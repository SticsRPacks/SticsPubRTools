#' Generating a BibTex file using a query to a Zotero web base
#'
#' @param file_path A BibTex file path (with a .bib extension)
#' @param library A group library name (appearing in groups in Zotero web)
#' @param query A character string to use for querying bibliographic base
#' @param query_type A query type: "full" (the default) for searching in all the fields,
#' or "basic" for searching only in title, creator, year fields
#' @param verbose A logical for displaying messages from the query process (TRUE) or not (FALSE)
#' @param overwrite A logical for deleting existing file (TRUE), or not (FALSE, default)
#'
#@return
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # No query
#' gen_bibtex_file("file.bib")
#'
#' # with query, searching in all fields
#' gen_bibtex_file(file = "file.bib", query = "1995" )
#'
#' # searching in basic fields (title, creator, year)
#' gen_bibtex_file(file = "file.bib", query = "1995", query_type = "basic" )
#'
#' # getting returned BibEntry object
#' bibentry_obj <- gen_bibtex_file("file.bib")
#'
#'}
#'
#'
gen_bibtex_file <- function( file_path = NULL ,
                             library = "livre_rouge",
                             query = "",
                             query_type ="full",
                             verbose = FALSE,
                             overwrite = FALSE) {

  # Checking file + .bib extension
  if (base::is.null(file_path)) stop("File path is missing !")

  if (!grepl(pattern = "\\.bib$", x = file_path)) stop("The file must have a .bib extension !")

  # Checking file, against overwrite arg.
  if ( file.exists(file_path) ) {
    if (overwrite) {
      unlink(x = file_path)
    } else {
      stop("Consider to use overwrite = TRUE, for overwriting the existing file !")
    }
  }


  # Getting BibEntry of all references from the zotero base
  BibEntry_obj <- get_references(library = library, query = query, query_type = query_type)

  # Checking content, aborting if NULL
  if(base::is.null(BibEntry_obj)) stop("Not any references returned by the query !")

  # Writing the .bib file
  RefManageR::WriteBib( bib = BibEntry_obj,
                        file = file_path,
                        biblatex = FALSE,
                        verbose = verbose)

  # Returning bib list as BibEntry object
  return(invisible(BibEntry_obj))
}


#TODO: see if specific functions may be added
# i.e. : gen_livre_rouge_bibtex_file, gen_stics_bibtex_file !


get_references <- function(library, query = "", query_type = "basic", verbose = FALSE) {

  # Fixing options for just warning
  RefManageR::BibOptions(check.entries = "warn")

  # First query (will contain the merged queries resutls in the end)
  BibEntry_obj_full <- read_zotero_references( library = library, query = query, query_type = query_type)

  # if no references found
  if(base::is.null(BibEntry_obj_full)) return()

  # Starting a multiple query and requests merging
  start <- length(BibEntry_obj_full) + 1

  while (TRUE) {

    if(verbose) print(start)

    BibEntry_obj <- read_zotero_references( library = library, query = query, query_type = query_type, start = start)

    # Reference number got from the query
    n_ref <-  length(BibEntry_obj)

    # No more references in the base from start id value
    if (!n_ref) break

    # Merging BibEntry objects
    BibEntry_obj_full <- merge(x = BibEntry_obj_full,
                               y = BibEntry_obj)

    # Next start id for the query
    start <- start + n_ref

  }

  # Returning a BibEntry object
  return(BibEntry_obj_full)
}



# @param start A numeric indicating from which record number in the base to start the query

read_zotero_references <- function(library, query = "", query_type = "basic", start = 1) {

  #group_id <- "2450934" # livre_rouge
  group_id <- get_group_id(group_name = library)

  if (base::is.null(group_id)) stop("Unknown library name",library)

  params <- get_query_params( query = query, query_type = query_type, start = start)

  r <- RefManageR::ReadZotero(group = group_id,
                              .params = params)
  return(r)
}


get_query_params <- function( key = NULL,
                              query="",
                              query_type = "basic",
                              start = 1,
                              collection = NULL,
                              item_type = NULL) {

  qmode <- get_query_mode(query_type = query_type)

  if (base::is.null(qmode)) stop("Unknown query mode (basic, full)")

  # Getting a default read access API key
  if (base::is.null(key)) key <- get_access_key(access = "write")

  # Base params list
  params <- list( q = query, key = key, qmode = qmode , start = start)

  # Specific params
  if (!base::is.null(collection)) params$collection <- collection # collection name !

  if (!base::is.null(item_type)) params$itemType <- item_type

  return(params)
}


get_group_id <- function(group_name = NULL) {

  groups <- get_groups()

  if (!group_name %in% names(groups)) return()

  return(groups[[group_name]])

}


get_groups <- function() {
  groups <- list()
  groups$livre_rouge <- "2450934"
  groups$equipe_projet_stics <- "857933"

  return(groups)
}

get_access_key <- function(access = "read") {

  # access API keys

  if (access == "read") return("3mSKS5qALTbb3ON4mPUfm22V")

  if (access=="write") return("aHvGXYt4WOluIE1guZ9pfeMF")

  return()

}


get_query_mode <- function(query_type) {

  qmodes <- get_query_modes()

  if (!query_type %in% names(qmodes)) return()

  return(qmodes[[query_type]])
}

get_query_modes <- function() {

  qmodes <- list()
  qmodes$basic <- "titleCreatorYear"
  qmodes$full <- "everything"

  return(qmodes)

}

