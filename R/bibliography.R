#' Generating a BibTex file using a query to a Zotero web library
#'
#' @param file_dir Optional, a BibTex file directory path (default: current)
#' @param library_name A group library name (appearing in groups in Zotero web)
#' @param collection_name Optional, a collection name (or vector of) belonging to the library
#' @param query Optional, a character string to use for querying bibliographic base
#' @param query_type Optional, a query type: "full" (the default) for searching
#' in all the fields, or "basic" for searching only in title, creator, year fields
#' @param verbose Optional, a logical used for displaying messages
#' from the query process (TRUE) or not (FALSE)
#' @param overwrite Optional, a logical for deleting existing file (TRUE),
#' or not (FALSE, default)
#'
#' @return An invisible BibEntry object or a list of
#'
#' @details The output file name(s) is/are either the library name (if not any collection are
#' given) or the collection name(s)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # No collection
#' gen_bibtex_file(library_name = "GroupLibraryName")
#'
#' # With a collection name
#' gen_bibtex_file(library_name = "GroupLibraryName", collection_name = "CollName")
#'
#' # With query, searching in all fields
#' gen_bibtex_file(library_name = "GroupLibraryName",
#' collection_name = "CollName", query = "1995" )
#'
#' # Getting returned BibEntry object
#' bibentry_obj <- gen_bibtex_file(library_name = "GroupLibraryName")
#'
#'}
#'
#'
gen_bibtex_file <- function( file_dir = getwd(),
                             library_name,
                             collection_name = NULL,
                             query = "",
                             query_type ="full",
                             verbose = FALSE,
                             overwrite = FALSE) {

  # loop over file_path content !
  if (length(collection_name) > 1) {
    BibEntry_objs <-list()
    for (i in 1:length(collection_name)){
      BibEntry_objs[[i]] <-
        gen_bibtex_file(file_dir = file_dir,
                        library_name = library_name,
                        collection_name = collection_name[i],
                        query = query,
                        query_type = query_type,
                        verbose = verbose,
                        overwrite = overwrite)
    }
    return(invisible(BibEntry_objs))
  }

  bib_src <- library_name

  if (! base::is.null(collection_name)) {
    bib_src <- collection_name
  }

  file_path <- file.path(file_dir, paste0(bib_src, ".bib"))

  if (verbose) {
    cat(sprintf("%s: %s\n","Getting references from collection", bib_src))
    #cat(paste0("generating BibTex file: ",file_path))
  }

  # Checking file, against overwrite arg.
  if ( file.exists(file_path) ) {
    if (overwrite) {
      unlink(x = file_path)
    } else {
      warning(file_path,
              ": file already exists",
              " consider to use overwrite = TRUE",
              " for overwriting it !")
      return(invisible())
    }
  }




  # Getting BibEntry of all references from the zotero base
  BibEntry_obj <- get_references(library_name = library_name,
                                 collection_name = collection_name,
                                 query = query,
                                 query_type = query_type,
                                 verbose = verbose)

  # Checking content, aborting if NULL
  if(base::is.null(BibEntry_obj)) stop("Not any references returned by the query !")

  # Writing the .bib file
  RefManageR::WriteBib( bib = BibEntry_obj,
                        file = file_path,
                        biblatex = FALSE,
                        verbose = TRUE)

  if (verbose) {
    cat(sprintf("%s %s %s: %s\n",
                "Writing",
                as.character(length(BibEntry_obj)),
                "references to file",
                file_path))
    #cat(paste0("generating BibTex file: ",file_path))
  }

  # Returning bib list as BibEntry object
  return(invisible(BibEntry_obj))
}



#' Generating or updating silently a BibTex file or a list of,
#' using a Zotero web library
#'
#' @param file_dir Optional, a BibTex file directory path (default: current)
#' @param library_name A group library name (appearing in groups in Zotero web)
#' @param collection_name Optional, a collection name (or vector of) belonging to the library
#' @param query Optional, a character string to use for querying bibliographic base
#' @param query_type Optional, a query type: "full" (the default) for searching
#' in all the fields, or "basic" for searching only in title, creator, year fields
#'
#' @return An invisible BibEntry object or a list of
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # For the default collection names vector
#' update_stics_bibtex_files()
#'
#' # For the hole library_name
#' update_stics_bibtex_files(collection_name = NULL)
#'
#'}
#'
#'
update_stics_bibtex_files <- function(file_dir = getwd(),
                                      library_name = "livre_rouge",
                                      collection_name = c("sticsredbook2020", "EPS_stics"),
                                      query = "",
                                      query_type ="full") {
  suppressWarnings(suppressMessages(
    gen_bibtex_file(file_dir = file_dir,
                    library_name = library_name,
                    collection_name = collection_name,
                    query = query,
                    query_type = query_type,
                    overwrite = TRUE,
                    verbose = TRUE)

  ))
}


gen_stics_json_files <- function(file_dir = getwd(),
                                 library_name = "livre_rouge",
                                 collection_name = c("sticsredbook2020", "EPS_stics")) {

  # test dir existence
  if (!dir.exists(file_dir)) stop("Directory does not exist: ",file_dir)

  # Getting group id
  group_id <- SticsPubRTools:::get_group_id(library_name)
  if (base::is.null(group_id)) stop("Unknown library name",library_name)

  # Getting API_KEY for querying the wb Zotero rest API
  collections_id <- SticsPubRTools:::get_collection_id(collection_name)
  # Getting names from existing ones
  collection_name <- names(collections_id)

  # Getting API key
  api_key <- SticsPubRTools:::get_access_key()
  curl_api_key_string <- paste0("\"Zotero-API-Key: ", api_key, "\"")

  # items URL
  files_list <- file.path(file_dir, paste0(collection_name, ".json") )
  collections_url <- paste0("https://api.zotero.org/groups/",group_id, "/collections/",
                            collections_id, "/items", " > ", files_list)

  # Setting curl query commands
  curl_commands <- paste("curl -H", curl_api_key_string, collections_url)


  return(unlist(lapply(curl_commands, system)))

}

#TODO: see if specific functions may be added
# i.e. : gen_livre_rouge_bibtex_file, gen_stics_bibtex_file !

# Get BibEntry for all references of a library,
# and optionally for a seficic collection of the library.
get_references <- function(library_name,
                           collection_name = NULL,
                           query = "",
                           query_type = "basic",
                           verbose = FALSE) {

  # Fixing options for just warning
  RefManageR::BibOptions(check.entries = "warn")

  # First query (will contain the merged queries resutls in the end)
  BibEntry_obj_full <- read_zotero_references( library_name = library_name,
                                               collection_name = collection_name,
                                               query = query,
                                               query_type = query_type)

  # if no references found
  if(base::is.null(BibEntry_obj_full)) return()

  start <- NULL
  #if(verbose) cat(paste0("start: ",start))

  # Starting a multiple query and requests merging
  n_ref <- length(BibEntry_obj_full)
  # if (verbose) cat(paste0("nb ref: ",n_ref))
  start <- n_ref + 1


  while (TRUE) {

    #if(verbose) cat(paste0("start: ",start))

    BibEntry_obj <- read_zotero_references( library_name = library_name,
                                            collection_name = collection_name,
                                            query = query,
                                            query_type = query_type,
                                            start = start)

    #print(BibEntry_obj)

    # Reference number got from the query
    n_ref <-  length(BibEntry_obj)

    #if (verbose) cat(paste0("nb ref: ",n_ref))

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




# Get BibEntry for a single query
# for getting a maximum number of 100 references (default)
# or a specific number and/or range in references ids from a library
# and optionally for a specific collection of the library.

# @param start A numeric indicating from which record number in the base to start the query
read_zotero_references <- function(library_name,
                                   collection_name = NULL,
                                   query = "",
                                   query_type = "basic",
                                   start = NULL,
                                   number = NULL) {

  #group_id , see get_groups for ids by group names list
  group_id <- get_group_id(group_name = library_name)

  if (base::is.null(group_id)) stop("Unknown library name",library_name)

  params <- get_query_params( query = query,
                              query_type = query_type,
                              start = start,
                              collection_name = collection_name,
                              limit = number)

  invisible(utils::capture.output(
    r <- RefManageR::ReadZotero(group = group_id, .params = params)
  ))

  #r <- RefManageR::ReadZotero(group = group_id, .params = params, delete.file = FALSE)



  return(invisible(r))
}

# Defining query parameters
get_query_params <- function( key = NULL,
                              query="",
                              query_type = "basic",
                              start = NULL,
                              collection_name = NULL,
                              item_type = NULL,
                              limit = NULL) {

  qmode <- get_query_mode(query_type = query_type)

  if (base::is.null(qmode)) stop("Unknown query mode (basic, full)")

  # Getting a default read access API key
  if (base::is.null(key)) key <- get_access_key()

  # Base params list
  params <- list( q = query, key = key, qmode = qmode)

  # Specific params
  if (!base::is.null(start)) params$start <- start

  if (!base::is.null(collection_name)) {
    params$collection <- get_collection_id(collection_name = collection_name)
  }

  if (!base::is.null(item_type)) params$itemType <- item_type

  if (!base::is.null(limit)) params$limit <- limit

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


get_collection_names <- function() {
  return(names(get_collections()))
}


get_collection_id <- function(collection_name = NULL) {

  collections <- get_collections()
  coll_names <- names(collections)

  if (is.null(collection_name)) collection_name <- coll_names

  exist_in_collections <- collection_name %in% coll_names

  if (!any(exist_in_collections)) return()

  if(!all(exist_in_collections)) warning("At least one collection does not exist: ",
                                         paste(collection_name[!exist_in_collections], collapse = " "))

  collection_name <- collection_name[exist_in_collections]

  collection_id <- unlist(lapply(collection_name, function(x) collections[[x]]))
  names(collection_id) <- collection_name

  return(collection_id)

}

get_collections <- function() {
  collections <- list()
  collections$sticsredbook2020 <- "5BN7SQTX"
  collections$EPS_stics <- "HWSYUI79"

  return(collections)
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

