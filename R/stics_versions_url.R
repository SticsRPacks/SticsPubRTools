check_version_params <- function(params) {

  if (!length(params)) invisible()

  versions_url <- get_stics_versions_url()
  versions <- names(versions_url)
  urls <- unlist(versions_url, use.names = FALSE)
  params_names <- names(params)

  version_in_params <- "stics_version" %in% params_names
  url_in_params <- "url" %in% params_names

  # print(version_in_params)
  # print(url_in_params)


  if ((version_in_params && !url_in_params) ||
      (!version_in_params && url_in_params)) {
    stop("Both url and stics_version must exist in params list !")
  }

  if (version_in_params && (!params$stics_version %in% versions)) {
    stop("\nGiven version in params does not exist, see versions list\n", paste(versions, collapse = "\n"))
  }

  if (url_in_params && (!params$url %in% urls)) {
    stop("\nGiven url in params does not exist, see urls list \n", paste(urls, collapse = "\n"))
  }

}

get_stics_versions_url <- function() {

  list(
    "V8.5"="https://w3.avignon.inra.fr/svn/modulostics/tags/stics_v8.5",
    "V9.0"="https://w3.avignon.inra.fr/svn/modulostics/tags/stics_v9.0",
    "V9.1"="https://w3.avignon.inra.fr/svn/modulostics/tags/stics_v9.1",
    "V9.2"="https://w3.avignon.inra.fr/svn/modulostics/tags/stics_v9.2",
    "V10.0"="https://w3.avignon.inra.fr/svn/modulostics/branches/modulo_stics_perennes"
  )
}
