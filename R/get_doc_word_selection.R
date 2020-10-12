get_doc_word_selection <- function(unique = TRUE) {

  sel <- get_editor_context(what = "selection")

  # test if only one selection
  if (length(sel) > 1) {
    warning("Multiple words selection !")
    return()
  }

  # test if it's null
  if (sel[[1]]$text == "") {
    warning("empty selection!")
    return()
  }

  # test if it'contains several words
  words <- strsplit(x = trimws(sel[[1]]$text), split = " ")[[1]]

  if (length(words) > 1 && unique) {
    warning("multiple words selected")
    return()
  }

  return(sel[[1]])
}



get_active_doc <- function(full_path = TRUE) {

  path <- get_editor_context(what = "path")

  if(full_path) return(path)

  return(basename(path))

}


get_editor_context <- function(what = NULL) {

  context <- rstudioapi::getSourceEditorContext()

  if (is.null(what)) return(context)

  if (length(what) > 1) {
    warning("Only one fieldname allowed !")
    return()
  }

  if(! what %in% names(context)) stop("Unknown context field name !")

  return(context[[what]])
}


