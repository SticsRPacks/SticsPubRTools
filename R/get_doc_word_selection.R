get_doc_word_selection <- function(unique = TRUE) {

  sel <- rstudioapi::getSourceEditorContext()

  # test if only one selection
  if (length(sel$selection) > 1) {
    warning("Multiple words selection !")
    return()
  }

  # test if it's null
  if (sel$selection[[1]]$text == "") {
    warning("empty selection!")
    return()
  }

  # test if it'contains several words
  words <- strsplit(x = trimws(sel$selection[[1]]$text), split = " ")[[1]]

  if (length(words) > 1 && unique) {
    warning("multiple words selected")
    return()
  }

  return(sel$selection[[1]])
}
