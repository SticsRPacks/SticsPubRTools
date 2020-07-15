get_label_from_name <- function(name = NULL, sep = "-", no_number_first = TRUE) {

  if ( !base::is.character(name) ) return()

  # Checking first character
  if (no_number_first) {
    is_char <- suppressWarnings(base::is.na(as.numeric(substr(name,1,1))))
    if (!all(is_char)) stop("Only letters as string starting character !")
  }

  # name treatment
  # convert to lower case valid variable name
  label <- make.names(tolower(name))

  # Replace one or more consecutive dots with "-"
  label <- gsub(pattern = "[\\.]{1,}",replacement = sep, x= label)

  # Removing trailing and heading "-"
  label <- trimws(x = label, whitespace = sep)




  return(label)

}
