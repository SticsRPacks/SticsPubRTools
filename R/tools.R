get_label_from_name <- function(name = NULL, sep = "-") {

  if ( !base::is.character(name) ) return()

  # name treatment
  # convert to lower case valid variable name
  label <- make.names(tolower(name))

  # Replace one or more consecutive dots with "-"
  label <- gsub(pattern = "[\\.]{1,}",replacement = sep, x= label)

  # Removing trailing and heading "-"
  label <- trimws(x = label, whitespace = sep)

  return(label)

}
