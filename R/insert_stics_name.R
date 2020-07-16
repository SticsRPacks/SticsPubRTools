#' Insert Stics names.
#'
#' Call this function as an addin to insert \code{ Stics names } at the cursor position.
#'
# @export
insert_stics_name <- function(name = NULL, format = FALSE, sampl = FALSE) {

  names <- get_names_list(name = name)

  if ( sampl ) names <- names[names$name == sample(names$name, 1), ]

  if (base::is.null(names)) warning("Not any available name containing", name)

  if ( dim(names)[1] > 1 ) {
    warning(paste("\nMultiple names containing",name))
    cat(names$name)
    return(invisible())
  }

  # Exact match for the name
  # no format
  fname <- names$name
  # Formating name before insertion
  if (format) {
  fname <- format_name(name = names$name, type = "par", kind = names$kind)
  }

  # check fname content
  rstudioapi::insertText(fname)
}



#' Get Stics names list
#'
#' Call this function for getting parameters names from SticsRFiles pkg
#'
# @export
get_names_list <- function(name = NULL, param = TRUE, stics_version = "last") {

  # for XML files
  # names <- SticsRFiles::get_param_info(parameter = name, version = stics_version)$name

  # For inputs.csv
  names <- SticsRFiles:::get_param_desc(name = name, version = stics_version)

  if (!base::is.null(names) & length(names)) return(names)

  return()
}



format_name <- function(name, type, kind = NULL) {

  types <- c("var", "par")

  if (!type %in% types ) return()

  if ( type == "var") return(paste0(" $", name,"$ "))

  if (base::is.null(kind)) return()

  ind <- dico_kind_to_index(kind = kind)

  name <- gsub(pattern = "\\_", replacement = "\\\\_", name)

  name_str <- paste0(" $\\mathbf{", name,"}","_{",ind,"}$ ")

  return(name_str)

}


dico_kind_to_index <- function( kind = NULL) {

  kinds <- list()

  kinds$STATION <- "C"
  kinds$PARPLT <- "P"
  kinds$PARAM <- "G"
  kinds$PARSOL <- "S"
  kinds$PARTEC <- "T"
  kinds$PARAMV6.PARAM <- "G"
  kinds$PARAMV6 <- "G"
  kinds$PARAMV6.PLT <- "GP"
  kinds$USM.USMXML <- "U"
  kinds$USM <- "U"
  kinds$USMXML <- "U"
  kinds$INIT <- "I"
  kinds$java <- "j"

  if (base::is.null(kind)) return(kinds)


  kind <- make.names(kind)

  if (! kind %in% names(kinds)) return()

  return(kinds[[kind]])

}


insert_stics_name_sample <- function() {
  insert_stics_name(format = TRUE, sampl = TRUE)
}

