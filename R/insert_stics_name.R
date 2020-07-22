#' Insert Stics names.
#'
#' Call this function as an addin to insert \code{ Stics names } at the cursor position.
#'
# @export
insert_stics_name <- function(name, kind = NULL, type = "par", format = FALSE) {

  fname <- name

  # Formating name before insertion
  if (format) {
    fname <- format_name(name = name, type = type, kind = kind)
    # print("formatting name")
    # print(type)
    # print(kind)
    # print(fname)
  }

  # check fname content
  rstudioapi::insertText(fname)
}



#' Get Stics names list
#'
#' Call this function for getting parameters or variables names from SticsRFiles pkg
#'
# @export
get_names_list <- function(type = "par", stics_version = "last") {

  type_list <- c("par", 'var')

  if (! type %in% type_list) stop()

  stics_version <- SticsRFiles:::check_version_compat(version_name = stics_version)

  names_var <- paste0(type, "_names")

  # Creating stics_names env in stics env
  if (! SticsRFiles:::stics_exists(name = "stics_names")) SticsRFiles:::stics_env(name = "stics_names")

  # If the variable param_names or var_names in the stics_names
  # environment, returning the value got from the environment.
  names <- SticsRFiles:::stics_get(name = paste0(names_var,"$", stics_version), env_name = "stics_names")

  if ( ! base::is.null(names)) return(names)


  if (type == "par") {
    # For inputs.csv
    names <- SticsRFiles:::get_param_desc(version = stics_version)
    # for XML files
    # names <- SticsRFiles::get_param_info(parameter = name, version = stics_version)$name
  }

  if (type == "var") {
    # For outputs.csv
    names <- SticsRFiles::get_var_info(version = stics_version)
  }


  # Create a variable: param_names or var_names in the stics_names environment
  if (! SticsRFiles:::stics_exists(name = names_var, env_name = "stics_names")) {
    SticsRFiles:::stics_set(name = names_var,
                            value = list(),
                            env_name = "stics_names")
  }

  SticsRFiles:::stics_set(name = paste0(names_var,"$", stics_version),
                          value = names,
                          env_name = "stics_names")

  # the first column: name for par, variable for var
  if (!base::is.null(names) & length(names)) return(names)

  return()
}



format_name <- function(name, type = "par", kind = NULL) {

  types <- c("var", "par")

  if (!type %in% types ) return()

  if ( type == "var" && base::is.null(kind)) return(paste0(" $", name,"$ "))

  if (base::is.null(kind)) return("")

  ind <- dico_kind_to_index(kind = kind)

  #name <- gsub(pattern = "\\_", replacement = "\\\\_", name)
  name <- make_pattern(name)

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

make_pattern <- function(name, symbol = c("(",")","_", ".")) {

  for (i in 1:length(symbol)) {
    s <- symbol[i]
    name <- gsub(pattern = paste0("\\",s), x = name, replacement = paste0("\\\\",s))
  }
  return(name)
}


