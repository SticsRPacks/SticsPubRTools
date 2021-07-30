# Insert Stics names.
#
# Call this function as an addin to insert \code{ Stics names } at the cursor position.
#
# @export
insert_stics_name <- function(name, kind = NULL, type = "par", format = FALSE, link = FALSE, latex = FALSE) {

  if ( !format && !link ) {
    rstudioapi::insertText(name)
    return(invisible())
  }
  # Formating name before insertion
  if ( !link ) {
    fname <- format_name(name = name, type = type, kind = kind, latex = latex)
    # print("formatting name")
    # print(type)
    # print(kind)
    # print(fname)
    rstudioapi::insertText(fname)
    return(invisible())
  }

  fname <- format_names_link(names = name, kinds = kind , type = type, inline = link, latex = latex)
  # check fname content
  rstudioapi::insertText(fname)
}


# Get Stics names list
#
# Call this function for getting parameters or variables names from SticsRFiles pkg
#
# @export
get_names_list <- function(type = "par", stics_version = "last") {

  types_list <- c("par", "var", "int", "all")

  if ( ! type %in% types_list ) stop()

  stics_version <- SticsRFiles:::check_version_compat(version_name = stics_version)



  # ----------------- commented: starting section using stics environment function ------------------#
  # names_var <- paste0(type, "_names")
  # # Creating stics_names env in stics env
  # if ( ! SticsRFiles::sticsexists(name = "stics_names") ) SticsRFiles::sticsenv(name = "stics_names")
  #
  # # If the variable param_names or var_names in the stics_names
  # # environment, returning the value got from the environment.
  # names <- suppressWarnings(
  #   SticsRFiles::sticsget(name = paste0(names_var,"$", stics_version), env_name = "stics_names")
  # )
  #if ( ! base::is.na(names)) return(names)
  # ----------------- commented: ending section using stics environment function ------------------#

  if ( type == "par" ) {
    # For inputs.csv
    names <- SticsRFiles:::get_param_desc(version = stics_version)
    # for XML files
    # names <- SticsRFiles::get_param_info(parameter = name, version = stics_version)$name
  }

  if ( type == "var" ) {
    # For outputs.csv
    names <- SticsRFiles::get_var_info(version = stics_version)
  }

  if (type == "int") {
    names <- all_int_var(version = stics_version, lib = TRUE)
  }


  if (type == "all") {
    names <- list()
    names$par <- SticsRFiles:::get_param_desc(version = stics_version)
    names$var <- SticsRFiles::get_var_info(version = stics_version)
    names$int <- all_int_var(version = stics_version, lib = TRUE)
  }
  # temporarily early return, without using of environment.
  return(names)


  # ----------------- section using stics environment function commented ------------------#
  # # Create a variable: param_names or var_names in the stics_names environment
  # if (! SticsRFiles::sticsexists(name = names_var, env_name = "stics_names")) {
  #   SticsRFiles::sticsset(name = names_var,
  #                         value = list(),
  #                         env_name = "stics_names")
  # }
  #
  # SticsRFiles::sticsset(name = paste0(names_var,"$", stics_version),
  #                       value = names,
  #                       env_name = "stics_names")
  # ----------------- commented: ending section using stics environment function ------------------#

  # the first column: name for par, variable for var
  if ( !base::is.null(names) & length(names) ) return(names)

  return()
}


get_usefull_cols <- function(df, ids) {

  if (!all(dim(df))) return(df)

  real_ids <- intersect(1:ncol(df), ids)

  if (!length(real_ids)) return(data.frame())

  return(subset(df,select = real_ids))
}

format_name <- function(name, type = "par", kind = NULL, inline = FALSE, latex = FALSE ) {

  types <- c("var", "par", "int")
  in_sign <- "$"
  escape <- ""
  if (latex) escape <- "\\"

  if ( !inline ) in_sign <- ""

  if ( ! type %in% types ) return()

  if ( type == "var" && base::is.null(kind) ) {
    return(paste0(in_sign, make_pattern(name), in_sign))
  }

  if ( type == "int" && base::is.null(kind) ) {
    return(paste0(in_sign, escape, "\\mathsf{",make_pattern(name),"}", in_sign))
  }

  # Identity
  if ( base::is.null(kind) ) return()

  ind <- dico_kind_to_index(kind = kind)

  #name <- gsub(pattern = "\\_", replacement = "\\\\_", name)
  name <- make_pattern(name)

  name_str <- paste0(in_sign, escape,"\\mathbf{", name,"}","_{",ind,"}", in_sign)

  return(name_str)

}

format_names <- function(names, kinds = NULL, type = "par", inline = FALSE, latex = FALSE) {

  # checking dimensions
  names_nb <- length(names)

  if ( base::is.null(kinds) ) {
    return(unlist(lapply(names, function(x) format_name(x, type = type, inline = inline, latex = latex))))
  }

  if( names_nb != length(kinds) ) {
    stop("Error: parameter kind number must match parameter number !")
  }

  out_names <- vector(mode = "character", names_nb)
  for ( i in 1:names_nb ) {
    out_names[i] <- format_name(name = names[i], kind = kinds[i], inline = inline, latex = latex)
  }

  return(out_names)
}


#format_names_link <- function(names, kinds = NULL , type = "par", target = FALSE, inline = FALSE, latex = FALSE) {
format_names_link <- function(names, kinds = NULL, type = "par", target = FALSE, inline = FALSE, latex = FALSE) {

  escape <- ""
  if (latex) escape <- "\\"

  if (type == "par") kinds <- SticsPubRTools:::get_names_kind(names = names)

  formatted_names <- format_names(names = names, kinds = kinds, type = type, inline = inline, latex = latex)

  names_label <- get_label_from_name(names)

  if ( target ) {
    fmt <- "[%s]{#%s-%s}"
    names_link <- sprintf(fmt = fmt, formatted_names, type, names_label )
  } else {
    fmt <- paste0("[%s](#%s-%s) ", escape,"\\index{%s}")
    names_link <- sprintf(fmt = fmt, formatted_names, type, names_label, names )
  }

  return(names_link)
}

format_names_target <- function(names, kinds = NULL, type = "par", inline = FALSE, latex = FALSE) {
  return(format_names_link(names = names, kinds = NULL, type = type, target = TRUE, inline = inline, latex = latex))
}


get_names_kind <- function(names, stics_version = "last") {
  stics_version <- SticsRFiles:::check_version_compat(version_name = stics_version)
  names_data <- get_names_list(type = "par", stics_version = stics_version)

  kinds <- rep(NA, length(names))
  idx_names <- names %in% names_data$name
  kinds[idx_names] <-  names_data$kind[names_data$name %in% names[idx_names]]

  if ( !all(idx_names) ) {
    warning(paste(names[!idx_names],collapse = ","),": unknown name(s) !")
  }

  kinds

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

  if ( base::is.null(kind) ) return(kinds)


  kind <- make.names(kind)
  idx <- kind %in% names(kinds)


  indexes <- unlist(lapply(kind[idx], function(x) kinds[[x]]))
  return(indexes)

}

make_pattern <- function(name, symbol = c("_", "."), where = NULL) {

  pos <- c("start", "end")
  if ( (!base::is.null(where)) && (! where %in% pos) ) where <- NULL

  for ( i in 1:length(symbol) ) {
    s <- symbol[i]
    name <- gsub(pattern = paste0("\\",s), x = name, replacement = paste0('\\\\',s))
  }

  if ( base::is.null(where) ) return(name)

  if ( where == "start" ) return(paste0("^", name))

  if ( where == "end" ) return(paste0(name, "$"))

  #return(name)
}



all_int_var <- function(version = "last", lib = FALSE){

  # lib = FALSE, searching locally in the book project dir
  # lib = TRUE, searching in the lib path in SticsRFiles installed package

  # DISABLED FOR THE MOMENT
  # Checking and getting the right version
  version <- SticsRFiles:::check_version_compat( version_name = version)
  # file_path <- file.path(SticsRFiles:::get_examples_path( file_type = "csv", version_name = version ), "internal_variables_v10.csv")

  # ALTERNATIVE: USING THE BOOK PROJECT FILE
  #file_path <- file.path(rstudioapi::getActiveProject(),"data","internal_variables_v10.csv")
  if (!lib) {
    # working in Stics book directory
    file_path <- file.path(getwd(),"data","internal_variables_v10.csv")
  } else {
    dir <- file.path(Sys.getenv()["R_LIBS_USER"],"SticsRFiles","extdata","csv", version)
    file_path <- file.path(dir,"internal_variables_v10.csv")
  }

  if (!file.exists(file_path)) return(invisible(data.frame()))

  var_df <- utils::read.csv2(file_path,
                             header = TRUE,
                             stringsAsFactors = FALSE)[,1:4]

  names(var_df) <- c("name", "definition", "unit", "internal")

  # Adding a version  attribute
  attr(x = var_df, which = "version") <- version
  return(var_df)
}

