insert_stics_unit <- function(name, braces = TRUE) {
  funit <- format_unit(name)
  if (braces) funit <- paste0(" (",funit,")")
  # check funit content
  rstudioapi::insertText(funit)
}



format_unit <- function(unit) {
  paste0("$",make_unit_pattern(unit),"$")
}

# TODO: evaluate if usefull
units_list <- function(unit) {
  units_list <- unlist(strsplit(x = unit, split = ".", fixed = TRUE))
  units_list <- unique(trimws(units_list))
  units_list
}

# TODO: test escape case
format_unit <- function(unit, escape = TRUE) {


  # setting all other values where unit_codes are NA

  rep <- format_code_values(get_code_values(unit))



  not_code <- is.na(rep)

  if (!any(not_code)) return(rep)

  # removing leading and trailing blanks
  rep[not_code] <- trimws(unit[not_code])

  # International unit system: not dots for multiplication, space instead
  rep[not_code] <- gsub(pattern = "\\.", x = rep[not_code], replacement = "\\ " )

  # replacing multiple spaces
  rep[not_code] <- gsub(' +','\\\\ ',rep[not_code])


  # replace SD
  # adimensional, non-dimensional, dimensionless
  # ND stands for non-dimensional
  rep[not_code] <- gsub(pattern = "SD", x = rep[not_code], replacement = "ND" )


  # rep[not_code] <- gsub(pattern = "\\.", x = rep[not_code], replacement = " \\\\cdot " )
  ##rep[not_code] <- gsub(pattern = "\\ ", x = rep[not_code], replacement = "\\\\ " )

  # replace % -> \%
  rep[not_code] <- gsub(pattern = "\\%", x = rep[not_code], replacement = "\\\\%" )

  # replace degree_C -> $^{\circ}C$
  # rep[not_code] <- gsub(pattern = "degreeC", x = rep[not_code], replacement = "^{\\\\circ}C" )
  # rep[not_code] <- gsub(pattern = "degree\\\\ C", x = rep[not_code], replacement = "^{\\\\circ}C" )
  # rep[not_code] <- gsub(pattern = "degrees\\\\ C", x = rep[not_code], replacement = "^{\\\\circ}C" )
  # rep[not_code] <- gsub(pattern = "degreesC", x = rep[not_code], replacement = "^{\\\\circ}C" )
  # rep[not_code] <- gsub(pattern = "degree_C", x = rep[not_code], replacement = "^{\\\\circ}C" )
  # rep[not_code] <- gsub(pattern = "([^a-zA-Z])degree_C", x = rep[not_code], replacement = " ^{\\\\circ}C" )

  # replace degree_d -> degree days
  rep[not_code] <- gsub(pattern = "degree_d", x = rep[not_code], replacement = "degree\\\\ days" )

  # TO BE CHECKED: replace d -> days
  #rep[not_code] <- gsub(pattern = "([^a-zA-Z])d([^a-zA-Z]+)", x = rep[not_code], replacement = "\\\\ day\\2" )
  rep[not_code] <- gsub(pattern = "([^a-zA-Z])d([^a-zA-Z]+)", x = rep[not_code], replacement = " day\\2" )
  rep[not_code] <- gsub(pattern = "^d$", x = rep[not_code], replacement = "days" )
  rep[not_code] <- gsub(pattern = "^d([^a-zA-Z])", x = rep[not_code], replacement = "day\\1" )

  # TODO: replace julian_d -> julian day
  rep[not_code] <- gsub(pattern = "julian_d", x = rep[not_code], replacement = "julian\\\\ day" )

  # replace "or"
  rep[not_code] <- gsub(pattern = "\\ or\\ ", x = rep[not_code], replacement = "\\\\ \\\\mathbf{or}\\\\ " )


  #replace unit format
  rep[not_code] <-  format_expo_values(rep[not_code])

  # Treating at last degree replacement, bc replacing them previously
  # leading to exponents pbs.
  rep[not_code] <- gsub(pattern = "degreeC", x = rep[not_code], replacement = "^{\\\\circ}C" )
  rep[not_code] <- gsub(pattern = "degree\\\\ C", x = rep[not_code], replacement = "^{\\\\circ}C" )
  rep[not_code] <- gsub(pattern = "degrees\\\\ C", x = rep[not_code], replacement = "^{\\\\circ}C" )
  rep[not_code] <- gsub(pattern = "degreesC", x = rep[not_code], replacement = "^{\\\\circ}C" )
  rep[not_code] <- gsub(pattern = "degree_C", x = rep[not_code], replacement = "^{\\\\circ}C" )
  rep[not_code] <- gsub(pattern = "([^a-zA-Z])degree_C", x = rep[not_code], replacement = " ^{\\\\circ}C" )



  return(paste0("$",rep,"$"))


}


get_code_values <- function(code_str) {
  if (length(code_str) > 1) {
    l <- lapply(X = code_str, FUN = get_code_values)
    return(l)
  }

  code_found <- grepl(pattern = "code",x = code_str)
  range_found <- grepl(pattern = "[0-9]-[0-9]",x = code_str)

  if (!code_found && !range_found) return(NA)

  # add detecting "code" in code_str
  strs <- unique(strsplit(trimws(gsub("\\D+"," ",code_str)), split= " "))
  v <- as.numeric(unlist(strs))
  if (!length(v)) {
    return(NA)
  } else {
    return(c(min(v), max(v), code_found))
  }

}

# TODO: see if other formats are necessary
# or change the defined format
format_code_values <- function(code_values) {

  #formats <- c("to", "from-to")
  #out_formats <- list("\\ to\\ ", c("from\\ ", "\\ to\\ "))

  format <- "\\ to\\ "

  if (is.list(code_values) & length(code_values) > 1) {
    l <- unlist(lapply(X = code_values, FUN = format_code_values))
    return(l)
  # } else {
  #   code_values <- unlist(code_values)
  }

  if (all(is.na(code_values))) return(code_values)

  code_found <- code_values[3]
  code_values <- code_values[1:2]

  out <- paste0(paste0(as.character(code_values)), collapse = format)
  # adding code explicitly, if exists in initial unit
  if (code_found) out <- paste0("code,\\ ", out)

  out
}

format_expo_values <- function(unit) {
  elts <- lapply(unit, function(x) strsplit(x,split=" ")[[1]] )
  #elts <- strsplit(unit,split=" ")[[1]]
  # paste(gsub(x = elts, pattern = "(.*[^-])([-]?[0-9]{1})(.*)", "\\1^{\\2}\\3" ),
  #       collapse = " ")
  unlist(lapply(elts, function(x) paste(gsub(x = x, pattern = "(.*[^-])([-]?[0-9]{1})(.*)", "\\1^{\\2}\\3" ),
                                 collapse = " ") ))
}

