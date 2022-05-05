# Correspondence between latex syntax and special characters

# Dictionary of correspondence of
# pattern strings for regex with accentuated characters.
spec_char_dict <- function() {
  dict <- list()

  #dict[["{\a'e}"]] <- "é"
  dict[["\\{\\\\a'e\\}"]] <- "é"

  # dict[["{\a`e}"]] <- "è"
  dict[["\\{\\\\a`e\\}"]] <- "è"

  # dict[["{\c c}"]] <- "ç"
  dict[["\\{\\\\c c\\}"]] <- "ç"

  # dict[["{\~a}"]] <- "ã"
  dict[["\\{\\\\~a\\}"]] <- "ã"

  # dict[["{\a'\i}"]] <- "í"
  dict[["\\{\\\\a'\\\\i\\}"]] <- "í"

  # dict[["{\a'a}"]] <- "á"
  dict[["\\{\\\\a'a\\}"]] <- "á"

  # dict[["{\a'o}"]] <- "ó"
  dict[["\\{\\\\a'o\\}"]] <- "ó"

  # dict[["{\~n}"]] <- "ñ"
  dict[["\\{\\\\~n\\}"]] <- "ñ"

  # dict[["{\"a}"]] <- "ä"
  dict[['\\{\\\\"a\\}']] <- "ä"

  # dict[["{\"e}"]] <- "ë"
  dict[['\\{\\\\"e\\}']] <- "ë"

  # dict[["{\"\i}"]] <- "ï"
  dict[['\\{\\\\"\\\\i\\}']] <- "ï"

  # dict[["{\"u}"]] <- "ü"
  dict[['\\{\\\\"u\\}']] <- "ü"

  # dict[["{\"o}"]] <- "ö"
  dict[['\\{\\\\"o\\}']] <- "ö"

  # dict[["{\^o}"]] <- "ô"
  dict[["\\{\\\\\\^o\\}"]] <- "ô"

  # dict[["{\^\i}"]] <- "î"
  dict[["\\{\\\\\\^\\\\i\\}"]] <- "î"
  #
  # dict[["{\r a}"]] <- "å"
  dict[["\\{\\\\r a\\}"]] <- "å"

  # dict[["{\r A}"]] <- "Å"
  dict[["\\{\\\\r A\\}"]] <- "Å"
  #
  # dict[["{\v z}"]] <- "ž"
  dict[["\\{\\\\v z\\}"]] <- "ž"

  # dict[["{\a`y}"]] <- "ý"
  dict[["\\{\\\\a`y\\}"]] <- "ý"

  # dict[["{\v Z}"]] <- "Ž"
  dict[["\\{\\\\v Z\\}"]] <- "Ž"

  # dict[["{\v c}"]] <- "č"
  dict[["\\{\\\\v c\\}"]] <- "č"
  #
  # dict[["{\o}"]] <- "ø"
  dict[["\\{\\\\o\\}"]] <- "ø"

  # TODO! to be completed

  return(dict)

}

# Replacing in string vector Latex encoding for an accentuated character
# with the right corresponding character
replace_spec_char <- function(char_vec, pattern, replacement) {

  unlist(lapply(X = char_vec, function(x) str_replace_all(x, pattern, replacement)))

}

# Replacing in string vector Latex encoding for accentuated characters
# with the right characters, according to a correspondence dictionary
replace_all_spec_char <- function(char_vec, dict = NULL) {

  if (is.null(dict))
    dict <- spec_char_dict()

  for (n in names(dict)) {
    char_vec <- replace_spec_char(char_vec, pattern = n, replacement = dict[[n]])
    #print(dict[[n]])
  }

  return(char_vec)
}


replace_file_spec_char <- function(file, out_file = NULL, overwrite = FALSE) {

  if (length(file) > 1) {
    lapply(file, function(x) replace_file_spec_char(x,
                                                    out_file = x,
                                                    overwrite = overwrite))
    return(invisible())
  }

  if(!file.exists(file))
    stop(file, ": does not exist!")

  file_lines <- readLines(file)

  if(length(file_lines) == 0)
    stop(file, ": is empty!")

  out_lines <- replace_all_spec_char(char_vec = file_lines)

  if(is.null(out_file))
    out_file <- file

  if(file.exists(out_file) && !overwrite)
    stop(out_file, ": already exists, consider setting overwrite argument to TRUE\n",
         "or add output file name in out_file argument.")

  writeLines(text = out_lines, con = out_file )

}
