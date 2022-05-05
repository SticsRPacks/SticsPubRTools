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
replace_spec_string <- function(string_vec, pattern, replacement) {

  unlist(lapply(X = string_vec, function(x) str_replace_all(x, pattern, replacement)))

}

# Replacing in string vector Latex encoding for accentuated characters
# with the right characters, according to a correspondence dictionary
replace_all_spec_string <- function(string_vec, dict = NULL) {

  if (is.null(dict))
    dict <- spec_char_dict()

  for (n in names(dict)) {
    string_vec <- replace_spec_string(string_vec, pattern = n, replacement = dict[[n]])
    print(dict[[n]])
  }

  return(string_vec)
}

