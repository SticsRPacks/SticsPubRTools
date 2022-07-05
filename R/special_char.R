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

  unlist(lapply(X = char_vec, function(x) stringr::str_replace_all(x, pattern, replacement)))

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


replace_file_spec_char <- function(file,
                                   out_file = NULL,
                                   overwrite = FALSE,
                                   line_filter = NULL) {

  if (length(file) > 1) {
    lapply(file, function(x) replace_file_spec_char(x,
                                                    out_file = x,
                                                    overwrite = overwrite,
                                                    line_filter = line_filter))
    return(invisible())
  }

  if(!file.exists(file))
    stop(file, ": does not exist!")

  file_lines <- readLines(file)

  if(length(file_lines) == 0)
    stop(file, ": is empty!")

  # replacing accentuated characters
  out_lines <- replace_all_spec_char(char_vec = file_lines)

  # removing extra curly braces
  out_lines <- replace_extra_braces(char_vec = out_lines,
                                    filter_tag = line_filter)

  if(is.null(out_file))
    out_file <- file

  if(file.exists(out_file) && !overwrite)
    stop(out_file, ": already exists, consider setting overwrite argument to TRUE\n",
         "or add output file name in out_file argument.")

  writeLines(text = out_lines, con = out_file )

}

replace_extra_braces_str <- function(char) {

  content <- unlist(strsplit(char, split = "\\{"))
  field <- content[1]
  content <- paste0(content[2:length(content)], collapse = "")
  s1 <- gsub(pattern = "\\{", content, replacement = "")
  s2 <- gsub(pattern = "\\},$", s1, replacement = "")
  s3 <- gsub(pattern = "\\}", s2, replacement = "")

  paste0(field, "{", s3, "},")

}

replace_extra_braces <- function(char_vec, filter_tag = NULL) {

  vec_ids <- 1:length(char_vec)

  # filter_tag one or more tags
  if(!is.null(filter_tag)) {
    patterns <- paste0("^\ *",filter_tag)
    #vec_ids <- unlist(lapply(patterns, function(x) grep(pattern = x, char_vec)))

    for (t in 1:length(filter_tag)){
      vec_ids <- grep(pattern = patterns[t], char_vec)
      new_char_vec <- unlist(lapply(X = char_vec[vec_ids], function(x) replace_extra_braces_str(x)))
      char_vec[vec_ids] <- new_char_vec
    }

  }
  char_vec

}




