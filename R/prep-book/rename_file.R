# getting the corresp list
get_file_corresp <- function(name) {

  new_file <- gsub("(e9782759201693)_c([0-9]*)", "\\2-chap", name)
  new_file <- gsub("^0", "", new_file)
  r <- as.list(new_file)
  names(r) <- name
  return(r)
}



# Rename file and keep a track of file change
rename_file <- function(name) {
  #dir <- dirname(name)
  new_file <- gsub("(e9782759201693)_c([0-9]*)", "\\2-chap", name)
  new_file <- gsub("^0", "", new_file)
  file.rename(from = name, to = new_file)
  return(data.frame(orig = name, new = new_file, stringsAsFactors = FALSE))
}
