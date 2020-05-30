replace_in_file <- function(file, pattern, replacement){
  a <- read_file(file) 
  a <- str_replace_all(a, pattern, replacement)
  write(a, file)
}
