html_to_Rmd <- function(html_files){

  nfiles <- length(html_files)
  Rmd_files <- vector(mode = "character", nfiles)
  for (f in 1:nfiles) {
    file_name <- gsub("\\.html", "", html_files[f])
    print(html_files[f])
    r <- system(command = glue("pandoc {file_name}.html -o {file_name}.Rmd"))
    if (!r) Rmd_files[f] <- glue("{file_name}.Rmd")
  }
  return(Rmd_files)
}
