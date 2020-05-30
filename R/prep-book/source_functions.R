
source_functions <- function(dir_path = getwd()) {
  func_list <- list.files(path = dir_path, pattern = "\\.R$")
  func_list <- setdiff(func_list,c("source_functions.R", "prep-red-book-script.R"))
  func_paths <- file.path(dir_path, func_list)
  lapply(func_paths, source)
  return()
}

# func_dir
#if (!exists("func_dir")) stop("Unknown variable: func_dir")
#source_functions(func_dir)
#source_functions()
