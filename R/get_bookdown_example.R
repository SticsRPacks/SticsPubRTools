#' Getting a minimal bookdown R project from rstudio GitHub
#'
#' @param dir Target directory to store the downloaded project folder
#'
#' @return The full path of the project folder
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Using the default directory (i.e. current returned by getwd())
#' example_dir <- get_bookdown_example()
#'
#' # Or specifying a directory path as function input
#' # Linux
#' example_dir <- get_bookdown_example("/path/to/dir")
#'
#' # Windows
#' example_dir <- get_bookdown_example("drive:/path/to/dir")
#' }
#'
get_bookdown_example <- function(dir = getwd()) {

  target_dir = normalizePath(dir, winslash = "/", mustWork = FALSE)

  example_zip <- file.path(target_dir,"example.zip")
  utils::download.file("https://github.com/rstudio/bookdown-demo/archive/master.zip", example_zip)
  df_name = utils::unzip(example_zip, exdir = target_dir, list = TRUE)
  # Add : filter usefull files only !
  utils::unzip(example_zip, exdir = target_dir)

  unlink(example_zip)

  normalizePath(file.path(target_dir,df_name[1,1]), winslash = "/")

}
