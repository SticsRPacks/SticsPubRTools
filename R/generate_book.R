#' Generating a bookdown document or a part of
#'
#' @param input_rmd Chapter Rmd file name (optional)
#' @param book_pkg_dir Book package directory path (optional, default: current directory)
#' @param output_format String defining the type of document to build
#' (optional, default: "bookdown::html_document2")
#'
#' @description The generation of an entire book is performed without
#' any given arguments using the current directory which must be an
#' package book directory or giving a path to the package. A part of the
#' book may be generated when a Rmd file name or a list of is given.
#'
#' @return A success status of the document generation
#' @export
#'
#' @examples
#' \dontrun{
#' generate_book()
#'
#' generate_book(input_rmd = "07-chap.Rmd")
#'
#' generate_book(input_rmd = "07-chap.Rmd", output_format = 'bookdown::pdf_document2')
#'
#' }
#'
#'
#'
generate_book <- function(input_rmd = NULL,
                          book_pkg_dir = getwd(),
                          output_format = NULL ) {

  book_name <- "book"
  full_book <- TRUE
  doc_format <- 'bookdown::gitbook'


  # Overloading default format
  if (! base::is.null(output_format)) doc_format <- output_format

  # For specific parts Rmd files building
  if (! base::is.null(input_rmd)) {
    book_name <- paste("Chapter(s)", input_rmd)
    full_book<- FALSE
  } else {
    input_rmd <- "index.Rmd"
  }

  # Checking if files exist
  if (!all(file.exists(input_rmd))) stop("Check if all files exist!")


  setwd(book_pkg_dir)
  # Rmd files path

  # TODO: change output_dir depending on output_format
  # html : one file in the book project root
  # gitbook : get_output_dir()

  # Build execution relative to input content
  ret <- try(
    if (full_book) {
      bookdown::render_book(input = input_rmd, output_format = doc_format)
      #output_dir <- get_output_dir()
    } else {
      bookdown::preview_chapter(input = input_rmd, output_format = doc_format)
      #output_dir <- book_pkg_dir
    })

  return(invisible(book_pkg_dir))
}


get_output_dir <- function(dir = getwd()) {
  f <- file.path(dir, "_bookdown.yml")
  if (!file.exists(f)) return()

  l <- readLines(f)
  idx <- grep(pattern = "output_dir: (.*)", x = l)
  if (!length(idx) | length(idx) > 1) return("_book")

  gsub(pattern = "output_dir: (.*)", x = l[idx], replacement = "\\1")
}
