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
#' book may be generated when a Rmd file name is given.
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
generate_book <- function(input_rmd = 'index.Rmd',
                          book_pkg_dir = getwd(),
                          output_format = NULL ) {

  book_name <- "book"
  full_book <- TRUE
  doc_format <- 'bookdown::html_document2'


  # Overloading default format
  if (! base::is.null(output_format)) doc_format <- output_format

  # For specific parts Rmd files building
  if (! base::is.null(input_rmd)) {
    book_name <- paste("Chapter", input_rmd)
    full_book<- FALSE
  }

  # Output message
  msg <- paste("generating ...", paste(book_name, collapse = ","))

  # Rmd files path
  input <- file.path(book_pkg_dir, input_rmd)

  # Build execution relative to input content
  ret <- try(
    if (full_book) {
      bookdown::render_book(input = input, output_format = doc_format)
    } else {
      bookdown::preview_chapter(input = input, output_format = doc_format)
    })

  # Generation success status
  status <- !methods::is(ret, "try-error")

  if (! status) msg <- paste("Error", msg)

  print(msg)

  return(status)
}
