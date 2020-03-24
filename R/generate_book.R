#' Generating a bookdown document or a part of
#'
#' @param chap_rmd Chapter Rmd file name (optional)
#' @param book_pkg_dir
#'
#' @description The generation of an entire book is performed without
#' any given arguments using the current directory which must be an
#' package book directory or giving a path to the package. A part of the
#' book may be generated when a Rmd file name is given.
#'
#' @return A status of the generated document
#' @export
#'
# @examples
generate_book <- function(chap_rmd = NULL,
                          book_pkg_dir = getwd(),
                          output_format = NULL ) {

  book_name <- "book"
  full_book <- TRUE
  doc_format <- 'bookdown::html_document2'

  if (! base::is.null(output_format)) doc_format <- output_format

  if (! base::is.null(chap_rmd)) {
    book_name <- paste("Chapter", chap_rmd)
    full_book<- FALSE
  }

  if (full_book) {
    bookdown::render_book(input = 'index.Rmd', output_format = doc_format)
  } else {
    bookdown::preview_chapter(input = chap_rmd, output_format = doc_format)
  }
  print(paste("Generating...", book_name))
}
