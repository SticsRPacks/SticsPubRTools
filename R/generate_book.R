#' Generating a bookdown document or a part of
#'
#' @param input_rmd Chapter Rmd file name (optional)
#' @param book_pkg_dir Book package directory path (optional, default: current directory)
#' @param output_format String defining the type of document to build (optional);
#' among "bookdown::pdf_document2", "bookdown::pdf_document2",
#' "bookdown::gitbook" (default)
#' @param params A list containing parameters to pass to the Rmd
#' document to parameterize the output or overload existing params in the
#' document
#'
#' @description The generation of an entire book is performed without
#' any given arguments using the current directory which must be an
#' package book directory or giving a path to the package. A part of the
#' book may be generated when a Rmd file name or a list of is given.
#'
#' @return A success status of the document generation
#' @export
#'
#' @seealso \code{\link{build_current_book_part}}
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
                          output_format = NULL,
                          params = list()) {

  book_name <- "book"
  full_book <- TRUE
  doc_format <- 'bookdown::gitbook'

  # Checking params content according to stics_version and url only
  # print(params)
  check_version_params(params = params)

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
      bookdown::render_book(input = input_rmd,
                            output_format = doc_format, params = params, envir = new.env())
    } else {
      bookdown::preview_chapter(input = input_rmd,
                                output_format = doc_format, params = params, envir = new.env())
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



build_book_part <- function( file_name = NULL, other_names = c("index", "Appendices")){

  # For a single file name
  if (is.null(file_name)) {
    in_doc <- get_active_doc()
  } else {
    in_doc <- file_name
  }

  if(length(file_name) > 1) stop("Only one file name is required")

  if (!grepl(pattern = "\\.Rmd$", x = in_doc)) stop("Not a Rmd file")

  if( !file.exists(in_doc)) stop("File does not exist!")

  # check if this is a bookdown part
  book_dir <- dirname(in_doc)
  print(book_dir)
  if(!file.exists(file.path(book_dir, "index.Rmd"))) stop("Not a bookdown part")

  # Getting appendices and references parts
  other_files <- unlist(lapply(other_names , function(x) grep(pattern = x, x = list.files(book_dir, pattern = "\\.Rmd$"), value = TRUE)))


  # Adding extra parts
  if(length(other_files)) in_doc <- c(in_doc, other_files)


  generate_book(input_rmd = unique(in_doc))

}


#' Generating a 'bookdown::gitbook' document from the current
#' edited bookdown chapter (Rmd)
#'
#' @description Generating an output document from the open Rmd file
#' in the RStudio editor without specifying neither the file name nor
#' the output format ('bookdown::gitbook')
#'
#' @seealso \code{\link{generate_book}} for specifying Rmd files list, and output
#' format
#'
# @return
#' @export
#'
#' @examples
#' \dontrun{
#'
#' build_current_book_part()
#'
#' }
#'
build_current_book_part <- function() {
  build_book_part()
}
