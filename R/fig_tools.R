library(ggplot2)

#' Saving a plot (i.e. ggplot object) to an image file
#'
#' @param plot_obj A plot object or function
#' @param img_name File name
#' @param img_format Image file format
#' @param img_dir Output file directory
#' @param img_save Logical for saving a plot in a file or not (default)
#' @param opts a knitr::opts_chunk object containing all above img_* arguments values
#' as elements
#'
#' @return a plot object
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  plot_render(plot_obj)
#'  plot_render(plot_obj, img_name = "myimage", img_save = TRUE)
#'
#' }
#'
#'
plot_render <- function(plot_obj = last_plot(),
                        img_name = NULL,
                        img_format = "png",
                        img_dir = getwd(),
                        img_save = FALSE,
                        opts = NULL) {

  is_opts <- is.list(opts) && ("get" %in% names(opts))


  if (is_opts) {
    file_name <- opts$get("label")
    if(!is.null(file_name)) img_name <- file_name

    file_format <- opts$get("img_format")
    if(!is.null(file_format)) img_format <- file_format

    file_dir <- opts$get("img_dir")
    if(!is.null(file_dir)) img_dir <- file_dir

    file_save <- opts$get("img_save")
    if(!is.null(file_save)) img_save <- file_save

  }

  # if (!is.ggplot(plot_obj) && !is.function(plot_obj))
  #   stop("The plot_obj object is neither a ggplot object nor a plot function !")

  if(is.function(plot_obj)) {
    #print("function")
    plot_out <- eval_fun(plot_obj)
  } else {
    #print("ggplot")
    plot_out <- plot_obj
  }


  # no image file generation
  if (!img_save) return(plot_out)

  if(img_save && is.null(img_name))
    stop("file name not set, unable to create it!")

  # file formats
  # TODO : to be completed
  files_formats <- c("png", "pdf", "eps")

  if (! img_format %in% files_formats)
    warning("Unknown format", img_format)

  img_dir <- normalizePath(img_dir, winslash = "/", mustWork = FALSE)
  if (!dir.exists(img_dir))
    stop("Directory does not exist: ", img_dir)


  img_file <- file.path(img_dir, paste0(img_name, ".", img_format))



  suppressMessages(
    ggsave(filename = img_file, plot = plot_out, device = img_format)
  )


  return(plot_out)
}


eval_fun <- function(FUN){
  FUN()
}


test_plot <- function() {

  d <- data.frame(x = c(1,2,3), y = 10 * c(1,2,3))

  ggplot(data = d, aes(x = x, y = y)) + geom_point()

}

