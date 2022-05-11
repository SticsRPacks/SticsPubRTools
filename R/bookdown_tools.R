library(yaml)

get_rmd_files <- function(dir = getwd() , filter = NULL, yaml = "_bookdown.yml", warn = FALSE) {

  def_filter <- c("index.Rmd", "References.Rmd", "Authors-list.Rmd",
                  "foreword.Rmd", "dedication.Rmd" )

  if (is.null(filter)) {
    filter <- def_filter
  } else {
    filter <- unique(c(filter, def_filter))
  }


  yml_file <- file.path(dir, yaml)

  if(!file.exists(yml_file))
    stop("yaml file does not exist: ", yml_file)

  yml_data <- yaml::yaml.load_file(yml_file, eval.expr = FALSE)

  if(is.null(yml_data) || (!("rmd_files" %in% names(yml_data))) )

    return()

  files <- setdiff(yml_data$rmd_files, filter)

  return(files)

  return()
}

get_rmd_chunk <- function(file, pattern = NULL, type = "fig") {

  if(is.null(pattern)) pattern <- sprintf(fmt = "^```\\{r %s-", type)

  file_lines  <- trimws(readLines(file))

  # removing empty lines
  file_lines <- file_lines[unlist(lapply(file_lines, function(x) nchar(x) > 0))]

  r_start <- grep(pattern = pattern, x = file_lines)

  r_end <- grep(pattern = "^```$", x = file_lines)


  l <- list()
  l$start_idx <- r_start
  l$end_idx <- r_end
  l$lines <- file_lines

  l

}

get_fig_name <- function(chunk_header) {
  # TODO
}

get_img_file <- function(chunk_content, ext = c("png", "jpg")) {
  # TODO
}

format_fig_ref <- function(name) {
  f <- "\\@ref(fig:%s)"

  sprintf(f,name )
}




