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

get_rmd_content <- function(file, comments = FALSE) {

  file_lines  <- readLines(file)
  # comment_idx <- grepl(pattern = "<!--", x = file_lines) |
  #   grepl(pattern = "-->", x = file_lines)
  #
  # if(any(comment_idx))
  #   file_lines <- file_lines[!comment_idx]

  file_lines

}

get_rmd_title_tag <- function(file_lines) {


  if(!any_title_lines(file_lines))
    stop("Not any chapter/part title line found in given lines")

  # TODO: call to get_title_lines

  #file_lines <- file_lines[grep(pattern = "^#{1}\\s", x = file_lines)]

  if(length(file_lines) > 1)
    stop("Multiple chapter titles in lines !")

  tag <- gsub(pattern = "(.*)\\{#([a-zA-Z]*)\\}",file_lines[1], replacement = "\\2")

  if(tag == "")
    stop("chapter title line not found in the 1st line: ",file_lines[1])

  if(length(grep(pattern = "#{2,}", x = tag)) > 1)
    tag <- title_to_tag(tag)

  tag


}

get_title_lines <- function(file_lines) {

  chunk_ids <- get_rmd_chunk(file_lines = file_lines, type = "all", ids = TRUE)

  hash_ids <- grep(pattern = "^#{1,}\\s", x = file_lines)

  # TODO: filter lines excluding lines in chunks lines (i.e. commented code lines)

  # Getting hash_ids in chunks lines
  #sel_ids <- unlist(lapply(hash_ids, function(x) which(x < chunk_ids[2,] & x > chunk_ids[1,])))
  sel_ids <- unlist(lapply(hash_ids, function(x) any(x < chunk_ids[2,] & x > chunk_ids[1,])))

  file_lines[hash_ids[!sel_ids]]


}

any_title_lines <- function(file_lines) {

  title_lines <- get_title_lines(file_lines)

  if(length(title_lines) == 0)
    return(FALSE)

  TRUE

}

title_to_tag <- function(rmd_title) {

  # get existing tag
  tag <- gsub(pattern = ".*(#{1,})(.*)\\{#(.*)\\}", x = rmd_title, replacement = "\\3")

  no_tag_ids <- grep(pattern = "#",tag)

  if (any(no_tag_ids)) {

    tag[no_tag_ids] <- gsub(pattern = ".*(#{1,})(.*)",
                            x = rmd_title[no_tag_ids],
                            replacement = "\\2")

    tag[no_tag_ids] <- tolower(gsub("[[:blank:]]","-",trimws(tag[no_tag_ids])))
  }

  tag


}

# TODO: Check and fix this function,
# get_title_lines does not return right results for Shoot-growth.Rmd
get_rmd_chunk <- function(file_lines, pattern = NULL, type = "fig", ids = FALSE) {

  fmt <- "^```\\{r %s-"

  types <- c("fig", "tab", "none", "all")

  # removing empty lines
  #file_lines <- file_lines[unlist(lapply(file_lines, function(x) nchar(x) > 0))]

  r_start_none <- grep(pattern = "^```\\{r,", x = file_lines)

  r_start_fig <- grep(pattern = sprintf(fmt = fmt, "fig"), x = file_lines)

  r_start_tab <- grep(pattern = sprintf(fmt = fmt, "tab"), x = file_lines)

  r_start <- c(r_start_none,r_start_fig, r_start_tab)

  r_end <- grep(pattern = "^`{3}\\s{0,}$", x = file_lines)

  label <- c(rep("r", length(r_start_none)),
             rep("fig", length(r_start_fig)),
             rep("tab", length(r_start_tab)))[order(r_start)]

  r_start <- sort(r_start)
  r_end <- sort(r_end)

  if (type != "all") {
    chunk_idx <- rbind(r_start[label == type], r_end[label == type])
  } else {
    chunk_idx <- rbind(r_start, r_end)
  }

  if(ids)
    return(chunk_idx)


  apply(chunk_idx,2, function(x) file_lines[x[1]:x[2]])

}


get_fig_name <- function(chunk_header) {
  # TODO
}

get_img_file <- function(chunk_content, ext = c("png", "jpg"), type = "fig") {
  # type: tab, fig
  # cf fig.cap, tab.cap ...

  # TODO
}

get_tab_name <- function(chunk_header) {

}


format_fig_ref <- function(name) {
  f <- "\\@ref(fig:%s)"

  sprintf(f,name )
}

format_tab_ref <- function(name) {
  f <- "\\@ref(tab:%s)"

  sprintf(f,name )
}



