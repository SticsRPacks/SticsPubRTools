library(stringr)
clean_html_rmd <- function(in_rmd_file, out_file = in_rmd_file){


  rmd_lines <- readLines( in_rmd_file )



  # removing lines with html tables tags
  rmd_lines <- remove_html_table_tags_lines(rmd_lines)

  # removing lines with only <div>, <dic class=*> or </div>
  rmd_lines <- remove_div_lines(rmd_lines)



  # removing paragraph tags <p><span> & </span></p>
  rmd_lines <- rep_paragraph_tags(rmd_lines)

  # change syntax for <sub> </sub> replace with ~, <sup> and </sup> replace with ^

  rmd_lines <- rep_sub_tags(rmd_lines)
  rmd_lines <- rep_sup_tags(rmd_lines)

  # Add <span class="i"> ... </span>
  rmd_lines <- rep_class_i_rmd(rmd_lines)

  # replace &lt; and &gt; by < and >
  rmd_lines <- rep_gt_lt(rmd_lines)

  # Removing line "<p>Ce document est la propriété..."
  rmd_lines <- rm_last_line(rmd_lines)

  writeLines(rmd_lines, out_file)


  }


clean_html_string <- function(in_string, suppress = FALSE) {
  rep_string <- NULL
  if (suppress) rep_string <- ""

  # Add <span class="i"> ... </span>
  in_string <- rep_class_i_rmd(in_string, rep_string)

  in_string <- rep_sub_tags(in_string, rep_string)
  in_string <- rep_sup_tags(in_string,rep_string)


  return(in_string)
}

rm_last_line <- function(lines) {

  idx <- grepl(pattern = "<p>Ce document est la propri", x = lines)

  if (!any(idx)) return(lines)

  return(lines[!idx])
}

rep_chapter_tag <- function(lines, file_chapter_data) {
  rep_lines <- str_replace_all(lines, "<div class=\"chapter\">", paste("#",file_chapter_data$title))
  rep_lines <- str_replace_all(rep_lines, paste0("<span id=\"",file_chapter_data$id,"\"></span>"), "")
  return(rep_lines)
}

rep_pref_tag <- function(lines, file_chapter_data) {
  rep_lines<- str_replace_all(lines, "<div class=\"frontMatter\">", paste("#",file_chapter_data$title, "{-}"))
  rep_lines <- str_replace_all(rep_lines, paste0("<span id=\"",file_chapter_data$id,"\"></span>"), "")
  return(rep_lines)
}

rep_ded_tag <- function(lines, file_chapter_data) {
  rep_lines<- str_replace_all(lines, "<div class=\"dedicationPage\">", paste("#",file_chapter_data$title, "{-}"))
  rep_lines <- str_replace_all(rep_lines, paste0("<span id=\"",file_chapter_data$id,"\"></span>"), "")
  return(rep_lines)
}

rep_paragraph_tags <- function(lines) {
  par_pattern <- "(<p><span>)(.*)(</span></p>)"
  rep_lines <- gsub(x = lines, pattern = par_pattern, replacement = "\n\\2")
  return(rep_lines)
}


rep_sub_tags <- function(lines, rep_string = NULL) {
  return(rep_sub_sup_tags(lines = lines, tag = "sub", rep_string = rep_string))
}

rep_sup_tags <- function(lines, rep_string = NULL) {
  return(rep_sub_sup_tags(lines = lines, tag = "sup", rep_string = rep_string))
}

rep_sub_sup_tags <- function(lines, tag, rep_string = NULL) {

  if (!base::is.null(rep_string)) {
    rep_tag <- rep_string
  } else if ( tag =="sup" ) {
    rep_tag <- "^"
  } else if ( tag =="sub" ) {
    rep_tag <- "~"
  } else {
    stop("Neither 'sup' nor 'sub' in given tag argument !")
  }

  #patt_start <- paste0('(.*)',"<", tag, ">",'(.*)')

  patt_start <- paste0("<", tag, ">")
  #rep_start <- paste0("\\1",rep_tag,"\\2")
  patt_end <- paste0("</", tag, ">")

  rep_lines <- gsub(pattern = patt_start, replacement = rep_tag, x = lines)
  rep_lines <- gsub(pattern = patt_end, replacement = rep_tag, x = rep_lines)

  return(rep_lines)

}


rep_class_i_rmd <- function(lines, rep_string = NULL) {

  rep_tag <- "\\*\\2\\*"
  if (!base::is.null(rep_string)) rep_tag <- "\\2"
  rep_lines <- gsub(pattern = '(<span class=\"i\">)([^\\/]+)(</span>)',x = lines, replacement = rep_tag)

  return(rep_lines)
}

rep_gt_lt <- function(lines) {
  rep_lines <- gsub(pattern = "(&gt;)", x = lines, replacement = ">")
  rep_lines <- gsub(pattern = "(&lt;)", x = rep_lines, replacement = ">")
}

remove_div_lines <- function(lines) {
  div_pattern <- "(<div|</div)(.*)"
  #rep_lines <- str_replace_all(lines, par_pattern, replacement = "\\1")
  lines_idx <- grepl(x = lines, pattern = div_pattern)
  return(lines[!lines_idx])
}

remove_html_table_tags_lines <- function(lines) {
  tb_pattern <- "(<table|<tr|<th|<td|<thead|<tbody|</table|</tr|</th|</td|</thead|</tbody)(.*)"
  lines_idx <- grepl(x = lines, pattern = tb_pattern)
  return(lines[!lines_idx])
}



set_parts_string_rmd <- function(in_rmd_file, file_part_data= NULL,part_type = "chapter", out_file = in_rmd_file) {
  part_types <- c("chapter", "section")
  # TODO: mettre le tableau avec id, file et name
  # en entree
  rmd_lines <- readLines( in_rmd_file )

  file_name <- gsub(pattern = "([a-zA-Z0-9\\-]+)\\.Rmd", x = basename(in_rmd_file), "\\1")

  file_data <- file_part_data[grep(pattern = file_name, x = file_part_data$file),]
  # Replacing chapters title
  if ( part_type == "chapter") {

    rmd_lines <- set_chap_string(rmd_lines, file_data)

  }

  # Replacing sections title
  if (part_type == "section") {

    rmd_lines <- set_sections_string(rmd_lines, file_part_data, in_rmd_file)

  }

  writeLines(rmd_lines, out_file)


}

set_chap_string <- function(lines, file_part_data){
  lines <- try(rep_chapter_tag(lines = lines, file_part_data), TRUE)
  lines <- try(rep_pref_tag(lines = lines, file_part_data), TRUE)
  lines <- try(rep_ded_tag(lines = lines, file_part_data), TRUE)

  # removing <span id="title10"></span>

  return(lines)
}

set_sections_string <- function(lines, part_names_data, file) {
  # part_names: data.frame
  # with names :
  # "file": file path/name
  # "id"  : html part id
  # "number": number of the section
  # "level": section/sub level  1, 2, 3
  # "title": title of the section

  # <div class="section1"><a id="title11"/><h1 class="title">2.1 The simulated events</h1>

  # getting part_names_data for the current file
  file_name <- gsub(pattern = "([a-zA-Z0-9\\-]+)\\.Rmd", x = basename(file), "\\1")
  idx <- grep(pattern = file_name, part_names_data$file)
  titles <- part_names_data$id[idx]

  if (all(is.na(titles))) return(lines)

  lines_ids <- unlist(lapply(titles, function(x) grep(pattern = x, x = lines)))

  # filtering part_names
  new_sections <- dplyr::filter(part_names_data, id %in% titles) %>%
    dplyr::mutate(new_section = paste0("\n",strrep(x = "#", level + 1 ), " ", title))


  lines[lines_ids] <- new_sections$new_section

  # suppress lines with : <div class="section">
  section_lines_ids <- grep(pattern = "<div class=\"section", lines)
  lines <- lines[! 1:length(lines) %in% section_lines_ids ]

  return(lines)

}



set_img_chunks_rmd <- function(in_rmd_file, in_illust_data, out_file = in_rmd_file) {
  rmd_lines <- readLines( in_rmd_file )

  rmd_lines <- rep_illust_lines(file_lines = rmd_lines, illust_data = illust_data, file = in_rmd_file)

  writeLines(rmd_lines, out_file)
}

rep_illust_lines <- function(file_lines, illust_data, file) {
  file_name <- gsub(pattern = "([a-zA-Z0-9\\-]+)\\.Rmd", x = basename(file), "\\1")
  idx <- grepl(pattern = file_name, illust_data$file)
  illust_data <- illust_data[idx,]

  # get lines id for illust_data$src
  img_lines_ids <- unlist(lapply(illust_data$src, function(x) grep(pattern = x, file_lines)))
  title_lines_ids <- rep(TRUE,length(lines))

  if (base::is.null(img_lines_ids)) return(file_lines)

  for (i in 1:length(img_lines_ids)) {
    id <- img_lines_ids[i]
    src <- illust_data$src[i]
    text_title <- illust_data$title[i]
    name <- def_chunk_name(src)
    width <- illust_data$width[i]

    if (illust_data$kind[i] == "math" || illust_data$kind[i] == "tab" ) {
      file_lines[id] <- format_img_math_tab_chunk(chunk_name = name, math_img = src, img_width = width)
    }

    if (illust_data$kind[i] == "text" ) {
      file_lines[id] <- format_img_text_chunk(chunk_name = name, text_img = src,
                                              text_title = text_title, img_width = width )
      #rep <- format_img_text_chunk(chunk_name = name, text_img = src, text_title = text_title )
      # remove all other lines
      #file_lines[id] <- rep
      title_line_id <- grepl(pattern = paste0("id=\"",illust_data$id[i],"\""), file_lines)
      title_lines_ids <- title_lines_ids & !title_line_id
    }
  }

  file_lines <- file_lines[title_lines_ids]
  return(file_lines)
}

set_tableau_chunks_rmd <- function(in_rmd_file, illust_data, out_file = in_rmd_file, data_dir = getwd()) {

  data_dir <- file.path(book_dir, "data")
  if (! dir.exists(data_dir)) dir.create(data_dir)

  rmd_lines <- readLines( in_rmd_file )

  rmd_lines <- rep_tableau_lines(file_lines = rmd_lines,
                                 illust_data = illust_data,
                                 file = in_rmd_file,
                                 data_dir = data_dir)

  writeLines(rmd_lines, out_file)
}


rep_tableau_lines <- function(file_lines, illust_data, file, data_dir) {

  file_name <- gsub(pattern = "([a-zA-Z0-9\\-]+)\\.Rmd", x = basename(file), "\\1")

  idx <- grepl(pattern = file_name, illust_data$file)

  if (!any(idx)) return(file_lines)


  illust_data <- illust_data[idx,]
  illust_data <- illust_data[illust_data$kind == "tableau",]

  if (!nrow(illust_data)) return(file_lines)


  # get lines id for illust_data$src
  id_patt <- paste0("id=\"",illust_data$id,"\"")
  tableau_lines_ids <- unlist(lapply(id_patt, function(x) grep(pattern = x, file_lines)))

  for (i in 1:length(tableau_lines_ids)) {

    id <- tableau_lines_ids[i]
    data <- illust_data$data[[i]]
    # keeping original tableau title: replacing "_" with "-"
    chunk_name <- paste0("tab-",gsub(pattern = "_", replacement = "-",illust_data$id[i]))
    file_lines[id] <- format_html_table_chunk(chunk_name, data, illust_data$title[i], data_dir = data_dir)

  }
  return(file_lines)
}


def_chunk_name <- function(img_src, type = "fig") {

  img_num <- as.numeric(gsub(pattern = ".*_i(.*)\\.(.*)", x = img_src, replacement = "\\1"))
  sprintf("%s-img-%s", type, as.character(img_num))
}
