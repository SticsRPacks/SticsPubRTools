
get_illust_pattern_data <- function() {

  illust_pattern <- list()
  illust_pattern$illust_name$text <- "Figure"
  illust_pattern$illust_name$math <- "eq."
  illust_pattern$illust_name$tab <- "Table"

  illust_pattern$num_pattern$text <- '.*>Figure([0-9\\.\\ a-z]+)<.*'
  illust_pattern$num_pattern$math <- '.*>eq.([0-9\\.\\ a-z]+)<.*'
  illust_pattern$num_pattern$tab <- '.*>Table([0-9\\.\\ a-z]+)<.*'
  illust_pattern$image_pattern$str_format <- '.*\"image_%s\".*src=\"([A-Za-z0-9_, \\/\\.]+)\".*'
  illust_pattern$title_pattern$tab <- '.*(Table[0-9\\.\\ a-z]+)(<\\/span>)?(<\\/span>)?(</a>)?(<span class=\"b\">)?(.)?<\\/span>(</a>)?([§ôΣ✄&°èéA-Za-z0-9\\–\\’\\?\\“\\”\\_\\,\\;\\.\\(\\)\\:\\<\\>\\/\\-\\% \\#\\"\\~\\=\\*]+)<\\/span>.*'
  illust_pattern$title_pattern$text<- '.*(Figure[0-9\\.\\ a-z]+)(<\\/span>)?(<\\/span>)?(</a>)?(<span class=\"b\">)?(.)?<\\/span>(</a>)?([§ôΣ✄&°èéA-Za-z0-9\\–\\’\\?\\“\\”\\_\\,\\;\\.\\(\\)\\:\\<\\>\\/\\-\\% \\#\\"\\~\\=\\*]+)<\\/span>.*'
  illust_pattern$title_pattern$math <- '.*>(eq.[0-9\\.\\ ]+)<.*'
  illust_pattern$id_pattern <- '.*<a id=\"(int_[0-9]+)\".*'
  return(illust_pattern)
}



get_illust_data <- function(file_path) {


  content <- readLines(file_path)

  illust_pattern <- get_illust_pattern_data()

  df_images_text <- get_illust_data_text(content, file_path, 'text', illust_pattern)

  df_images_math <- get_illust_data_text(content, file_path, 'math', illust_pattern)

  df_images_tab <- get_illust_data_text(content, file_path, 'tab', illust_pattern)

  df_tableaux <- get_illust_data_tableau(content, file_path, illust_pattern)


  return(rbind(df_images_text, df_images_math, df_images_tab, df_tableaux))

}

get_illust_data_tableau <- function(content, file_path, illust_pattern_data) {
  num_pattern <- illust_pattern_data$num_pattern$tab

  illust_pattern <- '<div class=\"tableau'
  title_pattern <- illust_pattern_data$title_pattern$tab
  id_pattern <- illust_pattern_data$id_pattern


  titles_idx <- which(unlist(lapply(X = content, function(x) any(grep(pattern = illust_pattern,x = x)))))
  titles_cont <- content[titles_idx]

  if (!length(titles_idx)) return()

  tables_deb <- grep(pattern = "<table", x = content)
  tables_end <- grep(pattern = "</table>", x = content) # supprimer </div>
  tables_data <- get_tableau_content(content, tables_deb, tables_end)

  titles = trimws(gsub(pattern = title_pattern, "\\8",titles_cont), which = "both")
  id = gsub(pattern = id_pattern, "\\1",titles_cont)

  num <- trimws(gsub(pattern = num_pattern, "\\1",titles_cont))
  num = gsub( pattern = "\\.",replacement =  "_", x = num)

  df <- data.frame(file = file_path,
                   kind = "tableau",
                   id = id,
                   num = num,
                   src = "NA",
                   width = "NA",
                   name = "NA",
                   title = titles,
                   img_lines_id = NA,
                   title_lines_id = titles_idx,
                   stringsAsFactors = F)
  df$data <- tables_data
  return(df)
}

get_illust_data_text <- function(content, file_path, kind, illust_pattern_data) {

  illust_pattern <- paste0('<div class=\"illustype_image_', kind )

  num_pattern <- illust_pattern_data$num_pattern[[kind]]
  image_pattern <- sprintf(illust_pattern_data$image_pattern$str_format, kind )
  id_pattern <- illust_pattern_data$id_pattern

  titles_idx <- which(unlist(lapply(X = content, function(x) any(grep(pattern = illust_pattern,x = x)))))

  if (!length(titles_idx)) return()

  if ( kind == "text" ){
    title_lines_id <- titles_idx + 1
  } else {
    title_lines_id <- titles_idx
  }

  titles_cont <- content[title_lines_id]

  title_pattern <- illust_pattern_data$title_pattern[[kind]]

  rep_str <- "\\8"
  if ( kind == "math") {
    rep_str <- "\\1"
  }

  # getting images information
  img_idx <- which(unlist(lapply(X = content, function(x) any(grep(pattern = image_pattern,x = x)))))
  img_cont <- content[img_idx]
  # minus sign not detected in gsub command, first replacing with _
  img_cont <- gsub(pattern = "\\-", replacement = "_", img_cont)
  img_src = gsub(pattern = image_pattern, "\\1",img_cont)

  # defining chunk name
  name <- def_chunk_name(img_src)

  # getting width
  width_patt <- ".*style=\"width:([0-9]+).*"
  width <- gsub(pattern = width_patt, x = img_cont, replacement = "\\1")


  # getting illustation number
  num <- trimws(gsub(pattern = num_pattern, "\\1",titles_cont))
  num = gsub( pattern = "\\.",replacement =  "_", x = num)
  num[grep(pattern = "<", x = num)] <- "NA"


  titles_cont <- gsub(pattern = "\\-", replacement = "_", titles_cont)

  # getting html id
  id = gsub(pattern = id_pattern, "\\1",titles_cont)
  id[grep(pattern = "<", x = id)] <- "NA"


  # Ne sont pas pris en compte avec l'expression apres !
  #titles_cont <- gsub(pattern = "=",replacement = "~",titles_cont)
  titles_cont <- gsub(pattern = "\\[",replacement = " ",titles_cont)
  titles_cont <- gsub(pattern = "\\]",replacement = " ",titles_cont)
  titles = trimws(gsub(pattern = title_pattern, replacement = rep_str, x = titles_cont), which = "both")
  titles[grep(pattern = illust_pattern, x = titles)] <- "NA"

  # remplacement des <sub>X</sub> et sup ... ?

  if (!length(id)) id <- NA
  if (!length(num)) num <- NA
  if (!length(img_src)) img_src <- NA
  if (!length(titles)) titles <- NA

  # Filtering lines ids for existing titles
  title_lines_id[titles == "NA"] <- NA

  return(data.frame(file = file_path,
                    kind = kind,
                    id = id,
                    num = num,
                    src = img_src,
                    name = name,
                    width = width,
                    title = titles,
                    img_lines_id = img_idx,
                    title_lines_id = title_lines_id,
                    data = NA,
                    stringsAsFactors = F))

}

get_title_img_list <- function() {
  lst <- vector(mode = "list", length(titles_idx))
  for (i in titles_idx) {
    first <- i + 1

  }
}

get_tableau_content <- function(content, ids_deb, ids_end){
  ids <- cbind(ids_deb, ids_end)
  tab_nb <- nrow(ids)
  tab_list <- vector(mode = "list", tab_nb)
  for (i in 1:tab_nb) {
    content[ids[i,2]] <- gsub(pattern = "</div>", replacement = "",content[ids[i,2]] )
    tab_list[[i]] <- content[ids[i,1]:ids[i,2]]
    tab_list[[i]]
  }
  return(tab_list)
}



lines_idx_mat2list <- function(idx_mat) {
  unlist(apply(idx_mat,MARGIN = 1,list), recursive = F)
}
