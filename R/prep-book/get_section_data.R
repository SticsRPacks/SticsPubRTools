get_section_data <- function(file) {

  content <- readLines(file)

  # Removing character emphasis,...
  content <- clean_html_string(content, suppress = TRUE)

  l <-unlist(lapply(X = content, function(x) any(grep('<div class="section',x))))
  line <- content[which(l)]
  # minus sign not detected in gsub command, first replacing with _
  line <- gsub(pattern = "\\-", replacement = "_", line)
  id = gsub(pattern = '.*<a id=\"([A-Za-z0-9_, ]+)\".*',"\\1",line)
  level <- lapply(gsub(pattern = '<div class=\"([A-Za-z0-9_, ]+)\".*',"\\1",line), function(x) substr(x,8,nchar(x)))
  # ne marche pas dans le cas ou il -y a un "-" dans la chaine de caracteres
  # ni si il y a un autre <span class="...
  title_full = gsub(pattern = ".*<h1 class=\"title\">([A-Za-z0-9\\,_\\.\\:\\-\\/\\(\\)\\'\\’\\>\\<\\ ]+)</h1>","\\1",line)
  # restoring minus sign
  title_full = gsub(pattern = "\\_", replacement = "-", title_full)
  split_title = strsplit(title_full, split = " ")

  number = lapply(split_title, function(x) x[1])
  title = lapply(split_title, function(x) paste(x[2:length(x)], collapse = " "))

  # rmd link
  rmdlink <- paste0("\\@ref(",to_rmd_autolink_name(title),")")
  rmdlink_named <-  paste0("[",title,"](#",to_rmd_autolink_name(title),")")

  if (!length(id)) id <- NA
  if (!length(number)) number <- NA
  if (!length(title)) title <- NA
  if (!length(level)) level <- NA

  section_data <- data.frame(file = file,
                             id = id,
                             number = unlist(number),
                             level = as.numeric(unlist(level)),
                             title = unlist(title),
                             rmdlink = rmdlink,
                             stringsAsFactors = F)
  section_data <- section_data[!is.na(section_data$id),]

  return(section_data)

}
