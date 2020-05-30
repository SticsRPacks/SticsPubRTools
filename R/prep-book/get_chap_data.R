library(stringr)

get_chap_data <- function(html_file) {
  cont <- readLines(html_file)
  l <- unlist(lapply(X = cont, function(x) any(grep('<div class="chapter">|<div class="frontMatter">',x))))
  line <- cont[which(l)+1]
  # minus sign not detetced in gsub command, first replacing with _
  line <- gsub(pattern = "\\-", replacement = "_", line)
  id = gsub(pattern = '<a id=\"([A-Za-z0-9_, ]+)\".*',"\\1",line)
  # hard coded but to get the right title
  line <- gsub(pattern = "</span> <span class=\"b\">operation", replacement = " operation", x = line)
  title = gsub(pattern = '.*<span class=\"b\">([A-Za-z0-9_, ]+)<.*',"\\1",line)

  # restoring minus sign
  title = gsub(pattern = "\\_", replacement = "-", title)

  # rmd link
  rmdlink <- paste0("\\@ref(",to_rmd_autolink_name(title),")")
  rmdlink_named <-  paste0("[",title,"](#",to_rmd_autolink_name(title),")")

  number <- as.numeric(gsub(x = html_file, pattern = ".*([0-9][0-9]).*", replacement = "\\1"))

  return(data.frame(file = html_file,
                    number = number,
                    id = id,
                    level = 0,
                    title = title,
                    rmdlink = rmdlink,
                    stringsAsFactors = FALSE))

}

to_rmd_autolink_name <- function(name) {
  return(gsub(pattern = " ", replacement = "-", x = str_squish(tolower(name))))
}
