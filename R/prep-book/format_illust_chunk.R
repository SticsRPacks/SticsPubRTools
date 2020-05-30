# Formatting chunks from data.frames data


format_img_math_tab_chunk <- function(chunk_name, math_img, img_width) {
  return(format_img_chunk(chunk_name = chunk_name,img_src = math_img, img_width = img_width))
}

format_img_text_chunk <- function(chunk_name, text_img, text_title, img_width) {
  return(format_img_chunk(chunk_name = chunk_name,img_src = text_img, img_title = text_title, img_width = img_width))
}


format_img_chunk <- function(chunk_name = NULL, img_src, img_title=NULL, img_width = NULL) {

  # ```{r tab-img-101, out.width='80%', fig.align='center', fig.cap='Table of the available options of formalisms for the microclimate calculation in STICS', echo = FALSE}
  # knitr::include_graphics('images/tab-img-101.jpg')
  # ```
  cap <- ""
  if(!base::is.null(img_title)) {
    cap <- paste0(", fig.cap='",img_title,"'")
  }
  name <- ""
  if (!base::is.null(chunk_name)) name <- chunk_name
  width <- ""
  if (!base::is.null(img_width)) width <- paste0(", out.width='",img_width,"%'")

   l <- vector(mode = "character", 3)
  l[1] <- paste0("\n```{r ",name, width,", fig.align='center'",cap ,", echo = FALSE","}")
  l[2] <- paste0("knitr::include_graphics('",img_src,"')")
  l[3] <- "```\n"

  paste(l,collapse = "\n")

}


format_html_table_chunk <- function(chunk_name, tab_text, tab_title, data_dir = NULL) {

  library("rvest")
  l <- vector(mode = "character", 6)
  l[2] <- ""

  if (!base::is.null(data_dir)){
    data <- html_table2df(tab_text)
    csv_file <- file.path(data_dir, paste0(chunk_name,".csv"))
    write.table(x = data, file = csv_file, row.names = FALSE, sep = ";", quote = TRUE)
    get_data <- paste0("df_tab <- read.table('",csv_file,"', sep=';', header = TRUE, stringsAsFactors = FALSE)")
  } else {
    l[2] <- "library('rvest')"
    if (length(tab_text) > 1) tab_text <- paste(tab_text, collapse="\n")
    get_data <- paste0("df_tab <- html_table2df('",tab_text,"')")
  }

  # if (length(tab_text)) tab_text <- paste(tab_text, collapse="\n")

  l[1] <- paste0("\n```{r ",chunk_name,", echo = FALSE", "}")
  l[3] <- get_data
  l[4] <- "col_names <- gsub(pattern = \"\\\\.\", names(df_tab), replacement = \" \")"
  l[5] <- paste0("knitr::kable(df_tab, caption = '",tab_title,"', col.names = col_names)")
  l[6] <- "```\n"

  paste(l,collapse = "\n")
}



# Html table
# SEE : test_html_tables.Rmd
# Convert an html table text to a data.frame with column names
html_table2df <- function(html_txt) {
  if (length(html_txt) > 1) html_txt <- paste(html_txt, collapse="")
  html_loaded <- read_html(html_txt)
  df_tab <- html_table(html_loaded, fill=TRUE) %>% as.data.frame(row.names = NULL)
  if (length(grep(pattern = "X1",x=names(df_tab)))) {
    names(df_tab) <- df_tab[1,]
    df_tab <- df_tab[2:nrow(df_tab),]
  }
  return(df_tab)
}
