library(tidyverse)
library(stringr)
library(glue)
library(magrittr)

# Bookdown minimal example url
example_url <- "https://github.com/rstudio/bookdown-demo/archive/master.zip"

book_title <- "Conceptual Basis, Formalisations and Parameterization of the Stics Crop Model"

# Root dir
proj_dir <- "/home/plecharpent/Work/projet_stics/projet_SticsRedBook"
root_dir <- file.path(proj_dir, "Chantiers_1stEd")

# repository wc dir 
repo_dir <- file.path(root_dir,"project_SticsRedBook_1stEd_repos")

# Preparing functions to be used
func_dir = file.path(proj_dir, "Tools/R")
source(file.path(func_dir, 'source_functions.R'))
source_functions(func_dir)

# Converting epub Stics Red Book to bookdown doc

dest_dir = file.path(root_dir, "test_bookdown_out")

if( !file.exists(dest_dir)) dir.create(dest_dir)

# Getting the bookdown example
ex_file <- file.path(dest_dir, "example.zip")
download.file(example_url, destfile = ex_file )
unzip(ex_file, exdir = dest_dir)
file.remove(ex_file)
ex_book_dir <- file.path(dest_dir, "bookdown-demo-master" )

# Getting epub files
epub_dir <- file.path(dest_dir, "epub_files")
dir.create(epub_dir,showWarnings = FALSE)
unzip(zipfile = file.path(root_dir,"E-PUB/Conceptual_Basis__Formalisations_and_Parameterization_of_the_Stics_Crop_Model_ed1_v1.epub"), exdir = dest_dir)
file.rename(file.path(dest_dir,"OEBPS"), file.path(dest_dir,"docs"))
file.remove(file.path(dest_dir,"docs","toc.ncx"))
file.remove(file.path(dest_dir,"mimetype"))
docs_dir <- file.path(dest_dir,"docs")

# Copying images folder
file.copy(file.path(docs_dir,"images"), dest_dir, recursive = TRUE, overwrite = TRUE)


# Copying bookdown files
files_list <- c(list.files(path = ex_book_dir, pattern = "*.css") , "preamble.tex",
                list.files(path = ex_book_dir, pattern = "*.yml"),
                "README.md", "DESCRIPTION", # "index.Rmd", "bookdown-demo.Rproj", "_build.sh")
                "bookdown-demo.Rproj", "_build.sh")

files_to_copy <- file.path(ex_book_dir, files_list)
file.copy(files_to_copy, to = dest_dir)
file.rename(file.path(dest_dir, "bookdown-demo.Rproj"), file.path(dest_dir, "SticsRedBook_1stEd.Rproj" ))


# Generating index file
index_path <- file.path(dest_dir, "index.Rmd")

index_content <- '---
title: "Conceptual Basis, Formalisations and Parameterization of the Stics Crop Model"
author:
- name: Nadine Brisson
- name: Marie Launay
- name: Bruno Mary
- name: Nicolas Beaudoin
date: "`r Sys.Date()`"
site: bookdown::bookdown_site
output:
  bookdown::pdf_document2:
    #keep_tex: yes
    latex_engine: xelatex
    toc: yes
  bookdown::html_document2:
    toc: yes
    toc_float: yes
    toc_depth: 2
lot: True
lof: True
documentclass: book
#bibliography: [book.bib, packages.bib]
#biblio-style: apalike
#link-citations: yes
#description: "Conceptual Basis, Formalisations and Parameterization of the Stics Crop Model"
---

```{r global_options, include=FALSE}
# TODO: see which options may be usefull
#knitr::opts_chunk$set(fig.width=12, fig.height=8, fig.path=\'Figs/\',
#                      echo=FALSE, warning=FALSE, message=FALSE)
```

<!-- For justifying paragraphs : TODO add in style or css ? -->
<style>
body {
text-align: justify}
</style>

```{r fig-img-cover, out.width=\'50%\', fig.align=\'left\', echo = FALSE}
knitr::include_graphics(\'images/e9782759201693_cover.jpg\')
```



'



# Replacement in yaml files

replace_in_file(file.path(dest_dir, "_bookdown.yml"), pattern = "bookdown-demo", replacement = gsub(pattern = "\\ ",replacement = "-", x = book_title))

replace_in_file(file.path(dest_dir, "_output.yml"), pattern = "A Minimal Book Example", replacement = book_title)


# Converting files others than chapter ones

files_path <- file.path(docs_dir, c("e9782759201693_fm01.html", "e9782759201693_ded01.html", "e9782759201693_fm02.html"))

purrr::walk(files_path, html_to_Rmd)

purrr::walk2(file.path(docs_dir,
                       c("e9782759201693_fm01.Rmd", "e9782759201693_ded01.Rmd", "e9782759201693_fm02.Rmd")),
             c("Collection <i>Update Sciences & Technologies</i> {-}","Decicace {-}","Contributors {-}"),
             clean_html_rmd)

# collection
# e9782759201693_fm02.html
index_content <- c(index_content,  readLines(file.path(docs_dir, "e9782759201693_fm01.Rmd")))

# dedicace
# e9782759201693_ded01.html
index_content <- c(index_content, readLines(file.path(docs_dir, "e9782759201693_ded01.Rmd")))

# contributors
# e9782759201693_fm02.html
index_content <- c(index_content, readLines(file.path(docs_dir, "e9782759201693_fm02.Rmd")))


# Writing Index.Rmd
writeLines(text = index_content, index_path)


# Getting preface as chapter 00
r <- file.copy(file.path(docs_dir, "e9782759201693_fm03.html"), file.path(docs_dir, "e9782759201693_c00.html"))

# Change in original html files
# all the references to eq, fig, tab, section , ...
# Get the correspondence file names list
chap_files <- list.files(path = docs_dir, pattern = "(e9782759201693)_c([0-9]*).html")
# Copying chapter files
r <- file.copy(file.path(docs_dir, chap_files), dest_dir)


# corresp list
corresp_list <- get_file_corresp(chap_files)

# Replacing names in files
initial_names <- names(corresp_list)
final_names <- unlist(corresp_list, use.names = FALSE)
for (i in 1: length(initial_names)) {
  for (j in 1:length(initial_names)) {
    replace_in_file(file.path(dest_dir,initial_names[i]), initial_names[j], final_names[j])
  }
}

# renaming files
file_names_change <- purrr::map_df(list.files(dest_dir,
                                              pattern = "(e9782759201693)_c([0-9])",
                                              full.names = TRUE), rename_file)


# html_files list
html_files <- list.files(dest_dir, pattern = "-chap.html", full.names = TRUE)




####################### AVANT MODIFICATION CONTENU FICHIERS ############################


# get chapters information data
chap_data <- purrr::map_df(html_files, get_chap_data)
# ord <- order(as.numeric(gsub(pattern = "-chap.html",replacement = "",x = basename(chap_data$file))))
# chap_data <- chap_data[ord,]


# Getting sections information data
section_data <- purrr::map_df(html_files, get_section_data)


# Getting figures information data
illust_data <- purrr::map_df(html_files, get_illust_data)
# some $data values are == NULL, replacing them
# TODO: fix it in functions !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!0

illust_data$data[unlist(lapply(illust_data$data, is.null))] <- "NA"
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


################## files conversion to Rmd ########################
# replace section, and other specific syntax in html files
# get Rmd files list
rmd_files <- html_to_Rmd(html_files)



############## modification contenu Rmd ####################

# setting chapter heading # with title
for (f in rmd_files) {
  set_parts_string_rmd(in_rmd_file = f, file_part_data = chap_data)
}

# add section names from html files in rmd files,
for (f in rmd_files) {
  set_parts_string_rmd(in_rmd_file = f, file_part_data = section_data, part_type = "section")
}

# Replacing images, by chunks
for (f in rmd_files) {
  set_img_chunks_rmd(in_rmd_file = f, in_illust_data = illust_data)
}

################ Cleaning html syntax in rmd files ###########################
for (f in rmd_files) {
  clean_html_rmd(in_rmd_file = f)
}

set_parts_string_rmd(file.path(dest_dir, "index.Rmd"), file_part_data = chap_data)
clean_html_rmd(file.path(dest_dir, "index.Rmd"))

# Replacing html tables by chunks
# Generation of csv files from html tables in data directory !
for (f in rmd_files) {
  set_tableau_chunks_rmd(in_rmd_file = f, illust_data = illust_data, data_dir = "data")
}


# Pass clean_html...rm_last_line
# pour Index
# ou voir pour mettre des include pour preface, dedicace,;...
# dans le fichier index !



# Cross referencing ...
# Using chap data and sections data
# link to chap
# § 2
# links to parts
# § 3.2.2.b

# links to figures, tab, equations
# <a href="11-chap.html#int_118" class="ref_int"><span>Figure 11.2</span></a>
# <a href="2-chap.html#int_123" class="ref_int"><span>Table 2.1</span></a>
# <a href="6-chap.html#int_248" class="ref_int"><span>eq. 6.31</span></a>


# links url
# <span class="u"><a href="http://www.isci.it/tools"><span>http://www.isci.it/tools</span></a></span>



################### Generating the book ############################
# setwd(dest_dir)
#bookdown::render_book('index.Rmd', 'bookdown::html_document2')
# bookdown::render_book('index.Rmd', 'bookdown::gitbook')



######### saving meta data / chap, section, illust ##########################
book_struct <- rbind(chap_data, section_data) %>%
  mutate(file = basename(file)) %>% 
  select(-rmdlink) %>%
  write.csv2(file = file.path(dest_dir,"data","book-struct-meta-data.csv"), 
             quote = TRUE, row.names = FALSE)

illust_data %>% select(-data, -title_lines_id,-img_lines_id,-name, -width) %>% 
  mutate(file = basename(file)) %>% 
  mutate(num=gsub(pattern = "_",x = num, replacement = ".")) %>%
  write.csv2(file = file.path(dest_dir,"data","book-illust-meta-data.csv"), 
             quote = TRUE, row.names = FALSE)



######################## copie des fichiers dans project_SticsRedBook_1stEd_repos ###############

# *.Rmd, *.tex, *.yaml, *.css
files_list <- list.files(pattern = "\\.(Rmd|tex|yml|css|Rproj)$", path = dest_dir)
file.copy(from = files_list, to = repo_dir)

# ajout les repertoires data (csv crees a partir des tableaux html), images
file.copy(from = file.path(dest_dir,"data"), to = repo_dir, recursive = TRUE)

file.copy(from = file.path(dest_dir,"images"), to = repo_dir, recursive = TRUE, overwrite = FALSE)


# voir fichiers README.md, DESCRIPTION, LICENSE: quoi y mettre ?

# versionnement des fichiers


