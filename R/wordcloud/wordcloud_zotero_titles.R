# source : http://www.sthda.com/french/wiki/text-mining-et-nuage-de-mots-avec-le-logiciel-r-5-etapes-simples-a-savoir
# Installer
# install.packages("tm")  # pour le text mining
# install.packages("SnowballC") # pour le text stemming
# install.packages("wordcloud") # générateur de word-cloud
# install.packages("RColorBrewer") # Palettes de couleurs
# Charger
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("DBI")
library("RSQLite")
library("dplyr")

old_mar <- par("mar")

bib_EPS <- SticsPubRTools:::get_references(library_name = "livre_rouge",
                                                   collection_name = "EPS_stics")

# Generating a data.frame
refs <- data.frame(title=unlist(lapply(bib_EPS, function(x) x$title)),
                         year=as.numeric(unlist(lapply(bib_EPS, function(x) x$year))),
                   stringsAsFactors = FALSE) %>%
  arrange(year)


filter <- FALSE
from <- min(refs$year) - 1
to <- max(refs$year) + 1
if (filter) refs %>% filter(year > from & year < to ) -> refs

# add filter on years
# TODO

text <- tolower(unlist(lapply(refs[["title"]],
                      function(x) strsplit(x = x, split = " "))))

# Getting words from titles



# Charger les données comme un corpus
docs <- Corpus(VectorSource(text))

# inspection du contenu
#inspect(docs)


# nettoyage du doc
# Convertir le texte en minuscule
docs <- tm_map(docs, content_transformer(tolower))
# Supprimer les nombres
docs <- tm_map(docs, removeNumbers)
# Supprimer les mots vides anglais
docs <- tm_map(docs, removeWords, stopwords("english"))
# Supprimer votre propre liste de mots non désirés
# TODO: faire une liste de mots à supprimer
to_remove_fr <- c("sur", "des", "stics", "les", "dans", "{", "}",".",
                  "à", "du") #, "(", ")", ":", ";", "?")
to_remove_eng <- c("across", "due", "model", "new", "use", "using",
                   "of", "and", "the", "in", "for", "on", "a",
                   "to", "their", "and", "three")
to_remove <- c(to_remove_fr, to_remove_eng )
docs <- tm_map(docs, removeWords, to_remove)
# Supprimer les ponctuations
docs <- tm_map(docs, removePunctuation)
# Supprimer les espaces vides supplémentaires
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


# Construire la matrice des mots
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


# genetation du nuage
# remove margins
par(mar = rep(0, 4))

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

# high quality output
# library(rsvg)
# library(svglite)
# #svglite("plot.svg", width = 10, height = 7, pointsize = 15, bg = "white")
# svglite("plot.svg")
# wordcloud(words = d$word, freq = d$freq, min.freq = 1,
#           max.words=150, random.order=FALSE, rot.per=0.35,
#           colors=brewer.pal(8, "Dark2"))
# dev.off()
#
# rsvg_png("plot.svg", "stics_wordcloud_zotero_titles.png")



par(mar=old_mar)
