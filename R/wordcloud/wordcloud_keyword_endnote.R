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

old_mar <- par("mar")

# Lire le fichier texte
# filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
# text <- readLines(filePath)

# import ref enl
enl <- "/home/plecharpent/Bureau/biblio_stics_fin2020.enl"
con <- dbConnect(RSQLite::SQLite(), enl)
table_names <- dbListTables(con)
contents <- lapply(setNames(nm = table_names), dbReadTable, con = con)

refs <- contents$enl_refs %>% mutate(n_year=as.numeric(year)) %>%
  arrange(n_year)

filter <- FALSE
from <- min(refs$n_year) - 1
to <- max(refs$n_year) + 1
if (filter) refs %>% filter(n_year > from & n_year < to ) -> refs

# add filter on years
# TODO

text <- unlist(lapply(refs[,"keywords"],
                      function(x) strsplit(x = x, split = "\r")))

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
docs <- tm_map(docs, removeWords, c("model", "stics"))
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
          colors=brewer.pal(8, "Dark2"), )

par(mar=old_mar)
