library(XML)
library(easyPubMed)
library(ggplot2)
library(topicmodels)
library(tidytext)
library(quanteda)
library(tidytext)
library(ggplot2)
library(dplyr)

#########

############### PART 1: extraction des textes

##### Option 1: 698 documents via une requete
Querry_String <- "cancer"
Ids <- get_pubmed_ids(Querry_String)  
# get_pubmed_ids() decompose les characteristiques de Querry_String en nombre de char et mot clef (ex: AND) et compose une querry comprehensible pour fetch_pubmed_data().
papers <- fetch_pubmed_data(Ids)
# papers est une structure type html qui contien tt les info pour tt les documents (ici +-700 doc) qui ont etes recueillis par fetch_pubmed_data()
print(papers)


Abstract <- unlist(xpathApply(papers, "//AbstractText", saveXML))
head(Abstract)
# position des "<AbstractText>"
Abstract_pos <- regexpr('<AbstractText>.*<\\/AbstractText>', Abstract)

# on enleve le <AbstractText> au debut de l' Abstract.
Abstract <- substr(Abstract, Abstract_pos + 14, Abstract_pos + attributes(Abstract_pos)$match.length - 16) 
head(Abstract)

Abstract <- Abstract[Abstract!=""]
head(Abstract)



############### PART 2: text mining

# Approche Bag of words:
NbrDoc<-100
raw <- Abstract[1:NbrDoc]

# Tokenize
tokens <- tokens(raw, what = "word", 
                 remove_numbers = TRUE, remove_punct = TRUE,
                 remove_symbols = TRUE, remove_hyphens = TRUE)

# for bigrams.
# test.tokens <- tokens_ngrams(test.tokens, n = 1:2)

# minimize capital letters
tokens <- tokens_tolower(tokens)

# stopwords
stop<-stopwords()
new_stopwords<-append(stop,c("fig.","eq.","abstracttext","p","cell","cells","proteins","protein","sub","sup","also","study"))
tokens <- tokens_select(tokens, new_stopwords, selection = "remove")

# stem
# tokens <- tokens_wordstem(tokens, language = "english")
# print(tokens)

# Create our first bag-of-words model.
tokens.dfm <- dfm(tokens, tolower = FALSE)

# Transform to a matrix and inspect.
tokens.matrix <- as.matrix(tokens.dfm)
dim(tokens.matrix)

 
#--------------------------------------------------------------------------


ap_lda <- LDA(tokens.matrix, k=20, control = list(seed = 1234))

ap_topics <- tidy(ap_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

#vector <- 1:k
#for (i in vector) {
#  N_ap_documents[i] <- filter(ap_documents,topic ==i,gamma > 0.01 ) 
#}


 