require("tm")
require("data.table")
require("dplyr")
require("SentimentAnalysis")

#load speech documents
speech_corpus <- Corpus(DirSource("Downloads/fintech-group-6-fta/data/"))
View(speech_corpus)

#Function to preprocess the text corpus
# Stemming/Lemmatization is optional in preprocessing
preprocess_text <- function(text_corpus){
  
  #Transform entire text corpus to lower case
  text_corpus <- tm::tm_map(text_corpus, content_transformer(tolower))
  
  #Remove redundant whitespaces
  #Ex: 'text   corpus' is converted to 'text corpus'
  text_corpus <- tm::tm_map(text_corpus, stripWhitespace)
  
  #text_corpus <- tm::tm_map(text_corpus, removePunctuation)
  
  #Remove numbers
  text_corpus <- tm::tm_map(text_corpus, removeNumbers)
  
  #Adjust the standard stopwords so that valence shifters are included
  #Valence shifters are words that change the polarity of the sentence
  #Even without having much standalone meaning to themselves
  #Types: Amplifiers, deamplifiers, adversary conjunctions
  adjusted_sw_vector <- lexicon::sw_loughran_mcdonald_long %in% lexicon::hash_valence_shifters[[1]]
  adjusted_stopwords <- lexicon::sw_loughran_mcdonald_long[-c(which(adjusted_sw_vector == TRUE))]
  preprocessed_corpus <- tm::tm_map(text_corpus, removeWords, adjusted_stopwords)
  
  return(preprocessed_corpus)
}

#Function to calculate sentiment score of each sentence within a speech
#Mean sentiment of all sentences is then taken to calculate speech sentiment
#Iteratively looped over the entire corpus to calculate net sentiment
calculate_sentiment <- function(corpus){
  preprocessed_corpus <- preprocess_text(corpus)

  net_sentiment <- rep(NA, length(preprocessed_corpus))

  for (i in 1:length(preprocessed_corpus)) {
    speech_sentences = sentimentr::get_sentences(as.character(preprocessed_corpus[[i]][1]))

    #The loughran-mcdonald lexicon is used to assign weights to each word in a sentence
    #Valence shifters have separate weights given in the hash_valence_shifters table
    speech_sentence_sentiment = sentimentr::sentiment(speech_sentences,lexicon::hash_sentiment_loughran_mcdonald,
                                                     valence_shifters_dt = lexicon::hash_valence_shifters,
                                                     n.before = Inf, n.after = Inf)

    net_sentiment[i] = mean(speech_sentence_sentiment$sentiment)
  }
  return(net_sentiment)
}

calculate_sentiment(speech_corpus)

#---------------Sanity Test-------------#
View(preprocess_text(speech_corpus))
pre_corpus <- preprocess_text(speech_corpus)
sentences <- sentimentr::get_sentences(as.character(pre_corpus[[1]][1]))
sentence_sentiment <- sentimentr::sentiment(sentences,lexicon::hash_sentiment_loughran_mcdonald, 
                                            valence_shifters_dt = lexicon::hash_valence_shifters,
                                            n.before = Inf, n.after = Inf)
netSentiment <- mean(sentence_sentiment$sentiment)
View(sentences)