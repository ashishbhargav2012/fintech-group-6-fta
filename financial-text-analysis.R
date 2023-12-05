require("tm")
require("data.table")
require("dplyr")
require("SentimentAnalysis")
library("rvest")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

# Function to scrape text from RBI website
fetch_rbi_speeches <- function() {
  base_url <- "https://rbi.org.in/Scripts/BS_SpeechesView.aspx"
  speech_qty <- readline("enter the number of speeches: ")
  speech_list <- list()
  for (i in 1:speech_qty) {
    id <- readline("enter a speech id: ")
    
    speech_url <- paste0(base_url, "?Id=", id)
    
    webpage <- read_html(speech_url)
    paragraphs <- webpage %>% html_nodes(".td p:not(.footnote)") %>% html_text()
    text <- paste(paragraphs, collapse = "\n")
    speech_list[[i]] <- text
  }
  speech_corpus <- Corpus(VectorSource(speech_list))
  return(speech_corpus)
}

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
  
  net_sentiment <- rep(NA, length(corpus))
  if(length(corpus)>0){
    for (i in 1:length(corpus)) {
      speech_sentences = sentimentr::get_sentences(as.character(corpus[[i]][1]))
      
      #The loughran-mcdonald lexicon is used to assign weights to each word in a sentence
      #Valence shifters have separate weights given in the hash_valence_shifters table
      speech_sentence_sentiment = sentimentr::sentiment(speech_sentences,lexicon::hash_sentiment_loughran_mcdonald,
                                                        valence_shifters_dt = lexicon::hash_valence_shifters,
                                                        n.before = Inf, n.after = Inf)
      
      net_sentiment[i] = mean(speech_sentence_sentiment$sentiment)
    }
    return(net_sentiment)
  }
  else{
    return(net_sentiment)
  }
}

generate_wordcloud <- function(corpus){
  document_matrix <- tm::TermDocumentMatrix(corpus)
  text_matrix <- as.matrix(document_matrix)
  v <- sort(rowSums(text_matrix),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}

financial_sentiment_main <- function(){
  speech_corpus <- fetch_rbi_speeches()
  preprocessed_corpus <- preprocess_text(speech_corpus)
  net_sentiment <- calculate_sentiment(preprocessed_corpus)
  generate_wordcloud(preprocessed_corpus)
  return(net_sentiment)
}

print(financial_sentiment_main())

#---------------Sanity Test-------------#
View(preprocess_text(speech_corpus))
pre_corpus <- preprocess_text(speech_corpus)
sentences <- sentimentr::get_sentences(as.character(pre_corpus[[1]][1]))
sentence_sentiment <- sentimentr::sentiment(sentences,lexicon::hash_sentiment_loughran_mcdonald, 
                                            valence_shifters_dt = lexicon::hash_valence_shifters,
                                            n.before = Inf, n.after = Inf)
netSentiment <- mean(sentence_sentiment$sentiment)
View(sentences)