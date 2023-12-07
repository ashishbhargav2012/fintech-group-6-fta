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
    
    tryCatch(
      {
        # Attempt to read HTML from the URL
        webpage <- read_html(speech_url)
        paragraphs <- webpage %>% html_nodes(".td p:not(.footnote)") %>% html_text()
        text <- paste(paragraphs, collapse = "\n")
        speech_list[[i]] <- text
      },
      error = function(e) {
        # Handle the error
        cat("Error:", conditionMessage(e), "\n")
        
        # You can choose to return a default value, log the error, or perform other actions
        # For example, returning a message or a specific value:
        webpage <- NULL
      }
    )
  }
  speech_corpus <- Corpus(VectorSource(speech_list))
  return(speech_corpus)
}

#Function to preprocess the text corpus
preprocess_text <- function(text_corpus){
  
  #Transform entire text corpus to lower case
  text_corpus <- tm::tm_map(text_corpus, content_transformer(tolower))
  
  #Remove redundant whitespaces
  text_corpus <- tm::tm_map(text_corpus, stripWhitespace)

  text_corpus <- tm::tm_map(text_corpus, content_transformer(function(x) {
    # Remove all special characters except for periods, exclamation marks, and question marks
    x <- gsub("[^a-zA-Z0-9.!? ]", "", x)
    
    # Handle decimals: Remove periods that are surrounded by two numbers
    x <- gsub("(?<=\\d)\\.(?=\\d)", "", x, perl = TRUE)
    
    return(x)
  }))

  #Remove numbers
  text_corpus <- tm::tm_map(text_corpus, removeNumbers)
  
  #Adjust the standard stopwords so that valence shifters are included
  adjusted_sw_vector <- lexicon::sw_loughran_mcdonald_long %in% lexicon::hash_valence_shifters[[1]]
  adjusted_stopwords <- lexicon::sw_loughran_mcdonald_long[-c(which(adjusted_sw_vector == TRUE))]
  preprocessed_corpus <- tm::tm_map(text_corpus, removeWords, adjusted_stopwords)
  
  return(preprocessed_corpus)
}

#Function to calculate sentiment score of each sentence within a speech
#Mean sentiment of all sentences is then taken to calculate speech sentiment
#Iteratively looped over the entire corpus to calculate net sentiment
calculate_sentiment <- function(corpus){
  
  sentiment_list <- list()
  net_sentiment <- rep(NA, length(corpus))
  net_sentiment_bow <- rep(NA, length(corpus))
  
  if(length(corpus)>0){
    for (i in 1:length(corpus)) {
      #Bag of Words sentiment
      bow_text <- sapply(corpus[[i]][1], as.character)
      bow_sentiment <- SentimentAnalysis::analyzeSentiment(bow_text)
      net_sentiment_bow[i] <- bow_sentiment$SentimentLM
      speech_sentences <- sentimentr::get_sentences(as.character(corpus[[i]][1]))
      
      #The loughran-mcdonald lexicon is used to assign weights to each word in a sentence
      #Valence shifters have separate weights given in the hash_valence_shifters table
      speech_sentence_sentiment <- sentimentr::sentiment(speech_sentences,lexicon::hash_sentiment_loughran_mcdonald,
                                                        valence_shifters_dt = lexicon::hash_valence_shifters,
                                                        n.before = Inf, n.after = Inf)
      
      net_sentiment[i] <- mean(speech_sentence_sentiment$sentiment)
    }
    sentiment_list[[1]] <- net_sentiment_bow
    sentiment_list[[2]] <- net_sentiment
    return(sentiment_list)
  }
  else{
    return(sentiment_list)
  }
}

generate_wordcloud <- function(corpus){
  document_matrix <- tm::TermDocumentMatrix(corpus)
  text_matrix <- as.matrix(document_matrix)
  v <- sort(rowSums(text_matrix),decreasing=TRUE)
  freq_dist <- data.frame(word = names(v),freq=v)
  head(freq_dist, 10)
  
  set.seed(1234)
  wordcloud(words = freq_dist$word, freq = freq_dist$freq, min.freq = 1,
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




