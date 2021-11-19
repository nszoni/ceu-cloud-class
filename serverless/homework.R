
# Loading packages with pacman
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(rvest, aws.translate, aws.comprehend,
               tm, dplyr, logger, RedditExtractoR, ggplot2, stringr, wordcloud)


# AWS Credentials ---------------------------------------------------------

keyfile = list.files(path=".", pattern="accessKeys.csv", full.names=TRUE)
if (identical(keyfile, character(0))){
  stop("ERROR: AWS key file not found")
} 

keyTable <- read.csv(keyfile, header = T) # *accessKeys.csv == the CSV downloaded from AWS containing your Access & Secret keys
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

#activate
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 


# Ingest from Reddit API ------------------------------------------------

hungary1 <- find_thread_urls(subreddit="hungary", sort_by="top", period = "month") #monthly top entries

hungary2 <- hungary[!(is.na(hungary$text) | hungary$text==""), ] #drop rows with no body
hungary3 <- hungary %>% dplyr::filter(nchar(text) < 2000 & nchar(text) > 500) #filter post for limitations
hungary4 <- hungary[!grepl("http", hungary$text),] #remove posts with only url shares
hungary4$text <- paste(hungary4$title, hungary4$text)


# Process raw data --------------------------------------------------------

iterpost <- function(sub){
  start.time <- Sys.time()
  
  text <- sub[['text']]
  
  for (i in 1:length(text)){
    post = text[i]
    
    sub[i, 'nchar'] <- nchar(post)
    
    log_info('Translating the post...')
    sub[i, 'text_en'] <- translate(post, from = "auto", to = "en") #Translating
    
    log_info('Preprocessing the post...')
    post <- sub[i, 'text_en'] %>%
      removeNumbers() %>%
      removePunctuation() %>%
      stripWhitespace() %>% 
      tolower() %>% 
      removeWords(c(stopwords(kind = "en"), 'im', 'dont')) %>% 
      str_squish()
    
    sub[i, 'text_processed'] <- post
    
    log_info('Detect sentiments...')
    sub[i, 'sentiment'] <- detect_sentiment(as.character(post))['Sentiment'] #Detect sentiment
    
    log_info('Detect keyphrases...')
    keyphrases <- detect_phrases(post)
    sub[i, 'keyphrase'] <- keyphrases[order(keyphrases$Score, decreasing = TRUE),]['Text'][1,] #Detect keywords
    
    log_info('Post No. {i} processed!')
    
  }
  return(sub)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}

hungary_processed <- iterpost(hungary4)

# Post cleanup ------------------------------------------------------------

# remove clutter
hungary_view <- hungary_processed[, !names(hungary_processed) %in% c('title','subreddit','url')]

# clear rowid
rownames(hungary_view) <- 1:nrow(hungary_view)
hungary_view$id <- 1:nrow(hungary_view)

word_corpus <- unnest_tokens(hungary_view, input = text_processed, output = word) %>% 
  count(id, word, name = "frequency", sort=TRUE)

# VIZ ---------------------------------------------------------------------

# distribution of character numbers
f1 <- ggplot(data = hungary_view) +
            geom_histogram(aes(nchar))

# word cloud
set.seed(1234) # for reproducibility 
wordcloud(words = word_corpus$word,
          freq = word_corpus$frequency,
          min.freq = 1,
          max.words=15,
          random.order=FALSE,
          rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#
# Specifyig the URL 
url1 <- 'https://www.origo.hu/itthon/20210916-kiakadtak-a-szulok-az-lmbtqpropaganda-maitt.html'
url2 <- 'https://444.hu/2021/09/15/a-time-a-meseorszag-mindenkie-cimu-mesekonyvert-a-vilag-100-legbefolyasosabb-embere-koze-sorolta-redai-dorottyat'

origo <- read_html(url1)
negynegynegy <- read_html(url2)

origo_text <- html_nodes(origo, 'h2 , p, .article-title') %>% html_text()
negynegynegy_text <- html_nodes(negynegynegy, '.jq , .full-width p, .full-width .anchor') %>% html_text()

get_sentiments <- function(x){
  x = paste(x, collapse = '')
  
  # Breaking the input text into character vectors of length.segm characters each
  char.segments <- function(x, segm.length){
    byte.counter <- nchar(x)
    f <- c(1, rep(0, segm.length - 1))
    f <- cumsum(rep(f, length.out = byte.counter))
    s <- split(unlist(strsplit(x,'')), f)
    unname(sapply(s, paste, collapse = ''))
  }
  
  five.thousand.byte.chunk <- char.segments(x, 4000)
  
  # Iterating through the chunks 
  for (i in 1:length(five.thousand.byte.chunk)) { 
    current_chunk = five.thousand.byte.chunk[i]
    if (current_chunk > "") {  
      # Some cats so that you can see the chunks and their byte sum
      
      current_chunk <- translate(current_chunk, from = "auto", to = "en")
      df <- detect_sentiment(unlist(current_chunk))
      df$text = current_chunk
      
      if (!exists('sentiments_df')){
        sentiments_df = df
      } else {
        sentiments_df = rbind(sentiments_df, df)
      }
    }
  }
  return(sentiments_df)
}

o <- get_sentiments(origo_text)
n <- get_sentiments(negynegynegy_text)

