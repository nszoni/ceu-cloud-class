###########################################
#                                         #
#        Reddit Sentiments with           #
#        AWS AI Services and R            #
#        Author: Son N. Nguyen            #
#           Date: 11/20/2021              #
#                                         #
###########################################


# Setup -------------------------------------------------------------------

# Loading packages with pacman
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(aws.translate, aws.comprehend,
               tm, tidytext, SnowballC, 
               tidyverse, logger, RedditExtractoR,
               stringr, wordcloud, data.table)

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

#drop rows with no body
hungary2 <- hungary1[!(is.na(hungary1$text) | hungary1$text==""), ]
#glue title and text
hungary2$text <- paste(hungary2$title, hungary2$text)
#remove clutter
hungary3 <- hungary2[, !names(hungary2) %in% c('title','subreddit','url')]
#filter post for limitations
hungary4 <- hungary3 %>% dplyr::filter(nchar(text) < 2000 & nchar(text) > 200) 
#remove posts with only url shares
hungary5 <- hungary4[!grepl("http", hungary4$text),] 

# assign rowid
rownames(hungary5) <- 1:nrow(hungary5)
hungary5$id <- 1:nrow(hungary5)

# Process raw data --------------------------------------------------------

subreddit_processer <- function(sub){
  #start timer
  start.time <- Sys.time()
  
  #extract vector of post bodies
  text <- sub[['text']]
  
  for (i in 1:length(text)){
    post = text[i]
    
    sub[i, 'nchar'] <- nchar(post)
    
    log_info('Preprocessing post No. {i}')
  
    # use language detection and translate non-english posts
    if (detect_language(post)['LanguageCode'] != 'en'){
      log_info('Translating...')
      sub[i, 'text_en'] <- translate(post, from = "auto", to = "en")
    } else {
      sub[i, 'text_en'] <- post
    }
  
    post <- sub[i, 'text_en'] %>%
      removeNumbers() %>%
      removePunctuation() %>%
      stripWhitespace() %>% 
      tolower() %>% 
      removeWords(tm::stopwords(kind = "en")) %>% #remove stopwords
      str_squish() #squish multiple subsequent spaces
    
    sub[i, 'text_processed'] <- post
    
    log_info('Detect sentiments...')
    
    sub[i, 'sentiment'] <- detect_sentiment(as.character(post))['Sentiment']
    
    log_info('Detect keyphrases...')
    
    keyphrases <- detect_phrases(post)
    
    # keep the keyphrase with the highest score
    sub[i, 'keyphrase'] <- keyphrases[order(keyphrases$Score, decreasing = TRUE),]['Text'][1,] 
    
    log_info('Post No. {i} processed!')
    
  }

  #stop timer
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  return(sub)
}

hungary_processed <- subreddit_processer(hungary5)

# Post cleanup ------------------------------------------------------------

# extract week of year
hungary_processed$weekofyear <- strftime(hungary_processed$date_utc, format='%V')

# tokenize keyphrases
word_corpus <- unnest_tokens(hungary_processed, input = keyphrase, output = word) %>% 
  count(id, word, name = "frequency")

# stemming
word_corpus$word <- wordStem(word_corpus$word, language = 'en')

# create word corpus and add up dedup
word_corpus_dedup <- word_corpus %>% 
  group_by(word) %>% 
  mutate(frequency = sum(frequency)) %>%
  arrange(desc(frequency)) %>% 
  distinct(word, .keep_all = T)

# VIZ ---------------------------------------------------------------------

# distribution of character numbers
dist_characters <- ggplot(data = hungary_processed, aes(nchar)) +
                    geom_histogram(aes(y=..density..), colour="firebrick4", fill="firebrick1")+
                    geom_vline(aes(xintercept=mean(nchar)),
                               color="firebrick3", linetype="dashed", size=1, alpha = 0.5) +
                    geom_text(aes(mean(nchar), 0.0015, label=paste0("Mean: ",round(mean(nchar), 2))), hjust = -0.5) +
                    labs(title = 'Character Distribution of Posts',
                         caption = 'r/hungary',
                         x = 'Number of characters',
                         y = 'Density') +
                    theme_bw()

# word cloud of keywords
set.seed(1234) # for reproducibility 

wordcloud(words = word_corpus$word,
            freq = word_corpus$frequency,
            min.freq = 1,
            max.words=30,
            random.order=FALSE,
            rot.per=0.35,
            colors=brewer.pal(8, "Dark2"))

# weekly activities
weekly_activity <- ggplot(data = hungary_processed %>% group_by(weekofyear) %>%
                         summarise(total=sum(comments)), aes(x=weekofyear, y=total, group = 1)) +
                    geom_point(color = 'red', size = 4) +
                    geom_line(color = 'red', lwd = 1) +
                    geom_hline(aes(yintercept=mean(total)),
                             color="red", linetype="dashed", size = 1, alpha = 0.5) +
                    geom_text(aes(1, mean(total), label=paste0("Mean: ",mean(total))), vjust = -1) +
                    labs(title = "Weekly Activity in Last Month's Top",
                         caption = 'r/hungary',
                         x = 'Week of Year',
                         y = 'Comments') +
                    scale_y_continuous(labels = scales::unit_format(unit = "K", scale = 1e-3)) +
                    theme_bw()
  
# weekly sentiments
weekly_sentiments <- ggplot(data = hungary_processed, aes(fill=sentiment, x=weekofyear)) + 
                      geom_bar(position='fill') +
                      labs(title = 'Weekly Post Sentiments in the Last Month',
                           caption = 'r/hungary',
                           x = 'Week of Year',
                           y = 'Share') +
                      scale_y_continuous(labels = scales::percent) +
                      scale_fill_viridis_d(name = 'Sentiment') +
                      theme_bw()

# most active negative posts by keyphrases
negative <- hungary_processed %>%
              filter(sentiment == 'NEGATIVE') %>% 
              arrange(desc(comments)) %>%
              slice(1:5) %>%
              ggplot(., aes(x=reorder(keyphrase, comments) , y=comments))+
              geom_bar(stat='identity', color = 'firebrick4', fill = 'firebrick1',  width = 0.8) +
              labs(title = 'Most Active Negative Posts',
                   subtitle = 'by keyphrases',
                   caption = 'r/hungary',
                   x = '',
                   y = 'Comments') +
              theme_bw() +
              coord_flip()

# most active positive posts by keyphrases

positive <- hungary_processed %>%
              filter(sentiment == 'POSITIVE') %>% 
              arrange(desc(comments)) %>%
              slice(1:5) %>%
              ggplot(., aes(x=reorder(keyphrase, comments) , y=comments))+
              geom_bar(stat='identity', color = 'green4', fill = 'green1',width = 0.8) +
              labs(title = 'Most Active Positive Posts',
                   subtitle = 'by keyphrases',
                   caption = 'r/hungary',
                   x = '',
                   y = 'Comments') +
              theme_bw() +
              coord_flip()
  