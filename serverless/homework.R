
# Loading packages with pacman
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(rvest, aws.translate, aws.comprehend,
               xml2)

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

#install.packages("rvest")
#install.packages("aws.translate", repos = c(getOption("repos"), "http://cloudyr.github.io/drat"))
#install.packages("aws.comprehend", repos = c(cloudyr = "http://cloudyr.github.io/drat", getOption("repos")))

# Specifying the URL 
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

