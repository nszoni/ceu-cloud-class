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

