
library(shiny)

# Install missing packages
Req_pkg<-function(x)
{x <- as.character(x)
  if (!require(x,character.only=TRUE))
  {
    install.packages(pkgs=x,repos="https://mran.microsoft.com/")
    require(x,character.only=TRUE)
  }
}

#Identifying packages required  
PrepareTwitter<-function()
{
  Req_pkg("twitteR")
  Req_pkg("stringr")
  Req_pkg("ROAuth")
  Req_pkg("RCurl")
  Req_pkg("ggplot2")
  Req_pkg("reshape")
  Req_pkg("tm")
  Req_pkg("RJSONIO")
  Req_pkg("wordcloud")
  Req_pkg("gridExtra")
  Req_pkg("plyr")
  Req_pkg("e1071")
}

PrepareTwitter()

#Server-side code
shinyServer(function(input, output, session)
{
  consumer_api_key <- "uuwtiGmzspqV5cwsGah3ZeUD1"
  consumer_api_secret <- "8l3FZF6PoDIIu5uUWbVDJTnvp4tyuqVvnK0iZ2eaA5KtrxeK5f"
  access_token <- "1191000603318771718-kXm3DIuesc2AoXrPkPVLgzNcbqye2G"
  access_token_secret <- "Q17BAWqcH6VZ96UHBguWtQPRGOiMjN9tDEqK8IwZmVWm8"
  setup_twitter_oauth(consumer_api_key, consumer_api_secret, access_token, access_token_secret)
  
  
  #Clean tweets
  Twitter_data_frame<-function(List_of_tweets)
  {
    
    df<- do.call("rbind",lapply(List_of_tweets,as.data.frame))
    #removal of emoticons
    df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #If you wish to print emoticons just comment this line
    df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text)
    return (df$text)
  }
  
  
  #Create DF using tweets
  positive_words = scan('positive_words.txt', what='character', comment.char=';') #Provide your path to "positive_words.txt" file
  negative_words = scan('negative_words.txt', what='character', comment.char=';') #Provide your path to "negative_words.txt" file
  
  database_of_words<-function()
  {
    positive_words<<-c(positive_words)
    negative_words<<-c(negative_words)
  }
  
  sentiment_of_scores <- function(sentences, positive_words, negative_words, .progress='none')
  {
    require(plyr)
    require(stringr)
    list=lapply(sentences, function(sentence, positive_words, negative_words)
    {
      sentence = gsub('[[:punct:]]',' ',sentence)
      sentence = gsub('[[:cntrl:]]','',sentence)
      sentence = gsub('\\d+','',sentence)
      sentence = gsub('\n','',sentence)
      
      sentence = tolower(sentence)
      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)
      pos.matches = match(words, positive_words)
      neg.matches = match(words, negative_words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      pp=sum(pos.matches)
      nn = sum(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)
      list1=c(score, pp, nn)
      return (list1)
    }, positive_words, negative_words)
    score_new=lapply(list, `[[`, 1)
    pp1=score=lapply(list, `[[`, 2)
    nn1=score=lapply(list, `[[`, 3)
    
    scores.df = data.frame(score=score_new, text=sentences)
    positive.df = data.frame(Positive=pp1, text=sentences)
    negative.df = data.frame(Negative=nn1, text=sentences)
    
    list_df=list(scores.df, positive.df, negative.df)
    return(list_df)
  }
  
  #Tabulate tweets with sentiment indices	
  library(reshape)
  sentiment_analysis_function<-function(result)
  {
    #Creating a copy of result data frame
    test1=result[[1]]
    test2=result[[2]]
    test3=result[[3]]
    
    #Creating three different data frames for Score, Positive and Negative
    #Removing text column from data frame
    test1$text=NULL
    test2$text=NULL
    test3$text=NULL
    #Storing the first row(Containing the sentiment scores) in variable q
    q1=test1[1,]
    q2=test2[1,]
    q3=test3[1,]
    qq1=melt(q1, var='Score')
    qq2=melt(q2, var='Positive')
    qq3=melt(q3, var='Negative') 
    qq1['Score'] = NULL
    qq2['Positive'] = NULL
    qq3['Negative'] = NULL
    #Creating data frame
    table1 = data.frame(Text=result[[1]]$text, Score=qq1)
    table2 = data.frame(Text=result[[2]]$text, Score=qq2)
    table3 = data.frame(Text=result[[3]]$text, Score=qq3)
    
    #Merging three data frames into one
    table_final=data.frame(Text=table1$Text, Positive=table2$value, Negative=table3$value, Score=table1$value)
    
    return(table_final)
  }
  
  percentage<-function(table_final)
  {
    #Positive Percentage
    
    #Renaming
    posScore=table_final$Positive
    negScore=table_final$Negative
    
    #Adding column
    table_final$PosPercent = posScore/ (posScore+negScore)
    
    #Replacing Nan with zero
    pp = table_final$PosPercent
    pp[is.nan(pp)] <- 0
    table_final$PosPercent = pp*100
    
    #Negative Percentage
    
    #Adding column
    table_final$NegPercent = negScore/ (posScore+negScore)
    
    #Replacing Nan with zero
    nn = table_final$NegPercent
    nn[is.nan(nn)] <- 0
    table_final$NegPercent = nn*100
    return(table_final)
  }
  
  database_of_words()
  
  List_of_tweets<-reactive({List_of_tweets<-searchTwitter(input$searchTerm, n=input$maxTweets, lang="en") })
  tweets<-reactive({tweets<-Twitter_data_frame(List_of_tweets() )})
  
  result<-reactive({result<-sentiment_of_scores(tweets(), positive_words, negative_words, .progress='none')})
  
  table_final<-reactive({table_final<-sentiment_analysis_function(  result() )})
  table_final_percentage<-reactive({table_final_percentage<-percentage(  table_final() )})
  
  output$tabledata<-renderTable(table_final_percentage())	
  
  #Generate wordclouds
  word_clouds<-function(text)
  {
    library(tm)
    library(wordcloud)
    corpus <- VCorpus(VectorSource(text)) #Fixed Corpus Transformation issue
    #clean text
    clean_text <- tm_map(corpus, removePunctuation)
    #clean_text <- tm_map(clean_text, content_transformation)
    clean_text <- tm_map(clean_text, content_transformer(tolower))
    clean_text <- tm_map(clean_text, removeWords, stopwords("english"))
    clean_text <- tm_map(clean_text, removeNumbers)
    clean_text <- tm_map(clean_text, stripWhitespace)
    return (clean_text)
  }
  text_word<-reactive({text_word<-word_clouds( tweets() )})
  
  output$word <- renderPlot({ wordcloud(text_word(),random.order=F,max.words=80, col=rainbow(100), main="WordCloud", scale=c(4.5, 1)) })
  
  #Generate histograms
  output$histPos<- renderPlot({ hist(table_final()$Positive, col=heat.colors(10), main="Histogram of Positive Sentiment", xlab = "Positive Score") })
  output$histNeg<- renderPlot({ hist(table_final()$Negative, col=terrain.colors(10), main="Histogram of Negative Sentiment", xlab = "Negative Score") })
  output$histScore<- renderPlot({ hist(table_final()$Score, col=topo.colors(10), main="Histogram of Score Sentiment", xlab = "Overall Score") })	
  
  #Generate pie charts
  slices <- reactive ({ slices <- c(sum(table_final()$Positive), sum(table_final()$Negative)) })
  labels <- c("Positive", "Negative")
  library(plotrix)
  output$piechart <- renderPlot({ pie3D(slices(), labels = labels, col=cm.colors(length(labels)),explode=0.00, main="Sentiment Analysis") })
  
  #Most trending hashtags
  top_trending_topics <- function(place)
  {
    avl_loc = availableTrendLocations()
    row_ind=which(avl_loc$name==place)
    woeid=avl_loc$woeid[row_ind]
    
    trend = getTrends(woeid)
    trends = trend[1:2]
    
    data_component_1 <- cbind(trends$name)
    data_component_2 <- unlist(strsplit(data_component_1, split=", "))
    data_component_3 <- grep("data_component_2", iconv(data_component_2, "latin1", "ASCII", sub="data_component_2"))
    data_component_4 <- data_component_2[-data_component_3]
    return (data_component_2)
  }
  
  trend_table<-reactive({ trend_table<-top_trending_topics(input$place) })
  output$trendtable <- renderTable(trend_table())
  
  # Top charts of a particular hashtag - Barplot
  top_tweeters<-function(tweetlist)
  {
    tweets <- twListToDF(tweetlist)
    tweets <- unique(tweets)
    # Make a table for the number of tweets per user
    d <- as.data.frame(table(tweets$screenName)) 
    d <- d[order(d$Freq, decreasing=T), ] #descending order of top charts according to frequency of tweets
    names(d) <- c("User","Tweets")
    return (d)
  }
  
  # Plot the table above for the top 20 charts
  d<-reactive({d<-top_tweeters(  List_of_tweets() ) })
  output$tweetersplot<-renderPlot ( barplot(head(d()$Tweets, 20), names=head(d()$User, 20), horiz=F, las=2, main="Top 20 users using this hashtag", col=1) )
  output$tweeterstable<-renderTable(head(d(),20))
  
  #TOP HASHTAGS OF USER
  tw1 <- reactive({ tw1 = userTimeline(input$user, n = 500) })
  tw <- reactive({ tw = twListToDF(tw1()) })
  vec1<-reactive ({ vec1 = tw()$text })
  
  hash_extraction_function = function(vec){
    
    hash.pattern = "#[[:alpha:]]+"
    have.hash = grep(x = vec, pattern = hash.pattern)
    
    hash.matches = gregexpr(pattern = hash.pattern,
                            text = vec[have.hash])
    extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
    
    df = data.frame(table(tolower(unlist(extracted.hash))))
    colnames(df) = c("tag","freq")
    df = df[order(df$freq,decreasing = TRUE),]
    return(df)
  }
  
  data_component_1<-reactive({ data_component_1 = head(hash_extraction_function(vec1()),50) })
  data_component_2<- reactive ({ data_component_2 = transform(data_component_1(),tag = reorder(tag,freq)) })
  
  p<- reactive ({ p = ggplot(data_component_2(), aes(x = tag, y = freq)) + geom_bar(stat="identity", fill = "blue")
  p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of the Twitter User") })
  output$tophashtagsplot <- renderPlot ({ p() })
  
  #Sentiment index
  t_p<-reactive({sum(table_final()$Positive)})
  t_n<-reactive({sum(table_final()$Negative)})
  t_t<-reactive({t_p()+t_n()})
  negSI<-reactive({t_n()/t_t()})
  posSI<-reactive({t_p()/t_t()})
  output$tp<-renderText({print(t_p())})
  output$tn<-renderText({print(t_n())})
  output$tt<-renderText({print(t_t())})
  output$nSI<-renderText({print(negSI())})
  output$pSI<-renderText({print(posSI())})
  output$nSpct<-renderText({print(negSI()*100)})
  output$pSpct<-renderText({print(posSI()*100)})
  
}) #shiny server
