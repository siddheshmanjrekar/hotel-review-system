# Define a server for the Shiny app
# Load
library("tm")
library("swirl")
library("tmap")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("ggplot2")
library("syuzhet")
library("devtools")
require(devtools)
library("plyr")
library("sentiment")
library("shinyjs")

function(input, output) {
  
  hide("image2")
  observeEvent(input$Sentiment_WC, {
    toggle("image2")
  })
  
  hide("bar")
  observeEvent(input$Bar_Graph, {
    toggle("bar")
  })
  
  hide("cloud")
  observeEvent(input$Word_Cloud, {
    toggle("cloud")
  })
  
  hide("sentiment")
  observeEvent(input$Sentiment_Analysis, {
    toggle("sentiment")
  })
  
  
  terms1 <- reactive({
    print(input$city)
    
    getTermMatrix1(input$city)
  })
  
  output$choose_dataset <- renderUI({
    selectInput('in4', 'Select Hotel', c(Choose='', terms1()), selectize=TRUE)
  })
  
  terms <- reactive({
    print(input$in4)

    getTermMatrix(input$in4)
  })
  
  terms3 <- reactive({
    getTermMatrix3(input$in4)
  })
  
  terms4 <- reactive({
    getTermMatrix4(input$in4)
  })
  scr <- reactive({
    getSCR(input$in4)
  })
  ccr <- reactive({
    getCCR(input$in4)
  })
  rcr <- reactive({
    getRCR(input$in4)
  })

  
  # Fill in the spot we created for a plot
  output$cloud <- renderPlot({

    validate(need(
      input$city, ''
    ))
    
    validate(need(
      input$in4, ''
    ))
    
    v <- terms()
    d <- data.frame(word = names(v),freq=v)
    head(d, 10)
    
    set.seed(1234)
    
    withProgress(message = 'Processing...',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:10) {
                     incProgress(1/5)
                     Sys.sleep(0.2)
                   }
                 })
    
    wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
    
  })
  
  output$ex1 <- renderUI({
    validate(need(
      input$city, ''
    ))
    
    validate(need(
      input$in4, ''
    ))
    
    s<-findAssocs(terms3(), terms = "room", corlimit = 0.45)
    helpText(s)
  })
  
  output$bar <- renderPlot({

    validate(need(
      input$city, ''
    ))
    
    validate(need(
      input$in4, ''
    ))
    
    v <- terms()
    d <- data.frame(word = names(v),freq=v)
    head(d, 10)
    
    set.seed(1234)
    
    withProgress(message = 'Processing...',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:10) {
                     incProgress(1/5)
                     Sys.sleep(0.2)
                   }
                 })
    
    barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col ="red", main ="Most frequent words", ylab = "Word frequencies")
  })
  
  
  output$sentiment <- renderPlot({
    
    validate(need(
      input$city, ''
    ))
    
    validate(need(
      input$in4, ''
    ))
    
    v <- terms()
    d <- data.frame(word = names(v),freq=v)
    head(d, 10)
    
    set.seed(1234)
    
    #fetch sentiment from words
    texts<- readLines(filePath)
    Sentiment<- get_nrc_sentiment(texts)
    text<-cbind(texts,Sentiment)
    
    #Count the sentiment words by caegory
    TotalSentiment <- data.frame(colSums(text[,c(2:11)]))
    names(TotalSentiment) <- "count"
    TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment ), TotalSentiment )
    rownames(TotalSentiment ) <-NULL
    
    
    #total segment scores of all texts
    p<- ggplot(data=TotalSentiment, aes(x = sentiment, y=count))+
      geom_bar(aes(fill=sentiment),stat="identity")+
      theme(legend.position= "none")+
      xlab("Sentiment")+ylab("Total Count") +ggtitle("Total Sentiment Score")
    
    withProgress(message = 'Processing...',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:35) {
                     incProgress(1/35)
                     Sys.sleep(0.2)
                   }
                 })
    print(p)
  })
  
  
  
  output$image2 <- renderImage({
    
    validate(need(
      input$city, ''
    ))
    
    validate(need(
      input$in4, ''
    ))
    
    myPath=paste("C:/Users/Sid Pc/Documents/Sent/",terms4(),".png", sep="")
    
    # Return a list containing the filename
    list(src = myPath,
         contentType = 'image/png',
         width = 700,
         height = 420,
         style="display: block; margin-left: auto; margin-right: auto;",
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  output$ServiceCriticReview  <- renderUI({
    validate(need(
      input$city, ''
    ))
    
    validate(need(
      input$in4, ''
    ))
    
    
    if(length(scr())==0){
      HTML(paste(h3("Top Review on Hotel \"Service\""),"No Service Specific comments found",sep = '<br/>'))
    }else{
      HTML(paste(h3("Top Review on Hotel \"Service\""),scr(),hr(),sep = '<br/>'))
    }
    
  })

  output$CleanlinessCriticReview  <- renderUI({
    validate(need(
      input$city, ''
    ))
    
    validate(need(
      input$in4, ''
    ))
    
    
    if(length(ccr())==0){
      HTML(paste(h3("Top Reviews on Hotel \"Cleanliness\""),"No Cleanliness Specific comments found",sep = '<br/>'))
    }else{
      HTML(paste(h3("Top Reviews on Hotel \"Cleanliness\""),ccr(),hr(),sep = '<br/>'))
    }
    
  })
  
  output$RoomsCriticReview  <- renderUI({
    validate(need(
      input$city, ''
    ))
    
    validate(need(
      input$in4, ''
    ))
    
    
    if(length(rcr())==0){
      HTML(paste(h3("Top Reviews on Hotel \"Rooms\""),"No Rooms Specific comments found",sep = '<br/>'))
    }else{
      HTML(paste(h3("Top Reviews on Hotel \"Rooms\""),rcr(),hr(),sep = '<br/>'))
    }
    
  })

  
  
  
}