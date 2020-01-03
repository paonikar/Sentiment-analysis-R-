

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Sentiment Analysis Using R"),
  
  # Getting User Inputs
  sidebarPanel(textInput("searchTerm", "Enter data to be searched with '#'", "#"),
  sliderInput("maxTweets","Set the number of recent tweets to be used for analysis:",min=5,max=1000,value=500), 
  submitButton(text="Analyse")),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Summarised Results",HTML("<div><h3>Sentiment analysis results:</h3></div>"),
               HTML("<div><t>Total positive score:</t></div>"),verbatimTextOutput("tp"),
               HTML("<div><t>Total negative score:</t></div>"),verbatimTextOutput("tn"),
               HTML("<div><t>Overall sentiment score:</t></div>"),verbatimTextOutput("tt"),
               HTML("<div><t>Negative sentiment index:</t></div>"),verbatimTextOutput("nSI"),
               HTML("<div><t>Positive sentiment index:</t></div>"),verbatimTextOutput("pSI"),
               HTML("<div><t>Negative sentiment in percent:</t></div>"),verbatimTextOutput("nSpct"),
               HTML("<div><t>Positive sentiment in percent:</t></div>"),verbatimTextOutput("pSpct")),
      
      tabPanel("Word-Cloud",HTML("<div><h3>Most used words associated with the selected hashtag</h3></div>"),plotOutput("word")),
      
      tabPanel("Histograms",HTML("<div><h3> Graphical portrayal of opinion-mining pertinent to this hashtag
                 </h3></div>"), plotOutput("histPos"), plotOutput("histNeg"), plotOutput("histScore")),
      
      tabPanel("Pie Chart",HTML("<div><h3>Pie Chart</h3></div>"), plotOutput("piechart")),
      
      tabPanel("Analysed Tweets",HTML( "<div><h3> Tweets tabulated corresponding to their sentiment scores </h3></div>"), tableOutput("tabledata")),
      
      tabPanel("Top Users",HTML("<div><h3> Top 20 users who used this hashtag</h3></div>"),plotOutput("tweetersplot"), tableOutput("tweeterstable")),
      
      tabPanel("Trending Topics",HTML("<div>Top trending topics according to location</div>"),
               selectInput("place","Select a location",
                           c("Worldwide", "Algeria", "Argentina", "Australia", "Austria", "Bahrain", "Belarus", "Belgium", 
                             "Brazil", "Canada", "Chile", "Colombia", "Denmark", "Dominican Republic", "Ecuador", "Egypt",
                             "France", "Germany", "Ghana", "Greece", "Guatemala", "India", "Indonesia", "Ireland", "Israel",
                             "Italy", "Japan", "Jordan", "Kenya", "Korea", "Kuwait", "Latvia", "Lebanon", "Malaysia", "Mexico",
                             "Netherlands", "New Zealand", "Nigeria", "Norway", "Oman", "Pakistan", "Panama", "Peru", "Philippines", 
                             "Poland", "Portugal", "Puerto Rico", "Qatar", "Russia", "Saudi Arabia", "Singapore", "South Africa", 
                             "Spain", "Sweden", "Switzerland", "Thailand", "Turkey", "Ukraine", "United Arab Emirates", 
                             "United Kingdom", "United States", "Venezuela", "Vietnam"), selected = "United States", selectize = TRUE),
               submitButton(text="Search"),HTML("<div><h3> Location-based hot-topics, current:</h3></div>"),
               tableOutput("trendtable"),
               HTML("<div> </div>")),
      
      tabPanel("User specific hashtag-usage",textInput("user", "Analyse Twitter handle:", "@"),submitButton(text="Analyse"),plotOutput("tophashtagsplot"),HTML
               ("<div> <h3>Hashtag frequencies in the tweets of the Twitter User</h3></div>"))
      )#end of tabset panel
      )#end of main panel
  
      ))#end of shinyUI