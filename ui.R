library(shiny)

shinyUI(fluidPage(
    tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Roboto');
@import url('https://fonts.googleapis.com/css?family=Prosto One');
  ")),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "my_styles.css"),
        tags$link(rel = "canonical", href="http://shiny.ellisco.com.au/twitter-monitor/")
    ),
    
    # Application title
    titlePanel("A representative random sample of tweets"),
    
    
    sidebarLayout(
        sidebarPanel(
            dateRangeInput('date',
                           label = 'Choose a date range:',
                           start = Sys.Date() - 1,
                           end = Sys.Date() - 1,
                           min = "2019-04-22",
                           max = Sys.Date()
            ),
            
            conditionalPanel("input.tabs == 'Hashtags' | input.tabs == 'Popular retweets'",
                             radioButtons(
                                 "langs",
                                 "Choose a language",
                                 list("English" = "en",
                                      "Japanese" = "ja",
                                      "Korean" = "ko",
                                      "Undetermined" = "und",
                                      "Spanish"= "es",
                                      "Thai" = "th",
                                      "Arabic" = "ar",
                                      "French" = "fr",
                                      "Turkish" = "tr",
                                      "Indian" = "in"),
                                 selected = "en")
                            
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = "tabs",
                        tabPanel("Hashtags",
                                 fluidRow(
                                   column(imageOutput("wcp", height = "600px"), width = 8),
                                   column(DT::dataTableOutput("hashes"), width = 4)
                                          ),
                                 textOutput("hashn")
                        ),
                        tabPanel("Popular retweets",
                                 dataTableOutput("retweets")
                        ),
                        tabPanel("Tweeters",
                                 imageOutput("tweeters")   
                        ),
                        tabPanel("Sampling",
                                 imageOutput("batches"),
                                 p("The image above shows the rate at which new data is being 
              sampled from Twitter into the database, in 30 second bursts.
              For example, around 1,000 to 3,000 tweets are loaded in each 
              burst.  Sampling bursts take place once an hour at a random
              time."),
                                 p("The actual sampling rate is unknown because it depends on the 
              proportion of tweets provided in Twitter's streaming API which 
              varies over time.
              Studies have estimated this to be between 1% and 40% of all
              actual tweets.  The database behind this app then has a sample of
              1/120th of those; so the sample here probably ranges between one
              in 12,000 and 1 in 300."),
                                 p("The number of tweets in the sample show both daily and weekly periodicity;
              peak time is around 16:30 CET each day.")
                        )
            )
        )
    )
)
)