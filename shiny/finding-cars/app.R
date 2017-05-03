#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(RCurl)
require(XML)
require(dplyr)
require(tidyr)
require(ggplot2)
require(knitr)
require(shinydashboard)


defaultURL <- 'http://www.trademe.co.nz/browse/categoryattributesearchresults.aspx?searchregion=2&cid=268&search=1&nofilters=1&originalsidebar=1&key=653340931&page=2&sort_order=motors_default&rptpath=1-268-'



# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
            textInput('URL', "Enter URL of Search", value = defaultURL, width = NULL, placeholder = NULL),
            textInput('PAGES', "Enter number of search result pages", value = "", width = NULL, placeholder = NULL),
            sliderInput("param_price_weighting", "Price Weighting:",
                        min = 0.01, max = 1, value = c(0.6,1)),
            sliderInput("param_year_weighting", "Year Weighting:",
                        min = 0.01, max = 1, value = c(0.3,0.7)),
            sliderInput("param_odo_weighting", "Odometer Weighting:",
                        min = 0.01, max = 1, value = c(0.01,0.4)),
            submitButton("Submit")
    
  ),
  dashboardBody(
            plotOutput("distPlot"),
            plotOutput("scatterPlot"),
            dataTableOutput(outputId="table")
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

  getCarData <- function(page, url){
    # Parses the website for the page of the search listings
    # extracts title, price, odometer, year, and location for further analysis
    
    theURL   <- url
    theURL   <- gsub('page=.', paste0('page=', page), theURL)
    webpage  <- htmlParse(theURL)
    title    <- xpathSApply(webpage, "//*/div[@class='listingTitle']", xmlValue)
    titleurl <- xpathSApply(webpage, "//*/div[@class='listingTitle']/a", xmlGetAttr, 'href')
    titleurl <- unique(titleurl)
    titleurl <- paste0("http://www.trademe.co.nz/", titleurl)
    
    title    <- gsub("[\r\n +\"+]", "", title)
    year     <- sub(".*(\\d+{4}).*$", "\\1", title)
    odometer <- xpathSApply(webpage, "//*/div[@class='rightArrows']", xmlValue)
    location <- odometer[2:length(odometer)]
    location <- location[seq(1, length(location), 3)]
    location <- gsub(",.*$", "", location)
    odometer <- data.frame(odo_data = subset(odometer, grepl("^([1-90-9\\<\\>])",odometer)))
    odometer <- odometer %>% 
      separate(odo_data, c('odo1', 'odo2', 'vehicle_type', 'engine', 'gearbox'), sep = ',', fill='right') %>% 
      mutate(odometer = (as.numeric(odo1) * 1000) + as.numeric(gsub("km", "", odo2))) %>%
      select(odometer, vehicle_type, engine, gearbox)
    
    price  <- xpathSApply(webpage, "//*/div[@class='listingPrice']", xmlValue)
    price  <- gsub("[\r\n +\"+]", "", price)
    price  <- as.numeric(gsub("[a-zA-Z\\,\\$]", "", price))
    price <- ifelse(price < 1000, price * 10, price)
    results <- cbind(title, year, odometer, price, location, titleurl)
    
    return(results)
    
  }
  
  
  loopThroughPages <- function(pages, url){
    # Just a wrapper to look through n pages and get the dataframe result.
    
    for (i in 1:pages){
      pageResults <- getCarData(i, url)
      if(i == 1){
        results <- pageResults
      } else {
        results <- rbind(results, pageResults)
      }
    }
    return(results)
  }
  
  getWeightedRanks <- function(df, trial){
    weightings[3] <- runif(1, input$param_price_weighting[1], input$param_price_weighting[2])   
    weightings[2] <- runif(1, input$param_odo_weighting[1], input$param_odo_weighting[2])
    weightings[1] <- runif(1, input$param_year_weighting[1], input$param_year_weighting[2])
    rankSet <- df %>% 
      filter(price <= 15000) %>% 
      mutate(rankYear = dense_rank(desc(year)),
             rankOdometer = dense_rank(odometer),
             rankPrice = dense_rank(price)) %>% 
      select(title, odometer, rankYear, rankOdometer, rankPrice)
    df$trialNum <- trial
    df$weightedRank <- rowSums(rankSet[, 3:5] * weightings, na.rm=T) / sum(weightings)
    return(df)
  }
  
  runMonteCarlo <- function(trials, df){
    
    for (i in 1:trials){
      
      mc_trial_results <- df %>% filter(price <= 15000) %>% getWeightedRanks(i)
      
      if(i == 1){
        Results <- mc_trial_results
      } else {
        Results <- rbind(Results, mc_trial_results)
      }
    }
    return(Results)
  }
  
  
  
  
    carData <- reactive({
        loopThroughPages(input$PAGES, input$URL)
    })
    
    
    mcResults <- reactive({
        runMonteCarlo(100, carData())
        
    })
    

    output$scatterPlot <- renderPlot({
        carData() %>% filter(price <= 15000, odometer<= 130000) %>% 
            ggplot(aes(odometer, price), size = year) + geom_point(aes(size=year)) + geom_smooth() +
            ggtitle("Comparing Odometer, Price, and Age")
    })
    
    output$distPlot <- renderPlot({
        p1 <- mcResults() %>% 
            filter(odometer > 0) %>% 
            ggplot(aes(weightedRank))
        p1 + geom_line(aes(group=title), stat="density", size=0.5, alpha=0.2) +
            ggtitle("distribution of ranks per vehicle after 100 trials")
    })
    

    
    output$table <- renderDataTable({
      print(mcResults()[1,])
        mcResults() %>%
            filter(odometer > 0) %>%
            mutate(titleurl = paste0("<a href='",titleurl,"'>","link","</a>")) %>% 
            group_by(title, odometer, price, year, location, titleurl) %>%
            summarise(medRank = mean(weightedRank)) %>%
            arrange(medRank) 
    }, escape = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)

