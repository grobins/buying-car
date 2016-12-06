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


getCarData <- function(page, url){
    # Parses the website for the page of the search listings
    # extracts title, price, odometer, year, and location for further analysis
    
    theURL   <- url
    theURL   <- gsub('page=.', paste0('page=', page), theURL)
    webpage  <- htmlParse(theURL)
    title    <- xpathSApply(webpage, "//*/div[@class='listingTitle']", xmlValue)
    title    <- gsub("[\r\n +\"+]", "", title)
    year     <- sub(".*(\\d+{4}).*$", "\\1", title)
    odometer <- xpathSApply(webpage, "//*/div[@class='rightArrows']", xmlValue)
    location <- odometer[2:length(odometer)]
    location <- location[seq(1, length(location), 3)]
    location <- gsub(",.*$", "", location)
    odometer <- data.frame(odo_data = subset(odometer, grepl("^([1-90-9\\<\\>])",odometer)))
    odometer <- odometer %>% 
        separate(odo_data, c('odo1', 'odo2', 'vehicle_type', 'engine', 'gearbox'), sep = ',') %>% 
        mutate(odometer = (as.numeric(odo1) * 1000) + as.numeric(gsub("km", "", odo2))) %>%
        select(odometer, vehicle_type, engine, gearbox)
    
    price  <- xpathSApply(webpage, "//*/div[@class='listingPrice']", xmlValue)
    price  <- gsub("[\r\n +\"+]", "", price)
    price  <- as.numeric(gsub("[a-zA-Z\\,\\$]", "", price))
    results <- cbind(title, year, odometer, price, location)
    
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
    weightings    <- runif(3, 1, 100)  # I started with all metrics having same range
    weightings[1] <- runif(1, 1, 40)   # but then realised I valued price > odometer > year
    weightings[2] <- runif(1, 20, 60)
    weightings[3] <- runif(1, 50, 100)
    rankSet <- df %>% 
        filter(price <= 15000) %>% 
        mutate(rankYear = dense_rank(desc(year)),
               rankOdometer = dense_rank(odometer),
               rankPrice = dense_rank(price)) %>% 
        select(title, odometer, rankYear, rankOdometer, rankPrice)
    df$trialNum <- trial
    df$weightedRank <- rowSums(rankSet[, 2:4] * weightings, na.rm=T) / sum(weightings)
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



# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel("Analyse Cars on a certain website."),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput('URL', "Enter URL of Search", value = "", width = NULL, placeholder = NULL),
            textInput('PAGES', "Enter number of search result pages", value = "", width = NULL, placeholder = NULL),
            submitButton("Submit")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("scatterPlot"),
            tableOutput(outputId="table")
        )
    )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

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
    

    
    output$table <- renderTable({
        mcResults() %>%
            filter(odometer > 0) %>%
            group_by(title, odometer, price, year, location) %>%
            summarise(medRank = median(weightedRank)) %>%
            arrange(medRank) %>%
            top_n(20)
    })
    
})

# Run the application 
shinyApp(ui = ui, server = server)

