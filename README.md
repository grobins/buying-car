---
title: "Buying a Car"
author: "Gareth Robins"
date: "4 December 2016"
output: html_document
---

Those who know me best understand that I don't like cars.  It's not that I don't appreciate nice cars, or have favourites, it's that I just don't have an encyclopeadic knowledge about the things.  I see them as transport from one place to another, and not as objects of desire, or entertainment.

The car I have is a piece of crap, a 2003 Ford Mondeo which lives up to its mnemonic of Fix Once Repair Daily.  Recently it has become too expensive to take to the mechanics to fix, and I have more important things to do than spend my weekends trying to fix it myself. 

The day that Google and Tesla solve the car ownership problem I'll be signing up.

So, it's car buuying time.  I hate this process even more than owning a car.  I now have to spend money and make a large purchase, denting my holiday and house plans, for another hunk of metal to take me to work and back.  So I thought I'd try to automate the process a little, and at the best narrow down the choice to my best options.  

We have a certain website in NZ that everyone goes it.  I shan't mention it because the script below involves scraping from their site, which they disallow.  But hey, I wasn't logged in so I didn't agree to those terms :)

Luckily I did have some broad criteria before searching.  I needed a 7 person car.  This limited the search dramatically, but I narrowed it down to the Toyota Wish for the purpose of explaining my methods.

![Not exactly an Astin Martin, but it'll do](img/2009_Toyota_Wish_03.jpg)



```{r setup, include=FALSE}

require(RCurl)
require(XML)
require(dplyr)
require(tidyr)
require(ggplot2)
require(knitr)

setwd("~/Documents/finding-cars/")
options(scipen = 999)
opts_chunk$set(echo = TRUE)
opts_chunk$set(out.width='800px', dpi=200)
```

## Scraping and Regex
Scraping pages in R is easy, but then you have to navigate the minefield of tags the developer has setup for you.  I was lucky in this case, the page had nice class names that I could extract with XPath, and using Regex I could remove all the parts of the listing I'm not interested in, giving me some nice data.

```{r functions}
getCarData <- function(page){
  # Parses the website for the page of the search listings
  # extracts title, price, odometer, year, and location for further analysis
  
  theURL   <- paste0("PASTE THE SECRET URL HERE :)")
  webpage  <- htmlParse(theURL)
  title    <- xpathSApply(webpage, "//*/div[@class='listingTitle']", xmlValue)
  title    <- gsub("[\r\n +\"+]", "", title)
  year     <- sub(".*(\\d+{4}).*$", "\\1", title)
  odometer <- xpathSApply(webpage, "//*/div[@class='rightArrows']", xmlValue)
  location <- odometers[2:length(odometer)]
  location <- location[seq(1, length(location), 3)]
  location <- gsub(",.*$", "", location)
  odometer <- data.frame(odo_data = subset(odometer, grepl("^([1-90-9\\<\\>])",odometer)))
  odometer <- odometer %>% 
              separate(odo_data, c('odo1', 'odo2', 'vehicle_type', 'engine', 'gearbox'), sep = ',') %>% 
              mutate(odometer = (as.numeric(odo1) * 1000) + as.numeric(gsub("km", "", odo2))) %>%
              select(odometer, vehicle_type, engine, gearbox)
  
  prices  <- xpathSApply(webpage, "//*/div[@class='listingPrice']", xmlValue)
  prices  <- gsub("[\r\n +\"+]", "", prices)
  prices  <- as.numeric(gsub("[a-zA-Z\\,\\$]", "", prices))
  results <- cbind(titles, years, odometers, prices, locations)
  
  return(results)
  
}


loopThroughPages <- function(pages){
  # Just a wrapper to look through n pages and get the dataframe result.
  
  for (i in 1:pages){
    pageResults <- getCarData(i)
    
    if(i == 1){
      results <- pageResults
    } else {
      results <- rbind(carData, pageResults)
    }
  }
  return(results)
}

# Only read in if I haven't already saved the results
#  There is a correction to one of the ads, that had a false odometer reading.
if(!file.exists('data/carData.csv')){
  carData <- loopThroughPages(13)
  carData[carData$titles == 'ToyotaWish2004' & carData$odometer == 10500, 'odometer'] <- 105000
  write.table(carData, 'data/carData-wish.csv', sep=';', row.names=F)
} else {
  carData <- read.table("data/carData-wish.csv", sep=';', header=T)

}

head(carData)

```
Nice clean data :)


## Quick look at the data
 Looks good, as I suspected lower odometers and newer years correlate with higher prices, but there is a distribution of prices at each level.
 
```{r pressure, echo=FALSE}
carData %>% filter(prices <= 15000, odometer<= 130000) %>% 
  ggplot(aes(odometer, prices), size = years) + geom_point(aes(size=years)) + geom_smooth() +
  ggtitle("Comparing Odometer, Price, and Age")

```


I looked at the model with the most results, and compared prices across locations.  I always thought it would be cheaper to buy a car further away from Auckland, but with the exception of Christchurch, Auckland is still cheaper to buy this particular model of the Toyota Wish.  I live on the North Shore in Auckland, and that is reflected in the price too!
```{r, echo=FALSE, message=FALSE, warning=FALSE}
carData %>% filter(years == 2005) %>% 
  group_by(locations) %>% 
  summarise(median = median(prices)) %>% 
  arrange(median)
```



## Monte Carlo

I love this technique for bringing some order to the chaos.  Low prices, low odomter, and low age are all important to me.  But I'm willing to make sacrifices in one of those metrics if the trade-off is worth it.  This is hard to do when browsing online as you are subconsiously being sucked in with the pictures of the cars.  In the script below I randomise the weightings I will give to the three metrics, and use these to find the weighted average rank for each vehicle in the dataset.

I also have a financial hard limit of $15000, so I didn't make sense 
```{r}
getWeightedRanks <- function(df, trial){
  weightings    <- runif(3, 1, 100)  # I started with all metrics having same range
  weightings[1] <- runif(1, 1, 40)   # but then realised I valued price > odometer > year
  weightings[2] <- runif(1, 20, 60)
  weightings[3] <- runif(1, 50, 100)
  rankSet <- carData %>% 
    filter(prices <= 15000) %>% 
    mutate(rankYear = dense_rank(desc(years)),
           rankOdometer = dense_rank(odometer),
           rankPrice = dense_rank(prices)) %>% 
    select(titles, odometer, rankYear, rankOdometer, rankPrice)
  df$trialNum <- trial
  df$weightedRank <- rowSums(rankSet[, 2:4] * weightings, na.rm=T) / sum(weightings)
  return(df)
}

runMonteCarlo <- function(trials){
  # Just 
  for (i in 1:trials){
  
    mc_trial_results <- carData %>% filter(prices <= 15000) %>% getWeightedRanks(i)
    
    if(i == 1){
      Results <- mc_trial_results
    } else {
      Results <- rbind(Results, mc_trial_results)
    }
  }
  return(Results)
}

mcResults <- runMonteCarlo(100)
```



# Distribution
Each line is a listing, and this plot shows the distribution of ranks for each vehicle after all the trials were complete.  I'm interested in the listings that show more consistently high rankings, no matter what the weightings are.  Those are the listings on the left of the graph and summarised in the table below
```{r}
p1 <- mcResults %>% 
  filter(odometer > 0) %>% 
  ggplot(aes(weightedRank))
p1 + geom_line(aes(group=titles), stat="density", size=0.5, alpha=0.2) +
  ggtitle("distribution of ranks per vehicle after 100 trials")

mcResults %>% 
  filter(odometer > 0, years >= 2006) %>% 
  group_by(titles, odometer, prices, years, locations) %>% 
  summarise(medRank = median(weightedRank)) %>% 
  arrange(medRank) %>% 
  head(20)
```



# Conclusion.

Now I have a list of a cars that I know are the best combination of Price, Odometer, and Age.  I can select the ones in the Auckland region from this list since they are priced lower, and perhaps will be more willing to negotiate (also, I'm not flying to Christchurch!).  I'll also take a buddy along that knows a lot more about the mechanical bits than I do.  

I'd like to extend this to all makes and models, not just the Toyota Wish, then I can find better bargains, because the Wish is priced quite highly.