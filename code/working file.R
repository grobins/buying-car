require(RCurl)
require(XML)
require(dplyr)
require(tidyr)
require(ggplot2)

setwd("~/Documents/finding-cars/")
options(scipen = 999)
getCarData <- function(page){
  
  webpage <- htmlParse(theURL)
  title <- xpathSApply(webpage, "//*/div[@class='listingTitle']", xmlValue)
  title <- gsub("[\r\n +\"+]", "", title)
  year <- sub(".*(\\d+{4}).*$", "\\1", title)
  odometer <- xpathSApply(webpage, "//*/div[@class='rightArrows']", xmlValue)
  location <- odometers[2:length(odometer)]
  location <- location[seq(1, length(location), 3)]
  location <- gsub(",.*$", "", location)
  odometer <- data.frame(odo_data = subset(odometer, grepl("^([1-90-9\\<\\>])",odometer)))
  odometer <- odometer %>% separate(odo_data, c('odo1', 'odo2', 'vehicle_type', 'engine', 'gearbox'), sep = ',') %>% 
    mutate(odometer = (as.numeric(odo1) * 1000) + as.numeric(gsub("km", "", odo2))) %>%
    select(odometer, vehicle_type, engine, gearbox)
  
  prices <- xpathSApply(webpage, "//*/div[@class='listingPrice']", xmlValue)
  prices <- gsub("[\r\n +\"+]", "", prices)
  prices <- as.numeric(gsub("[a-zA-Z\\,\\$]", "", prices))
  
  results <- cbind(titles, years, odometers, prices, locations)
  
  return(results)
  
}


for (i in 1:13){
  
  pageResults <- getCarData(i)
  
  if(i == 1){
    carData <- pageResults
  } else {
    carData <- rbind(carData, pageResults)
  }
  print(i)
  #Sys.sleep(1)
  
  
}

# corrections

carData[carData$titles == 'ToyotaWish2004' & carData$odometer == 10500, 'odometer'] <- 105000


write.table(carData, 'data/carData-wish.csv', sep=';', row.names=F)
carData <- read.table("data/carData-wish.csv", sep=';', header=T)

carData %>% filter(prices <= 15000, odometer<= 130000) %>% 
  ggplot(aes(odometer, prices), size = years) + geom_point(aes(size=years)) + geom_smooth() +
  facet_grid(. ~ locations)


carData %>% filter(years == 2005) %>% 
  group_by(locations) %>% 
  summarise(median = median(prices)) %>% 
  arrange(median)
  
  
  ggplot(aes(x=1, y=prices, fill=locations)) +
  geom_boxplot() + facet_grid(. ~ locations)

# montecarlo

getWeightedRanks <- function(df, trial){
  weightings <- runif(3, 1, 100)
  weightings[1] <- runif(1, 1, 40)
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
  # return(df[, c('titles', 'odometer', 'trialNum', 'weightedRank')])
  return(df)
}


runMonteCarlo <- function(trials){
  for (i in 1:trials){
    
    mc_trial_results <- carData %>% filter(prices <= 15000) %>% getWeightedRanks(i)
    
    if(i == 1){
      Results <- mc_trial_results
    } else {
      Results <- rbind(Results, mc_trial_results)
    }
    print(i)
    
  }
  return(Results)
}



mcResults <- runMonteCarlo(1000)


p1 <- mcResults %>% 
  filter(odometer > 0) %>% 
  ggplot(aes(weightedRank))
p1 + geom_line(aes(group=titles), stat="density", size=0.5, alpha=0.2) +
  ggtitle("distribution of ranks per vehicle after 1000 trials")


mcResults %>% 
  filter(odometer > 0, years >= 2006) %>% 
  group_by(titles, odometer, prices, years, locations) %>% 
  summarise(medRank = median(weightedRank)) %>% 
  arrange(medRank) %>% 
  head(20)

