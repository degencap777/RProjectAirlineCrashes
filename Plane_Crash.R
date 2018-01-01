setwd("C:/Users/jimmy/workspace/R/Jimmy DATA MINING")
# load required packages
library(readr)    # to read functions to import xls file
library(stringr)  # to use regx and other string functions
library(tidyverse)  # to manipulate data
library(dplyr)      # to manipulate data
library(ggplot2)    # to plot graph
library(readr)      # to read flat/tabular text files
library(lubridate)  # to manipulate as date
library(tm)         # to perform text mining operations (for wordcloud here)
library(caret)      # to spilt data and and select featured data
library(wordcloud)  # to write most reasons for crash in a cloud form
library(gridExtra)  # to arrange multiple grid based plots on a page
library(RColorBrewer)# to have nice color palettes
library(DT)         # to have html representation of the data
library("dplyr")
library("xml2")
library("rvest")
library("tidyr")
library("magrittr")
# select from which years to get plane crash data and create URL for each
years <- c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
urlYears <- paste0("http://www.planecrashinfo.com/", years, 
                   "/", years, ".htm") 
# get URL for each plane crash table from each year selected

urls <- mapply(function(urlYear, year) {
  rowNumber <- urlYear %>% 
    read_html %>% 
    html_table(header = TRUE) %>% 
    .[[1]] %>% nrow
  
  urlsSpec <- paste0("http://www.planecrashinfo.com/", year, 
                     "/", year, "-", 1:rowNumber, ".htm")
},
urlYears,
years
) %>% unlist 
# retrieve information 

data <- lapply(urls, function(url) {
  url %>% 
    # read each crash table
    read_html %>% 
    html_table %>% 
    data.frame %>%  
    setNames(c("Vars", "Vals")) %>%
    # header is a colunm and values are in a column -> tidy up
    spread(Vars, Vals) %>% 
    .[-1]
})
# data list to data.frame and set appropriate variable names
data %<>% 
  bind_rows %>% 
  setNames(gsub(":", "", colnames(.)) %>% tolower)
# pick relevant variables
data %<>% select(date, location, route, operator, aboard, fatalities, summary) 
# clean location variable
data %<>% mutate(location = gsub("Near | near", "", location)) %>% 
  mutate(location = gsub(",.*,", ",", location))
# extract total number of people aboard and fatalities 
data$aboard <- strsplit(data$aboard, " ") %>% 
  lapply("[[", 1)  %>% 
  as.numeric

data$fatalities <- strsplit(data$fatalities, " ") %>% 
  lapply("[[", 1) %>% 
  as.numeric

data$deathRate <- round((data$fatalities/data$aboard) * 100, 2)

data <- as_tibble(data)
#1. Omitting Na values 
data <- na.omit(data)
#2. Spilting Date Column
data <- data %>% separate(date, into = c("Month","Day","Year"))
data$location <- sapply(data$location, as.character)
data$location <- gsub(".*,", "", data$location)
#remove white space at beginning
data$location <- str_trim(data$location, side = "both")
#Convert string back to factors
data$location <- sapply(data$location, as.factor)

write.csv(data, file = "Airline_Incidents.csv")

#Monthly
months <- as.data.frame(table(data$Month))
A2 <- ggplot(months, aes(Var1, Freq)) + 
  geom_bar(stat = "identity", fill = "Navy", width = 0.3) + 
  xlab("Month") + ylab("Crashes") +
  ggtitle("Total number of crashes per month")

#Yearly
years <- as.data.frame(table(data$Year))
A1 <- ggplot(years, aes(y = Freq, x = Var1, group = 1))  + 
  geom_line(size = 1, linetype = 1, color = "Navy") + 
  geom_point(size = 3, shape = 20)+ 
  geom_smooth() +
  xlab("Years") + ylab("Crashes") + 
  scale_x_discrete(breaks = seq(from = 2007, to = 2017, by = 10)) + 
  ggtitle("Total number of crashes per year")

grid.arrange(A1, A2, nrow = 2, heights=2:1)

Fatalities <- data %>% group_by(Year) %>% 
  summarise(total_fatalities = sum(fatalities), total_passengers = sum(aboard))

f1 <- ggplot(Fatalities, aes(y = (total_fatalities/total_passengers)*100, x = Year, group = 10))  + 
  geom_line(size = 1, linetype = 1, color = "Red") + 
  geom_point(size = 3, shape = 20) + 
  geom_smooth() +
  xlab("Years") + ylab("% Fatalities") + 
  scale_x_discrete(breaks = seq(from = 2007, to = 2017, by = 10)) +
  ggtitle("Percent of fatalities per year")
f1

Location_Crash <-   data %>% group_by(location) %>% 
  summarise(total_fatalities = sum(fatalities)) %>% arrange(desc(total_fatalities))

L1 <- ggplot(Location_Crash[1:20,], aes(x = reorder(location, -total_fatalities), y = total_fatalities, alpha = total_fatalities)) + 
  geom_bar(stat = "identity", fill = "maroon", width = 0.5) +
  xlab("Countries") + ylab("Number of fatalities") + 
  ggtitle("Top 20 Countries with Maximum Fatalities")
L1

crash_operator <-   data %>% group_by(operator) %>% 
  summarise(Freq = n()) %>% arrange(desc(Freq))

operator <- ggplot(crash_operator[1:20,], aes(x = reorder(factor(operator), Freq), y = Freq, alpha = Freq)) + 
  geom_bar(stat = "identity", fill = "Blue", width = 0.05) + geom_point(stat = "identity") + 
  xlab("Aircraft Operators") + ylab("Crashes") + ggtitle("Top 20 airline operators by crash rate") + 
  coord_flip() 
operator

# Word Cloud
summary <- Corpus(VectorSource(data$summary))

corpus_clean <- tm_map(summary, tolower)
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removeWords, "flight")
corpus_clean <- tm_map(corpus_clean, removeWords, "crashed")
corpus_clean <- tm_map(corpus_clean, removeWords, "plane")
corpus_clean <- tm_map(corpus_clean, removeWords, "aircraft")
corpus_clean <- tm_map(corpus_clean, removeWords, "airport")
corpus_clean <- tm_map(corpus_clean, removeWords, "pilot")
corpus_clean <- tm_map(corpus_clean, removeWords, "killed")

tdm <- TermDocumentMatrix(corpus_clean)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]

wordcloud(corpus_clean, max.words = 100, min.freq = 35, random.order = FALSE, colors=pal)
