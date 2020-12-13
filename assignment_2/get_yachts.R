# load libraries
library(rvest)
library(data.table)
library(tidyverse)
library(jsonlite)
library(httr)

###########################################
###########################################
###    USE CONVERTED CURL COMMAND TO    ###
### DOWNLOAD JSON OF YACHT NAMES / URLS ###
###########################################
###########################################

# copy the curl bash command and use curl.trillworks.com to convert to R code
headers = c(
  `accept` = '*/*',
  `Referer` = 'https://www.boatinternational.com/',
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.88 Safari/537.36',
  `content-type` = 'application/json'
)

# below, edit the "limit" variable from 10 to a number large enough to capture all the data
# note: values of 10,000+ are refused, but for this data we don't need a number that high
params = list(
  `operationName` = 'searchPublishedContent',
  `variables` = '{"limit":1000,"offset":10,"sort":"updated:desc","rawQueryStringified":"{\\"bool\\":{\\"must\\":[{\\"bool\\":{\\"must\\":[{\\"bool\\":{\\"should\\":[]}}]}}],\\"filter\\":{\\"bool\\":{\\"must\\":[{\\"term\\":{\\"docType\\":\\"published_yacht_for_sale\\"}},{\\"bool\\":{\\"should\\":[{\\"match\\":{\\"parentId.keyword\\":\\"32a4c780-388f-11ea-850e-31ecb8b8ac17\\"}}]}}]}},\\"must_not\\":[{\\"ids\\":{\\"values\\":[\\"32a4c780-388f-11ea-850e-31ecb8b8ac17\\"]}}]}}"}',
  `extensions` = '{"persistedQuery":{"version":1,"sha256Hash":"a02cb3561985e40e00a4e7defae903557d7832b2ec80ffe960a369596aec8ea8"}}'
)

res <- httr::GET(url = 'https://api.boatinternational.com/graphql', httr::add_headers(.headers=headers), query = params)

# parse the JSON into a dataframe
df <- fromJSON(content(res, 'text'))

# get the relative URLs for every yacht, so we can write a function which scrapes every page
yacht_urls <- df$data$searchPublishedContent$results$url

# convert relative URLs to full URLs
yacht_urls <- unname(
  sapply(yacht_urls, function(x){
    new_url <- paste0("https://www.boatinternational.com", x)
    return(new_url)
}))


# grab the prices and put them in a table next to the URLs
# use this to join prices to the main DF later
# scraping prices is awkward because of inconsistencies with the way prices are displayed
# and URLs are the perfect joining variable, because they are unique (unlike yacht names)
yacht_prices <- df$data$searchPublishedContent$results$formattedPrice
yacht_info <- data.frame(link = yacht_urls, PRICE = yacht_prices)

###########################################
###########################################
###     SCRAPE EVERY YACHT'S PAGE       ###
###   CREATE DATAFRAME OF ALL YACHTS    ###
###########################################
###########################################


# define a function which scrapes the tables on every yacht page
get_page_stats <- function(url){
  
  # this shows the user that the script is still running
  print(url)
  
  # read the webpage
  t <- read_html(url)
  
  # this will hold the scraped information
  info_table <- list()
  
  # grab stat names from the top table
  stat_titles <- t %>% 
    html_nodes('.stats__title') %>% 
    html_text()
    
  # grab stat values from the top table
  stat_values <- t %>% 
    html_nodes('.stats__value') %>% 
    html_text()
 
  # put names and values from the top table in the list
  # only perform this action if the number of keys and values match up, and they are not 0
  if (length(stat_titles) ==  length(stat_values) & length(stat_titles) > 0){ 
    for (i in 1:length(stat_titles)) {
      info_table[[stat_titles[i]]] <- stat_values[i]
    }
  }
  
  # grab specification names from the bottom table
  spec_titles <- t %>% 
    html_nodes('.spec-block__title') %>% 
    html_text()
  
  # grab specification values from the bottom table
  spec_values <- t %>% 
    html_nodes('.spec-block__data') %>% 
    html_text()
  
  # put names and values from the bottom table in the list
  # only perform this action if the number of keys and values match up, and they are not 0
  if (length(spec_titles) == length(spec_values) & length(spec_titles) > 0){
    for (i in 1:length(spec_titles)){
      info_table[[spec_titles[i]]] <- spec_values[i]
    }
  }
  
  # add the url to the list
  info_table$link <- url
  
  return(info_table)
}

# call the function on every yacht URL
# WARNING: this can be slow at times
df <- rbindlist(lapply(yacht_urls, get_page_stats), fill = T)



###########################################
###########################################
###                CLEAN                ###
###          YACHT INFORMATION          ###
###########################################
###########################################

# drop useless column
df <- df %>% 
  select(-V2)

# rename V1 to something understandable
df <- df %>% 
  rename(range_valid_at_speed = V1)

# convert prices to EUR in the dataframe made earlier with information from the JSON
yacht_info2 <- yacht_info %>% mutate(eur = price_to_EUR(PRICE))

# join the price data from the JSON
df2 <- left_join(df, yacht_info, by = "link")

# define a function for converting prices to EUR
price_to_EUR <- function(price_with_symbol){
  
  if(is.na(price_with_symbol)){
    price_with_symbol <- NULL
  }
  
  else if(str_detect(price_with_symbol, "€") == TRUE){
    price_with_symbol <- price_with_symbol %>% 
      str_replace("€", "") %>% 
      str_replace_all(",", "") %>% 
      as.numeric()
  }
  
  else if(str_detect(price_with_symbol, "\\$") == TRUE){
    price_with_symbol <- price_with_symbol %>% 
      str_replace("\\$", "") %>% 
      str_replace_all(",", "") %>% 
      as.numeric()
    price_with_symbol <- price_with_symbol * 0.82
  }
  
  else if(str_detect(price_with_symbol, "£") == TRUE){
    price_with_symbol <- price_with_symbol %>% 
      str_replace("£", "") %>% 
      str_replace_all(",", "") %>% 
      as.numeric()
    price_with_symbol <- price_with_symbol *1.10
  }
  
  else{
    price_with_symbol <- NULL
  }
  
  
  return(price_with_symbol)
}

df2 <- df2 %>% mutate(EUR_price = price_to_EUR(PRICE))
