# load libraries
library(rvest)
library(data.table)
library(tidyverse)
library(jsonlite)
library(httr)
library(ggthemes)

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



# drop useless column ("V2") 
# and a column which has information that is repeated in another column ("DISPLACEMENT")
df <- df %>% 
  select(-V2, DISPLACEMENT)

# rename the column "V1" to something understandable
df <- df %>% 
  rename(range_valid_at_speed = V1)



# set currency conversion rates
USD_to_EUR <- 0.82
GBP_to_EUR <- 1.10

# define a function for converting prices to EUR
# this function returns a numeric 0 for invalid input values
price_to_EUR <- function(price_with_symbol){
  
  # set NAs to 0
  if(is.na(price_with_symbol)){
    price_with_symbol <- 0
  }
  
  # EUR: strip symbols, change data type to numeric
  else if(str_detect(price_with_symbol, "€") == TRUE){
    price_with_symbol <- price_with_symbol %>% 
      str_replace("€", "") %>% 
      str_replace_all(",", "") %>% 
      as.numeric()
  }
  
  # USD: strip symbols, change data type to numeric, multiply by USD_to_EUR
  else if(str_detect(price_with_symbol, "\\$") == TRUE){
    price_with_symbol <- price_with_symbol %>% 
      str_replace("\\$", "") %>% 
      str_replace_all(",", "") %>% 
      as.numeric() %>%  `*` (USD_to_EUR)
  }
  
  # GBP: strip symbols, change data type to numeric, multiply by GBP_to_EUR
  else if(str_detect(price_with_symbol, "£") == TRUE){
    price_with_symbol <- price_with_symbol %>% 
      str_replace("£", "") %>% 
      str_replace_all(",", "") %>% 
      as.numeric() %>% `*` (GBP_to_EUR)
  }
  
  # other: set to 0
  # for example, entries that say "POA"
  else{
    price_with_symbol <- 0
  }
  
  
  return(price_with_symbol)
}

# convert yacht prices from original currency to EUR and store in a vector
converted_prices <- unname(sapply(yacht_prices, price_to_EUR))

# add converted prices to the basic dataframe made from JSON at the beginning
# values of 0 converted to NA
yacht_info <- yacht_info %>% 
  mutate(PRICE_EUR = na_if(converted_prices, 0))

# join the price data from the simple dataframe (JSON source) to the main DF
df <- left_join(df, yacht_info, by = "link")

# rename columns to include measurement units
df <- df %>% rename('TOP SPEED kn' = 'TOP SPEED',
              'RANGE nm' = 'RANGE', 
              'CRUISING SPEED kn' = 'CRUISING SPEED', 
              'BEAM m' = 'BEAM', 
              'Length Overall in meters:' = 'Length Overall:', 
              'Beam in meters:' = 'Beam:', 
              'Max Draught in meters:' = 'Max Draught:', 
              'Max Speed in kn:' = 'Max Speed:',
              'Cruising Speed in kn:' = 'Cruising Speed:',
              'Range in nm:' = 'Range:',
              'Range Valid at Speed in kn:' = 'range_valid_at_speed',
              'Total Power in hp:' = 'Total power:',
              'Fuel Capacity in liters:' = 'Fuel Capacity:',
              'Water Capacity in liters:' = 'Water Capacity:',
              'Length at Waterline in meters:' = 'Length at Waterline:')

# change character datatypes to numeric, sometimes with string manipulation
df <- df %>% 
  mutate(`TOP SPEED kn` = as.numeric(str_extract(`TOP SPEED kn`, "\\d*\\.*\\d*")),
         `RANGE nm` = as.numeric(str_extract(`RANGE nm`, "\\d*\\.*\\d*")),
         `GUESTS` = as.numeric(`GUESTS`),
         `CREW` = as.numeric(`CREW`),
         `CRUISING SPEED kn` = as.numeric(str_extract(`CRUISING SPEED kn`, pattern = "\\d*\\.*\\d*")),
         `BEAM m` = as.numeric(str_extract(`BEAM m`, pattern = "\\d*\\.*\\d*")),
         `GUEST CABIN` = as.numeric(`GUEST CABIN`),
         `Length Overall in meters:` = as.numeric(str_extract(`Length Overall in meters:`, pattern = "\\d*\\.*\\d*")),
         `Beam in meters:` = as.numeric(str_extract(`Beam in meters:`, pattern = "\\d*\\.*\\d*")),
         `Max Draught in meters:` = as.numeric(str_extract(`Max Draught in meters:`, pattern = "\\d*\\.*\\d*")),
         `Gross Tonnage:` = as.numeric(str_extract(`Gross Tonnage:`, pattern = "\\d*\\.*\\d*")),
         `Year of Build:` = as.factor(`Year of Build:`),
         `Number of Decks:` = as.numeric(`Number of Decks:`),
         `Max Speed in kn:` = as.numeric(str_extract(`Max Speed in kn:`, pattern = "\\d*\\.*\\d*")),
         `Cruising Speed in kn:` = as.numeric(str_extract(`Cruising Speed in kn:`, pattern = "\\d*\\.*\\d*")),
         `Range in nm:` = as.numeric(str_extract(`Range in nm:`, pattern = "\\d*\\.*\\d*")),
         `Range Valid at Speed in kn:`= as.numeric(str_extract(`Range Valid at Speed in kn:`, pattern = "\\d*\\.*\\d*")),
         `Total Power in hp:` = as.numeric(str_extract(`Total Power in hp:`, pattern = "\\d*\\.*\\d*")),
         `Guests:` = as.numeric(`Guests:`),
         `Passenger Rooms:` = as.numeric(`Passenger Rooms:`),
         `Master Rooms:` = as.numeric(`Master Rooms:`),
         `Double Rooms: ` = as.numeric(`Double Rooms: `),
         `Twin Rooms:` = as.numeric(`Twin Rooms:`),
         `For sale to U.S. Residents while in U.S. Waters:` = as.factor(`For sale to U.S. Residents while in U.S. Waters:`),
         `Displacement Tonnage:` = as.numeric(`Displacement Tonnage:`),
         `Fuel Capacity in liters:` = as.numeric(str_extract(`Fuel Capacity in liters:`, pattern = "\\d*\\.*\\d*")),
         `Water Capacity in liters:` = as.numeric(str_extract(`Water Capacity in liters:`, pattern = "\\d*\\.*\\d*")),
         `Length at Waterline in meters:` = as.numeric(str_extract(`Length at Waterline in meters:`, pattern = "\\d*\\.*\\d*")),
         `VIP Rooms:` = as.numeric(`VIP Rooms:`)
         )
  

#OPTIONAL: save clean DF to RDS object
#saveRDS(df, "yacht_DF.RDS")


###########################################
###########################################
###               ANALYZE               ###
###          YACHT INFORMATION          ###
###########################################
###########################################

# Examine the relationship between yacht length and price
df %>% 
  ggplot(aes(x = `Length Overall in meters:` , y = (PRICE_EUR / 1000000))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(x = "Yacht Length in Meters", y = "Price, in Millions (EUR)", title = "Relationship Between Price (EUR) and Yacht Length") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_gdocs()


# Look at the model-year distribution of yachts for sale right now
df %>% 
  filter(`Year of Build:` %in% 2000:2020) %>% 
  ggplot(aes(x = `Year of Build:`)) +
  geom_histogram(stat = "count") +
  labs(x = "Year Built", y = "Number of Yachts Listed", title = "Number of Yachts for Sale, Manufactured 2000 - 2020") +
  theme_gdocs()

# Take a look at the yachts speeds for some of the big engine makers
df %>% 
  filter(`Engine Make:` %in% c("Caterpillar ", "MTU ", "Cummins ") ) %>% 
  ggplot(aes(x = `Engine Make:`, y = `Max Speed in kn:`, color = `Engine Make:`) ) +
  geom_boxplot() +
  labs(x = "", y = "Max Speed in Knots", title = "Max Speed by Engine Manufacturer") +
  theme_gdocs()
