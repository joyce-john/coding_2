### Web Scraping Assignment

# load the required libraries
library(tidyverse)
library(rvest)
library(data.table)


get_results <- function(page_count, search_term){

# make an index for dealing with duplicates
keep_list <- seq(from = 1, to = 20, by = 2)

# this function gets 1 page
get_petapixel_page <- function(my_url){
  
  # read the supplied URL into an object
  t <- read_html(my_url)
  
  # grab article titles
  title <- t %>% 
    html_nodes('.post-medium h3')%>%
    html_text()
  
  # grab dates published
  date <- t %>% 
    html_nodes('.post-meta__option--datetime time') %>% 
    html_text()
  
  # grab links to the articles
  # works but scrapes every link twice. use an index to deal with duplicates
  article_link <- 
    t %>% 
    html_nodes('.post-medium a')%>%
    html_attr('href')
  
  # reassign article link to itself with an index that selects alternating items
  article_link <- article_link[keep_list]
  
  # grab links to the thumbnail images in search results
  thumbnail_link <- t %>% 
    html_nodes('.size-archive-thumb')%>%
    html_attr('src')
  
  # dataframe of the scraped data
  page_df <- data.frame("title" = title, 
                        "date" = date, 
                        "article_link" = article_link,
                        "thumbnail_link" =  thumbnail_link)
  
  return(page_df)
}

# constructs urls with the user-supplied page_count and search_term
url_list <- paste0("https://petapixel.com/page/", 1:page_count, "/?s=", search_term)

# lapply passes every URL in the url_list as an argument to the scraping function and returns a list
all_pages_df <- lapply(url_list, get_petapixel_page)

# bind the lists into a dataframe
all_pages_df <- rbindlist(all_pages_df)

return(all_pages_df)
}

test <- get_results(3, "pentax")
