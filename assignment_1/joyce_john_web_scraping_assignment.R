### Web Scraping Assignment

# load the required libraries
library(tidyverse)
library(rvest)
library(data.table)

# set the URL to be scraped
my_url <- 'https://petapixel.com/?s=pentax'

# create the URL list which adjusts to the user argument
page_count <- 5
url_list <- paste0("https://petapixel.com/page/", 1:page_count, "/?s=pentax")

# write it out so I can inspect it and see if the relevant content is there
write_html(t, 't.html')

# make an index for dealing with duplicates
keep_list <- seq(from = 1, to = 20, by = 2)


get_1_pentax_page <- function(my_url){
  
  t <- read_html(my_url)
  
  title <- t %>% 
    html_nodes('.post-medium h3')%>%
    html_text()
  
  date <- t %>% 
    html_nodes('.post-meta__option--datetime time') %>% 
    html_text()
  
  # works but scrapes every link twice. use an index to deal with duplicates
  article_link <- 
    t %>% 
    html_nodes('.post-medium a')%>%
    html_attr('href')
  
  # fix duplicates
  article_link <- article_link[keep_list]
  
  thumbnail_link <- t %>% 
    html_nodes('.size-archive-thumb')%>%
    html_attr('src')
  
  page_df <- data.frame("title" = title, 
                        "date" = date, 
                        "article_link" = article_link,
                        "thumbnail_link" =  thumbnail_link)
  
  return(page_df)
}
