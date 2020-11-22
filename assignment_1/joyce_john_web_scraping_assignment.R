### Web Scraping Assignment

#load the required libraries
library(tidyverse)
library(rvest)
library(data.table)

# set the URL to be scraped
my_url <- 'https://petapixel.com/?s=pentax'

# write the page to a variable
t <- read_html(my_url)

# write it out so I can inspect it and see if the relevant content is there
write_html(t, 't.html')

# make an index for dealing with duplicates
keep_list <- seq(from = 1, to = 20, by = 2)

# works but scrapes every title twice. use an index to deal with duplicates
article_link <- 
  t %>% 
  html_nodes('.post-medium a')%>%
  html_attr('href')

#works
title <- t %>% 
  html_nodes('.post-medium h3')%>%
  html_text()

# works
time_published <- t %>% 
  html_nodes('.post-meta__option--datetime time') %>% 
  html_text()

# works
thumbnail_link <- t %>% 
  html_nodes('.size-archive-thumb')%>%
  html_attr('src')

get_1_pentax_page <- function(my_url){
  
  
