### Web Scraping Assignment

#load the required libraries
library(rvest)
library(data.table)

# set the URL to be scraped
my_url <- 'https://petapixel.com/?s=pentax'

# write the page to a variable
t <- read_html(my_url)

# write it out so I can inspect it and see if the relevant content is there
write_html(t, 't.html')

boxes <- 
  t %>% 
  html_nodes('h3')

box_dfs <- data.frame()
boxes[[1]]

keep_list <- seq(from = 1, to = 20, by = 2)

# works but creates duplicates, could simply index with keep_list to remove them
my_relative_link <- 
  t %>% 
  html_nodes('.post-medium a')%>%
  html_attr('href')

#works
my_title <- t %>% html_nodes('.post-medium h3')%>%
  html_text()

# works
my_time <- t %>% html_nodes('.post-meta__option--datetime time') %>% html_text()

# works
my_thumbnail <- t %>% 
  html_nodes('.size-archive-thumb')%>%
  html_attr('src')

get_1_pentax_page <- function(my_url){
  
  t <- read_html(my_url)
  boxes <- 
    t %>% 
    html_nodes('.post-medium')
  
  box_dfs <- lapply(boxes, function(x){
    tlist <- list()
    tlist[['my_title']] <- 
      x %>% 
      html_nodes('h3')%>%
      html_text()
    
    my_relative_link <- 
      x%>% 
      html_nodes('post-medium__thumbnail')%>%
      html_attr('href')

    return(box_dfs)
  })}

df <- get_1_pentax_page(my_url)
