### Web Scraping Assignment

# load the required libraries
library(tidyverse)
library(rvest)
library(data.table)

# this function scrapes multiple pages from Petapixel.com
# takes a number and a search term in quotes as arguments
get_results <- function(page_count, search_term) {
  
  # this function gets 1 page
  get_petapixel_page <- function(one_url) {
   
    # read the supplied URL into an object
    t <- read_html(one_url)
    
    # grab article titles
    title <- t %>%
      html_nodes('.post-medium h3') %>%
      html_text()
    
    # grab dates published
    date <- t %>%
      html_nodes('.post-meta__option--datetime time') %>%
      html_text()
    
    # grab links to the articles. it scrapes every link twice, but unique() removes duplicates
    article_link <-
      t %>%
      html_nodes('.post-medium a') %>%
      html_attr('href') %>% 
      unique()
    
    # grab links to the thumbnail images shown in search results
    thumbnail_link <- t %>%
      html_nodes('.size-archive-thumb') %>%
      html_attr('src')
    
    # dataframe of the scraped data
    page_df <- data.frame(
      "title" = title,
      "date" = date,
      "article_link" = article_link,
      "thumbnail_link" =  thumbnail_link
    )
    
    return(page_df)
  }
  
  # constructs a list of URLs to scrape with the user-supplied page_count and search_term
  url_list <-
    paste0("https://petapixel.com/page/",
           1:page_count,
           "/?s=",
           search_term)
  
  # lapply sends each URL to the scraping function, returns a list, and rbindlist binds it to a dataframe
  all_pages_df <- rbindlist(lapply(url_list, get_petapixel_page))
  
  # save the scraped content to CSV and RDS formats
  write.csv(all_pages_df, "petapixel_articles.csv")
  saveRDS(all_pages_df, "petapixel_articles.rds")
  
  # print a message to alert the user that function is finished
  print("Output saved to CSV and RDS formats.")
}

# call the function with arguments like this:
# get_results(number of pages to scrape, "topic")
get_results(2, "pentax")
