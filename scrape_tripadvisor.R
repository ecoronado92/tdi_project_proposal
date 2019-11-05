library(tidyverse)
library(rvest)

#set base url for country
base_url <- "https://www.tripadvisor.com/Hotels-g150768-oa"
country_url <- "-Mexico-Hotels.html"

num_pages <- 11 # Number of pages for Mexico
city_urls <- c()

# Scrape city specific urls (220 cities)
for (i in 1:num_pages){
  tmp_url <- paste0(base_url, i*20, country_url) 
  tmp_page <- read_html(tmp_url) %>% 
    html_nodes(".city") %>% 
    html_attr("href")
  
  city_urls <- c(city_urls, tmp_page)
  
  Sys.sleep(0.5)
}

# Get listings per city (~6,000)
listings <- c()
for(i in 1:length(city_urls)){
  tmp_url <- paste0("https://www.tripadvisor.com", city_urls[i])
  
  tmp_page <- read_html(tmp_url) %>% 
    html_nodes(".prominent") %>% 
    html_attr("href")
  
  listings <- c(listings, tmp_page)
  Sys.sleep(1)
}

# Helper function to extra multiple reviews per listing site
extract_reviews <- function(pre_str, post_str, rev_page){
  
  tmp_url <- paste0(pre_str, rev_page, post_str) # Build listing url
  tripadv_page <- read_html(tmp_url) # get listing html
  
  # Get hotel name
  list_name <- tripadv_page %>% html_nodes("#HEADING") %>% html_text()
  
  # Get review title
  title <- tripadv_page %>% 
    html_nodes(".hotels-review-list-parts-ReviewTitle__reviewTitleText--3QrTy") %>% 
    html_text()
  
  # Get review
  reviews <- tripadv_page %>% 
    html_nodes(".hotels-review-list-parts-ExpandableReview__reviewText--3oMkH") %>% 
    html_text() %>% 
    str_trim()
  
  # Get rating fron ui_bubble tage
  ratings <- tripadv_page %>% html_nodes(".hotels-review-list-parts-RatingLine__bubbles--1oCI4") %>% 
    html_nodes(".ui_bubble_rating") %>% 
    html_attr(., "class") %>% 
    str_remove_all(.,".*_") %>% 
    as.numeric()/10
  
  # Get info about author (note: future work will get username to avoid confusion)
  author <- tripadv_page %>%  
    html_nodes(".social-member-event-MemberEventOnObjectBlock__event_type--3njyv span") %>% 
    html_text()
  
  # build df to store review info
  tmp_dataframe <- tibble(hotel_name = list_name,
                              review_title = title,
                              author_info = author,
                              rating = ratings,
                              comment = reviews)
  
  return(tmp_dataframe)
  
  Sys.sleep(0.5) #sys.sleep to avoid IP ban
  
}

# create master df
master_df <- tibble(hotel_name = "",
       review_title = "",
       author_info = "",
       rating = 0,
       comment = "")


# for all listing, extract reviews in tmp df and append to master_df
for (j in 2:length(listings)){
 
  # Main page url
  main_url <-  paste0("https://www.tripadvisor.com/",listings[j])
  
  # Necessary to build looping structure for review urls
  pre_str <- str_extract(main_url, ".*Reviews-")
  post_str <- str_extract(main_url, "Reviews-.*") %>% 
    str_remove("Reviews")
  
  tmp_html <- read_html(main_url)  
  
  # Count how many reviews are present to set iteration
  num_reviews <- tmp_html %>% 
    html_nodes(".styleguide-grouping-tabs-GroupingTabs__active--3c2GR .hotels-community-content-common-TabBar__tabCount--1vwdQ") %>% 
    html_text() %>% 
    str_remove(., ",") %>% 
    as.numeric()
  
  # Get main info, and first 5 reviews
  review_df <- extract_reviews(pre_str, post_str, rev_page = "")
  page_idx <- 5
  
  # Catch error if no reviews
  if (is_empty(num_reviews)){
    num_reviews <- 0
  } 
  
  # If reviews >5, then loop through to get other reviews
  if ( ceiling(num_reviews/5) !=0){
    for (i in 2:ceiling(num_reviews/5)){
      idx_str <- paste0("or", page_idx) # Indexing of tripadvisor review pages
      tmp_df <- extract_reviews(pre_str, post_str, rev_page = idx_str)
      
      review_df <- bind_rows(review_df, tmp_df)
      
      page_idx = page_idx + 5 # for every review page, add 5 to access new reviews
    }
    
  } 
  
  print(paste(j, idx_str)) # Avoid system idle
  master_df <- bind_rows(master_df, review_df) # bind to master df
  saveRDS(master_df, "tripadvisor_reviews.rds") # save master df in case connection is lost
}




