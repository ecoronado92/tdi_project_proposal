# TDI Project Proposal (Spring 2020)

The following repo contains an early rough draft of The Data Incubator 2020 project proposal:
 - `scrape_tripadvisor.R`: Script to scrape data from TripAdvisor. __Note__: TripAdvisor is the sole proprietary of said data.
 - `eda_tripadvisor.R`: Script to generate figures 1 and 2 from this repository
 - `tripadvisor_reviews.rds`: Subset of scraped TripAdvisor reviews data
 
 The `tripadvisor_reviews.rds` contains the following fields:
 - `id`: review id
 - `hotel_name` : hotel name
 - `review_title`: title given to review
 - `comment`: user review
 - `author_info`: simple information about who wrote the review (e.g. username and date)
 - `rating`: numerical value of rating based on 1-5 bubbles in webpage
 
 
