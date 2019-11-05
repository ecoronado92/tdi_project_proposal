library(tidyverse)
library(tm)
library(plotrix)
library(dendextend)
library(quanteda)
library(SnowballC)
# Load trip advisor reviews
tripadvisor <- readRDS("./tripadvisor_reviews.rds") %>% 
  filter(rating != 0) %>% 
  rownames_to_column("id") %>% 
  mutate(review_title = str_trim(review_title),
         comment = str_to_lower(str_trim(comment)),
         reviewer = factor(str_remove(author_info,"\\s\\bwrote.*")),
         rating_label = if_else(rating <=3, "Bad", "Good"))
# Check rating balance from dataset
#table(tripadvisor$rating_label) 
# Extract positive reviews only
pos_rev <- tripadvisor %>% 
  filter(rating_label == "Good")

# Extract negative reviews only
neg_rev <- tripadvisor %>% 
  filter(rating_label == "Bad")
# Get top n reviewers
n_revs <- 5
top_15 <- tripadvisor %>% group_by(reviewer) %>% count() %>% arrange(desc(n)) %>% ungroup() %>% top_n(n_revs) %>% select(reviewer) %>%mutate(reviewer = as.character(reviewer))
# Exclude anonymous reviewers
top_reviewers <- tripadvisor %>% 
  filter(reviewer %in% top_15$reviewer) %>% 
  filter(!(reviewer %in% c("A TripAdvisor Member") ))

# Get possible words specifying the hotel
hotel_name_words <- sapply(listings, 
                           FUN = function(x){
                             tmp_str <- str_remove(x, ".*Reviews-") %>%
                               str_remove(., "\\.html") %>% 
                               strsplit(.,"(_)|(-)") %>% 
                               unlist() %>% unique()
                             return(tmp_str)
                             })
# Combine words
hotel_name_words <- hotel_name_words %>% unlist() %>% paste() %>% unique()
  
# create other common word vector
common_words <- c("but", "we", "also", "get","like",
                  "made", "can", "im", "just", "i",
                  "one", "time", "will",
                  "night", "two", "day", "amazing", "best",
                  "mexico", "went", "make", "mexican", "see", "fiesta",
                  "cabo", "great", "nice", "good")
# combine all words
extra_words <- c(common_words, hotel_name_words)

# Helper function to build corpus and remove extra words aside of stopping words
build_corpus <- function(df, extra_w ){
  # build corpus
  corpus <- VCorpus(VectorSource(df$comment)) %>% 
    tm_map(., removePunctuation) %>%                    # Remove punctuations
    tm_map(., removeWords, stopwords("english")) %>%    # Remove stopwords
    tm_map(., removeWords, stopwords("spanish")) %>% 
    tm_map(., removeWords, extra_w ) %>%                # Remove extra words provided
    tm_map(., stemDocument)
  return(corpus)
}

# Build general corpus for positive and neg reviews
pos_corpus <- build_corpus(pos_rev, extra_words)
neg_corpus <- build_corpus(neg_rev, extra_words)

# Get counts for top 15 most common in positive reviews
pos_tdm <- TermDocumentMatrix(pos_corpus)
pos_mat <- as.matrix(pos_tdm)
pos_count <- pos_mat %>%
  rowSums() %>% 
  data.frame("cnt" = .) %>% 
  rownames_to_column() %>% 
  arrange(desc(cnt)) %>% 
  top_n(15) %>% 
  mutate(rating_label = "Positive")
  
# Get counts for top 15 most common in negative reviews
neg_tdm <- TermDocumentMatrix(neg_corpus)
neg_mat <- as.matrix(neg_tdm)
neg_count <- neg_mat %>%
  rowSums() %>% 
  data.frame("cnt" = .) %>% 
  rownames_to_column() %>% 
  arrange(desc(cnt)) %>% 
  top_n(15) %>% 
  mutate(rating_label = "Negative")

# Combine in into one df to build graph
cnt_df <- bind_rows(pos_count, neg_count) %>% 
  arrange(desc(cnt))

# Build figure 1
p1 <- ggplot(cnt_df, aes(x = reorder(rowname, cnt), y = cnt, fill = rating_label)) +
  geom_col() +
  facet_wrap(.~ rating_label, scales = "free") +
  theme_bw() +
  labs(title = "Top words across positive and negative reviews",
       subtitle = "n = 16,400 reviews w/ stop words removed",
       y = "Count", x = "") +
  scale_fill_discrete(name = "Rating Class") +
  coord_flip() +
  theme(axis.text.x = element_text(size = 7),
        legend.position = "bottom")
ggsave(filename = "./figure1.png", plot = p1, device = "png", dpi = 150, units = "in",
       width = 5, height = 5)

# Helper function to build ngrams
build_ngrams <- function(df, n_gram, extra_w ){
  tmp_ngram <- tokens(df$comment) %>%
    tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
    tokens_remove(stopwords("english"), padding  = TRUE) %>%
    tokens_remove(extra_w, padding = TRUE) %>% 
    tokens_ngrams(n = n_gram) %>%
    dfm()
  return(tmp_ngram)
}

# Build positive and negative trigrams
pos_trigram <- build_ngrams(pos_rev, n_gram = 3, extra_words)
neg_trigram <- build_ngrams(neg_rev, n_gram = 3, extra_words)

# Create dataframe with top 15 positive and negative trigrams for plotting
pos_trigram <- data.frame(cnt =topfeatures(pos_trigram, n = 15)) %>% 
  rownames_to_column() %>% 
  mutate(rating_label = "Positive")

neg_trigram <- data.frame(cnt = topfeatures(neg_trigram, n = 15)) %>% 
  rownames_to_column() %>% 
  mutate(rating_label = "Negative")

trigram_combined <- bind_rows(pos_trigram, neg_trigram)

# Plot Figure 1a
p2 <- ggplot(trigram_combined, aes(x = reorder(rowname, cnt), y = cnt, fill = rating_label)) +
  geom_col() +
  facet_wrap(.~ rating_label, scales = "free") +
  theme_bw() +
  labs(title = "Top words across positive and negative reviews",
       subtitle = "n = 16,400 reviews w/ stop words removed",
       y = "Count", x = "") +
  scale_fill_discrete(name = "Rating Class") +
  coord_flip() +
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.position = "bottom")
ggsave(filename = "./figure1a.png", plot = p2, device = "png", dpi = 150, units = "in",
       width = 5, height = 5)

# Get top reviewer names
reviewer_names <- unique(as.character(top_reviewers$reviewer))

master_reviewer <- tibble(rowname = "",
                              cnt = 0,
                              rating_label = "",
                              reviewer = "")
# Loop through top reviewers and generate master reviewer df for plot
# Use same methods to generate figure 1
for (rev in reviewer_names){
  tmp_df <- top_reviewers %>% 
    filter(as.character(reviewer) == rev)
  tmp_pos <- tmp_df %>% 
    filter(rating_label == "Good")
  
  tmp_pos_corpus <- build_corpus(tmp_pos, extra_words)
  p_tdm <- TermDocumentMatrix(tmp_pos_corpus)
  tmp_pos_mat <- as.matrix(p_tdm)
  tmp_pos_count <- tmp_pos_mat %>%
    rowSums() %>% 
    data.frame("cnt" = .) %>% 
    rownames_to_column() %>% 
    arrange(desc(cnt)) %>% 
    top_n(5) %>% 
    mutate(rating_label = "Positive")
  tmp_neg <- tmp_df %>% 
    filter(rating_label == "Bad")
  tmp_neg_corpus <- build_corpus(tmp_neg, extra_words)
  
  n_tdm <- TermDocumentMatrix(tmp_neg_corpus)
  tmp_neg_mat <- as.matrix(n_tdm)
  tmp_neg_count <- tmp_neg_mat %>%
    rowSums() %>% 
    data.frame("cnt" = .) %>% 
    rownames_to_column() %>% 
    arrange(desc(cnt)) %>% 
    top_n(5) %>% 
    mutate(rating_label = "Negative")
  
  # If user only has negative or positive reviews, exclude for now
  if (nrow(tmp_neg_count) != 0 & nrow(tmp_pos_count) != 0 ){
    tmp_combine <- bind_rows(tmp_pos_count, tmp_neg_count) %>% 
      mutate(reviewer = rep(rev, nrow(tmp_pos_count) + nrow(tmp_neg_count) ))
    
    master_reviewer <- bind_rows(master_reviewer, tmp_combine)
  }
}

# Remove initialization row
master_reviewer <- master_reviewer %>% 
  filter(cnt != 0) 

# Figure 2
p3 <- ggplot(master_reviewer, aes(x = reorder(rowname, cnt), y = cnt, fill = rating_label)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(.~ reviewer, scales = "free") +
  theme_bw() +
  labs(title = "Common words across reviews from top 4 reviewers",
       subtitle = "Stacked bar charts; total counts",
       y = "Count", x = "") +
  scale_fill_discrete(name = "Rating Class") +
  coord_flip() +
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.position = "bottom")
ggsave(filename = "./figure2.png", plot = p3, device = "png", dpi = 150, units = "in",
       width = 5, height = 5)





