library(dplyr)     
library(ggplot2)        
library(tidytext)      
library(stringr)        
library(tidyverse)    
library(viridis)        
library(tidytext)  

# load raw data
total_row_data <- read.csv("total_raw_data.csv")

after_1960_speech <- read.csv("after_1960_speech.csv")
total_speech <- read.csv("total_speech.csv")

# 1. get TF-IDF noun, verb, adj, adv
# filtering pos
total_data <- total_speech %>% filter(upos == "NOUN" | upos == "VERB" | upos == "ADV" | upos == "ADJ")
total_count <- total_data %>% group_by(doc_id) %>% count(token) %>% filter(str_count(token) > 1)

# get too common word
tf_idf <- total_count %>% bind_tf_idf(term = token, document = doc_id, n = n)
tf_idf <- tf_idf[tf_idf$n > 2, ]
tf_idf_exception <- tf_idf %>% group_by(doc_id) %>% slice_max(tf_idf, n = 20, with_ties = T)

# apply stop word
stop_word <- c("thank", "back", "again", "today", "very", "week", "get", "amerizan", "all", "few", "same", "hour", "too", "no", "th", "st", "let", "why", "go", "away", "how", "winter","govern", "certain", "uncertain", "moon","decade", "include", "importantly","catch")
tf_idf_top <- tf_idf %>% filter(!token %in% stop_word) %>% group_by(doc_id) %>% slice_max(tf_idf, n = 1, with_ties = F) %>% arrange(-tf_idf)

# get highest TF-IDF word and speech 
top5_speech <- tf_idf_top[1:5, ]

tf_idf_top10 <- tf_idf %>% filter(doc_id %in% top5_speech$doc_id, !token %in% stop_word) %>% group_by(doc_id) %>% slice_max(tf_idf, n = 10, with_ties = F)
temp_data <- total_row_data[unique(tf_idf_top10$doc_id), ]
tf_idf_result <- left_join(tf_idf_top10, temp_data, by = "doc_id")

# make plot
ggplot(tf_idf_result, aes(x = reorder_within(token, tf_idf, name), y = tf_idf, fill = name)) + 
  geom_col(show.legend = F) + 
  coord_flip() + 
  xlab(NULL) +
  ylab(NULL) +
  facet_wrap(~name, scales = "free") + 
  scale_x_reordered() + 
  scale_fill_viridis_d() + 
  theme_bw() + 
  labs(title = "U.S top 10 TF-IDF president inaugural speech ") +
  theme(text = element_text(family = "a"),
        plot.title = element_text(size = 16, hjust = 0.5))

# 2. get TF-IDF by party 
# make a party columns
party_data <- total_data %>% mutate(party = ifelse(is.na(party), "previous", party))
party_data <- party_data %>% group_by(party) %>% count(token) %>% filter(str_count(token) > 1)

# get TF-IDF
tf_idf_party <- party_data %>% bind_tf_idf(term = token, document = party, n = n)
tf_idf_party <- tf_idf_party[tf_idf_party$n > 3, ]
tf_idf_party_exception <- tf_idf_party %>% group_by(party) %>% slice_max(tf_idf, n = 20, with_ties = T)

# apply stop word
tf_idf_party_top10 <- tf_idf_party %>% filter(!token %in% stop_word) %>% group_by(party) %>% slice_max(tf_idf, n = 10, with_ties = F) %>% arrange(-tf_idf)

# make plot
ggplot(tf_idf_party_top10, aes(x = reorder_within(token, tf_idf, party), y = tf_idf, fill = party)) + 
  geom_col(show.legend = F) + 
  coord_flip() + 
  xlab(NULL) +
  ylab(NULL) +
  facet_wrap(~party, scales = "free") + 
  scale_x_reordered() + 
  scale_fill_viridis_d() + 
  theme_bw() + 
  labs(title = "U.S top 10 TF-IDF president inaugural speech by party") +
  theme(text = element_text(family = "a"),
        plot.title = element_text(size = 16, hjust = 0.5))

# 3. get Noun TF-IDF by party
# make a party columns
total_data <- total_speech %>% filter(upos == "NOUN")

party_data <- total_data %>% mutate(party = ifelse(is.na(party), "previous", party))
party_data <- party_data %>% group_by(party) %>% count(token) %>% filter(str_count(token) > 1)

# get TF-IDF
tf_idf_party <- party_data %>% bind_tf_idf(term = token, document = party, n = n)
tf_idf_party <- tf_idf_party[tf_idf_party$n > 3, ]
tf_idf_party_exception <- tf_idf_party %>% group_by(party) %>% slice_max(tf_idf, n = 20, with_ties = T)
View(tf_idf_party_exception)
# get stop word
stop_word <- c("winter", "age", "season", "day", "today", "eye", "face", "one", "other", "people", "ten", "decade", "moon")

# apply stop word
tf_idf_party_noun <- tf_idf_party %>% filter(!token %in% stop_word) %>% group_by(party) %>% slice_max(tf_idf, n = 10, with_ties = F) %>% arrange(-tf_idf)

# make plot
ggplot(tf_idf_party_noun, aes(x = reorder_within(token, tf_idf, party), y = tf_idf, fill = party)) + 
  geom_col(show.legend = F) + 
  coord_flip() + 
  xlab(NULL) +
  ylab(NULL) +
  facet_wrap(~party, scales = "free") + 
  scale_x_reordered() + 
  scale_fill_viridis_d() + 
  theme_bw() + 
  labs(title = "U.S top 10 TF-IDF(Noun) president inaugural speech by party") +
  theme(text = element_text(family = "a"),
        plot.title = element_text(size = 16, hjust = 0.5))

# make file
write.csv(tf_idf_party_top10, "TF-IDF.csv", row.names = FALSE)
write.csv(tf_idf_party_noun, "TF-IDF(noun).csv", row.names = FALSE)
