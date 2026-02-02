library(devtools)

# GitHub에서 설치
install_github("nikita-moor/ldatuning")

library(ldatuning)
library(topicmodels)
library(tm)
library(textclean)
library(stringr)
library(dplyr)
library(ggplot2)
library(showtext)
library(scales)
library(tidytext)
library(tidyr)
library(DBI)
library(RMariaDB)
library(plotly)
library(widyr)
library(ggraph)
library(tidygraph)

#1.4 change pont
font_add(family = "a", regular = "Oswald-Regular.ttf")
showtext_auto()

# load database
con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port=3307,
                 user = "root",
                 password = "", 
                 dbname = "president_text_analysis") # db name

# load data
demo_data <- dbReadTable(con, "demo_data") %>% mutate(party = "democratic")
repu_data <- dbReadTable(con, "repu_data") %>% mutate(party = "republican")
raw_data <- dbReadTable(con, "president_data") %>% mutate(doc_id = row_number()) %>% filter(party == "democratic" | party == "republican")
total_data <- bind_rows(demo_data, raw_data)

spoken_data <- dbReadTable(con, "spoken_token")
spoken_raw_data <- dbReadTable(con, "spoken_address") %>% mutate(doc_id = row_number())
spoken_data <- spoken_data %>% filter(party == "democratic" | party == "republican")
inaugral_data <- dbReadTable(con, "inagural_token")
inaugral_raw_data <- dbReadTable(con, "inagural_address")%>% mutate(doc_id = row_number())
inaugral_data <- inaugral_data  %>% filter(party == "democratic" | party == "republican")
weekly_data <- dbReadTable(con, "weekly_token")
weekly_raw_data <- dbReadTable(con, "weekly_address")%>% mutate(doc_id = row_number())
weekly_data <- weekly_data %>% filter(party == "democratic" | party == "republican")
union_data <- dbReadTable(con, "union_token")
union_raw_data <- dbReadTable(con, "union_address")%>% mutate(doc_id = row_number())
union_data <- union_data %>% filter(party == "democratic" | party == "republican")

count_word <- inaugral_data %>% add_count(lemma) %>% select(-n)

stop_words <- c("thing", "more", "time", "today", "year", "t", "other", "many", "lot", "first", "day", "american", "able")

president <- c("Joseph R. Biden, Jr.", "Barack Obama", "William J. Clinton", "Lyndon B. Johnson", "John F. Kennedy", "Donald J. Trump (2nd Term)", "Donald J. Trump (1st Term)", "George W. Bush", "Ronald Reagan", "Richard Nixon")

topic_modeling <- function(name_list){
  for(i in name_list){
    inaugral <- inaugral_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% count(doc_id, lemma, sort = T) %>% arrange(doc_id) %>% mutate(doc_id = 1)
    weekly <- weekly_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% count(doc_id, lemma, sort = T) %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id))
    union <- union_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% count(doc_id, lemma, sort = T) %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + max(weekly$doc_id, 0) + 1)
    spoken <- spoken_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% count(doc_id, lemma, sort = T)  %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + max(union$doc_id, 0) + 1 )
    
    count_word <- bind_rows(inaugral, weekly, union, spoken) %>% filter(n > 1)

    count_word <- count_word %>% filter(!lemma %in% stop_words)
    
    dtm_comment <- count_word %>% cast_dtm(document = doc_id, term = lemma, value = n)
    
    lda_model <- LDA(dtm_comment, k = 8, method = "Gibbs", control = list(seed = 1234)) 
    doc_topic <- tidy(lda_model, matrix = "gamma")
    doc_class <- doc_topic %>% group_by(document) %>% slice_max(gamma, n = 1, with_ties = F)
    doc_class$document <- as.integer(doc_class$document) 
    new_topic <- count_word %>% left_join(doc_class, by = c("doc_id" = "document")) 
    new_topic <- new_topic %>% group_by(doc_id) %>% slice_max(n, n = 20) %>% mutate(name = i)
    
    result <- bind_rows(result, new_topic)
    
    print(paste0(i, " is done"))
  }
  return(result)
}

# find how many topic is work
models <- FindTopicsNumber(dtm = dtm_comment, topics = 2:20, return_models = T, control = list(seed = 1234))
models %>% select(topics, Griffiths2004)
FindTopicsNumber_plot(models)

result <- topic_modeling(president)

dbWriteTable(con, "LDA", result, overwrite = TRUE)

