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
library(stm)

#1.4 change pont
font_add(family = "a", regular = "Oswald-Regular.ttf")
showtext_auto()

# load database
con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port=3307,
                 user = "root",
                 password = "Jblee0713!!", 
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
inaugral_raw_data <- dbReadTable(con, "inagural_address")%>% mutate(doc_id = row_number()) %>% filter(party == "democratic" | party == "republican")
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


stm_modeling <- function(name_list, type){
  topic <- tibble()
  term <- tibble()
  for(i in name_list){
  inaugral <- inaugral_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words) %>% arrange(doc_id) %>% mutate(doc_id = 1) 
  inaugral <- inaugral %>% group_by(doc_id) %>% summarise(document = paste(token, collapse = " ")) %>% left_join(inaugral_raw_data, by = "doc_id") %>% select("doc_id", "document.x", "name", "year", "party") 
  
  weekly <- weekly_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words) %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + 1)
  weekly <- weekly %>% group_by(doc_id) %>% summarise(document = paste(token, collapse = " ")) %>% left_join(weekly_raw_data, by = "doc_id") %>% select("doc_id", "document.x", "name", "year", "party")
  
  union <- union_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words) %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + max(weekly$doc_id, 0))
  union <- union %>% group_by(doc_id) %>% summarise(document = paste(token, collapse = " ")) %>% left_join(union_raw_data, by = "doc_id") %>% select("doc_id", "document.x", "name", "year", "party")
  
  spoken <- spoken_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words)  %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + max(union$doc_id, 0))
  spoken <- spoken %>% group_by(doc_id) %>% summarise(document = paste(token, collapse = " ")) %>% left_join(spoken_raw_data, by = "doc_id") %>% select("doc_id", "document.x", "name", "year", "party")
  
  total_document <- bind_rows(inaugral, weekly, union, spoken)

  colnames(total_document) <- c("doc_id", "document", "name", "year", "party")
  
  processed <- textProcessor(documents = total_document$document, metadata = total_document %>% select(year, name, party))
  out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
  
  stm_model <- stm(
    documents = out$documents,
    vocab = out$vocab,
    K = 8,
    prevalence =~ s(year),
    data = out$meta,
    seed = 1234,
    gamma.prior = "L1"
  )
  
  doc_topic <- tidy(stm_model, matrix = "gamma") %>%
    rename(doc_id = document)
  
  doc_topic <- doc_topic %>%
    left_join(total_document %>% select(doc_id, name, year),
              by = "doc_id")
  
  topic_term <- tidy(stm_model, matrix = "beta")
  
  top_terms <- topic_term %>%
    group_by(topic) %>%
    slice_max(beta, n = 10, with_ties = FALSE)
  
  top_terms <- top_terms %>% mutate(name = i)
  
  topic <- bind_rows(topic, doc_topic)
  term <- bind_rows(term, top_terms)
  }
  if(type == 0){
    return(topic)
  } else if(type == 1){
    return(term)
  }
}

stm_party <- function(type){
  inaugral <- inaugral_data %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words) %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id))
  inaugral <- inaugral %>% group_by(doc_id) %>% summarise(document = paste(token, collapse = " ")) %>% left_join(inaugral_raw_data, by = "doc_id") %>% select("doc_id", "document.x", "name", "year", "party") 
  
  weekly <- weekly_data  %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words) %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + max(dense_rank(inaugral$doc_id), 0)) 
  weekly <- weekly %>% group_by(doc_id) %>% summarise(document = paste(token, collapse = " ")) %>% left_join(weekly_raw_data, by = "doc_id") %>% select("doc_id", "document.x", "name", "year", "party")
  
  union <- union_data %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words) %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + max(weekly$doc_id, 0)) 
  union <- union %>% group_by(doc_id) %>% summarise(document = paste(token, collapse = " ")) %>% left_join(union_raw_data, by = "doc_id") %>% select("doc_id", "document.x", "name", "year", "party")
  
  spoken <- spoken_data %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words)  %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + max(union$doc_id, 0)) 
  spoken <- spoken %>% group_by(doc_id) %>% summarise(document = paste(token, collapse = " ")) %>% left_join(spoken_raw_data, by = "doc_id") %>% select("doc_id", "document.x", "name", "year", "party")
  
  total_document <- bind_rows(inaugral, weekly, union, spoken) %>% filter(!is.na(party))

  colnames(total_document) <- c("doc_id", "document", "name", "year", "party")
  
  processed <- textProcessor(documents = total_document$document, metadata = total_document %>% select(year, name, party))
  out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
  
  stm_model_party <- stm(
    documents = out$documents,
    vocab = out$vocab,
    K = 8,
    prevalence =~ party,
    data = out$meta,
    seed = 1234
  )
  
  doc_topic_party <- tidy(stm_model_party, matrix = "gamma") %>%
    rename(doc_id = document)
  
  doc_topic_party <- doc_topic_party %>%
    left_join(total_document %>% select(doc_id, name, year, party),
              by = "doc_id")
  
  topic_term_party <- tidy(stm_model_party, matrix = "beta")
  
  top_terms_party <- topic_term_party %>%
    group_by(topic) %>%
    slice_max(beta, n = 10, with_ties = FALSE)
  
  top_terms_party <- top_terms_party
  
  if(type == 0){
    return(doc_topic_party)
  } else if(type == 1){
    return(top_terms_party)
  }
}

stm_topic <- stm_modeling(president, 0)
stm_term <- stm_modeling(president, 1)

stm_party_topic <- stm_party(0)
stm_party_term <- stm_party(1)


plot(stm_model, type = "perspectives", topics = c(1,2))

effects <- estimateEffect(1:8 ~ s(year), stm_model, out$meta)


dbWriteTable(con, "STM", stm_topic, overwrite = TRUE)
dbWriteTable(con, "STM_topic", stm_term, overwrite = TRUE)

dbWriteTable(con, "STM_party", stm_party_topic, overwrite = TRUE)
dbWriteTable(con, "STM_topic_party", stm_party_term, overwrite = TRUE)


