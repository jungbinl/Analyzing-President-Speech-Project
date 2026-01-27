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

ttr <- demo_data %>% group_by(doc_id) %>% summarise(count = n(), u = n_distinct(lemma), ratio = u / count)
avg_ttr <- ttr %>% summarise(avg = mean(ratio))
avg_ttr[1, 2] = "total"

ttr <- function(df, party){
  ttr <- df %>% group_by(doc_id) %>% summarise(count = n(), u = n_distinct(lemma), ratio = u / count)
  avg_ttr <- ttr %>% summarise(avg = mean(ratio))
  avg_ttr[1, 2] = "ttr"
  
  # nous
  nous <- df %>% filter(upos == "NOUN")
  ttr_nous <- nous %>% group_by(doc_id) %>% summarise(count = n(), u = n_distinct(token), ratio = u / count)
  avg_ttr_nous <- ttr_nous %>% summarise(avg = mean(ratio))
  avg_ttr_nous[1, 2] = "noun"
  
  # verb
  verb <- df %>% filter(upos == "VERB")
  ttr_verb <- verb %>% group_by(doc_id) %>% summarise(count = n(), u = n_distinct(token), ratio = u / count)
  avg_ttr_verb <- ttr_verb %>% summarise(avg = mean(ratio))
  avg_ttr_verb[1, 2] = "verb"
  
  # adverb, adjective
  ad <- df %>% filter(upos == "ADV" | upos == "ADJ")
  ttr_ad <- ad %>% group_by(doc_id) %>% summarise(count = n(), u = n_distinct(token), ratio = u / count)
  avg_ttr_ad <- ttr_ad %>% summarise(avg = mean(ratio))
  avg_ttr_ad[1, 2] = "adv, adj"
  
  ttr_result <- bind_rows(avg_ttr, avg_ttr_ad, avg_ttr_nous, avg_ttr_verb) %>% mutate(party = party)
  colnames(ttr_result) <-c("avg", "type", "party")
  return(ttr_result)
  
}

raw_ttr <- function(df, raw_df){

  ttr <- df %>% group_by(doc_id) %>% summarise(count = n(), u = n_distinct(lemma), ratio = u / count) %>% mutate(pos = "total")
  
  # nous
  nous <- df %>% filter(upos == "NOUN")
  ttr_nous <- nous %>% group_by(doc_id) %>% summarise(count = n(), u = n_distinct(lemma), ratio = u / count) %>% mutate(pos = "noun")
  
  # verb
  verb <- df %>% filter(upos == "VERB")
  ttr_verb <- verb %>% group_by(doc_id) %>% summarise(count = n(), u = n_distinct(lemma), ratio = u / count) %>% mutate(pos = "verb")
  
  # adverb, adjective
  ad <- df %>% filter(upos == "ADV" | upos == "ADJ")
  ttr_ad <- ad %>% group_by(doc_id) %>% summarise(count = n(), u = n_distinct(lemma), ratio = u / count) %>% mutate(pos = "ad")
  
  ttr_result <- bind_rows(ttr, ttr_nous, ttr_verb, ttr_ad)
  colnames(ttr_result) <-c("doc_id", "count", "unique","ttr","pos", "party")
  ttr_result <- ttr_result %>% left_join(raw_df, by = "doc_id")
  return(ttr_result)
  
}

demo <- ttr(demo_data, "democratic")
repu <- ttr(repu_data, "republican")

result <- bind_rows(demo, repu)

raw_inaugural <- raw_ttr(inaugral_data, inaugral_raw_data) %>% select("doc_id", "count", "unique", "ttr", "pos", "name", "party")
raw_weekly  <- raw_ttr(weekly_data, weekly_raw_data) %>% select("doc_id", "count", "unique", "ttr", "pos", "name", "party")
raw_union <- raw_ttr(union_data, union_raw_data) %>% select("doc_id", "count", "unique", "ttr", "pos", "name", "party")
raw_spoken <- raw_ttr(spoken_data, spoken_raw_data) %>% select("doc_id", "count", "unique", "ttr", "pos", "name", "party")

raw_result <- bind_rows(raw_inaugural, raw_weekly, raw_union, raw_spoken)

result <- raw_result %>% group_by(name) %>% summarise(ttr_mean = mean(ttr)) %>% left_join(raw_data, by ="name") %>% distinct(name, .keep_all = TRUE)


dbWriteTable(con, "ttr_result", result)

# check is it saved
dbListTables(con)

dbDisconnect(con)
