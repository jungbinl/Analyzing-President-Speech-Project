library(stringr)
library(dplyr)
library(ggplot2)
library(showtext)
library(scales)
library(tidytext)
library(tidyr)
library(DBI)
library(RMariaDB)

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
demo_data <- dbReadTable(con, "demo_data")
repu_data <- dbReadTable(con, "repu_data")

# Type-Token Ratio(nous, verb, adverbs ,adjectives)

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

raw_ttr <- function(df, party){
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
  
  ttr_result <- bind_rows(ttr, ttr_nous, ttr_verb, ttr_ad) %>% mutate(party = party)
  colnames(ttr_result) <-c("doc_id", "count", "unique","ttr","pos", "party")
  return(ttr_result)
  
}

demo <- ttr(demo_data, "democratic")
repu <- ttr(repu_data, "republican")

result <- bind_rows(demo, repu)

ggplot(result, aes(x = party, y = avg, fill = type)) + 
  geom_col(show.legend = T) + 
  facet_wrap(~ type, scales = "free_y") + 
  geom_text(aes(label = round(avg,2)), vjust = 9) + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "average Type-Token Ratio") + 
  theme_minimal() + 
  scale_fill_discrete(name = "avg_type", labels = c("avg" = "total", "avg_nous" = "nous", "avg_verb" = "verb", "avg_ad" = "adverb, adjective")) + 
  theme(text = element_text(family = "a", size = 13), plot.title = element_text(hjust = 0.5, size = 17), axis.text.y = element_text(hjust = 1),  legend.position = "bottom")

raw_demo <- raw_ttr(demo_data, "democratic")
raw_repu <- raw_ttr(repu_data, "republican")
raw_result <- bind_rows(raw_demo, raw_repu)

dbWriteTable(con, "ttr_result", raw_result)

# check is it saved
dbListTables(con)
