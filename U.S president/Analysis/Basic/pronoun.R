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
inaugral_raw_data <- dbReadTable(con, "inagural_address")%>% mutate(doc_id = row_number())
inaugral_data <- inaugral_data  %>% filter(party == "democratic" | party == "republican")
weekly_data <- dbReadTable(con, "weekly_token")
weekly_raw_data <- dbReadTable(con, "weekly_address")%>% mutate(doc_id = row_number())
weekly_data <- weekly_data %>% filter(party == "democratic" | party == "republican")
union_data <- dbReadTable(con, "union_token")
union_raw_data <- dbReadTable(con, "union_address")%>% mutate(doc_id = row_number())
union_data <- union_data %>% filter(party == "democratic" | party == "republican")

# pronouns usage
# labeling
pronoun <- function(df, party){
  pron <- df %>% filter(upos == "PRON")
  
  self_focused_word <- c("i", "me", "my", "mine", "myself")
  self_focused <- pron %>% filter(token %in% self_focused_word) %>% mutate(person = "self", plural = "one")
  
  self_focused_multi_word <- c("we", "our", "ours", "ourselves", "us")
  self_focused_multi <- pron %>% filter(token %in% self_focused_multi_word) %>% mutate(person = "self", plural = "multi")
  
  you_focused_word <- c("you","your", "yours", "yourself")
  you_focused <- pron %>% filter(token %in% you_focused_word) %>% mutate(person = "you", plural = "one")
  
  other_focused_word <- c("he", "him", "his", "himself", "she", "her", "herself", "it", "its", "itself")
  other_focused <- pron %>% filter(token %in% other_focused_word) %>% mutate(person = "other", plural = "one")
  
  other_focuse_multi_word <- c("they", "them", "their", "theirs", "themselves")
  other_focused_multi <- pron %>% filter(token %in% other_focuse_multi_word) %>% mutate(person = "other", plural = "multi")
  
  indefinite_word <- c("anyone","anybody", "anything", "someone", "something", "everyone", "everybody", "everything", "nobody", "nothing")
  indefinite <- pron %>% filter(token %in% indefinite_word) %>% mutate(person = "indefinite", plural = "one")
  
  total <- bind_rows(self_focused, self_focused_multi, you_focused, other_focused_multi, other_focused, indefinite) %>% select("token", "person", "plural", "party")
  if(party == "total"){
    total_both <- total %>% group_by(plural, person) %>% summarise(n = n())
    return(total_both)
  } else if(party == "democratic"){
    demo_both <- total %>% filter(party == "democratic") %>% group_by(plural, person) %>% summarise(n = n())
    return(demo_both)
  } else if(party == "republican"){
    repu_both <- total %>% filter(party == "republican") %>% group_by(plural, person) %>% summarise(n = n())
    return(repu_both)
  }
  
}

raw_pronoun <- function(df){
  pron <- df %>% filter(upos == "PRON")
  
  self_focused_word <- c("i", "me", "my", "mine", "myself")
  self_focused <- pron %>% filter(token %in% self_focused_word) %>% mutate(person = "self", plural = "one")
  
  self_focused_multi_word <- c("we", "our", "ours", "ourselves", "us")
  self_focused_multi <- pron %>% filter(token %in% self_focused_multi_word) %>% mutate(person = "self", plural = "multi")
  
  you_focused_word <- c("you","your", "yours", "yourself")
  you_focused <- pron %>% filter(token %in% you_focused_word) %>% mutate(person = "you", plural = "one")
  
  other_focused_word <- c("he", "him", "his", "himself", "she", "her", "herself", "it", "its", "itself")
  other_focused <- pron %>% filter(token %in% other_focused_word) %>% mutate(person = "other", plural = "one")
  
  other_focuse_multi_word <- c("they", "them", "their", "theirs", "themselves")
  other_focused_multi <- pron %>% filter(token %in% other_focuse_multi_word) %>% mutate(person = "other", plural = "multi")
  
  indefinite_word <- c("anyone","anybody", "anything", "someone", "something", "everyone", "everybody", "everything", "nobody", "nothing")
  indefinite <- pron %>% filter(token %in% indefinite_word) %>% mutate(person = "indefinite", plural = "one")
  
  total <- bind_rows(self_focused, self_focused_multi, you_focused, other_focused_multi, other_focused, indefinite) %>% group_by(name, person, plural) %>% summarise(pronoun = n())
  
  
  return(total)
}


graph <-function(df, type){
  title = paste0(type, " pronouns usage")
  p <- ggplot(df, aes(x = reorder_within(person, n, plural), y = n, fill = plural)) + 
    geom_col(show.legend = F) + 
    geom_text(aes(label = round(n, 3)), vjust = -0.4) +
    facet_wrap(~plural, scales = "free_x") + 
    xlab(NULL) + ylab(NULL) + 
    labs(title = title) + 
    theme_minimal() + 
    theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))
  return(p)
}

pronoun_total_data <- pronoun(data, "total")
pronoun_demo_data <- pronoun(data, "democratic")
pronoun_repu_data <- pronoun(data, "republican")

graph(pronoun_total_data, "total")
graph(pronoun_demo_data, "democratic")
graph(pronoun_repu_data, "republican")

pronoun_ratio <- function(df, type) {
  
  demo_both <- pronoun(df, "democratic")
  repu_both <- pronoun(df, "republican")
  
  # 1. democratic party pronoun ratio
  total_demo = sum(demo_both$n)
  demo_plural <- demo_both %>% group_by(plural) %>% summarise(ratio = sum(n) / total_demo) %>% mutate(party = "democratic")
  demo_person <- demo_both %>% group_by(person) %>% summarise(ratio = sum(n) / total_demo) %>% mutate(party = "democratic")
  # 2. republican party pronoun ratio
  total_repu = sum(repu_both$n)
  repu_plural <- repu_both %>% group_by(plural) %>% summarise(ratio = sum(n) / total_repu) %>% mutate(party = "republican")
  repu_person <- repu_both %>% group_by(person) %>% summarise(ratio = sum(n) / total_repu) %>% mutate(party = "republican")
  
  plural_ratio <- bind_rows(demo_plural, repu_plural)
  person_ratio <- bind_rows(demo_person, repu_person)
  
  if(type == "plural"){
    return(plural_ratio)
  } else if(type == "person"){
    return(person_ratio)
  }
  
}

raw <- raw_data %>% distinct(name, .keep_all = T)

inaugral_raw <- raw_pronoun(inaugral_data)
weekly_raw <- raw_pronoun(weekly_data)
union_raw <- raw_pronoun(union_data)
spoken_raw <- raw_pronoun(spoken_data)

total <- bind_rows(inaugral_raw, weekly_raw, union_raw, spoken_raw) %>% group_by(name, person, plural) %>% summarise(n = sum(pronoun)) %>% group_by(name) %>% mutate(total_n = sum(n), ratio = n / total_n) 

total_raw_result <- total %>% left_join(raw, by = "name") %>% select(name, person, plural, n, ratio, party)

plural_ratio <- pronoun_ratio(data, "plural")
person_ratio <- pronoun_ratio(data, "person")

ggplot(plural_ratio, aes(x = party, y = ratio, fill = plural)) + 
  geom_col(show.legend = F) + 
  geom_text(aes(label = round(ratio, 3)), vjust = -0.4) +
  facet_wrap(~plural, scales = "free_x") + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "republican party pronouns usage") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

ggplot(person_ratio, aes(x = party, y = ratio, fill = person)) + 
  geom_col(show.legend = F) + 
  geom_text(aes(label = round(ratio, 3)), vjust = 1.5) +
  facet_wrap(~person, scales = "free") + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "republican party pronouns usage") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))


dbWriteTable(con, "pronoun_result", total_raw_result)

# check is it saved
dbListTables(con)

dbDisconnect(con)
