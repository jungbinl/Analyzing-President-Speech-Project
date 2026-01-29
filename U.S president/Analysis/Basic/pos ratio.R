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

# 2. pos ratio by party
pos_ratio <- function(df, party){
  total <- df %>% filter(party == party) %>% count()
  noun <- df %>% filter(party == party & upos == "NOUN") %>% count()
  verb <- df %>% filter(party == party & upos == "VERB") %>% count()
  adv <- df %>% filter(party == party & upos == "ADV") %>% count()
  adj <- df %>% filter(party == party & upos == "ADJ") %>% count()
  
  noun_ratio <- (noun / total) %>% mutate(party = "repu", pos = "noun")
  verb_ratio <- (verb / total) %>% mutate(party = "repu", pos = "verb")
  adv_ratio <- (adv / total) %>% mutate(party = "repu", pos = "adverb")
  adj_ratio <- (adj / total) %>% mutate(party = "repu", pos = "adjective")
  
  ratio <- bind_rows(noun_ratio, verb_ratio, adv_ratio, adj_ratio)
  
  return(ratio)
}

pos_ratio_graph <- function(df, party){
  string <- paste0(party, " party pos ratio in the whole address")
  p <- ggplot(df, aes(x = pos, y = n, fill = pos)) + 
    geom_col(show.legend = T) + 
    geom_text(aes(label = round(n, 3)), vjust = -0.4) +
    xlab(NULL) + ylab(NULL) + 
    labs(title = string) + 
    theme_classic() + 
    theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))
  return(p)
}

pos_ratio_raw <- function(df, party){
  total <- df %>% group_by(name) %>% count()
  colnames(total) <- c("name", "total")
  noun <- df %>% group_by(name) %>% filter(upos == "NOUN") %>% count() %>% mutate(pos = "noun") %>% left_join(total, by = "name")
  verb <- df %>% group_by(name) %>% filter(upos == "VERB") %>% count() %>% mutate(pos = "verb") %>% left_join(total, by = "name")
  adv <- df %>% group_by(name) %>% filter(upos == "ADV") %>% count() %>% mutate(pos = "adv") %>% left_join(total, by = "name")
  adj <- df %>% group_by(name) %>% filter(upos == "ADJ") %>% count() %>% mutate(pos = "adj") %>% left_join(total, by = "name")

  raw_ratio <- bind_rows(noun, verb, adv, adj)
  
  
  raw_ratio <- raw_ratio %>% mutate(pos_ratio = n/total)
  return(raw_ratio)
}

raw <- raw_data %>% distinct(name, .keep_all = T)

inaugral_raw <- pos_ratio_raw(inaugral_data)
weekly_raw <- pos_ratio_raw(weekly_data)
union_raw <- pos_ratio_raw(union_data)
spoken_raw <- pos_ratio_raw(spoken_data)

total_raw_result <- bind_rows(inaugral_raw, weekly_raw, union_raw, spoken_raw) %>% group_by(name, pos) %>% summarise(pos_ratio = mean(pos_ratio)) %>% left_join(raw, by = "name") %>% select(name, pos, pos_ratio, party)

pos_demo_result <- pos_ratio(demo_data, "democratic")
pos_repu_result <- pos_ratio(repu_data, "republican")

pos_ratio_graph(pos_demo_result, "democratic")
pos_ratio_graph(pos_repu_result, "democratic")



# 3. get difference
pos_demo_result <- pos_demo_result %>% mutate(party = "democratic")
pos_repu_result <- pos_repu_result %>% mutate(party = "repulican")

result <- bind_rows(pos_demo_result, pos_repu_result)

ggplot(result, aes(x = party, y = n, fill = party)) + 
  geom_col(show.legend = T) + 
  facet_wrap(~pos, scales = "free") +
  geom_text(aes(label = round(n, 3)), vjust = 1.5) +
  labs(title = "pos ratio difference in the whole inaugural address") + 
  xlab(NULL) + ylab(NULL) +
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))


dbWriteTable(con, "pos_ratio_result", total_raw_result)

# check is it saved
dbListTables(con)

dbDisconnect(con)
