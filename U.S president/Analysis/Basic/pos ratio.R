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

dbWriteTable(con, "pos_ratio_result", result)

# check is it saved
dbListTables(con)

dbDisconnect(con)
