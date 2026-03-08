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

# stop word, too common and too many used
stop_word <- c()

count_word <- function(df, party){
  data <- df[df[ ,"party"] == party, ]
  data_count <- data %>% filter(upos == "NOUN") %>% count(lemma) %>% as.data.frame()
  data_exception <- data_count %>% filter(!lemma %in% stop_word, n > 10, str_count(lemma) > 1)
  top10_except <- data_exception %>% slice_max(n, n = 20, with_ties = T) %>% mutate(party = party)
  return(top10_except)
}

count_word_raw <- function(df){
  data <- df
  data_count <- data %>% filter(upos == "NOUN") %>% group_by(name) %>% count(lemma) %>% as.data.frame()
  data_exception <- data_count %>% filter(!lemma %in% stop_word, n > 10, str_count(lemma) > 1)
  top100_except <- data_exception %>% group_by(name) %>% slice_max(order_by = n, n = 100, with_ties = FALSE)
  return(top100_except)
}

count_graph <- function(df) {
  if(df$party[1] == "democratic"){
    title = "top 20 word in America democratic president"
  } else{
    title = "top 20 word in America republican president"
  }
  ggplot(df, aes(
    x = reorder(lemma, n),
    y = n,
    fill = lemma
  )) + geom_col(show.legend = F) + coord_flip() + geom_text(aes(label = n), hjust = -0.3) +  theme_classic() + labs(title = title, x = NULL, y = NULL) + theme(
    text = element_text(family = "a"),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 1)
  )
}

# get top 20 word 
demo_result <- count_word(demo_data, "democratic")
repu_result <- count_word(repu_data, "republican")

# based on top 20 wordm get stop word
stop_word <- c("government", "people", "country","nation", "time", "citizen", "constitution", "year", "men","today", "world", "day", "way", "thing", "lot")

demo_result <- count_word(demo_data, "democratic")
repu_result <- count_word(repu_data, "republican")

total_raw_result <- count_word_raw(total_data)

# make a each graph 
count_graph(demo_result)
count_graph(repu_result)

# compare republic, demo party
top10_total <- bind_rows(demo_result, repu_result)

ggplot(top10_total, aes(x = reorder_within(lemma, n, party), y = n, fill = party)) + 
  geom_col(show.legend = F, color = "black", size = 0.5) + 
  coord_flip() + 
  geom_text(aes(label = n), hjust = -0.1) + 
  labs(title = "top 10 word by party in the US president inaugural adress", x = NULL, y = NULL, caption = "Source: Inaugural Address Text Data") + facet_wrap( ~ party, scales = "free_y", ncol = 2)  + theme_bw() + 
  theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5, size = 16), panel.grid = element_blank(), axis.text.y = element_text(hjust = 1), legend.position = "bottom") + scale_x_reordered()


dbWriteTable(con, "count_result", total_raw_result)

# check is it saved
dbListTables(con)

dbDisconnect(con)
