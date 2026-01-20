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

# 2.1 count word they used

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port=3307,
                 user = "root",
                 password = "Jblee0713!!", 
                 dbname = "president_text_analysis") # db name

# stop word, too common and too many used
stop_word <- c()

# same as this method, count word each party(use same stop word)
# demo party
demo_data <- dbReadTable(con, "demo_data")
repu_data <- dbReadTable(con, "repu_data")

count_word <- function(df, party){
  data <- df[df[ ,"party"] == party, ]
  data_count <- data %>% filter(upos == "NOUN") %>% count(lemma) %>% as.data.frame()
  data_exception <- data_count %>% filter(!lemma %in% stop_word, n > 10, str_count(lemma) > 1)
  top10_except <- data_exception %>% slice_max(n, n = 20, with_ties = T) %>% mutate(party = party)
  return(top10_except)
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


# save data in the Database
dbWriteTable(con, "demo_count_result", demo_result)
dbWriteTable(con, "repu_count_result", repu_result)

# check is it saved
dbListTables(con)
