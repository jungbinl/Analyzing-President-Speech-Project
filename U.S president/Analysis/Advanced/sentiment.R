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
raw_data <- dbReadTable(con, "president_data") %>% mutate(doc_id = row_number())

## sentiment anaylsis

swn <- function(type = 1){
  swn <- read.delim("SentiWordNet_3.0.0.txt", comment.char = "#", header = FALSE, stringsAsFactors = FALSE)
  swn <- swn %>% as.data.frame()
  colnames(swn) = c("upos", "id", "posScore", "negScore", "token")
  
  repu_data <- repu_data %>% filter(party == "republican")
  
  repu_data_count_upos <- repu_data %>% group_by(upos, token) %>% count(token) %>% mutate(upos = ifelse(upos == "NOUN", "n", ifelse(upos == "VERB", "v", ifelse(upos == "ADV" | upos == "ADJ", "a", "r"))))
  
  swn_repu <- left_join(repu_data_count_upos, swn, by = c("token", "upos")) %>% filter(!is.na(posScore)) %>% mutate(party = "repu")
  
  demo_data <- demo_data %>% filter(party == "democratic")
  demo_data_count_upos <- demo_data %>% group_by(upos, token) %>% count(token) %>% mutate(upos = ifelse(upos == "NOUN", "n", ifelse(upos == "VERB", "v", ifelse(upos == "ADV" | upos == "ADJ", "a", "r"))))
  
  swn_demo <- left_join(demo_data_count_upos, swn, by = c("token", "upos")) %>% filter(!is.na(posScore)) %>% mutate(party = "demo")
  
  swn_result <- bind_rows(swn_demo, swn_repu) %>% group_by(party, upos, token) %>% summarise(posmean = mean(posScore), negmean = mean(negScore)) %>% mutate(Score = posmean - negmean)
  
  swn_result <- swn_result %>% mutate(sentiment = ifelse(Score > 0.1, "pos", ifelse(Score < -0.1, "neg", "neu")))
  
  swn_result_count <- swn_result %>% group_by(party, sentiment) %>% count(sentiment)
  swn_result_count <- swn_result_count[swn_result_count[ , 2] != "neu", ]
  
  swn_result_score <- swn_result %>% group_by(party) %>% summarise(meanScore = mean(Score))
  if(type == 1){
    return(swn_result_score)
  } else if(type == 2){
    return(swn_result_count)
  }
}

swn_mean <- swn()
swn_count <- swn(2)

raw_swn <- function(df, type){
  swn <- read.delim("SentiWordNet_3.0.0.txt", comment.char = "#", header = FALSE, stringsAsFactors = FALSE)
  swn <- swn %>% as.data.frame()
  colnames(swn) = c("upos", "id", "posScore", "negScore", "token")
  
  
  data_count_upos <- df %>% group_by(name, upos, token) %>% count(token) %>% mutate(upos = ifelse(upos == "NOUN", "n", ifelse(upos == "VERB", "v", ifelse(upos == "ADV" | upos == "ADJ", "a", "r"))))
  
  swn_data <- left_join(data_count_upos, swn, by = c("token", "upos")) %>% filter(!is.na(posScore))
  
  swn_result <- swn_data %>% group_by(name, upos, token) %>% summarise(posmean = mean(posScore), negmean = mean(negScore)) %>% mutate(Score = posmean - negmean)
  
  swn_result <- swn_result %>% mutate(sentiment = ifelse(Score > 0.1, "pos", ifelse(Score < -0.1, "neg", "neu")))
  
  swn_result_count <- swn_result %>% group_by(name, sentiment) %>% count(sentiment)
  swn_result_count <- swn_result_count[swn_result_count[ , 2] != "neu", ]
  
  swn_result_score <- swn_result %>% group_by(name) %>% summarise(meanScore = mean(Score))
  if(type == "count"){
    return(swn_result_count)
  } else if(type == "score"){
    return(swn_result_score)
  }
}

raw <- raw_data %>% distinct(name, .keep_all = T)

inaugral_raw_count <- raw_swn(inaugral_data, "count")
weekly_raw_count <- raw_swn(weekly_data, "count")
union_raw_count <- raw_swn(union_data, "count")
spoken_raw_count <- raw_swn(spoken_data, "count")

total_count <- bind_rows(inaugral_raw_count, weekly_raw_count, union_raw_count, spoken_raw_count) %>% group_by(name, sentiment) %>% summarise(count = sum(n)) %>% left_join(raw, by = "name") %>% select(name, sentiment, count, party)

inaugral_raw_score <- raw_swn(inaugral_data, "score")
weekly_raw_score <- raw_swn(weekly_data, "score")
union_raw_score <- raw_swn(union_data, "score")
spoken_raw_score <- raw_swn(spoken_data, "score")

total_score <- bind_rows(inaugral_raw_score, weekly_raw_score, union_raw_score, spoken_raw_score) %>% group_by(name, meanScore) %>% summarise(meanScore = sum(meanScore)) %>% left_join(raw, by = "name") %>% select(name, meanScore, party)

ggplot(swn_count, aes(x = sentiment, y = n, fill = party)) + 
  geom_col(show.legend = T) + 
  geom_text(aes(label = n), vjust = -0.4) + 
  xlab(NULL) + ylab(NULL) + 
  facet_wrap(~ party) + 
  labs(title = "number of sentiment word in inaugural adress") + 
  theme_gray() + 
  theme(text = element_text(family = "a", size = 13), plot.title = element_text(hjust = 0.5, size = 17), axis.text.y = element_text(hjust = 1),  legend.position = "bottom")

ggplot(swn_mean, aes(x = party, y = meanScore, fill = party)) + 
  geom_col(show.legend = T) + 
  geom_text(aes(label = round(meanScore, 2)), vjust = -0.4) + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "mean score of sentiment word in inaugural adress") + 
  theme_gray() + 
  theme(text = element_text(family = "a", size = 13), plot.title = element_text(hjust = 0.5, size = 17), axis.text.y = element_text(hjust = 1),  legend.position = "bottom")

dbWriteTable(con, "sentiment_count_result", total_count)
dbWriteTable(con, "sentiment_score_result", total_score)

# check is it saved
dbListTables(con)

dbDisconnect(con)
