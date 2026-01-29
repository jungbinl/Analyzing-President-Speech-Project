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

# emotion analysis
nrc <- get_sentiments("nrc")
colnames(nrc) = c("token", "sentiment")
raw <- raw_data %>% distinct(name, .keep_all = T)

# calulate emotion score
raw_emotion <- function(df){
  total_count <- inaugral_data %>% group_by(name, token) %>% count()
  emo_data <- left_join(total_count, nrc, by = "token") %>% na.omit()
  emo_count <- emo_data %>% group_by(name, sentiment) %>% summarise(score = sum(n))
  emo_count <- left_join(emo_count, raw, by = "name") %>% select(name, sentiment, score, party)
  
}

emotion <- function(df, type){
  total_count <- df %>% group_by(doc_id, token) %>% count()
  emo_data <- left_join(total_count, nrc, by = "token") %>% na.omit()
  emo_count <- emo_data %>% group_by(doc_id, sentiment) %>% summarise(score = sum(n))
  emo_count <- left_join(emo_count, raw_data, by = "doc_id") %>% select("year", "name", "doc_id", "sentiment", "score")
  
  emo_count <- emo_count %>% filter(sentiment != "positive", sentiment != "negative")
  emo_neg_word = c("anger", "disgust", "fear", "sadness")
  emo_neg <- emo_count %>% filter(sentiment %in% emo_neg_word)
  emo_pos <- emo_count %>% filter(!sentiment %in% emo_neg_word)
  
  if(type == "pos"){
    return(emo_pos)
  } else if(type == "neg"){
    return(emo_neg)
  }
}

inaugral_raw <- raw_emotion(inaugral_data)
weekly_raw <- raw_emotion(weekly_data)
union_raw <- raw_emotion(union_data)
spoken_raw <- raw_emotion(spoken_data)

total_raw_result <- bind_rows(inaugral_raw, weekly_raw, union_raw, spoken_raw) %>% group_by(name, sentiment) %>% summarise(score = sum(score)) %>% left_join(raw, by = "name") %>% select(name, sentiment, score, party) %>% filter(!is.na(score))

neg <- emotion(data, "neg")
pos <- emotion(data, "pos")

graph <- function(df, type){
  string <- paste0("Changes in ", type, " emotional tone across speeches")
  p <- ggplot(df, aes(x = year, y = score, color = sentiment, group = sentiment)) + 
    geom_line(linewidth = 1, alpha = 0.9) +        
    geom_point(size = 2) +                           
    scale_color_brewer(palette = "Set2") +          
    xlab(NULL) + 
    ylab(NULL) + 
    labs(title = string,
         color = "Sentiment") +                     
    theme_minimal(base_family = "a") +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "bottom",                 
      legend.key.width = unit(1.5, "cm"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
  ggplotly(p) %>% layout(legend = list(orientation = "h",x = 0.5, xanchor = "center",y = -0.2))
}

graph(neg, "neg")
graph(pos, "pos")

# emotion score by party
emotion_party <- function(df){
  party_count <- df %>% group_by(party, doc_id, token) %>% count()
  party_emo_data <- left_join(party_count, nrc, by = "token") %>% na.omit()
  party_emo_count <- party_emo_data %>% group_by(doc_id, sentiment) %>% summarise(score = sum(n))
  party_emo_count <- left_join(party_emo_count, raw_data, by = "doc_id") %>% select("year", "name", "doc_id", "sentiment", "score", "party")
  party_emo_count <- party_emo_count %>% filter(sentiment != "positive", sentiment != "negative")
  demo_result <- party_emo_count %>% filter(party == "democratic")
  repu_result <- party_emo_count %>% filter(party == "republican")
  demo_emo_score <- party_emo_count %>% filter(party == "democratic") %>% group_by(sentiment) %>% summarise(mean = mean(score))
  repu_emo_score <- party_emo_count %>% filter(party == "republican") %>% group_by(sentiment) %>% summarise(mean = mean(score))
  emo_result <- party_emo_count %>% group_by(party, sentiment) %>% summarise(mean = mean(score)) %>% filter(!is.na(party))
  return(emo_result)
}

result <- emotion_party(data)

# emotional tone score across speeches by party
ggplot(result, aes(x = party, y = mean, fill = sentiment)) + 
  geom_col(show.legend = F) + 
  geom_text(aes(label = round(mean, 3)), vjust = 1.5) +
  facet_wrap(~sentiment, scale = "free") + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "emotion mean data by party") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

# difference in emotional tone score across speeches by party
emo_result <- result %>% group_by(sentiment) %>% summarise(diff = max(mean) - min(mean))

ggplot(emo_result, aes(x = sentiment, y = diff, fill = sentiment)) + 
  geom_col(show.legend = F) + 
  geom_text(aes(label = round(diff, 3)), vjust = 1.5) +
  xlab(NULL) + ylab(NULL) + 
  labs(title = "emotion diff by party") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

dbWriteTable(con, "emotion_result", total_raw_result)

# check is it saved
dbListTables(con)

dbDisconnect(con)
