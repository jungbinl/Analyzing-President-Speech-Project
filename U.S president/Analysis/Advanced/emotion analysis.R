library(dplyr)
library(tidytext)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(showtext)
library(textdata)

nrc <- get_sentiments("nrc")
colnames(nrc) = c("token", "sentiment")

after_1960_speech <- read.csv("after_1960_speech.csv")

total_data <- read.csv("total_speech.csv")
total_raw_data <- read.csv("total_raw_data.csv") %>% mutate(doc_id = row_number())

# calulate emotion score
total_count <- total_data %>% group_by(doc_id, token) %>% count()
emo_data <- left_join(total_count, nrc, by = "token") %>% na.omit()
emo_count <- emo_data %>% group_by(doc_id, sentiment) %>% summarise(score = sum(n))
emo_count <- left_join(emo_count, total_raw_data, by = "doc_id") %>% select("year", "name", "doc_id", "sentiment", "score")

emo_count <- emo_count %>% filter(sentiment != "positive", sentiment != "negative")
emo_neg_word = c("anger", "disgust", "fear", "sadness")
emo_neg <- emo_count %>% filter(sentiment %in% emo_neg_word)
emo_pos <- emo_count %>% filter(!sentiment %in% emo_neg_word)

# changes in emotional tone across speeches
p1 <- ggplot(emo_neg, aes(x = year, y = score, color = sentiment, group = sentiment)) + 
  geom_line(linewidth = 1, alpha = 0.9) +        
  geom_point(size = 2) +                           
  scale_color_brewer(palette = "Set2") +          
  xlab(NULL) + 
  ylab(NULL) + 
  labs(title = "Changes in negative emotional tone across speeches",
       color = "Sentiment") +                     
  theme_minimal(base_family = "a") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom",                 
    legend.key.width = unit(1.5, "cm"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

p2 <- ggplot(emo_pos, aes(x = year, y = score, color = sentiment, group = sentiment)) + 
  geom_line(linewidth = 1, alpha = 0.9) +        
  geom_point(size = 2) +                           
  scale_color_brewer(palette = "Set3") +          
  xlab(NULL) + 
  ylab(NULL) + 
  labs(title = "Changes in positive emotional tone across speeches",
       color = "Sentiment") +                     
  theme_minimal(base_family = "a") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom",                 
    legend.key.width = unit(1.5, "cm"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

ggplotly(p1) %>% layout(legend = list(orientation = "h",x = 0.5, xanchor = "center",y = -0.2))
ggplotly(p2) %>% layout(legend = list(orientation = "h",x = 0.5, xanchor = "center",y = -0.2))

# emotion score by party
party_count <- after_1960_speech %>% group_by(party, doc_id, token) %>% count()
party_emo_data <- left_join(party_count, nrc, by = "token") %>% na.omit()
party_emo_count <- party_emo_data %>% group_by(doc_id, sentiment) %>% summarise(score = sum(n))
party_emo_count <- left_join(party_emo_count, total_raw_data, by = "doc_id") %>% select("year", "name", "doc_id", "sentiment", "score", "party")
party_emo_count <- party_emo_count %>% filter(sentiment != "positive", sentiment != "negative")
demo_result <- party_emo_count %>% filter(party == "democratic")
repu_result <- party_emo_count %>% filter(party == "republican")
demo_emo_score <- party_emo_count %>% filter(party == "democratic") %>% group_by(sentiment) %>% summarise(mean = mean(score))
repu_emo_score <- party_emo_count %>% filter(party == "republican") %>% group_by(sentiment) %>% summarise(mean = mean(score))

# emotional tone score across speeches by party
emo_result <- party_emo_count %>% group_by(party, sentiment) %>% summarise(mean = mean(score))

ggplot(emo_result, aes(x = party, y = mean, fill = sentiment)) + 
  geom_col(show.legend = F) + 
  geom_text(aes(label = round(mean, 3)), vjust = 1.5) +
  facet_wrap(~sentiment, scale = "free") + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "emotion mean data by party") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

# difference in emotional tone score across speeches by party
emo_result <- emo_result %>% group_by(sentiment) %>% summarise(diff = max(mean) - min(mean))

ggplot(emo_result, aes(x = sentiment, y = diff, fill = sentiment)) + 
  geom_col(show.legend = F) + 
  geom_text(aes(label = round(diff, 3)), vjust = 1.5) +
  xlab(NULL) + ylab(NULL) + 
  labs(title = "emotion diff by party") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

write.csv(repu_result, "repu_emotion_score.csv", row.names = FALSE)
write.csv(demo_result, "demo_emotion_score.csv", row.names = FALSE)
write.csv(demo_emo_score, "demo_emotion_mean.csv", row.names = FALSE)
write.csv(repu_emo_score, "repu_emotion_mean.csv", row.names = FALSE)
