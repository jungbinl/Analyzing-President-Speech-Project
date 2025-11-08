library(ggplot2)
library(dplyr)
library(stringr)
library(textstem)
library(scales)
library(tidytext)
library(tidyr)
library(showtext)

total_speech <- read.csv("total_speech.csv")
after_1960_speech <- read.csv("after_1960_speech.csv")

# change pont
font_add(family = "a", regular = "Oswald-Regular.ttf")
showtext_auto()

## sentiment anaylsis
swn <- read.delim("SentiWordNet_3.0.0.txt", comment.char = "#", header = FALSE, stringsAsFactors = FALSE)
swn <- swn %>% as.data.frame()
colnames(swn) = c("upos", "id", "posScore", "negScore", "token")

repu_data_count_upos <- repu_data %>% group_by(upos, token) %>% count(token) %>% mutate(upos = ifelse(upos == "NOUN", "n", ifelse(upos == "VERB", "v", ifelse(upos == "ADV" | upos == "ADJ", "a", "r"))))

swn_repu <- left_join(repu_data_count_upos, swn, by = c("token", "upos")) %>% filter(!is.na(posScore)) %>% mutate(party = "repu")

demo_data_count_upos <- demo_data %>% group_by(upos, token) %>% count(token) %>% mutate(upos = ifelse(upos == "NOUN", "n", ifelse(upos == "VERB", "v", ifelse(upos == "ADV" | upos == "ADJ", "a", "r"))))

swn_demo <- left_join(demo_data_count_upos, swn, by = c("token", "upos")) %>% filter(!is.na(posScore)) %>% mutate(party = "demo")

swn_result <- bind_rows(swn_demo, swn_repu) %>% group_by(party, upos, token) %>% summarise(posmean = mean(posScore), negmean = mean(negScore)) %>% mutate(Score = posmean - negmean)

swn_result <- swn_result %>% mutate(sentiment = ifelse(Score > 0.1, "pos", ifelse(Score < -0.1, "neg", "neu")))

swn_result_count <- swn_result %>% group_by(party, sentiment) %>% count(sentiment)
swn_result_count <- swn_result_count[swn_result_count[ , 2] != "neu", ]

ggplot(swn_result_count, aes(x = sentiment, y = n, fill = party)) + geom_col(show.legend = T) + geom_text(aes(label = n), vjust = -0.4) + xlab(NULL) + ylab(NULL) + facet_wrap(~ party) + labs(title = "number of sentiment word in inaugural adress") + theme_gray() + theme(text = element_text(family = "a", size = 13), plot.title = element_text(hjust = 0.5, size = 17), axis.text.y = element_text(hjust = 1),  legend.position = "bottom")

swn_result_score <- swn_result %>% group_by(party) %>% summarise(meanScore = sum(Score))

ggplot(swn_result_score, aes(x = party, y = meanScore, fill = party)) + 
  geom_col(show.legend = T) + 
  geom_text(aes(label = round(meanScore, 2)), vjust = -0.4) + 
  xlab(NULL) + 
  ylab(NULL) + 
  labs(title = "mean score of sentiment word in inaugural adress") + 
  theme_gray() + 
  theme(text = element_text(family = "a", size = 13), plot.title = element_text(hjust = 0.5, size = 17), axis.text.y = element_text(hjust = 1),  legend.position = "bottom")

write.csv(swn_result_count, "swn_count.csv", row.names = FALSE)
write.csv(swn_result_score, "swn_score.csv", row.names = FALSE)


