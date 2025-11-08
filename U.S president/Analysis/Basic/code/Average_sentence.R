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


# get average length of whole president speech sentence
total_speech_sentence <- total_speech %>% distinct(doc_id, sentence_id, .keep_all = TRUE) %>% select(sentence) %>% mutate(length = "")

# get letter used in the each sentence
for(i in 1:nrow(total_speech_sentence)){
  total_speech_sentence[i, 2] = str_count(total_speech_sentence[i, 1])
}

# change type string to numeric
total_speech_sentence[ ,2] <- as.numeric(total_speech_sentence$length)

# check is there any common word
check_sentence <- total_speech_sentence %>% arrange(length)
head(check_sentence,30)

# delete common word

total_speech_sentence <- total_speech_sentence[total_speech_sentence$length > 15, ]
stop_sentence <- c("Thank you America", "FELLOW COUNTRYMEN", "My fellow citizens", "My Fellow citizens", "Thank you very much")
total_speech_sentence <- total_speech_sentence %>% filter(!sentence %in% stop_sentence)

# get average

avg1 <- total_speech_sentence %>% summarise(avg = mean(length)) %>% mutate(type = "total")

# demo party
demo_data <- after_1960_speech[after_1960_speech[ ,"party"] == "democratic", ]
demo_data$token <- tolower(demo_data$token)
demo_data$token <- lemmatize_words(demo_data$token)

# republic party
repu_data <- after_1960_speech[after_1960_speech[ ,"party"] == "republican", ]
repu_data$token <- tolower(repu_data$token)
repu_data$token <- lemmatize_words(repu_data$token)

# same as republican and democratic speech 
repu_data_sentence <- repu_data %>% distinct(doc_id, sentence_id, .keep_all = TRUE) %>% select(sentence) %>% mutate(length = "")
deom_data_sentence <- demo_data %>% distinct(doc_id, sentence_id, .keep_all = TRUE) %>% select(sentence) %>% mutate(length = "")
for(i in 1:nrow(repu_data_sentence)){
  repu_data_sentence[i, 2] = str_count(repu_data_sentence[i, 1])
}
for(i in 1:nrow(deom_data_sentence)){
  deom_data_sentence[i, 2] = str_count(deom_data_sentence[i, 1])
}

repu_data_sentence[ ,2] <- as.numeric(repu_data_sentence$length)
deom_data_sentence[ ,2] <- as.numeric(deom_data_sentence$length)

repu_data_sentence <- repu_data_sentence[repu_data_sentence$length > 15, ]
deom_data_sentence <- deom_data_sentence[deom_data_sentence$length > 15, ]
repu_data_sentence <- repu_data_sentence %>% filter(!sentence %in% stop_sentence)
deom_data_sentence <- deom_data_sentence %>% filter(!sentence %in% stop_sentence)
avg2 <- repu_data_sentence %>% summarise(avg = mean(length)) %>% mutate(type = "republican")
avg3 <- deom_data_sentence %>% summarise(avg = mean(length)) %>% mutate(type = "democratic")

# combine data, and make a ggplot
result <- bind_rows(avg1, avg2, avg3)
ggplot(result, aes(x = type, y = avg, fill = type)) + 
  geom_col(show.legend = T) + 
  geom_text(aes(label = round(avg, 2)), vjust = -0.5) + 
  labs(title = "average of president speech sentence length") + 
  xlab(NULL) + 
  ylab(NULL) + 
  theme_gray() + 
  theme(text = element_text(family = "a", size = 13), plot.title = element_text(hjust = 0.5, size = 17), panel.grid = element_blank(), axis.text.y = element_text(hjust = 1),  legend.position = "bottom")

## no difference, previous president speak more
previous <- total_speech[total_speech$year < 1960, ]
previous_sentence <- previous %>% distinct(doc_id, sentence_id, .keep_all = TRUE) %>% select(sentence) %>% mutate(length = "")

for(i in 1:nrow(previous_sentence)){
  previous_sentence[i, 2] = str_count(previous_sentence[i, 1])
}

previous_sentence[ ,2] <- as.numeric(previous_sentence$length)
previous_sentence <- previous_sentence[previous_sentence$length > 15, ]
previous_sentence <- previous_sentence %>% filter(!sentence %in% stop_sentence)
avg_1 <- data.frame(
  avg = mean(c(mean(repu_data_sentence$length), mean(deom_data_sentence$length))),
  type = "after 1960"
)

avg_2 <- previous_sentence %>% summarise(avg = mean(length)) %>% mutate(type = "previous")

result2 <- bind_rows(avg_1, avg_2)
ggplot(result2, aes(x = type, y = avg, fill = type)) + geom_col(show.legend = T) + 
  geom_text(aes(label = round(avg, 2), vjust = -0.5)) + 
  labs(title = "the average of president speech sentence length(before 1960, after 1960)") + 
  xlab(NULL) + 
  ylab(NULL) + 
  theme_classic() + 
  theme(text = element_text(family = "a", size = 13), plot.title = element_text(hjust = 0.5, size = 17), panel.grid = element_blank(), axis.text.y = element_text(hjust = 1),  legend.position = "bottom")

write.csv(result, "average_sentence_length.csv", row.names = FALSE)
write.csv(result2, "average_sentence_lengt_previous.csv", row.names = FALSE)

