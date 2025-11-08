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

# get top 10 most use word in the speech(to get stop word)
nous <- total_speech %>% filter(upos == "NOUN") %>% count(token) %>% as.data.frame()
nous_data <- nous %>% filter(n > 20, str_count(token) > 1)

top10_total <- nous_data %>% slice_max(n, n = 10, with_ties = T)

ggplot(top10_total, aes(x = reorder(token, n), y = n, fill = token)) + 
  geom_col(show.legend = F) + coord_flip() + geom_text(aes(label = n), hjust = -0.3) +  
  theme_classic() + 
  labs(title = "top 10 word in the America president inaugural adress", x = NULL, y = NULL) + 
  theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5), panel.grid = element_blank(), axis.text.y = element_text(hjust = 1))

# filtering stop word, too often use 
nous_data %>% slice_max(n, n = 20, with_ties = T)
stop_word <- c("government", "people", "country","nation", "time", "citizen", "constitution", "year", "men","today", "world", "day", "way")
nous_data_exception <- nous %>% filter(!token %in% stop_word, n > 10, str_count(token) > 1)
top10_except <- nous_data_exception %>% slice_max(n, n = 10, with_ties = T)

ggplot(top10_except, aes(x = reorder(token, n), y = n, fill = token)) + geom_col(show.legend = F) + 
  coord_flip() + 
  geom_text(aes(label = n), hjust = -0.3) +  
  theme_classic() + 
  labs(title = "top 10 word in the America president inaugural adress", x = NULL, y = NULL) + 
  theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5), panel.grid = element_blank(), axis.text.y = element_text(hjust = 1))

# same as this method, count word each party(use same stop word)
# demo party
demo_data <- after_1960_speech[after_1960_speech[ ,"party"] == "democratic", ]
demo_data$token <- tolower(demo_data$token)
demo_data$token <- lemmatize_words(demo_data$token)
demo_data_count <- demo_data %>% filter(upos == "NOUN") %>% count(token) %>% as.data.frame()
demo_data_exception <- demo_data_count %>% filter(!token %in% stop_word, n > 10, str_count(token) > 1)

top10_except_demo <- demo_data_exception %>% slice_max(n, n = 10, with_ties = T) %>% mutate(party = "democratic")

# republic party
repu_data <- after_1960_speech[after_1960_speech[ ,"party"] == "republican", ]
repu_data$token <- tolower(repu_data$token)
repu_data$token <- lemmatize_words(repu_data$token)
repu_data_count <- repu_data %>% filter(upos == "NOUN") %>% count(token) %>% as.data.frame()
repu_data_exception <- repu_data_count %>% filter(!token %in% stop_word, n > 10, str_count(token) > 1)
top10_except_repu <- repu_data_exception %>% slice_max(n, n = 10, with_ties = T)  %>% mutate(party = "republican")

# compare republic, demo party
top10_total <- bind_rows(top10_except_demo, top10_except_repu)

ggplot(top10_total, aes(x = reorder_within(token, n, party), y = n, fill = party)) + 
  geom_col(show.legend = F, color = "black", size = 0.5) + 
  coord_flip() + 
  geom_text(aes(label = n), hjust = -0.1) + 
  labs(title = "top 10 word by party in the US president inaugural adress", x = NULL, y = NULL, caption = "Source: Inaugural Address Text Data") + facet_wrap( ~ party, scales = "free_y", ncol = 2)  + theme_bw() + 
  theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5, size = 16), panel.grid = element_blank(), axis.text.y = element_text(hjust = 1), legend.position = "bottom") + scale_x_reordered()

write.csv(top10_total, "word_frequency.csv", row.names = FALSE)
write.csv(top10_except, "whole_word_frequency.csv", row.names = FALSE)
