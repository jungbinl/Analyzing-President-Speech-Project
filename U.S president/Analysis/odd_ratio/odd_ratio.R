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

## odds ratio
odd_ratio_noun = after_1960_speech[after_1960_speech[ ,"upos"] == "NOUN", ] %>% count(party, token) %>% pivot_wider(names_from = party, values_from = n, values_fill = list(n = 0))
odd_ratio = odd_ratio_noun %>% mutate(ratio_demo = ((democratic+1)/(sum(democratic+1))), ratio_repu = ((republican+1)/(sum(republican+1))), odds_ratio = (ratio_demo/ratio_repu))

top_odd_ratio <- bind_rows(odd_ratio %>% slice_max(order_by = odds_ratio, n = 10), odd_ratio %>% slice_min(order_by = odds_ratio, n = 10))
top_odd_ratio <- top_odd_ratio %>% mutate(party = ifelse(odds_ratio > 1, "democratic", "republican"))

ggplot(top_odd_ratio, aes(x = reorder_within(token, odds_ratio, party), y = odds_ratio, fill = party)) + 
  geom_col(show.legend = T) + 
  geom_text(aes(label = round(odds_ratio, 2)), hjust = 1.2) + 
  facet_wrap(~ party, scales = "free") + coord_flip() + 
  xlab(NULL) + 
  ylab(NULL) + 
  labs(title = "odds ratio between democratic and republican president speech") + 
  theme_light() + 
  theme(text = element_text(family = "a", size = 13), plot.title = element_text(hjust = 0.5, size = 17), axis.text.y = element_text(hjust = 1),  legend.position = "bottom") + scale_x_reordered()

## log odds ratio
log_odd_ratio = odd_ratio %>% mutate(log_odd_ratio = log(odds_ratio))
top_log_odd_ratio <- bind_rows(log_odd_ratio %>% slice_max(order_by = log_odd_ratio, n = 10), log_odd_ratio %>% slice_min(order_by = log_odd_ratio, n = 10))
top_log_odd_ratio <- top_log_odd_ratio %>% mutate(party = ifelse(odds_ratio > 1, "democratic", "republican"))

ggplot(top_log_odd_ratio, aes(x = reorder(token, log_odd_ratio), y = log_odd_ratio, fill = party)) + 
  geom_col(show.legend = T) + 
  geom_text(aes(label = round(log_odd_ratio, 2)), hjust = 1.1) + 
  coord_flip() + 
  xlab(NULL) + 
  ylab(NULL) + 
  labs(title = "log odds ratio between democratic and republican president speech") + 
  theme_gray() + 
  theme(text = element_text(family = "a", size = 13), plot.title = element_text(hjust = 0.5, size = 17), axis.text.y = element_text(hjust = 1),  legend.position = "bottom")

write.csv(top_odd_ratio, "odds_ratio.csv", row.names = FALSE)
write.csv(top_log_odd_ratio, "log_odds_ratio.csv", row.names = FALSE)
