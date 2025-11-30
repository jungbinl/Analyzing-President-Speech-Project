library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(showtext)

# pronouns usage

# load data
after_1960_speech <- read.csv("after_1960_speech.csv")

total_data <- read.csv("total_speech.csv")

font_add(family = "a", regular = "Oswald-Regular.ttf")
showtext_auto()

# labeling

pron <- total_data %>% filter(upos == "PRON")

self_focused_word <- c("i", "me", "my", "mine", "myself")
self_focused <- pron %>% filter(token %in% self_focused_word) %>% mutate(person = "self", plural = "one")

self_focused_multi_word <- c("we", "our", "ours", "ourselves", "us")
self_focused_multi <- pron %>% filter(token %in% self_focused_multi_word) %>% mutate(person = "self", plural = "multi")

you_focused_word <- c("you","your", "yours", "yourself")
you_focused <- pron %>% filter(token %in% you_focused_word) %>% mutate(person = "you", plural = "one")

other_focused_word <- c("he", "him", "his", "himself", "she", "her", "herself", "it", "its", "itself")
other_focused <- pron %>% filter(token %in% other_focused_word) %>% mutate(person = "other", plural = "one")

other_focuse_multi_word <- c("they", "them", "their", "theirs", "themselves")
other_focused_multi <- pron %>% filter(token %in% other_focuse_multi_word) %>% mutate(person = "other", plural = "multi")

indefinite_word <- c("anyone","anybody", "anything", "someone", "something", "everyone", "everybody", "everything", "nobody", "nothing")
indefinite <- pron %>% filter(token %in% indefinite_word) %>% mutate(person = "indefinite", plural = "one")

total <- bind_rows(self_focused, self_focused_multi, you_focused, other_focused_multi, other_focused, indefinite) %>% select("token", "person", "plural", "party")

## get pronoun usage
# 1. total pronoun usage
total_both <- total %>% group_by(plural, person) %>% summarise(n = n())
ggplot(total_both, aes(x = reorder_within(person, n, plural), y = n, fill = plural)) + 
  geom_col(show.legend = F) + 
  geom_text(aes(label = round(n, 3)), vjust = -0.4) +
  facet_wrap(~plural, scales = "free_x") + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "total pronouns usage") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

# 2. democratic party pronoun usage
demo_both <- total %>% filter(party == "democratic") %>% group_by(plural, person) %>% summarise(n = n())
ggplot(demo_both, aes(x = reorder_within(person, n, plural), y = n, fill = plural)) + 
  geom_col(show.legend = F) + 
  geom_text(aes(label = round(n, 3)), vjust = -0.4) +
  facet_wrap(~plural, scales = "free_x") + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "democratic party pronouns usage") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

# 3. republican party pronoun usage
repu_both <- total %>% filter(party == "republican") %>% group_by(plural, person) %>% summarise(n = n())
ggplot(repu_both, aes(x = reorder_within(person, n, plural), y = n, fill = plural)) + 
  geom_col(show.legend = F) + 
  geom_text(aes(label = round(n, 3)), vjust = -0.4) +
  facet_wrap(~plural, scales = "free_x") + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "republican party pronouns usage") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

## get pronouns ratio
# 1. democratic party pronoun ratio
total_demo = sum(demo_both$n)
demo_plural <- demo_both %>% group_by(plural) %>% summarise(ratio = sum(n) / total_demo) %>% mutate(party = "democratic")
demo_person <- demo_both %>% group_by(person) %>% summarise(ratio = sum(n) / total_demo) %>% mutate(party = "democratic")
# 2. republican party pronoun ratio
total_repu = sum(repu_both$n)
repu_plural <- repu_both %>% group_by(plural) %>% summarise(ratio = sum(n) / total_repu) %>% mutate(party = "republican")
repu_person <- repu_both %>% group_by(person) %>% summarise(ratio = sum(n) / total_repu) %>% mutate(party = "republican")

# 3. visualization
plural_ratio <- bind_rows(demo_plural, repu_plural)
person_ratio <- bind_rows(demo_person, repu_person)

ggplot(plural_ratio, aes(x = party, y = ratio, fill = plural)) + 
  geom_col(show.legend = F) + 
  geom_text(aes(label = round(ratio, 3)), vjust = -0.4) +
  facet_wrap(~plural, scales = "free_x") + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "republican party pronouns usage") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

ggplot(person_ratio, aes(x = party, y = ratio, fill = person)) + 
  geom_col(show.legend = F) + 
  geom_text(aes(label = round(ratio, 3)), vjust = 1.5) +
  facet_wrap(~person, scales = "free") + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "republican party pronouns usage") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

write.csv(repu_both, "repu_pronoun.csv", row.names = FALSE)
write.csv(demo_both, "demo_pronoun.csv", row.names = FALSE)
write.csv(plural_ratio, "plural_ratio.csv", row.names = FALSE)
write.csv(person_ratio, "person_ratio.csv", row.names = FALSE)
