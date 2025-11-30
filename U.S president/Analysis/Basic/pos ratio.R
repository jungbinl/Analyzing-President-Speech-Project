library(dplyr)
library(ggplot2)
library(tidyr)
library(showtext)

after_1960_speech <- read.csv("after_1960_speech.csv")

total_data <- read.csv("total_speech.csv")

# 1. whole speech pos ratio
total <- total_data %>% count()
noun <- total_data %>% filter(upos == "NOUN") %>% count()
verb <- total_data %>% filter(upos == "VERB") %>% count()
adv <- total_data %>% filter(upos == "ADV") %>% count()
adj <- total_data %>% filter(upos == "ADJ") %>% count()

noun_ratio <- (noun / total) %>% mutate(pos = "noun")
verb_ratio <- (verb / total) %>% mutate(pos = "verb")
adv_ratio <- (adv / total) %>% mutate(pos = "adverb")
adj_ratio <- (adj / total) %>% mutate(pos = "adjective")

total_ratio <- bind_rows(noun_ratio, verb_ratio, adv_ratio, adj_ratio) %>% mutate(party = "total")

ggplot(total_ratio, aes(x = pos, y = n, fill = pos)) + 
  geom_col(show.legend = T) + 
  geom_text(aes(label = round(n, 2)), vjust = -0.4) +
  xlab(NULL) + ylab(NULL) + 
  labs(title = "pos ratio in the whole inaugural address") + 
  theme_classic() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))
  

# 2. pos ratio by party
# 2-1 republican
total_repu <- after_1960_speech %>% filter(party == "republican") %>% count()
noun_repu <- after_1960_speech %>% filter(party == "republican" & upos == "NOUN") %>% count()
verb_repu <- after_1960_speech %>% filter(party == "republican" & upos == "VERB") %>% count()
adv_repu <- after_1960_speech %>% filter(party == "republican" & upos == "ADV") %>% count()
adj_repu <- after_1960_speech %>% filter(party == "republican" & upos == "ADJ") %>% count()

noun_ratio_repu <- (noun_repu / total_repu) %>% mutate(party = "repu", pos = "noun")
verb_ratio_repu <- (verb_repu / total_repu) %>% mutate(party = "repu", pos = "verb")
adv_ratio_repu <- (adv_repu / total_repu) %>% mutate(party = "repu", pos = "adverb")
adj_ratio_repu <- (adj_repu / total_repu) %>% mutate(party = "repu", pos = "adjective")

repu_ratio <- bind_rows(noun_ratio_repu, verb_ratio_repu, adv_ratio_repu, adj_ratio_repu)

ggplot(repu_ratio, aes(x = pos, y = n, fill = pos)) + 
  geom_col(show.legend = T) + 
  geom_text(aes(label = round(n, 3)), vjust = -0.4) +
  xlab(NULL) + ylab(NULL) + 
  labs(title = "republican party pos ratio in the whole inaugural address") + 
  theme_classic() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

# 2-2 democratic
total_demo <- after_1960_speech %>% filter(party == "democratic") %>% count()
noun_demo <- after_1960_speech %>% filter(party == "democratic" & upos == "NOUN") %>% count()
verb_demo <- after_1960_speech %>% filter(party == "democratic" & upos == "VERB") %>% count()
adv_demo <- after_1960_speech %>% filter(party == "democratic" & upos == "ADV") %>% count()
adj_demo <- after_1960_speech %>% filter(party == "democratic" & upos == "ADJ") %>% count()

noun_ratio_demo <- (noun_demo / total_demo) %>% mutate(party = "demo", pos = "noun")
verb_ratio_demo <- (verb_demo / total_demo) %>% mutate(party = "demo", pos = "verb")
adv_ratio_demo <- (adv_demo / total_demo) %>% mutate(party = "demo", pos = "adverb")
adj_ratio_demo <- (adj_demo / total_demo) %>% mutate(party = "demo", pos = "adjective")

demo_ratio <- bind_rows(noun_ratio_demo, verb_ratio_demo, adv_ratio_demo, adj_ratio_demo)

ggplot(demo_ratio, aes(x = pos, y = n, fill = pos)) + 
  geom_col(show.legend = T) + 
  geom_text(aes(label = round(n, 3)), vjust = -0.4) +
  xlab(NULL) + ylab(NULL) + 
  labs(title = "democratic party pos ratio in the whole inaugural address") + 
  theme_classic() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

# 3. get difference
result <- bind_rows(total_ratio, demo_ratio, repu_ratio)

ggplot(result, aes(x = party, y = n, fill = party)) + 
  geom_col(show.legend = T) + 
  facet_wrap(~pos, scales = "free") +
  geom_text(aes(label = round(n, 3)), vjust = 1.5) +
  labs(title = "pos ratio difference in the whole inaugural address") + 
  xlab(NULL) + ylab(NULL) +
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

write.csv(result, "pos ratio.csv", row.names = FALSE)
