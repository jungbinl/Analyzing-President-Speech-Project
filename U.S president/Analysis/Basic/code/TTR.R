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

# Type-Token Ratio(nous, verb, adverbs ,adjectives)
# total word
ttr <- total_speech %>% group_by(doc_id) %>% summarise(count = n(), u = n_distinct(token), ratio = u / count)
ttr <- bind_cols(ttr, total_data)
avg_ttr <- ttr %>% group_by(party) %>% summarise(avg = mean(ratio))
avg_ttr[3, 1] = "total"
avg_ttr[3, 2] =  round(mean(avg_ttr$avg),3)

# nous
nous <- total_speech %>% filter(upos == "NOUN")
ttr_nous <- nous %>% group_by(doc_id) %>% summarise(count = n(), u = n_distinct(token), ratio = u / count)
ttr_nous <- bind_cols(ttr_nous, total_data)
avg_ttr_nous <- ttr_nous %>% group_by(party) %>% summarise(avg_nous = mean(ratio))
avg_ttr_nous[3, 1] = "total"
avg_ttr_nous[3, 2] =  round(mean(avg_ttr_nous$avg_nous),3)

# verb
verb <- total_speech %>% filter(upos == "VERB")
ttr_verb <- verb %>% group_by(doc_id) %>% summarise(count = n(), u = n_distinct(token), ratio = u / count)
ttr_verb <- bind_cols(ttr_verb, total_data)
avg_ttr_verb <- ttr_verb %>% group_by(party) %>% summarise(avg_verb = mean(ratio))
avg_ttr_verb[3, 1] = "total"
avg_ttr_verb[3, 2] =  round(mean(avg_ttr_verb$avg_verb),3)

# adverb, adjective
ad <- total_speech %>% filter(upos == "ADV" | upos == "ADJ")
ttr_ad <- ad %>% group_by(doc_id) %>% summarise(count = n(), u = n_distinct(token), ratio = u / count)
ttr_ad <- bind_cols(ttr_ad, total_data)
avg_ttr_ad <- ttr_ad %>% group_by(party) %>% summarise(avg_ad = mean(ratio))
avg_ttr_ad[3, 1] = "total"
avg_ttr_ad[3, 2] =  round(mean(avg_ttr_ad$avg_ad),3)

ttr_result <- left_join(avg_ttr, left_join(avg_ttr_nous, left_join(avg_ttr_verb, avg_ttr_ad, by = "party"), by = "party"), by = "party")

ttr_long <- ttr_result[ttr_result$party != "total", ] %>% pivot_longer(cols = c(avg, avg_nous, avg_verb, avg_ad), names_to = "value_type", values_to = "avg")

ggplot(ttr_long, aes(x = party, y = avg, fill = value_type)) + geom_col(show.legend = T) + facet_wrap(~ value_type, scales = "free_y") + geom_text(aes(label = round(avg,2)), vjust = 9) + xlab(NULL) + ylab(NULL) + labs(title = "average Type-Token Ratio") + theme_minimal() + scale_fill_discrete(name = "avg_type", labels = c("avg" = "total", "avg_nous" = "nous", "avg_verb" = "verb", "avg_ad" = "adverb, adjective")) + theme(text = element_text(family = "a", size = 13), plot.title = element_text(hjust = 0.5, size = 17), axis.text.y = element_text(hjust = 1),  legend.position = "bottom")

write.csv(ttr_long, "TTR.csv", row.names = FALSE)
