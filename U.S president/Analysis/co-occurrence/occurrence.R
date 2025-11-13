library(ggraph)
library(stringr)
library(dplyr)
library(tidytext)
library(tidyr)
library(showtext)
library(widyr)
library(tidygraph)

after_1960_speech <- read.csv("after_1960_speech.csv")

after_1960_data_demo <- after_1960_speech %>% filter(upos == "NOUN" | upos == "VERB" | upos == "ADJ") %>% filter(party == "democratic") %>% select("sentence_id", "sentence", "token")

after_1960_data_repu <- after_1960_speech %>% filter(upos == "NOUN" | upos == "VERB" | upos == "ADJ") %>% filter(party == "republican") %>% select("sentence_id", "sentence", "token")

# 1. co-occurance
# get pair
demo_pair <- after_1960_data_demo %>% pairwise_count(item = token, feature = sentence_id, sort = T)
repu_pair <- after_1960_data_repu %>% pairwise_count(item = token, feature = sentence_id, sort = T)

demo_sub_pair <- demo_pair %>% filter(n >= 15) %>% as_tbl_graph()
repu_sub_pair <- repu_pair %>% filter(n >= 20) %>% as_tbl_graph()

ggraph(demo_sub_pair, layout = "fr") + 
  geom_edge_link(color = "gray", alpha = 0.5) + 
  geom_node_point(color = "red", size = 5) + 
  geom_node_text(aes(label = name), repel = T, size = 5, family = "a") + 
  labs(title = "democratic president inaugural address relationship", x = NULL, y = NULL) + 
  theme_void() + 
  theme(text = element_text(family = "a"),
        plot.title = element_text(size = 12, hjust = 0.5))

ggraph(repu_sub_pair, layout = "fr") + 
  geom_edge_link(color = "gray", alpha = 0.5) + 
  geom_node_point(color = "red", size = 5) + 
  geom_node_text(aes(label = name), repel = T, size = 5, family = "a") + 
  labs(title = "republican president inaugural address relationship", x = NULL, y = NULL) + 
  theme_void() + 
  theme(text = element_text(family = "a"),
        plot.title = element_text(size = 12, hjust = 0.5))

# 2. co-occurrence with centrality
demo_graph <- demo_pair %>% filter(n >= 15) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))
repu_graph <- repu_pair %>% filter(n >= 20) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))

set.seed(1234)
ggraph(repu_graph, layout = "fr") + 
  geom_edge_link(color = "gray", alpha = 0.5) + 
  geom_node_point(aes(size = centrality, color = group), show.legend = F) + 
  scale_size(range = c(5, 15)) + 
  geom_node_text(aes(label = name), repel = T, size = 4, family = "a") + 
  theme_graph() +
  labs(title = "republican president inaugural address word relationship with centrality", x = NULL, y = NULL) +
  theme(text = element_text(family = "a"),
        plot.title = element_text(size = 16, hjust = 0.5, vjust = 2))

ggraph(demo_graph, layout = "fr") + 
  geom_edge_link(color = "gray", alpha = 0.5) + 
  geom_node_point(aes(size = centrality, color = group), show.legend = F) + 
  scale_size(range = c(5, 15)) + 
  geom_node_text(aes(label = name), repel = T, size = 4, family = "a") + 
  theme_graph() + 
  labs(title = "democratic president inaugural address word relationship with centrality", x = NULL, y = NULL) +
  theme(text = element_text(family = "a"),
        plot.title = element_text(size = 16, hjust = 0.5, vjust = 2))

# 3. correlation with word by party

demo_word_cors <- after_1960_data_demo %>% add_count(token) %>% filter(n >= 20) %>% pairwise_cor(item = token, feature = sentence_id, sort = T)
repu_word_cors <- after_1960_data_repu %>% add_count(token) %>% filter(n >= 20) %>% pairwise_cor(item = token, feature = sentence_id, sort = T)

demo_target <- c("together", "people", "stand", "nation", "make", "work")
repu_target <- c("peace","come","freedom", "people", "world", "nation")

demo_top_cors <- demo_word_cors %>% filter(item1 %in% demo_target) %>% group_by(item1) %>% slice_max(correlation, n = 8)
repu_top_cors <- repu_word_cors %>% filter(item1 %in% repu_target) %>% group_by(item1) %>% slice_max(correlation, n = 8)

# make plot
ggplot(demo_top_cors, aes(x = reorder_within(item2, correlation, item1), y = correlation, fill = item1)) + 
  geom_col(show.legend = F) + 
  coord_flip() + 
  facet_wrap(~ item1, scales = "free") + 
  scale_x_reordered() + 
  labs(title = "demo_top_cors", x = NULL, y = NULL) + 
  theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5))

ggplot(repu_top_cors, aes(x = reorder_within(item2, correlation, item1), y = correlation, fill = item1)) + 
  geom_col(show.legend = F) + 
  coord_flip() + 
  facet_wrap(~ item1, scales = "free") + 
  scale_x_reordered() + 
  labs(title = "repu_top_cors", x = NULL, y = NULL) + 
  theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5))

# make a network graph
demo_graph_cors <- demo_word_cors %>% filter(correlation > 0.3) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))

repu_graph_cors <- repu_word_cors %>% filter(correlation > 0.3) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))

set.seed(1234)
ggraph(demo_graph_cors, layout = "fr") + 
  geom_edge_link(color = "gray", aes(edge_alpha = correlation, edge_width = correlation), show.legend = F) +
  scale_edge_width(range = c(1, 4)) + 
  geom_node_point(aes(size = centrality, color = group), show.legend = F) + 
  scale_size(range = c(5, 15)) + 
  geom_node_text(aes(label = name), repel = T, size = 5, family = "a") + 
  theme_graph() + 
  labs(title = "democratic word correlation with network type graph", x = NULL, y = NULL) + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 14, hjust = 0.5))

ggraph(repu_graph_cors, layout = "fr") + 
  geom_edge_link(color = "gray", aes(edge_alpha = correlation, edge_width = correlation), show.legend = F) +
  scale_edge_width(range = c(1, 4)) + 
  geom_node_point(aes(size = centrality, color = group), show.legend = F) + 
  scale_size(range = c(5, 15)) + 
  geom_node_text(aes(label = name), repel = T, size = 5, family = "a") + 
  theme_graph() + 
  labs(title = "republican word correlation with network type graph", x = NULL, y = NULL) + 
  theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5))

# 4. bigram
after_1960_data_demo <- after_1960_speech %>% filter(upos == "NOUN" | upos == "VERB" | upos == "ADJ") %>% filter(party == "democratic")

after_1960_data_demo <- after_1960_data_demo %>% group_by(doc_id, sentence_id) %>% summarise(sentence = paste(token, collapse = " "))

demo_bigram <- after_1960_data_demo[ , 3] %>% unnest_tokens(input = sentence, output = bigram, token = "ngrams", n = 2)
demo_bigram <- demo_bigram %>% separate(bigram, c("word1", "word2"), sep = " ")
demo_pair_bigram <- demo_bigram %>% count(word1, word2, sort = T) %>% na.omit()
demo_pair_bigram_graph <- demo_pair_bigram %>% filter(n >= 4) %>% as_tbl_graph()

after_1960_data_repu <- after_1960_speech %>% filter(upos == "NOUN" | upos == "VERB" | upos == "ADJ") %>% filter(party == "republican")

after_1960_data_repu <- after_1960_data_repu %>% group_by(doc_id, sentence_id) %>% summarise(sentence = paste(token, collapse = " "))

repu_bigram <- after_1960_data_repu[ , 3] %>% unnest_tokens(input = sentence, output = bigram, token = "ngrams", n = 2)
repu_bigram <- repu_bigram %>% separate(bigram, c("word1", "word2"), sep = " ")
repu_pair_bigram <- repu_bigram %>% count(word1, word2, sort = T) %>% na.omit()
repu_pair_bigram_graph <- repu_pair_bigram %>% filter(n >= 4) %>% as_tbl_graph()

set.seed(1234)
ggraph(demo_pair_bigram_graph, layout = "fr") + 
  geom_edge_link(color = "gray", aes(edge_alpha = n, edge_width = n)) + 
  geom_node_point(color = "red", size = 5) + 
  geom_node_text(aes(label = name), repel = T, size = 5, family = "a") + 
  theme_graph() + 
  labs(title = "democratic word bigram with network type graph", x = NULL, y = NULL) + 
  theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5))

ggraph(repu_pair_bigram_graph, layout = "fr") + 
  geom_edge_link(color = "gray", aes(edge_alpha = n, edge_width = n)) + 
  geom_node_point(color = "red", size = 5) + 
  geom_node_text(aes(label = name), repel = T, size = 5, family = "a") + 
  theme_graph() + 
  labs(title = "republican word bigram with network type graph", x = NULL, y = NULL) + 
  theme(text = element_text(family = "a"), plot.title = element_text(hjust = 0.5))

write.csv(demo_pair, "demo_pair.csv", row.names = FALSE)
write.csv(repu_pair, "repu_pair.csv", row.names = FALSE)
write.csv(demo_word_cors, "demo_word_cors.csv", row.names = FALSE)
write.csv(repu_word_cors, "repu_word_cors.csv", row.names = FALSE)
write.csv(demo_pair_bigram, "demo_pair_bigram.csv", row.names = FALSE)
write.csv(repu_pair_bigram, "repu_pair_bigram.csv", row.names = FALSE)
