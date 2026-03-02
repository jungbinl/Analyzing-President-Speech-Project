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
library(widyr)
library(ggraph)
library(tidygraph)

#1.4 change pont
font_add(family = "a", regular = "Oswald-Regular.ttf")
showtext_auto()

# load database
con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port=3307,
                 user = "root",
                 password = "", 
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

president <- c("Joseph R. Biden, Jr.", "Barack Obama", "William J. Clinton", "Lyndon B. Johnson", "John F. Kennedy", "Donald J. Trump (2nd Term)", "Donald J. Trump (1st Term)", "George W. Bush", "Ronald Reagan", "Richard Nixon")

stop_words <- c("thing", "more", "time", "today", "year", "t", "other", "many", "lot", "first", "day", "american", "able")

# 1. co-occurance
pair <- function(name_list, type){
  total_result <- tibble()
  edges_result <- tibble()
  node_result <- tibble()
  for(i in name_list) {
    inaugral <- inaugral_data %>% filter(upos == "NOUN" |
                                           upos == "ADJ") %>% filter(name == i) %>% filter(!lemma %in% stop_words)
    inaugral_pair <- inaugral %>% pairwise_count(item = lemma,
                                                 feature = doc_id,
                                                 sort = T)
    
    union <- union_data %>% filter(upos == "NOUN" |
                                     upos == "ADJ") %>% filter(name == i) %>% filter(!lemma %in% stop_words)
    union_pair <- union %>% pairwise_count(item = lemma,
                                           feature = doc_id,
                                           sort = T)
    
    weekly <- weekly_data %>% filter(upos == "NOUN" |
                                       upos == "ADJ") %>% filter(name == i) %>% filter(!lemma %in% stop_words)
    weekly_pair <- weekly %>% pairwise_count(item = lemma,
                                             feature = doc_id,
                                             sort = T)
    
    spoken <- spoken_data %>% filter(upos == "NOUN" |
                                       upos == "ADJ") %>% filter(name == i) %>% filter(!lemma %in% stop_words)
    spoken_pair <- spoken %>% pairwise_count(item = lemma,
                                             feature = doc_id,
                                             sort = T)
    
    total <- bind_rows(inaugral_pair, union_pair, weekly_pair, spoken_pair) %>% group_by(item1, item2) %>% summarise(n = sum(n)) %>% filter(n > 20) %>% mutate(name = i) %>% ungroup()
    
    total <- total %>% slice_max(order_by = n, n = 200)
    
    graph <- total %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))
    
    edges <- graph %>%
      activate(edges) %>%      
      as_tibble() %>%
      rename(Source = from, Target = to, Weight = n) %>% mutate(name = i)
    
    nodes <- graph %>%
      activate(nodes) %>%
      as_tibble() %>%
      rename(Id = name) %>% mutate(name = i)
    
    edges_result <- bind_rows(edges_result, edges)
    node_result <- bind_rows(node_result, nodes)
    total_result <- bind_rows(total_result, total)
    print(paste0(i, " is done"))
    
  }
  return(list(
    edges = edges_result,
    nodes = node_result,
    total = total_result
  ))
}

pair_result <- pair("Donald J. Trump (2nd Term)", "e")
data <- pair("Donald J. Trump (2nd Term)", "e")
data[[1]]
data[[2]]
data[[3]]


democratic <- c("Joseph R. Biden, Jr.", "Barack Obama", "William J. Clinton", "Lyndon B. Johnson", "John F. Kennedy")
republican <- c("Donald J. Trump (2nd Term)", "Donald J. Trump (1st Term)", "George W. Bush", "Ronald Reagan", "Richard Nixon")
name <- "Joseph R. Biden, Jr."

demo_sub_pair <- pair(democratic) %>% slice_max(order_by = n, n = 100)  %>% as_tbl_graph()
repu_sub_pair <- pair(rebpulican) %>% slice_max(order_by = n, n = 100)  %>% as_tbl_graph()

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
demo_graph <- pair(democratic) %>% filter(n >= 100) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))
repu_graph <- pair(republican) %>% filter(n >= 100) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))

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

weekly_raw_data %>% filter(is.na(name))
pair_cors <- function(name_list){
  total_result <- tibble()
  edges_result <- tibble()
  node_result <- tibble()
  for(i in name_list) {
    inaugral <- inaugral_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words) %>% arrange(doc_id) %>% mutate(doc_id = 1) %>% select("doc_id", "lemma", "name")
    
    weekly <- weekly_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words) %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + 1) %>% select("doc_id", "lemma", "name")
    
    
    union <- union_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words) %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + max(weekly$doc_id, 0)) %>% select("doc_id", "lemma", "name")
    
    spoken <- spoken_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words)  %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + max(union$doc_id, 0)) %>% select("doc_id", "lemma", "name")
    
    total_document <- bind_rows(inaugral, weekly, union, spoken)
    
    colnames(total_document) <- c("doc_id", "lemma", "name")
    
    total <- total_document %>%
      distinct(doc_id, lemma) %>% add_count(lemma) %>% filter(n >= 20) %>% pairwise_cor(item = lemma, feature = doc_id, sort = T)
    
    total_top <- total %>% slice_max(order_by = correlation, n = 50)
    total_bottom <- total %>% slice_min(order_by = correlation, n = 50)
    total <- bind_rows(total_top, total_bottom) %>% mutate(name = i)
    
    graph <- total %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))
    
    edges <- graph %>%
      activate(edges) %>%      
      as_tibble() %>%
      rename(Source = from, Target = to, Cor = correlation) %>% mutate(name = i)
    
    nodes <- graph %>%
      activate(nodes) %>%
      as_tibble() %>%
      rename(Id = name) %>% mutate(name = i)
    
    edges_result <- bind_rows(edges_result, edges)
    node_result <- bind_rows(node_result, nodes)
    total_result <- bind_rows(total_result, total)
    print(paste0(i, " is done"))
    
  }
  return(list(
    edges = edges_result,
    nodes = node_result,
    total = total_result
  ))
}

demo_top_cors <- pair_cors(democratic, "t")

cor_long <- demo_top_cors %>% slice_max(correlation, n = 8)

ggplot(cor_long, aes(x = item1, y = item2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "red", mid = "pink", midpoint = 0.5) +
  theme_minimal() +
  labs(title = "Token Correlation Heatmap", x = "Item1", y = "Item2")

repu_top_cors <- pair_cors(republican, "t")

# make plot
ggplot(cor_long, aes(x = item1, y = item2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "red", mid = "pink", midpoint = 0.5) +
  theme_minimal() +
  labs(title = "Token Correlation Heatmap", x = "Item1", y = "Item2")

# make a network graph
demo_graph_cors <- pair_cors(democratic, "t") %>% filter(correlation > 0.55) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))

repu_graph_cors <- pair_cors(republican, "t") %>% filter(correlation > 0.55) %>% as_tbl_graph(directed = F) %>% mutate(centrality = centrality_degree(), group = as.factor(group_infomap()))

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
bigram <- function(name_list, type){
  total_result <- tibble()
  edges_result <- tibble()
  node_result <- tibble()
  for(i in name_list) {
    inaugral <- inaugral_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words) %>% arrange(doc_id) %>% mutate(doc_id = 1) %>% select("doc_id","sentence_id", "upos", "lemma", "name")
    
    weekly <- weekly_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words) %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + 1) %>% select("doc_id","sentence_id", "upos", "lemma", "name")
    
    
    union <- union_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words) %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + max(weekly$doc_id, 0)) %>% select("doc_id","sentence_id", "upos", "lemma", "name")
    
    spoken <- spoken_data %>% filter(name == i) %>% filter(upos == "NOUN" | upos == "ADJ") %>% filter(!token %in% stop_words)  %>% arrange(doc_id) %>% mutate(doc_id = dense_rank(doc_id) + max(union$doc_id, 0)) %>% select("doc_id","sentence_id", "upos", "lemma", "name")
    
    total_document <- bind_rows(inaugral, weekly, union, spoken)
    
    colnames(total_document) <- c("doc_id","sentence_id", "upos", "lemma", "name")
    
    data <- total_document %>% filter(upos == "NOUN" |
                                     upos == "ADJ") %>% filter(!lemma %in% stop_words)
    data <- data %>% group_by(doc_id, sentence_id) %>% summarise(sentence = paste(lemma, collapse = " "))
    bigram <- data %>% unnest_tokens(
      input = sentence,
      output = bigram,
      token = "ngrams",
      n = 2
    )
    bigram <- bigram %>% separate(bigram, c("word1", "word2"), sep = " ") %>% count(word1, word2, sort = T) %>% na.omit() %>% filter(word1 != word2) %>% mutate(name = i) %>% ungroup()
    
    bigram <- bigram %>% slice_max(order_by = n, n = 200)
    
    graph <- bigram %>% as_tbl_graph()
    
    edges <- graph %>%
      activate(edges) %>%
      as_tibble() %>%
      rename(Source = from,
             Target = to,
             count = n) %>% mutate(name = i) %>% mutate(name = i)
    
    nodes <- graph %>%
      activate(nodes) %>%
      as_tibble() %>%
      rename(Id = name) %>% mutate(name = i) %>% mutate(name = i)
    
    edges_result <- bind_rows(edges_result, edges)
    node_result <- bind_rows(node_result, nodes)
    total_result <- bind_rows(total_result, bigram)
    print(paste0(i, " is done"))
  }
  
  return(list(
    edges = edges_result,
    nodes = node_result,
    total = total_result
  ))
}

nodes <- tibble(name = unique(c(a$word1, a$word2)))

demo_pair_bigram_graph <- bigram(democratic, "t") %>% filter(n >= 7) %>% as_tbl_graph()
repu_pair_bigram_graph <- bigram(republican, "t") %>% filter(n >= 7) %>% as_tbl_graph()

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
View(pair_edge)

pair_data <- pair(president)

pair_edge <- pair_data[[1]]
pair_node <- pair_data[[2]]
pair_total <- pair_data[[3]]

pair_total <- pair_total %>%
  mutate(
    item_min = pmin(item1, item2),
    item_max = pmax(item1, item2)
  ) %>% group_by(name, item_min, item_max) %>% summarise(n = max(n))

pair_total <- pair_total %>% left_join(pair_node, by = c("item1" = "Id", "name"))

colnames(pair_total) <- c("name", "item1", "item2", "n")

dbWriteTable(con, "pair_edge", pair_edge, overwrite = TRUE)
dbWriteTable(con, "pair_node", pair_node, overwrite = TRUE)
dbWriteTable(con, "pair_data", pair_total, overwrite = TRUE)

cor_data <- pair_cors(president)

cor_edge <- cor_data[[1]]
cor_node <- cor_data[[2]]
cor_total <- cor_data[[3]]

cor_total <- cor_total %>%
  mutate(
    item_min = pmin(item1, item2),
    item_max = pmax(item1, item2)
  ) %>% group_by(name, item_min, item_max) %>% summarise(correlation = max(correlation))

colnames(cor_total) <- c("name", "item1", "item2", "correlation")

cor_total <- cor_total %>% left_join(cor_node, by = c("item1" = "Id", "name")) %>%
  mutate(label = if_else(correlation > 0, "pos", "neg"))

View(cor_total)

dbWriteTable(con, "cor_edge", cor_edge, overwrite = TRUE)
dbWriteTable(con, "cor_node", cor_node, overwrite = TRUE)
dbWriteTable(con, "cor_data", cor_total, overwrite = TRUE)

bigram <- bigram(president)

bigram_edge <- bigram[[1]]
bigram_node <- bigram[[2]]
bigram_total <- bigram[[3]]
bigram_total %>% arrange(item1)
View(bigram_total %>% arrange(word1))
bigram_total <- bigram_total %>%
  mutate(
    item_min = pmin(word1, word2),
    item_max = pmax(word1, word2)
  ) %>% group_by(name, item_min, item_max) %>% summarise(n = max(n))

colnames(bigram_total) <- c("name", "item1", "item2", "n")

bigram_total <- bigram_total %>% left_join(bigram_node, by = c("item1" = "Id", "name"))
View(bigram_total)
# bigram
dbWriteTable(con, "bigram_edge", bigram_edge, overwrite = TRUE)
dbWriteTable(con, "bigram_node", bigram_node, overwrite = TRUE)
dbWriteTable(con, "bigram_data", bigram_total, overwrite = TRUE)
# check is it saved
dbListTables(con)

dbDisconnect(con)


