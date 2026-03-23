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

new_data <- data.frame(doc_id = integer(),document_depth = numeric())

spoken_data <- dbReadTable(con, "spoken_token")
spoken_data <- spoken_data %>% filter(party == "democratic" | party == "republican")
inaugral_data <- dbReadTable(con, "inagural_token")
inaugral_data <- inaugral_data  %>% filter(party == "democratic" | party == "republican")
weekly_data <- dbReadTable(con, "weekly_token")
weekly_data <- weekly_data %>% filter(party == "democratic" | party == "republican")
union_data <- dbReadTable(con, "union_token")
union_data <- union_data %>% filter(party == "democratic" | party == "republican")


parse_tree <- function(df){
  for(d in unique(df$doc_id)){
    sum <- 0
    for(s in 1:max(df[df$doc_id == d, 3])){
      test <- df[df$sentence_id == s & df$doc_id == d, ]
      max <- 0
      for (a in 1:nrow(test)) {
        count <- 0
        temp_a = a
        while(temp_a != 0) {
          temp <- test[test$token_id == temp_a, ]
          temp_a <- temp$head_token_id
          count <- count + 1
        }
        if(count-1 > max){
          max = count -1
        }
      }
      sum = sum + max
    }
    mean <- sum / max(df[df$doc_id == d, 3])
    new_data[d, ] = c(d, mean)
    print(d)
  }
  
  tree_depth <- left_join(raw_data, new_data, by = "doc_id") %>% as.data.frame()
  
  return(tree_depth)
}

inaugural_depth <- parse_tree(inaugral_data)
weekly_depth <- parse_tree(weekly_data)
union_depth <- parse_tree(union_data)
spoken_depth <- parse_tree(spoken_data)
total_depth <- bind_rows(inaugural_depth, weekly_depth, union_depth, spoken_depth) %>% filter(!is.na(document_depth))

total_result <- total_depth %>% group_by(name) %>% summarise(depth = mean(document_depth))
total_result <- total_result %>% left_join(raw_data, by = "name") %>% distinct(name, .keep_all = TRUE)

total_demo <- total_result %>% filter(party == "democratic")
total_repu <- total_result %>% filter(party == "republican")

tree_depth_result <- total_demo %>% summarise(mean = mean(depth), max = max(depth), min = min(depth), median = median(depth))
tree_depth_long <- tree_depth_result %>% pivot_longer(cols = everything(),names_to = "statistic", values_to = "value")

ggplot(tree_depth_long, aes(x = statistic, y = value, fill = statistic)) + 
  geom_col() + 
  geom_text(aes(label = round(value, 3)), vjust = 1.5) +
  xlab(NULL) + ylab(NULL) + 
  labs(title = "parse tree depth democratic") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

tree_depth_result <- total_repu %>% summarise(mean = mean(depth), max = max(depth), min = min(depth), median = median(depth))
tree_depth_long <- tree_depth_result %>% pivot_longer(cols = everything(),names_to = "statistic", values_to = "value")

ggplot(tree_depth_long, aes(x = statistic, y = value, fill = statistic)) + 
  geom_col() + 
  geom_text(aes(label = round(value, 3)), vjust = 1.5) +
  xlab(NULL) + ylab(NULL) + 
  labs(title = "parse tree depth republican") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

# compare between two party
total_tree_depth <- total_result %>% group_by(party) %>% summarise(mean = mean(depth))

ggplot(total_tree_depth, aes(x = party, y = mean, fill = party)) + 
  geom_col() + 
  geom_text(aes(label = round(mean, 3)), vjust = 1.5) +
  xlab(NULL) + ylab(NULL) + 
  labs(title = "parse tree depth") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

dbWriteTable(con, "depth_result", total_result)

# check is it saved
dbListTables(con)

dbDisconnect(con)
