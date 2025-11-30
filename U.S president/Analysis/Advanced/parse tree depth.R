library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)

total_data <- read.csv("total_speech.csv")
raw_data <- read.csv("total_raw_data.csv") %>% mutate(doc_id = row_number())
new_data <- data.frame('doc_id' = 1, 'depth' = 2)

# parse tree depth function
for(d in 1:max(total_data$doc_id)){
  sum <- 0
  for(s in 1:max(total_data[total_data$doc_id == d, 3])){
    test <- total_data[total_data$sentence_id == s & total_data$doc_id == d, ]
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
  mean <- sum / max(total_data[total_data$doc_id == d, 3])
  new_data[d, ] = c(d, mean)
  print(d)
}

tree_depth <- left_join(raw_data, new_data, by = "doc_id") %>% as.data.frame()
tree_depth_result <- tree_depth %>% summarise(mean = mean(depth), max = max(depth), min = min(depth), median = median(depth))
tree_depth_long <- tree_depth_result %>% pivot_longer(cols = everything(),names_to = "statistic", values_to = "value")

ggplot(tree_depth_long, aes(x = statistic, y = value, fill = statistic)) + 
  geom_col() + 
  geom_text(aes(label = round(value, 3)), vjust = 1.5) +
  xlab(NULL) + ylab(NULL) + 
  labs(title = "parse tree depth") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

# compare between two party
total_tree_depth <- tree_depth %>% filter(party == "democratic" | party == "republican")
total_tree_depth <- total_tree_depth %>% group_by(party) %>% summarise(mean = mean(depth))

ggplot(total_tree_depth, aes(x = party, y = mean, fill = party)) + 
  geom_col() + 
  geom_text(aes(label = round(mean, 3)), vjust = 1.5) +
  xlab(NULL) + ylab(NULL) + 
  labs(title = "parse tree depth") + 
  theme_minimal() + 
  theme(text = element_text(family = "a"), plot.title = element_text(size = 15,hjust = 0.5))

write.csv(tree_depth, "parse_tree_depth.csv", row.names = F)
