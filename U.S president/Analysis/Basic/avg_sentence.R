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
                 password = "Jblee0713!!", 
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

# too common, too many used
stop_sentence <- c()

# average function
raw_average <- function(df, raw_data){
  data <- df %>% group_by(doc_id, sentence_id) %>% mutate(number = 1)
  data1 <- data %>% group_by(doc_id, sentence_id) %>% summarise(m = sum(number))
  data2 <- data1 %>% group_by(doc_id) %>% summarise(avg = mean(m)) %>% left_join(raw_data, by = "doc_id") %>% select("doc_id", "avg", "name", "party")
  return(data2)
}

average <- function(df, party){
  data <- df %>% group_by(doc_id, sentence_id) %>% mutate(number = 1)
  data1 <- data %>% group_by(doc_id, sentence_id) %>% summarise(m = sum(number))
  data2 <- data1 %>% group_by(doc_id) %>% summarise(avg = mean(m))
  avg <- data2 %>% summarise(avg = mean(avg)) %>% mutate(type = party)
  return(avg)
}

demo_avg <- average(demo_data, "democratic")
repu_avg <- average(repu_data, "republican")

stop_sentence <- c("Thank you America", "FELLOW COUNTRYMEN", "My fellow citizens", "My Fellow citizens", "Thank you very much")

demo_avg <- average(demo_data, "democratic")
repu_avg <- average(repu_data, "republican")

# combine data, and make a ggplot
result <- bind_rows(demo_avg, repu_avg)
ggplot(result, aes(x = type, y = avg, fill = type)) + geom_col(show.legend = T) + geom_text(aes(label = round(avg, 2)), vjust = -0.5) + labs(title = "average of president speech sentence length") + xlab(NULL) + ylab(NULL) + theme_gray() + theme(text = element_text(family = "a", size = 13), plot.title = element_text(hjust = 0.5, size = 17), panel.grid = element_blank(), axis.text.y = element_text(hjust = 1),  legend.position = "bottom")

# save in the database
inaugral_raw <- raw_average(inaugral_data, inaugral_raw_data)
weekly_raw <- raw_average(weekly_data, weekly_raw_data)
union_raw <- raw_average(union_data, union_raw_data)
spoken_raw <- raw_average(spoken_data, spoken_raw_data)

total_raw <- bind_rows(inaugral_raw, weekly_raw, union_raw, spoken_raw) %>% group_by(name) %>% summarise(avg_mean = mean(avg))
inaugral_raw <- inaugral_raw %>% distinct(name, .keep_all = T)

total_raw_result <- total_raw %>% left_join(inaugral_raw, by = "name") %>% select(name, avg_mean, doc_id, party)

dbWriteTable(con, "avg_result", total_raw_result)

# check is it saved
dbListTables(con)

dbDisconnect(con)
