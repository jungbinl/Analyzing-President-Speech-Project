library(stringr)
library(dplyr)
library(ggplot2)
library(showtext)
library(scales)
library(tidytext)
library(tidyr)
library(DBI)
library(RMariaDB)

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
demo_data <- dbReadTable(con, "demo_data")
repu_data <- dbReadTable(con, "repu_data")

# too common, too many used
stop_sentence <- c()

# average function
raw_average <- function(df, party){
  data <- df %>% group_by(doc_id, sentence_id) %>% mutate(number = 1)
  data1 <- data %>% group_by(doc_id, sentence_id) %>% summarise(m = sum(number))
  data2 <- data1 %>% group_by(doc_id) %>% summarise(avg = mean(m))
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
demo_raw <- raw_average(demo_data, "democratic")
repu_raw <- raw_average(repu_data, "republican")

dbWriteTable(con, "demo_avg_sentence_result", demo_raw)
dbWriteTable(con, "repu_avg_sentence_result", repu_raw)

# check is it saved
dbListTables(con)
