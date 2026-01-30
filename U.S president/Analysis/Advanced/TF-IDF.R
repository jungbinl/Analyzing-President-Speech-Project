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

# filtering pos
president <- c("Joseph R. Biden, Jr.", "Barack Obama", "William J. Clinton", "Lyndon B. Johnson", "John F. Kennedy", "Donald J. Trump (2nd Term)", "Donald J. Trump (1st Term)", "George W. Bush", "Ronald Reagan", "Richard Nixon")

total1 = tibble()

tf_idf <- function(name_list){
  for(i in name_list){
  inaugral_data1 <- inaugral_data %>% filter(upos == "NOUN" | upos == "VERB" | upos == "ADV" | upos == "ADJ") %>% filter(name == i)
  inaugral_count <- inaugral_data1 %>% group_by(name, doc_id, token) %>% count(token) %>% filter(str_count(token) > 1) %>% mutate(doc_id = 1)

  weekly_data1 <- weekly_data %>% filter(upos == "NOUN" | upos == "VERB" | upos == "ADV" | upos == "ADJ") %>% filter(name == i)
  weekly_count <- weekly_data1 %>% group_by(name, doc_id, token) %>% count(token) %>% filter(str_count(token) > 1) %>% ungroup() %>% mutate(doc_id = dense_rank(doc_id)+1)

  union_data1 <- union_data %>% filter(upos == "NOUN" | upos == "VERB" | upos == "ADV" | upos == "ADJ") %>% filter(name == i)
  union_count <- union_data1 %>% group_by(name, doc_id, token) %>% count(token) %>% filter(str_count(token) > 1) %>% ungroup() %>% mutate(doc_id = dense_rank(doc_id) + max(weekly_count$doc_id, 0))

  spoken_data1 <- spoken_data %>% filter(upos == "NOUN" | upos == "VERB" | upos == "ADV" | upos == "ADJ") %>% filter(name == i)
  spoken_count <- spoken_data1 %>% group_by(name, doc_id, token) %>% count(token) %>% filter(str_count(token) > 1) %>% ungroup() %>% mutate(doc_id = dense_rank(doc_id) + max(union_count$doc_id,0))
  
  total = bind_rows(inaugral_count, weekly_count, union_count, spoken_count)
  
  tf_idf <- total %>% bind_tf_idf(term = token, document = doc_id, n = n)
  tf_idf <- tf_idf[tf_idf$n > 3, ]
  tf_idf_result <- tf_idf %>% group_by(name, token) %>% summarise(tf_idf = mean(tf_idf))
  
  total1 <- bind_rows(total1, tf_idf_result)
  
  print(paste0(i, " is done"))
  }
  
  return(total1)
}

raw <- raw_data %>% distinct(name, .keep_all = T)

total_raw_result <- tf_idf(president)
total_raw_result <- total_raw_result %>% left_join(raw, by = "name") %>% select("name", "token", "tf_idf", "party")
# get TF-IDF by party 

# make a party columns
party_data <- total_data
party_data <- party_data %>% group_by(party) %>% count(token) %>% filter(str_count(token) > 1)

# get TF-IDF
tf_idf_party <- party_data %>% bind_tf_idf(term = token, document = party, n = n)
tf_idf_party <- tf_idf_party[tf_idf_party$n > 3, ]
tf_idf_party_top10 <- tf_idf_party %>% group_by(party) %>% slice_max(tf_idf, n = 10, with_ties = T)

# make plot
ggplot(tf_idf_party_top10, aes(x = reorder_within(token, tf_idf, party), y = tf_idf, fill = party)) + 
  geom_col(show.legend = F) + 
  coord_flip() + 
  xlab(NULL) +
  ylab(NULL) +
  facet_wrap(~party, scales = "free") + 
  scale_x_reordered() + 
  scale_fill_viridis_d() + 
  theme_bw() + 
  labs(title = "U.S top 10 TF-IDF president inaugural speech by party") +
  theme(text = element_text(family = "a"),
        plot.title = element_text(size = 16, hjust = 0.5))


dbWriteTable(con, "tf_idf_result", total_raw_result)

# check is it saved
dbListTables(con)

dbDisconnect(con)
