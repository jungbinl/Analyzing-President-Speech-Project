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
library(tidygraph)
library(ggraph)

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
raw_data <- dbReadTable(con, "president_data") %>% mutate(doc_id = row_number())

spoken_data <- dbReadTable(con, "spoken_token")
spoken_data <- spoken_data %>% filter(party == "democratic" | party == "republican")
inaugral_data <- dbReadTable(con, "inagural_token")
inaugral_data <- inaugral_data  %>% filter(party == "democratic" | party == "republican")
weekly_data <- dbReadTable(con, "weekly_token")
weekly_data <- weekly_data %>% filter(party == "democratic" | party == "republican")
union_data <- dbReadTable(con, "union_token")
union_data <- union_data %>% filter(party == "democratic" | party == "republican")

## odds ratio
df <- bind_rows(demo_data, repu_data)


odd_ratio <- function(df, type = 1) {
  odd_ratio_noun = df[df[, "upos"] == "NOUN", ] %>% count(party, lemma) %>% pivot_wider(
    names_from = "party",
    values_from = n,
    values_fill = list(n = 0)
  )
  odd_ratio_noun <- odd_ratio_noun %>% filter(democratic > 10 & republican > 10 & str_count(lemma) > 2)
  odd_ratio = odd_ratio_noun %>% mutate(
    ratio_demo = ((democratic + 1) / (sum(democratic + 1))),
    ratio_repu = ((republican + 1) / (sum(republican + 1))),
    odds_ratio = (ratio_demo / ratio_repu)
  )
  
  top_odd_ratio <- bind_rows(
    odd_ratio %>% slice_max(order_by = odds_ratio, n = 10),
    odd_ratio %>% slice_min(order_by = odds_ratio, n = 10)
  )
  top_odd_ratio <- top_odd_ratio %>% mutate(party = ifelse(odds_ratio > 1, "democratic", "republican"))
  if(type ==  1){
    return(odd_ratio)
  } else{
    return(top_odd_ratio)
  }
}

raw_odd_ratio <- function(df1, df2) {
  
  combined_raw <- bind_rows(df1, df2) %>%
    filter(upos == "NOUN", str_count(lemma) > 2) %>%
    count(party, lemma) %>%
    filter(n > 10) # 최소 빈도 제한

  total <- combined_raw %>%
    pivot_wider(
      names_from = party,
      values_from = n,
      values_fill = list(n = 0)
    )

  final_data <- total %>%
    mutate(
      ratio_demo = (democratic + 1) / sum(democratic + 1),
      ratio_repu = (republican + 1) / sum(republican + 1),
      odds_ratio = ratio_demo / ratio_repu,
      party_label = if_else(odds_ratio > 1, "democratic", "republican")
    ) %>%
    transmute(
      term = lemma,
      odds_ratio = odds_ratio,
      party = party_label
    )
  
  return(final_data)
}

result <- odd_ratio(df_odd_ratio, 0)
raw_result <- odd_ratio(df_odd_ratio , 1)

ggplot(result, aes(x = reorder_within(lemma, odds_ratio, party), y = odds_ratio, fill = party)) + 
  geom_col(show.legend = T) + 
  geom_text(aes(label = round(odds_ratio, 2)), hjust = 1.2) + 
  facet_wrap(~ party, scales = "free") + 
  coord_flip() + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "odds ratio between democratic and republican president speech") + 
  theme_light() + 
  theme(text = element_text(family = "a", size = 13), plot.title = element_text(hjust = 0.5, size = 17), axis.text.y = element_text(hjust = 1),  legend.position = "bottom") + scale_x_reordered()

## log odds ratio

log_odd_ratio <- function(df, type = 1) {
  data <- odd_ratio(df, 1)
  log_odd_ratio = data %>% mutate(log_odd_ratio = log(data$odds_ratio))
  top_log_odd_ratio <- bind_rows(
    log_odd_ratio %>% slice_max(order_by = log_odd_ratio, n = 10),
    log_odd_ratio %>% slice_min(order_by = log_odd_ratio, n = 10)
  )
  top_log_odd_ratio <- top_log_odd_ratio %>% mutate(party = ifelse(odds_ratio > 1, "democratic", "republican"))
  if(type ==  1){
    return(log_odd_ratio)
  } else{
    return(top_log_odd_ratio)
  }
}

log_result <- log_odd_ratio(df_odd_ratio , 0)
raw_log_result <- log_odd_ratio(df_odd_ratio, 1)

ggplot(log_result, aes(x = reorder(lemma, log_odd_ratio), y = log_odd_ratio, fill = party)) + 
  geom_col(show.legend = T) + 
  geom_text(aes(label = round(log_odd_ratio, 2)), hjust = 1.1) + 
  coord_flip() + 
  xlab(NULL) + ylab(NULL) + 
  labs(title = "log odds ratio between democratic and republican president speech") + 
  theme_gray() + 
  theme(text = element_text(family = "a", size = 13), plot.title = element_text(hjust = 0.5, size = 17), axis.text.y = element_text(hjust = 1),  legend.position = "bottom")

democratic <- c("Joseph R. Biden, Jr.", "Barack Obama", "William J. Clinton", "Lyndon B. Johnson", "John F. Kennedy")
republican <- c("Donald J. Trump (2nd Term)", "Donald J. Trump (1st Term)", "George W. Bush", "Ronald Reagan", "Richard Nixon")

total <- raw_odd_ratio(demo_data, repu_data)

total_data <- total %>% slice_max(order_by = odds_ratio, n = 20)
total_data2 <- total %>% slice_min(order_by = odds_ratio, n = 20)

total <- bind_rows(total_data, total_data2)

log_total <- total %>% mutate(log_odd_ratio = log(odds_ratio))

dbWriteTable(con, "odd_ratio_result", total, overwrite = TRUE)
dbWriteTable(con, "log_odd_ratio_result", log_total)

# check is it saved
dbListTables(con)

dbDisconnect(con)
