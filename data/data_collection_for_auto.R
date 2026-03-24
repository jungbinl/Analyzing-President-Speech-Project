library(rvest)
library(dplyr)
library(stringr)
library(DBI)
library(RMariaDB)
library(dplyr)
library(stringr)

today_str <- format(Sys.Date(), "%B %e, %Y") %>% str_squish()

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port = 3307,
                 user = "root",
                 password = "", 
                 dbname = "president_text_analysis")

# load language model
model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(model$file_model)

# inaugural address

main_url <- "https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/inaugural-addresses?items_per_page=5"
main_page <- read_html(main_url)

links <- address_page %>% 
  html_nodes("div.field-title p a") %>% 
  html_attr("href")

full_links <- paste0("https://www.presidency.ucsb.edu", links)
full_links <- full_links[1:5]

other_data <- data.frame()

for(i in 1:length(full_links)){
  inner_data <- read_html(full_links[i])
  latest_date <- inner_data %>% html_nodes("div.field-docs-start-date-time") %>% html_text(trim = T)
  print(latest_date)
  if(latest_date == today_str) {
    document = inner_data %>% html_nodes("div.field-docs-content") %>% html_text(trim = T)
    name <- inner_data %>% html_nodes("h3.diet-title") %>% html_text()
    year <- inner_data %>% html_nodes("div.field-docs-start-date-time") %>% html_text() %>% str_squish() %>% str_sub(-4) %>% as.numeric()
    temp_df <- data.frame(name = name, document = document, year = year, party = '', type = "I")
    other_data <- bind_rows(other_data, temp_df)
    print("completed")
  }
}

if (nrow(other_data) > 0) {
  dbWriteTable(con, 
               name = "inaugural_address", 
               value = other_data, 
               append = TRUE, 
               row.names = FALSE)
  # load raw data from database
  total_data <- dbReadTable(con, "inagural_address")
  total_data <- tail(total_data, 1)
  total_data <- total_data %>% as_tibble()
  total_data <- total_data %>% mutate(document = str_replace_all(document, "[^A-Za-z]", " "), document = str_squish(document)) %>% mutate(doc_id = row_number())
  
  # inaugural address tokenization
  total_speech = udpipe_annotate(ud_model, x = total_data$document) %>% as.data.frame() 
  total_speech <- total_speech %>% mutate(doc_id = str_replace_all(doc_id, "[^0-9]", ""), doc_id = str_squish(doc_id), doc_id = as.numeric(doc_id))
  total_speech <- total_speech %>% left_join(total_data, by = "doc_id")
  total_speech$raw_token <- total_speech$token
  total_speech$token <- total_speech$lemma %>% tolower() %>% mutate(doc_id = total_data$id)
  total_speech <- total_speech %>%
    select(doc_id, paragraph_id, sentence_id, token_id, token,
           lemma, upos, xpos, head_token_id, dep_rel, name, year, party)
  
  # make a empty dataset
  dbWriteTable(con, "inagural_token", total_speech, append = TRUE, field.types = c(
    doc_id        = "INT",
    paragraph_id  = "INT",
    sentence_id   = "INT",
    token_id      = "INT",
    token         = "VARCHAR(100)",
    lemma         = "VARCHAR(100)",
    upos          = "VARCHAR(5)",
    xpos          = "VARCHAR(4)",
    head_token_id = "INT",
    dep_rel       = "VARCHAR(12)",
    name          = "VARCHAR(100)",
    year          = "INT",
    party         = "VARCHAR(10)"
  ))
  
}

# State of the Union speech
page_url <- 'https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/state-the-union-addresses?items_per_page=5'
page_data <- read_html(page_url)
every_url <- page_data %>% html_nodes("div.field-title a") %>% html_attr("href")

url = paste0('https://www.presidency.ucsb.edu', every_url)
url = url[1:5]

#set raw data
raw_data = data.frame()

old_data <- dbReadTable(con, "union_address")

# collect every president speech
for(i in 1 : length(url)){
  inner_data <- read_html(url[i])
  latest_date <- inner_data %>% html_nodes("div.field-docs-start-date-time") %>% html_text(trim = T)
  print(latest_date)
  if(latest_date == today_str){
    document = inner_data %>% html_nodes("div.field-docs-content") %>% html_text(trim = T)
    name <- inner_data %>% html_nodes("h3.diet-title") %>% html_text()
    year <- inner_data %>% html_nodes("div.field-docs-start-date-time") %>% html_text() %>% str_squish() %>% str_sub(-4) %>% as.numeric()
    temp_df <- data.frame(name = name, document = document, year = year, party = '', type = "U")
    raw_data <- bind_rows(raw_data, temp_df)
    print("completed")
  }
}

if (nrow(raw_data) > 0) {
  combined_data <- bind_rows(old_data, raw_data) %>%
    select(-any_of("id")) %>%           
    arrange(year, name) %>%             
    mutate(id = row_number()) %>%         
    select(id, everything())              

  dbWriteTable(con, 
               name = "union_address", 
               value = combined_data, 
               overwrite = TRUE, 
               row.names = FALSE,
               field.types = c(document = "LONGTEXT"))
  
  print("completed!")
  
  # union data tokenazation
  union_data <- dbReadTable(con, "union_address")
  union_data <- tail(union_data, 1)
  union_data <- union_data %>% as_tibble()
  union_data <- union_data %>% mutate(document = str_replace_all(document, "[^A-Za-z]", " "), document = str_squish(document)) %>% mutate(doc_id = row_number())
  
  # inaugural address tokenization
  total_speech = udpipe_annotate(ud_model, x = union_data$document) %>% as.data.frame() 
  total_speech <- total_speech %>% mutate(doc_id = str_replace_all(doc_id, "[^0-9]", ""), doc_id = str_squish(doc_id), doc_id = as.numeric(doc_id))
  total_speech <- total_speech %>% left_join(union_data, by = "doc_id")
  total_speech$raw_token <- total_speech$token
  total_speech$token <- total_speech$lemma %>% tolower() %>% mutate(doc_id = union_data$id)
  total_speech <- total_speech %>%
    select(doc_id, paragraph_id, sentence_id, token_id, token,
           lemma, upos, xpos, head_token_id, dep_rel, name, year, party)
  dbAppendTable(
    con,
    "union_token",
    total_speech,
    
  )
} else {
  print("not compledted!")
}

# State of the weekly speech
page_url <- 'https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/saturday-weekly-addresses?items_per_page=5'
page_data <- read_html(page_url)
every_url <- page_data %>% html_nodes("div.field-title a") %>% html_attr("href")

url = paste0('https://www.presidency.ucsb.edu', every_url)
url = url[1:5]

#set raw data
raw_data = data.frame()

old_data <- dbReadTable(con, "weekly_address")

# collect every president speech
for(i in 1 : length(url)){
  inner_data <- read_html(url[i])
  latest_date <- inner_data %>% html_nodes("div.field-docs-start-date-time") %>% html_text(trim = T)
  print(latest_date)
  if(latest_date == today_str){
    document = inner_data %>% html_nodes("div.field-docs-content") %>% html_text(trim = T)
    name <- inner_data %>% html_nodes("h3.diet-title") %>% html_text()
    year <- inner_data %>% html_nodes("div.field-docs-start-date-time") %>% html_text() %>% str_squish() %>% str_sub(-4) %>% as.numeric()
    temp_df <- data.frame(name = name, document = document, year = year, party = '', type = "U")
    raw_data <- bind_rows(raw_data, temp_df)
    print("completed")
  }
}

if (nrow(raw_data) > 0) {
  combined_data <- bind_rows(old_data, raw_data) %>%
    select(-any_of("id")) %>%           
    arrange(year, name) %>%             
    mutate(id = row_number()) %>%         
    select(id, everything())              
  
  dbWriteTable(con, 
               name = "weekly_address", 
               value = combined_data, 
               overwrite = TRUE, 
               row.names = FALSE,
               field.types = c(document = "LONGTEXT"))
  
  # load raw data from database
  total_data <- dbReadTable(con, "weekly_address")
  total_data <- tail(total_data, 1)
  total_data <- total_data %>% as_tibble()
  total_data <- total_data %>% mutate(document = str_replace_all(document, "[^A-Za-z]", " "), document = str_squish(document)) %>% mutate(doc_id = row_number())
  
  # weekly address tokenization
  total_speech = udpipe_annotate(ud_model, x = total_data$document) %>% as.data.frame() 
  total_speech <- total_speech %>% mutate(doc_id = str_replace_all(doc_id, "[^0-9]", ""), doc_id = str_squish(doc_id), doc_id = as.numeric(doc_id))
  total_speech <- total_speech %>% left_join(total_data, by = "doc_id")
  total_speech$raw_token <- total_speech$token
  total_speech$token <- total_speech$lemma %>% tolower() %>% mutate(doc_id = total_data$id)
  total_speech <- total_speech %>%
    select(doc_id, paragraph_id, sentence_id, token_id, token,
           lemma, upos, xpos, head_token_id, dep_rel, name, year, party)
  dbAppendTable(
    con,
    "weekly_token",
    total_speech,
    
  )
  
  print("completed!")
} else {
  print("not compledted!")
}

# State of the spoken speech
page_url <- 'https://www.presidency.ucsb.edu/documents/app-categories/presidential/spoken-addresses-and-remarks?items_per_page=5'
page_data <- read_html(page_url)
every_url <- page_data %>% html_nodes("div.field-title a") %>% html_attr("href")

url = paste0('https://www.presidency.ucsb.edu', every_url)
url = url[1:5]

#set raw data
raw_data = data.frame()

old_data <- dbReadTable(con, "spoken_address")

# collect every president speech
for(i in 1 : length(url)){
  inner_data <- read_html(url[i])
  latest_date <- inner_data %>% html_nodes("div.field-docs-start-date-time") %>% html_text(trim = T)
  print(latest_date)
  if(latest_date == today_str){
    document = inner_data %>% html_nodes("div.field-docs-content") %>% html_text(trim = T)
    name <- inner_data %>% html_nodes("h3.diet-title") %>% html_text()
    year <- inner_data %>% html_nodes("div.field-docs-start-date-time") %>% html_text() %>% str_squish() %>% str_sub(-4) %>% as.numeric()
    temp_df <- data.frame(name = name, document = document, year = year, party = '', type = "U")
    raw_data <- bind_rows(raw_data, temp_df)
    print("completed")
  }
}

if (nrow(raw_data) > 0) {
  combined_data <- bind_rows(old_data, raw_data) %>%
    select(-any_of("id")) %>%           
    arrange(year, name) %>%             
    mutate(id = row_number()) %>%         
    select(id, everything())              
  
  dbWriteTable(con, 
               name = "spoken_address", 
               value = combined_data, 
               overwrite = TRUE, 
               row.names = FALSE,
               field.types = c(document = "LONGTEXT"))
  # load raw data from database
  total_data <- dbReadTable(con, "spoken_address")
  total_data <- tail(total_data, 1)
  total_data <- total_data %>% as_tibble()
  total_data <- total_data %>% mutate(document = str_replace_all(document, "[^A-Za-z]", " "), document = str_squish(document)) %>% mutate(doc_id = row_number())
  
  # spoken address tokenization
  total_speech = udpipe_annotate(ud_model, x = total_data$document) %>% as.data.frame() 
  total_speech <- total_speech %>% mutate(doc_id = str_replace_all(doc_id, "[^0-9]", ""), doc_id = str_squish(doc_id), doc_id = as.numeric(doc_id))
  total_speech <- total_speech %>% left_join(total_data, by = "doc_id")
  total_speech$raw_token <- total_speech$token
  total_speech$token <- total_speech$lemma %>% tolower() %>% mutate(doc_id = total_data$id)
  total_speech <- total_speech %>%
    select(doc_id, paragraph_id, sentence_id, token_id, token,
           lemma, upos, xpos, head_token_id, dep_rel, name, year, party)
  dbAppendTable(
    con,
    "spoken_token",
    total_speech,
    
  )
  
  print("completed!")
} else {
  print("not compledted!")
}

