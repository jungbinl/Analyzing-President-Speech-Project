library(udpipe)
library(dplyr)
library(tibble)
library(stringr)
library(parallel)
library(tidyr)
library(DBI)
library(RMariaDB)

# load language model
model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(model$file_model)

# load raw data from database
total_data <- dbReadTable(con, "inagural_address")
total_data <- total_data %>% as_tibble()
total_data <- total_data %>% mutate(document = str_replace_all(document, "[^A-Za-z]", " "), document = str_squish(document)) %>% mutate(doc_id = row_number())

# inaugural address tokenization
total_speech = udpipe_annotate(ud_model, x = total_data$document) %>% as.data.frame() 
total_speech <- total_speech %>% mutate(doc_id = str_replace_all(doc_id, "[^0-9]", ""), doc_id = str_squish(doc_id), doc_id = as.numeric(doc_id))
total_speech <- total_speech %>% left_join(total_data, by = "doc_id")
total_speech$raw_token <- total_speech$token
total_speech$token <- total_speech$lemma %>% tolower()
total_speech <- total_speech %>%
  select(doc_id, paragraph_id, sentence_id, token_id, token,
         lemma, upos, xpos, dep_rel, name, year, party)

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port=3307,
                 user = "root",
                 password = "Jblee0713!!", 
                 dbname = "president_text_analysis") # db name

# make a empty dataset
dbWriteTable(con, "inagural_token", total_speech, overwrite=TRUE, field.types = c(
  doc_id        = "INT",
  paragraph_id  = "INT",
  sentence_id   = "INT",
  token_id      = "INT",
  token         = "VARCHAR(100)",
  lemma         = "VARCHAR(100)",
  upos          = "VARCHAR(5)",
  xpos          = "VARCHAR(4)",
  dep_rel       = "VARCHAR(12)",
  name          = "VARCHAR(100)",
  year          = "INT",
  party         = "VARCHAR(10)"
))

# union data tokenazation

union_data <- dbReadTable(con, "union_address")
union_data <- union_data %>% as_tibble()
union_data <- union_data %>% mutate(document = str_replace_all(document, "[^A-Za-z]", " "), document = str_squish(document)) %>% mutate(doc_id = row_number())

a = 1
b = 30

union_speech = list()

for(i in 1 : floor(nrow(union_data) / 30)){
  temp_data <- union_data[a:b , ]
  union_speech[[i]] = temp_data
  a = a + 30
  b = b + 30
  print(paste0(i, " is completed"))
}

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port=3307,
                 user = "root",
                 password = "Jblee0713!!", 
                 dbname = "president_text_analysis") # db name

# make empty table 
dbWriteTable(
  con,
  "union_token",
  total_speech[0, ],
  overwrite = TRUE,
  field.types = c(
    doc_id        = "INT",
    paragraph_id  = "INT",
    sentence_id   = "INT",
    token_id      = "VARCHAR(3)",
    token         = "VARCHAR(100)",
    lemma         = "VARCHAR(100)",
    upos          = "VARCHAR(5)",
    xpos          = "VARCHAR(4)",
    dep_rel       = "VARCHAR(12)",
    name          = "TEXT",
    year          = "INT",
    party         = "VARCHAR(10)"
  )
)

# store data separate table(too many data)

for(i in 1:floor(nrow(union_data) / 30)) {
  union_token = udpipe_annotate(ud_model, x = union_speech[[i]]$document) %>% as.data.frame()
  union_token <- union_token %>% mutate(
    doc_id = str_replace_all(doc_id, "[^0-9]", ""),
    doc_id = str_squish(doc_id),
    doc_id = as.numeric(doc_id)
  )
  temp_data <- data.frame(
    doc_id = 1 : 30,
    id = (30*(i-1) + 1):(30*i)
  )
  union_token <- union_token %>% left_join(union_speech[[i]], by = "doc_id")
  union_token = left_join(union_token, temp_data, by="doc_id") %>% mutate(doc_id = id)
  union_token$raw_token <- union_token$token
  union_token$token <- union_token$lemma %>% tolower()
  union_token <- union_token %>%
    select(doc_id, paragraph_id, sentence_id, token_id, token,
           lemma, upos, xpos, dep_rel, name, year, party)
  dbAppendTable(
    con,
    "union_token",
    union_token,
    
  )
  print(paste0(i, " page is done"))
}

# disconnect database
dbDisconnect(con)

# weekly data tokenaization

weekly_data <- dbReadTable(con, "weekly_address")
weekly_data <- weekly_data %>% as_tibble()
weekly_data <- weekly_data %>% mutate(document = str_replace_all(document, "[^A-Za-z]", " "), document = str_squish(document)) %>% mutate(doc_id = row_number())

a = 1
b = 30

weekly_speech = list()

for(i in 1 : floor(nrow(weekly_data) / 30)){
  temp_data <- weekly_data[a:b , ]
  weekly_speech[[i]] = temp_data
  a = a + 30
  b = b + 30
  print(paste0(i, " is completed"))
}

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port=3307,
                 user = "root",
                 password = "Jblee0713!!", 
                 dbname = "president_text_analysis") # db name

dbWriteTable(
  con,
  "weekly_token",
  total_speech[0, ],
  overwrite = TRUE,
  field.types = c(
    doc_id        = "INT",
    paragraph_id  = "INT",
    sentence_id   = "INT",
    token_id      = "VARCHAR(3)",
    token         = "VARCHAR(100)",
    lemma         = "VARCHAR(100)",
    upos          = "VARCHAR(5)",
    xpos          = "VARCHAR(4)",
    dep_rel       = "VARCHAR(12)",
    name          = "TEXT",
    year          = "INT",
    party         = "VARCHAR(10)"
  )
)

for(i in 54:floor(nrow(weekly_data) / 30)) {
  weekly_token = udpipe_annotate(ud_model, x = weekly_speech[[i]]$document) %>% as.data.frame()
  weekly_token <- weekly_token %>% mutate(
    doc_id = str_replace_all(doc_id, "[^0-9]", ""),
    doc_id = str_squish(doc_id),
    doc_id = as.numeric(doc_id)
  )
  temp_data <- data.frame(
    doc_id = 1 : 30,
    id = (30*(i-1) + 1):(30*i)
  )
  weekly_token <- weekly_token %>% left_join(weekly_speech[[i]], by = "doc_id")
  weekly_token = left_join(weekly_token, temp_data, by="doc_id") %>% mutate(doc_id = id)
  weekly_token$raw_token <- weekly_token$token
  weekly_token$token <- weekly_token$lemma %>% tolower()
  weekly_token <- weekly_token %>%
    select(doc_id, paragraph_id, sentence_id, token_id, token,
           lemma, upos, xpos, dep_rel, name, year, party)
  dbAppendTable(
    con,
    "weekly_token",
    weekly_token,
    
  )
  print(paste0(i, " page is done"))
}

dbDisconnect(con)

# spoken data tokenaization

spoken_data <- dbReadTable(con, "spoken_address")
spoken_data <- spoken_data %>% as_tibble()
spoken_data <- spoken_data %>% mutate(document = str_replace_all(document, "[^A-Za-z]", " "), document = str_squish(document)) %>% mutate(doc_id = row_number())

a = 1
b = 30

spoken_speech = list()

for(i in 1 : floor(nrow(spoken_data) / 30)){
  temp_data <- spoken_data[a:b , ]
  spoken_speech[[i]] = temp_data
  a = a + 30
  b = b + 30
  print(paste0(i, " is completed"))
}

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port=3307,
                 user = "root",
                 password = "Jblee0713!!", 
                 dbname = "president_text_analysis") # db name

dbWriteTable(
  con,
  "spoken_token",
  total_speech[0, ],
  overwrite = TRUE,
  field.types = c(
    doc_id        = "INT",
    paragraph_id  = "INT",
    sentence_id   = "INT",
    token_id      = "VARCHAR(3)",
    token         = "VARCHAR(100)",
    lemma         = "VARCHAR(100)",
    upos          = "VARCHAR(5)",
    xpos          = "VARCHAR(4)",
    dep_rel       = "VARCHAR(12)",
    name          = "TEXT",
    year          = "INT",
    party         = "VARCHAR(10)"
  )
)

for(i in 1:floor(nrow(spoken_data) / 30)) {
  spoken_token = udpipe_annotate(ud_model, x = spoken_speech[[i]]$document) %>% as.data.frame()
  spoken_token <- spoken_token %>% mutate(
    doc_id = str_replace_all(doc_id, "[^0-9]", ""),
    doc_id = str_squish(doc_id),
    doc_id = as.numeric(doc_id)
  )
  temp_data <- data.frame(
    doc_id = 1 : 30,
    id = (30*(i-1) + 1):(30*i)
  )
  spoken_token <- spoken_token %>% left_join(spoken_speech[[i]], by = "doc_id")
  spoken_token = left_join(spoken_token, temp_data, by="doc_id") %>% mutate(doc_id = id)
  spoken_token$raw_token <- spoken_token$token
  spoken_token$token <- spoken_token$lemma %>% tolower()
  spoken_token <- spoken_token %>%
    select(doc_id, paragraph_id, sentence_id, token_id, token,
           lemma, upos, xpos, dep_rel, name, year, party)
  dbAppendTable(
    con,
    "spoken_token",
    spoken_token,
    
  )
  print(paste0(i, " page is done"))
}

dbDisconnect(con)
