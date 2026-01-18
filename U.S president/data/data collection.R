library(rvest)        
library(dplyr)       
library(stringr)      
library(tidyr)        
library(purrr)       
library(DBI)
library(RMariaDB)


# 1.1 data collection
# raw data website, get a total document in the website, however, there is another file. So it is not actually total document 
page_url <- 'https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/inaugural-addresses?items_per_page=5'
page_data <- read_html(page_url)
number <- page_data %>% html_nodes("div.tax-count") %>% html_text()
n = as.numeric(str_extract_all(number, "(?<=of )\\d+"))

#set raw data
raw_data = data.frame(name = c(), document = c(), year = c())

# collect every president inaugural-address
for(i in 1 : n){
  tryCatch({
    url = paste0('https://www.presidency.ucsb.edu/documents/inaugural-address-', i)
    data <- read_html(url)
    document <- data %>% html_nodes("div.field-docs-content") %>% html_text(trim = T)
    name <- data %>% html_nodes("h3.diet-title") %>% html_text()
    year <- data %>% html_nodes("div.field-docs-start-date-time") %>% html_text() %>% str_squish() %>% str_sub(-4) %>% as.numeric()
    raw_data[i, 1] = name
    raw_data[i ,2] = document
    raw_data[i, 3] = year
    Sys.sleep(2)
    print(paste0(name, " is Done and row is ", i))
  }, error = function(e){
    print("document is not found")
  }
  )
}

# collect second, third inaugural-address
url_main <- "https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/inaugural-addresses?items_per_page=60"
address <- read_html(url_main)
type <- address %>% html_nodes("div.field-title") %>% html_text() %>% str_squish() %>% as.data.frame() %>% filter(. != "Inaugural Address", . != "Inaugural Address.")
type$. <- sapply(type$., function(x) {
  x <- tolower(x)          
  x <- gsub(" ", "-", x)   
  x <- gsub("\\.", "", x) 
  x <- gsub("\'", "" , x)
  x <- gsub("of-", "", x)
  return(x)
})
type[6,1] = paste0(type[6,1], "-0")
type[7,1] = paste0(type[7,1], "-1")

other_data = data.frame(name = c(), document = c(), year = c())

for(i in 1:7){
  url_others = paste0("https://www.presidency.ucsb.edu/documents/", type[i , ])
  data <- read_html(url_others)
  document <- data %>% html_nodes("div.field-docs-content") %>% html_text(trim = T)
  name <- data %>% html_nodes("h3.diet-title") %>% html_text()
  year <- data %>% html_nodes("div.field-docs-start-date-time") %>% html_text() %>% str_squish() %>% str_sub(-4) %>% as.numeric()
  other_data[i, 1] = name
  other_data[i ,2] = document
  other_data[i, 3] = year
  Sys.sleep(2)
  print(paste0(name, " is Done and row is ", i))
}



#change column names, and merge
colnames(raw_data) = c("name", "document", "year")
colnames(other_data) = c("name", "document", "year") 
raw_speech_data <- bind_rows(raw_data, other_data)

#1.2 Labeling the political ideology of presidents since 1960
raw_speech_data <-raw_speech_data %>% arrange(-year) %>% mutate(party = NA)

raw_speech_data[1 ,4] = "republican"
raw_speech_data[2 ,4] = "democratic"
raw_speech_data[3 ,4] = "republican"
raw_speech_data[4 ,4] = "democratic"
raw_speech_data[5 ,4] = "democratic"
raw_speech_data[6 ,4] = "republican"
raw_speech_data[7 ,4] = "republican"
raw_speech_data[8 ,4] = "democratic"
raw_speech_data[9 ,4] = "democratic"
raw_speech_data[10 ,4] = "republican"
raw_speech_data[11,4] = "republican"
raw_speech_data[12,4] = "republican"
raw_speech_data[13,4] = "republican"
raw_speech_data[14,4] = "democratic"
raw_speech_data[15,4] = "democratic"

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port=3307,
                 user = "root",
                 password = "Jblee0713!!", 
                 dbname = "president_text_analysis") # db name

temp = raw_speech_data[ , c(1,3,4)]

# store data
dbWriteTable(con, "inagural_address", raw_speech_data, overwrite=TRUE, field.types = c(name = "text", document = "LONGTEXT"))

dbWriteTable(con, "president_data", temp, overwrite=TRUE, field.types = c(name = "text"))

# State of the Union speech

page_url <- 'https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/state-the-union-addresses?items_per_page=60'
page_data <- read_html(page_url)
every_url <- page_data %>% html_nodes("div.field-title a") %>% html_attr("href")
n = 60

#set raw data
raw_data = data.frame(name = c(), document = c(), year = c())

# collect every president speech
for(i in 1 : n){
  tryCatch({
    url = paste0('https://www.presidency.ucsb.edu', every_url[i])
    data <- read_html(url)
    document <- data %>% html_nodes("div.field-docs-content") %>% html_text(trim = T)
    name <- data %>% html_nodes("h3.diet-title") %>% html_text()
    year <- data %>% html_nodes("div.field-docs-start-date-time") %>% html_text() %>% str_squish() %>% str_sub(-4) %>% as.numeric()
    raw_data[i, 1] = name
    raw_data[i ,2] = document
    raw_data[i, 3] = year
    Sys.sleep(2)
    print(paste0(name, " is Done and row is ", i))
  }, error = function(e){
    print("document is not found")
  }
  )
}

union1 <- raw_data

page_url <- 'https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/state-the-union-addresses?items_per_page=60&page=1'
page_data <- read_html(page_url)
every_url <- page_data %>% html_nodes("div.field-title a") %>% html_attr("href")

#set raw data
raw_data = data.frame(name = c(), document = c(), year = c())

# collect every president speech
for(i in 1 : 60){
  tryCatch({
    url = paste0('https://www.presidency.ucsb.edu', every_url[i])
    data <- read_html(url)
    document <- data %>% html_nodes("div.field-docs-content") %>% html_text(trim = T)
    name <- data %>% html_nodes("h3.diet-title") %>% html_text()
    year <- data %>% html_nodes("div.field-docs-start-date-time") %>% html_text() %>% str_squish() %>% str_sub(-4) %>% as.numeric()
    raw_data[i, 1] = name
    raw_data[i ,2] = document
    raw_data[i, 3] = year
    Sys.sleep(2)
    print(paste0(name, " is Done and row is ", i))
  }, error = function(e){
    print("document is not found")
  }
  )
}

union2 = raw_data

colnames(union1) = c("name", "document", "year")
colnames(union2) = c("name", "document", "year") 
union_raw_speech_data <- bind_rows(union1, union2)

union = left_join(union_raw_speech_data, total, by="name") %>% select(name, document.x, year.y, party)
colnames(union) = c("name", "document", "year", "party")

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port=3307,
                 user = "root",
                 password = "Jblee0713!!", 
                 dbname = "president_text_analysis") # db name

# store data
dbWriteTable(con, "union_address", union, overwrite=TRUE, field.types = c(name = "text", document = "LONGTEXT"))

# saturday weekly address
page_url <- 'https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/saturday-weekly-addresses?items_per_page=60'
page_data <- read_html(page_url)
every_url <- page_data %>% html_nodes("div.field-title a") %>% html_attr("href")

#set raw data
raw_data = data.frame(name = c(), document = c(), year = c())

# collect every president inaugural-address
for(i in 1 : 60){
  tryCatch({
    url = paste0('https://www.presidency.ucsb.edu', every_url[i])
    data <- read_html(url)
    document <- data %>% html_nodes("div.field-docs-content") %>% html_text(trim = T)
    name <- data %>% html_nodes("h3.diet-title") %>% html_text()
    year <- data %>% html_nodes("div.field-docs-start-date-time") %>% html_text() %>% str_squish() %>% str_sub(-4) %>% as.numeric()
    raw_data[i, 1] = name
    raw_data[i ,2] = document
    raw_data[i, 3] = year
    Sys.sleep(2)
    print(paste0(name, " is Done and row is ", i))
  }, error = function(e){
    print("document is not found")
  }
  )
}

weekly_data <- raw_data
weekly = list()

# saturday weekly address otehr page
for(j in 1 : 27){
  page_url <- paste0('https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/saturday-weekly-addresses?items_per_page=60&page=', j)
  page_data <- read_html(page_url)
  every_url <- page_data %>% html_nodes("div.field-title a") %>% html_attr("href")
  
  #set raw data
  raw_data = data.frame(name = c(), document = c(), year = c())
  
  # collect every president inaugural-address
  for(i in 1 : 60){
    tryCatch({
      url = paste0('https://www.presidency.ucsb.edu', every_url[i])
      data <- read_html(url)
      document <- data %>% html_nodes("div.field-docs-content") %>% html_text(trim = T)
      name <- data %>% html_nodes("h3.diet-title") %>% html_text()
      year <- data %>% html_nodes("div.field-docs-start-date-time") %>% html_text() %>% str_squish() %>% str_sub(-4) %>% as.numeric()
      raw_data[i, 1] = name
      raw_data[i ,2] = document
      raw_data[i, 3] = year
      Sys.sleep(2)
      print(paste0(name, " is Done and row is ", i))
    }, error = function(e){
      print("document is not found")
    }
    )
  }
  weekly[[j]] = raw_data
  print(paste0(j, "page is finished"))
  rm(page_data)
  gc()
  closeAllConnections()
}

closeAllConnections()

for(i in 2 : 27){
  other_data = bind_rows(other_data, weekly[[i]]) 
}

colnames(other_data) = c("name", "document", "year")

other_data = bind_rows(other_data, weekly_data)

weekly = left_join(other_data, total, by="name") %>% select(name, document.x, year.y, party)
colnames(weekly) = c("name", "document", "year", "party")

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port=3307,
                 user = "root",
                 password = "Jblee0713!!", 
                 dbname = "president_text_analysis") # db name

# store data
dbWriteTable(con, "weekly_address", weekly, overwrite=TRUE, field.types = c(name = "text", document = "LONGTEXT"))

# Spoken address
page_url <- 'https://www.presidency.ucsb.edu/documents/app-categories/presidential/spoken-addresses-and-remarks?items_per_page=10'
page_data <- read_html(page_url)
every_url <- page_data %>% html_nodes("div.field-title a") %>% html_attr("href")
set.seed(Sys.time())
a = sample(1:10, 1)
#set raw data
raw_data = data.frame(name = c(), document = c(), year = c())
# collect every president inaugural-address
url = paste0('https://www.presidency.ucsb.edu', every_url[a])
data <- read_html(url)
document <- data %>% html_nodes("div.field-docs-content") %>% html_text(trim = T)
name <- data %>% html_nodes("h3.diet-title") %>% html_text()
year <- data %>% html_nodes("div.field-docs-start-date-time") %>% html_text() %>% str_squish() %>% str_sub(-4) %>% as.numeric()
raw_data[1, 1] = name
raw_data[1 ,2] = document
raw_data[1, 3] = year

spoken_data <- raw_data

raw_data = data.frame(name = c(),
                      document = c(),
                      year = c())

# spoken address data collection with random sample
for(j in 1:3310) {
  page_url <- paste0(
    'https://www.presidency.ucsb.edu/documents/app-categories/presidential/spoken-addresses-and-remarks?items_per_page=10&page=',
    j
  )
  page_data <- read_html(page_url)
  every_url <- page_data %>% html_nodes("div.field-title a") %>% html_attr("href")
  #set raw data
  
  # collect every president inaugural-address
  tryCatch({
    set.seed(Sys.time())
    a = sample(1:10, 1)
    url = paste0('https://www.presidency.ucsb.edu', every_url[a])
    data <- read_html(url)
    document <- data %>% html_nodes("div.field-docs-content") %>% html_text(trim = T)
    name <- data %>% html_nodes("h3.diet-title") %>% html_text()
    year <- data %>% html_nodes("div.field-docs-start-date-time") %>% html_text() %>% str_squish() %>% str_sub(-4) %>% as.numeric()
    raw_data[j, 1] = name
    raw_data[j, 2] = document
    raw_data[j, 3] = year
    Sys.sleep(2)
    print(paste0(name, " is Done and row is ", j))
  }, error = function(e) {
    print("document is not found")
  })
  print(paste0(j, "page is finished"))
  rm(page_data)
  gc()
  closeAllConnections()
}

spoken_data = bind_rows(spoken_data, raw_data)

colnames(spoken_data) = c("name", "document", "year")

a = left_join(spoken_data, total, by="name") %>% select(name, document.x, year.y, party)
colnames(a) = c("name", "document", "year", "party")

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "127.0.0.1",
                 port=3307,
                 user = "root",
                 password = "Jblee0713!!", 
                 dbname = "president_text_analysis") # db name

# store data
dbWriteTable(con, "spoken_address", a, overwrite=TRUE, field.types = c(name = "text", document = "LONGTEXT"))

# check it is in DB
dbListTables(con)

write.csv(raw_speech_data, "total_raw_data.csv", row.names = FALSE)
write.csv(union_raw_speech_data, "union.csv", row.names = FALSE)
write.csv(other_data, "weekly.csv", row.names = FALSE)
write.csv(a, "spoken.csv", row.names = FALSE)

