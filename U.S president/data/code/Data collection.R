library(httr)
library(stringr)
library(dplyr)
library(rvest)
library(textstem)

## 1. data collection, resource, tokenization ##
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

#1.3 preprocessing
total_data <- raw_speech_data
total_data <- total_data %>% as_tibble()
total_data <- total_data %>% mutate(document = str_replace_all(document, "[^A-Za-z]", " "), document = str_squish(document)) %>% mutate(doc_id = row_number())

after_1960_data <- raw_speech_data[raw_speech_data$year > 1960, ] %>% as_tibble() %>% mutate(document = str_replace_all(document, "[^A-Za-z]", " "), document = str_squish(document)) %>% mutate(doc_id = row_number())

write.csv(total_data, "total_raw_data.csv", row.names = FALSE)
write.csv(after_1960_data, "after_1960_raw_data.csv", row.names = FALSE)
