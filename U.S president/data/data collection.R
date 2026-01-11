library(rvest)        
library(dplyr)       
library(stringr)      
library(tidyr)        
library(purrr)       


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

after_1960 <- raw_speech_data %>% filter(!is.na(party))

write.csv(raw_speech_data, "total_raw_data.csv", row.names = FALSE)
write.csv(after_1960_speech, "after_1960_raw_data.csv", row.names = FALSE)

total = read.csv("total_raw_data.csv")

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
View(union_raw_speech_data)
write.csv(union_raw_speech_data, "union.csv", row.names = FALSE)

a = read.csv("union.csv" )
View(a)


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
View(weekly_data)
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
other_data = weekly[[1]]
for(i in 2 : 27){
  other_data = bind_rows(other_data, weekly[[i]]) 
}

colnames(other_data) = c("name", "document", "year")

write.csv(other_data, "weekly.csv", row.names = FALSE)
