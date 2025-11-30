library(udpipe)
library(dplyr)
library(tibble)
library(stringr)

model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(model$file_model)

total_data <- read.csv("total_raw_data.csv")
total_data <- total_data %>% as_tibble()
total_data <- total_data %>% mutate(document = str_replace_all(document, "[^A-Za-z]", " "), document = str_squish(document)) %>% mutate(doc_id = row_number())

total_speech = udpipe_annotate(ud_model, x = total_data$document) %>% as.data.frame() 
total_speech <- total_speech %>% mutate(doc_id = str_replace_all(doc_id, "[^0-9]", ""), doc_id = str_squish(doc_id), doc_id = as.numeric(doc_id))
total_speech <- total_speech %>% left_join(total_data, by = "doc_id")
total_speech$raw_token <- total_speech$token
total_speech$token <- total_speech$lemma %>% tolower()

after_1960_data <- total_data[total_data$year > 1960, ] %>% as_tibble() %>% mutate(document = str_replace_all(document, "[^A-Za-z]", " "), document = str_squish(document)) %>% mutate(doc_id = row_number())

after_1960_speech = udpipe_annotate(ud_model, x = after_1960_data$document) %>% as.data.frame()
after_1960_speech = after_1960_speech %>% filter(str_count(token) > 1)
after_1960_speech <- after_1960_speech %>% mutate(doc_id = str_replace_all(doc_id, "[^0-9]", ""), doc_id = str_squish(doc_id), doc_id = as.numeric(doc_id))
after_1960_speech <- after_1960_speech %>% left_join(after_1960_data, by = "doc_id")
after_1960_speech$raw_token <- after_1960_speech$token
after_1960_speech$token <- after_1960_speech$lemma %>% tolower()

write.csv(total_speech, "total_speech_data.csv", row.names = FALSE)
write.csv(after_1960_speech, "after_1960_speech.csv", row.names = FALSE)
